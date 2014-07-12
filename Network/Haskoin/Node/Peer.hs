{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.Peer 
( ManagerRequest(..)
, DecodedMerkleBlock(..)
, MerkleRoot
, TxHash
, peer
) where

import Control.Applicative 
import Control.Monad 
import Control.Monad.Trans
import Control.Monad.Logger 
import Control.Monad.Trans.Resource
import Control.Concurrent (forkFinally, ThreadId, myThreadId)
import Control.Exception (throwIO, throw)
import qualified Control.Monad.State as S
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM

import Database.Persist.Sqlite

import Data.Maybe
import Data.Word
import qualified Data.Text as T
import Data.Conduit 
    ( Conduit
    , Sink
    , yield
    , awaitForever
    , ($$), ($=)
    )
import Data.Conduit.Network 
    ( AppData
    , appSink
    , appSource
    )
import Data.Conduit.TMChan

import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as BS

import Network.Haskoin.Node.HeaderChain
import Network.Haskoin.Node.Util
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Util

type PeerHandle = S.StateT PeerSession (LoggingT IO)
type MerkleRoot = Word256

data PeerSession = PeerSession
    { peerId         :: ThreadId
    , peerChan       :: TBMChan Message
    , mngrChan       :: TBMChan (ThreadId, ManagerRequest)
    , peerVersion    :: Maybe Version
    -- Aggregate all the transaction of a merkle block before sending
    -- them up to the manager
    , inflightMerkle :: Maybe DecodedMerkleBlock
    } 

-- Data sent from peers to the central manager queue
data ManagerRequest
    = PeerHandshake Version
    | PeerDisconnect 
    | PeerMerkleBlock DecodedMerkleBlock
    | PeerMessage Message   
    | UserBloomFilter BloomFilter
    | UserPublishTx Tx
    | UserRequestTx [TxHash]

data DecodedMerkleBlock = DecodedMerkleBlock
    { decodedMerkle :: MerkleBlock
    , decodedRoot   :: MerkleRoot
    , expectedTxs   :: [TxHash]
    , merkleTxs     :: [Tx]
    }

-- TODO: Move constants elsewhere ?
minProtocolVersion :: Word32 
minProtocolVersion = 60001

maxHeaders :: Int
maxHeaders = 2000

peer :: TBMChan Message -> TBMChan (ThreadId, ManagerRequest)
     -> AppData -> IO ()
peer pChan mChan remote = do
    tid <- myThreadId 
    let session = PeerSession { peerId         = tid
                              , mngrChan       = mChan
                              , peerChan       = pChan
                              , peerVersion    = Nothing
                              , inflightMerkle = Nothing
                              }
    -- Thread sending messages to the remote peer
    -- TODO: Make sure that we catch a socket disconnect here. We want the
    -- cleanup code in PeerManager to run in such a case.
    let pipe = (sourceTBMChan $ peerChan session)
               $= encodeMessage 
               $$ (appSink remote)
    -- TODO: Handle the error here
    _ <- forkFinally pipe $ \ret -> case ret of
        Left e -> throwIO e
        Right x -> error $ unwords
            [ "Thread stopped with result:"
            , show x
            ]

    -- process incomming messages from the remote peer
    runStdoutLoggingT $ flip S.evalStateT session $
        (appSource remote) $= decodeMessage $$ processMessage
         
processMessage :: Sink Message PeerHandle ()
processMessage = awaitForever $ \msg -> lift $ do
    merkleM <- S.gets inflightMerkle
    when (isJust merkleM && not (isTx msg)) $ do
        let dmb = fromJust merkleM
            txs = merkleTxs dmb
        S.modify $ \s -> s{ inflightMerkle = Nothing }
        -- Keep the same transaction order as in the merkle block
        let orderedTxs = catMaybes $ matchTemplate txs (expectedTxs dmb) f
            f a b      = txHash a == b
        sendManager $ PeerMerkleBlock dmb{ merkleTxs = orderedTxs }
    case msg of
        MVersion v     -> processVersion v
        MVerAck        -> processVerAck 
        MMerkleBlock m -> processMerkleBlock m
        MTx t          -> processTx t
        _              -> sendManager $ PeerMessage msg
  where
    isTx (MTx _) = True
    isTx _       = False

processVersion :: Version -> PeerHandle ()
processVersion remoteVer = go =<< S.get
  where
    go session
        | isJust $ peerVersion session = do
            $(logDebug) "Duplicate version message"
            sendMessage $ MReject $ 
                reject MCVersion RejectDuplicate "Duplicate version message"
            -- Misbehaving = 1
        | version remoteVer < minProtocolVersion = do
            $(logDebug) $ T.pack $ unwords 
                [ "Connected to a peer speaking protocol version"
                , show $ version $ fromJust $ peerVersion session
                , "but need" 
                , show $ minProtocolVersion
                ]
            liftIO $ throwIO $ ProtocolException "Bad peer version"
        | otherwise = do
            $(logInfo) $ T.pack $ unwords
                [ "Connected to", show $ addrSend remoteVer
                , ": Version =",  show $ version remoteVer 
                , ", subVer =",   show $ userAgent remoteVer 
                , ", services =", show $ services remoteVer 
                , ", time =",     show $ timestamp remoteVer 
                , ", blocks =",   show $ startHeight remoteVer
                ]
            S.modify $ \s -> s{ peerVersion = Just remoteVer }
            sendMessage MVerAck
            -- Notify the manager that the handshake was succesfull
            sendManager $ PeerHandshake remoteVer

processVerAck :: PeerHandle ()
processVerAck = do
    $(logDebug) "Version ACK received."

processMerkleBlock :: MerkleBlock -> PeerHandle ()
processMerkleBlock mb@(MerkleBlock h ntx hs fs)
    -- TODO: Handle this error better
    | isLeft matchesE = error $ fromLeft matchesE
    | null match      = sendManager $ PeerMerkleBlock dmb
    | otherwise       = S.modify $ \s -> s{ inflightMerkle = Just dmb }
  where
    matchesE      = extractMatches fs hs $ fromIntegral ntx
    (root, match) = fromRight matchesE
    dmb           = DecodedMerkleBlock { decodedMerkle = mb
                                       , decodedRoot   = root
                                       , expectedTxs   = match
                                       , merkleTxs     = []
                                       }

processTx :: Tx -> PeerHandle ()
processTx tx = do
    merkleM <- S.gets inflightMerkle
    let dmb@(DecodedMerkleBlock mb root match txs) = fromJust merkleM
    -- If the transaction is part of a merkle block, buffer it. We will send
    -- everything to the manager together.
    if isJust merkleM
        then 
            if txHash tx `elem` match
                then S.modify $ \s -> 
                    s{ inflightMerkle = Just dmb{ merkleTxs = tx : txs } }
                else do
                    S.modify $ \s -> s{ inflightMerkle = Nothing }
                    -- Keep the same transaction order as in the merkle block
                    let orderedTxs = catMaybes $ matchTemplate txs match f
                        f a b      = txHash a == b
                    sendManager $ PeerMerkleBlock dmb{ merkleTxs = orderedTxs }
                    sendManager $ PeerMessage $ MTx tx
        else sendManager $ PeerMessage $ MTx tx

sendMessage :: Message -> PeerHandle ()
sendMessage msg = do
    chan <- S.gets peerChan
    liftIO . atomically $ writeTBMChan chan msg

sendManager :: ManagerRequest -> PeerHandle ()
sendManager req = do
    pid  <- S.gets peerId
    chan <- S.gets mngrChan
    liftIO . atomically $ writeTBMChan chan (pid,req)

decodeMessage :: MonadLogger m => Conduit BS.ByteString m Message
decodeMessage = do
    -- Message header is always 24 bytes
    headerBytes <- toStrictBS <$> CB.take 24
    -- Introspection required to know the length of the payload
    let headerE                     = decodeToEither headerBytes
        (MessageHeader _ cmd len _) = fromRight headerE

    when (isLeft headerE) $ do
        $(logError) $ T.pack $ unwords
            [ "Could not decode message header:"
            , fromLeft headerE 
            , "Bytes:"
            , bsToHex headerBytes
            ]
        -- TODO: Is this ground for deconnection or can we recover?
        throw $ ProtocolException "Could not decode message header"

    payloadBytes <- if len > 0
                        then toStrictBS <$> (CB.take $ fromIntegral len)
                        else return BS.empty

    let resE = decodeToEither $ headerBytes `BS.append` payloadBytes
        res  = fromRight resE

    when (isLeft resE) $ do
        $(logError) $ T.pack $ unwords
            [ "Could not decode message payload:"
            , fromLeft resE
            ]
        -- TODO: Is this ground for deconnection or can we recover?
        throw $ ProtocolException "Could not decode message payload"

    yield res
    decodeMessage

encodeMessage :: MonadIO m => Conduit Message m BS.ByteString
encodeMessage = awaitForever $ yield . encode'


