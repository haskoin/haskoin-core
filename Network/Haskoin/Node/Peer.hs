{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.Haskoin.Node.Peer 
( startPeer
, PeerMessage(..)
, DecodedMerkleBlock(..)
, RemoteHost(..)
, NodeException(..)
) where

import Control.Applicative ((<$>))
import Control.Monad (void, when, unless)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.State.Class 
    ( MonadState
    , get
    , gets
    , modify
    )
import qualified Control.Monad.State as S (evalStateT)
import Control.Exception (Exception, throwIO, throw)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async (withAsync)
import Control.Monad.Logger 
    ( LoggingT
    , MonadLogger
    , runStdoutLoggingT
    , logDebug
    , logInfo
    , logError
    , logWarn
    )

import Data.Word (Word32)
import Data.Typeable (Typeable)
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Conduit.Network (ClientSettings)
import Data.Conduit 
    ( Conduit
    , Sink
    , yield
    , awaitForever
    , ($$), ($=)
    )
import Data.Conduit.TMChan 
    ( TBMChan
    , sourceTBMChan
    , writeTBMChan
    )
import Data.Conduit.Network 
    ( AppData
    , appSink
    , appSource
    )
import qualified Data.Text as T (pack)
import qualified Data.Conduit.Binary as CB (take)
import qualified Data.ByteString as BS (ByteString, null, empty, append)

import Network.Haskoin.Node.Message
import Network.Haskoin.Node.Types
import Network.Haskoin.Crypto
import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Util

-- TODO: Move constants elsewhere ?
minProtocolVersion :: Word32 
minProtocolVersion = 60001

maxHeaders :: Int
maxHeaders = 2000

data PeerSession = PeerSession
    { remoteSettings :: RemoteHost
    , peerChan       :: TBMChan Message
    , mngrChan       :: TBMChan PeerMessage
    , peerVersion    :: Maybe Version
    -- Aggregate all the transaction of a merkle block before sending
    -- them up to the manager
    , inflightMerkle :: Maybe DecodedMerkleBlock
    } 

-- Data sent from peers to the central manager queue
data PeerMessage
    = PeerHandshake RemoteHost Version
    | PeerDisconnect RemoteHost
    | PeerMerkleBlock RemoteHost DecodedMerkleBlock
    | PeerMessage RemoteHost Message   

data RemoteHost = RemoteHost
    { remoteHost :: String
    , remotePort :: Int
    } deriving (Eq, Ord, Show, Read)

data DecodedMerkleBlock = DecodedMerkleBlock
    { decodedMerkle :: MerkleBlock
    , decodedRoot   :: MerkleRoot
    , expectedTxs   :: [TxHash]
    , merkleTxs     :: [Tx]
    } deriving (Eq, Read, Show)

data NodeException
    = NodeException String
    deriving (Eq, Read, Show, Typeable)

instance Exception NodeException

startPeer :: TBMChan Message 
          -> TBMChan PeerMessage 
          -> RemoteHost 
          -> AppData -> IO ()
startPeer pChan mChan remote ad = 
    void $ withAsync peerEncode $ \_ -> 
        runStdoutLoggingT $ flip S.evalStateT session $
            (appSource ad) $= decodeMessage $$ processMessage
  where
    peerEncode = (sourceTBMChan pChan) $= encodeMessage $$ (appSink ad)
    session = PeerSession 
        { remoteSettings = remote
        , mngrChan       = mChan
        , peerChan       = pChan
        , peerVersion    = Nothing
        , inflightMerkle = Nothing
        }

-- Process incomming messages from the remote peer
processMessage :: (MonadLogger m, MonadIO m, MonadState PeerSession m) 
               => Sink Message m ()
processMessage = awaitForever $ \msg -> lift $ do
    remote <- gets remoteSettings
    merkleM <- gets inflightMerkle
    -- After a merkle block we expect to receive transactions related to that
    -- merkle block. As soon as we get a different message than a transaction,
    -- we know that we are done processing the merkle block.
    when (isJust merkleM && not (isTx msg)) $ endMerkleBlock $ fromJust merkleM
    case msg of
        MVersion v     -> processVersion v
        MVerAck        -> processVerAck 
        MMerkleBlock m -> processMerkleBlock m
        MTx t          -> processTx t
        MPing (Ping n) -> sendMessage $ MPong $ Pong n
        _              -> sendManager $ PeerMessage remote msg
  where
    isTx (MTx _) = True
    isTx _       = False

processVersion :: (MonadLogger m, MonadIO m, MonadState PeerSession m) 
               => Version -> m ()
processVersion remoteVer = go =<< get
  where
    go session
        | isJust $ peerVersion session = do
            $(logWarn) "Duplicate version message"
            sendMessage $ MReject $ 
                reject MCVersion RejectDuplicate "Duplicate version message"
            -- Misbehaving = 1
        | version remoteVer < minProtocolVersion = do
            $(logWarn) $ T.pack $ unwords 
                [ "Connected to a peer speaking protocol version"
                , show $ version $ fromJust $ peerVersion session
                , "but need" 
                , show $ minProtocolVersion
                ]
            liftIO $ throwIO $ NodeException "Bad peer version"
        | otherwise = do
            $(logInfo) $ T.pack $ unwords
                [ "Connected to", show $ addrSend remoteVer
                , ": Version =",  show $ version remoteVer 
                , ", subVer =",   show $ userAgent remoteVer 
                , ", services =", show $ services remoteVer 
                , ", time =",     show $ timestamp remoteVer 
                , ", blocks =",   show $ startHeight remoteVer
                ]
            modify $ \s -> s{ peerVersion = Just remoteVer }
            sendMessage MVerAck
            -- Notify the manager that the handshake was succesfull
            remote <- gets remoteSettings
            sendManager $ PeerHandshake remote remoteVer

processVerAck :: MonadLogger m => m ()
processVerAck = $(logInfo) "Version ACK received"

processMerkleBlock :: (MonadIO m, MonadState PeerSession m) 
                   => MerkleBlock -> m ()
processMerkleBlock mb@(MerkleBlock _ ntx hs fs)
    -- TODO: Handle this error better
    | isLeft matchesE = error $ fromLeft matchesE
    | null match      = do
        remote <- gets remoteSettings
        sendManager $ PeerMerkleBlock remote dmb
    | otherwise       = modify $ \s -> s{ inflightMerkle = Just dmb }
  where
    matchesE      = extractMatches fs hs $ fromIntegral ntx
    (root, match) = fromRight matchesE
    dmb           = DecodedMerkleBlock { decodedMerkle = mb
                                       , decodedRoot   = root
                                       , expectedTxs   = match
                                       , merkleTxs     = []
                                       }

processTx :: (MonadIO m, MonadState PeerSession m) => Tx -> m ()
processTx tx = do
    remote <- gets remoteSettings
    merkleM <- gets inflightMerkle
    let dmb@(DecodedMerkleBlock _ _ match txs) = fromJust merkleM
    -- If the transaction is part of a merkle block, buffer it. We will send
    -- everything to the manager together.
    if isJust merkleM
        then 
            if txHash tx `elem` match
                then modify $ \s -> 
                    s{ inflightMerkle = Just dmb{ merkleTxs = tx : txs } }
                else do
                    endMerkleBlock dmb
                    sendManager $ PeerMessage remote $ MTx tx
        else sendManager $ PeerMessage remote $ MTx tx

endMerkleBlock :: (MonadIO m, MonadState PeerSession m) 
               => DecodedMerkleBlock -> m ()
endMerkleBlock dmb@(DecodedMerkleBlock _ _ match txs) = do
    remote <- gets remoteSettings
    modify $ \s -> s{ inflightMerkle = Nothing }
    sendManager $ PeerMerkleBlock remote dmb{ merkleTxs = orderedTxs }
  where
    -- Keep the same transaction order as in the merkle block
    orderedTxs = catMaybes $ matchTemplate txs match f
    f a b      = txHash a == b

sendMessage :: (MonadIO m, MonadState PeerSession m) => Message -> m ()
sendMessage msg = do
    chan <- gets peerChan
    liftIO . atomically $ writeTBMChan chan msg

sendManager :: (MonadIO m, MonadState PeerSession m) => PeerMessage -> m ()
sendManager req = do
    chan <- gets mngrChan
    liftIO . atomically $ writeTBMChan chan req

decodeMessage :: MonadLogger m => Conduit BS.ByteString m Message
decodeMessage = do
    -- Message header is always 24 bytes
    headerBytes <- toStrictBS <$> CB.take 24

    unless (BS.null headerBytes) $ do
        -- Introspection required to know the length of the payload
        let headerE = decodeToEither headerBytes
            (MessageHeader _ cmd len _) = fromRight headerE

        when (isLeft headerE) $ do
            $(logError) $ T.pack $ unwords
                [ "Could not decode message header:"
                , fromLeft headerE
                , "Bytes:"
                , bsToHex headerBytes
                ]
            -- TODO: Is this ground for deconnection or can we recover?
            throw $ NodeException "Could not decode message header"

        payloadBytes <- if len > 0
                            then toStrictBS <$> (CB.take $ fromIntegral len)
                            else return BS.empty

        let resE = decodeToEither $ headerBytes `BS.append` payloadBytes
            res = fromRight resE

        when (isLeft resE) $ do
            $(logError) $ T.pack $ unwords
                [ "Could not decode message payload:"
                , fromLeft resE
                ]
            -- TODO: Is this ground for deconnection or can we recover?
            throw $ NodeException "Could not decode message payload"

        yield res
        decodeMessage

encodeMessage :: MonadIO m => Conduit Message m BS.ByteString
encodeMessage = awaitForever $ yield . encode'

