{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Stratum.Conduit
( -- * Conduit Interface for JSON-RPC
  RPCInMsg(..)
, RPCConduits(..)
, runRPCConduits
, runRPCClientTCP
, runRPCServerTCP
, queryRPC
  -- ** Low-Level Conduit Interface for JSON-RPC
, RPCSession(..)
, initRPCSession
, rpcEncode
, rpcNewMsg
, rpcDecodeMsg
) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.TMChan
import Data.Maybe
import Data.Text (Text)
import Network.Haskoin.Stratum.RPC

-- | RPC conduits. First three types are for outgoing messages: request (qo),
-- notification (no), and response (ro). Next three types are for
-- incoming messages: request (qi), notification (ni) and response
-- (ri). Last is the monad context for the conduits.
data RPCConduits qo no ro qi ni ri m
    = RPCConduits
        { rpcMsgSource :: Source m (RPCInMsg qo qi ni ri)
        , rpcMsgSink   :: Sink (RPCMsg qo no ro) m ()
        }

type RPCReqMap qo = HashMap RPCId (RPCReq qo)

data RPCInMsg qo qi ni ri
    = RPCInMsg  { rpcInMsg :: !(RPCMsg qi ni ri)
                , rpcInReq :: !(Maybe (RPCReq qo))
                }
    | RPCInErr  { rpcInErr :: !RPCErr
                }
    deriving (Eq, Show)

-- | Session state.
data RPCSession qo = RPCSession
    { rpcLastId  :: TVar RPCId             -- ^ Last used id
    , rpcReqMap  :: TVar (RPCReqMap qo)  -- ^ Requests sent
    , rpcLast    :: TVar Bool              -- ^ No more requests
    }

-- | Create initial session.
initRPCSession :: STM (RPCSession qo)
initRPCSession = RPCSession <$> newTVar (RPCIdInt 0)
                            <*> newTVar M.empty
                            <*> newTVar False

-- | Conduit that serializes JSON documents in lines.
rpcEncode :: (ToJSON a, Monad m) => Conduit a m ByteString
rpcEncode = CL.map (L8.toStrict . flip L8.append "\n" . encode)

-- | Conduit for outgoing JSON-RPC messages.
rpcNewMsg :: ( ToJSON qo, ToRPCReq qo, ToJSON no, ToRPCNotif no
             , ToJSON ro, MonadIO m )
          => RPCSession qo
          -> Conduit (RPCMsg qo no ro) m (RPCMsg qo no ro)
rpcNewMsg qs = await >>= \nqM -> case nqM of
    Nothing ->
        liftIO (atomically $ writeTVar (rpcLast qs) True) >> return ()
    Just (RPCMReq rq) -> do
        msg <- RPCMReq <$> liftIO (atomically $ addId rq)
        yield msg >> rpcNewMsg qs
    Just msg ->
        yield msg >> rpcNewMsg qs
  where
    addId rq = case getReqId rq of
        RPCIdNull -> do
            i <- readTVar (rpcLastId qs)
            h <- readTVar (rpcReqMap qs)
            let i'  = succ i
                rq' = rq { getReqId = i' }
                h'  = M.insert i' rq' h
            writeTVar (rpcLastId qs) i'
            writeTVar (rpcReqMap qs) h'
            return rq'
        i -> do
            h <- readTVar (rpcReqMap qs)
            writeTVar (rpcReqMap qs) (M.insert i rq h)
            return rq

-- | Conduit to decode incoming JSON-RPC messages.
-- Left is an error if decoding an incoming request went wrong.
-- Right is the incoming message decoded, with optional request if it is a
-- response.
rpcDecodeMsg
    :: (FromRPCReq qi, FromRPCNotif ni, FromRPCResult ri, MonadIO m)
    => Bool          -- ^ RPCv1
    -> Bool          -- ^ Close on last response
    -> RPCSession qo
    -> Conduit ByteString m (RPCInMsg qo qi ni ri)
rpcDecodeMsg r1 c qs = CB.lines =$= f where
    f = await >>= \bsM -> case bsM of
        Nothing ->
            return ()
        Just bs -> do
            (m, d) <- liftIO . atomically $ decodeSTM bs
            yield m >> unless d f

    decodeSTM bs = readTVar (rpcReqMap qs) >>= \h -> case decodeMsg h bs of
        Right m@(RPCMRes rs) -> do
            (rq, dis) <- requestSTM h $ getResId rs
            return (RPCInMsg m (Just rq), dis)
        Right m@(RPCMErr re) -> case getErrId re of
            Nothing ->
                return (RPCInMsg m Nothing, False)
            Just i -> do
                (rq, dis) <- requestSTM h i
                return (RPCInMsg m (Just rq), dis)
        Right m -> return (RPCInMsg m Nothing, False)
        Left  e -> return (RPCInErr e, False)

    requestSTM h i = do
       let rq = fromJust $ i `M.lookup` h
           h' = M.delete i h
       writeTVar (rpcReqMap qs) h'
       t <- readTVar $ rpcLast qs
       return (rq, c && t && M.null h')

    decodeMsg h bs = case eitherDecodeStrict' bs of
        Left  e -> Left $ rpcParseError r1 e
        Right v -> case parseEither (topParse h) v of
            Left  e -> Left $ rpcParseError r1 e
            Right x -> x

    topParse h v = parseRequest v
               <|> parseNotif v
               <|> parseResponse h v
               <|> return (Left $ rpcInvalidError r1 v)

    parseResponse h = withObject "response" $ \o -> do
        r <- o .:? "result" .!= Null
        e <- o .:? "error"  .!= Null
        when (r == Null && e == Null) mzero
        i <- o .:? "id" .!= RPCIdNull
        rM <- o .:? "jsonrpc"
        let r1' = rM /= Just ("2.0" :: Text)
        case i of
            RPCIdNull ->
                Right . RPCMErr <$> parseRPCErr Nothing o
            _ -> case M.lookup i h of
                Nothing -> return . Left $ rpcIdError r1' i
                Just rq -> do
                    rsE <- parseRPCRes rq (Object o)
                    return $ case rsE of
                        Left  er -> Right $ RPCMErr er
                        Right rs -> Right $ RPCMRes rs

    parseRequest v = flip fmap (parseRPCReq v) $ \rqE -> case rqE of
        Left   e -> Left e
        Right rq -> Right $ RPCMReq rq

    parseNotif v = flip fmap (parseRPCNotif v) $ \rnE -> case rnE of
        Left   e -> Left e
        Right rn -> Right $ RPCMNotif rn

-- | Send requests and get responses (or errors).
queryRPC :: (ToJSON qo, ToRPCReq qo, FromRPCResult ri)
         => Bool    -- ^ RPCv1
         -> [qo]
         -> (RPCConduits qo () () () () ri IO)
         -> IO [RPCInMsg qo () () ri]
queryRPC r1 qs rc = do
    withAsync (rpcMsgSource rc $$ CL.consume) $ \a -> do
    CL.sourceList qs $= CL.map r $$ rpcMsgSink rc
    wait a
  where
    r q = if r1
        then RPCMReq (RPCReq1 (rpcReqMethod q) q RPCIdNull)
        else RPCMReq (RPCReq  (rpcReqMethod q) q RPCIdNull)

-- | Run JSON-RPC Conduits.
runRPCConduits :: ( FromRPCReq    qi
                  , FromRPCNotif  ni
                  , FromRPCResult ri 
                  , ToJSON qo, ToRPCReq   qo
                  , ToJSON no, ToRPCNotif no
                  , ToJSON ro )
               => Bool          -- ^ RPCv1
               -> Bool          -- ^ Disconnect on last response
               -> Sink ByteString IO ()
               -> Source IO ByteString
               -> (RPCConduits qo no ro qi ni ri IO -> IO a)
               -> IO a
runRPCConduits r1 d rpcSnk rpcSrc f = do
    (reqChan, msgChan) <- atomically $ do
        q <- newTBMChan 128
        m <- newTBMChan 128
        return (q, m)
    let inbSrc = sourceTBMChan msgChan
        inbSnk = sinkTBMChan   msgChan True
        outSrc = sourceTBMChan reqChan
        outSnk = sinkTBMChan   reqChan True
    withAsync (rpcThread outSrc inbSnk) (g inbSrc outSnk)
  where
    rpcThread outSrc inbSnk = do
        qs <- atomically initRPCSession
        withAsync (outThread qs outSrc) $ \a -> do
        _ <- rpcSrc $= rpcDecodeMsg r1 d qs $$ inbSnk
        wait a
    outThread qs outSrc =
        outSrc $= rpcNewMsg qs $= rpcEncode $$ rpcSnk
    g inbSrc outSnk a = do
        let src = toProducer inbSrc
            snk = toConsumer outSnk
        x <- f (RPCConduits src snk)
        _ <- wait a
        return x

runRPCClientTCP :: ( FromRPCReq    qi
                   , FromRPCNotif  ni
                   , FromRPCResult ri
                   , ToJSON qo, ToRPCReq   qo
                   , ToJSON no, ToRPCNotif no
                   , ToJSON ro )
                => Bool   -- ^ RPCv1
                -> Bool   -- ^ Disconnect on last response
                -> ClientSettings
                -> (RPCConduits qo no ro qi ni ri IO -> IO a)
                -> IO a
runRPCClientTCP r1 d cs f = runTCPClient cs $ \ad -> do
    runRPCConduits r1 d (appSink ad) (appSource ad) f

runRPCServerTCP :: ( FromRPCReq    qi
                   , FromRPCNotif  ni
                   , FromRPCResult ri
                   , ToJSON qo, ToRPCReq   qo
                   , ToJSON no, ToRPCNotif no
                   , ToJSON ro )
                => Bool   -- ^ RPCv1
                -> ServerSettings
                -> (RPCConduits qo no ro qi ni ri IO -> IO ())
                -> IO ()
runRPCServerTCP r1 ss f = runTCPServer ss $ \cl -> do
    runRPCConduits r1 False (appSink cl) (appSource cl) f

