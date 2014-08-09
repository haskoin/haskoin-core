{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Network.Haskoin.Stratum.Tests (tests) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.List
import Data.Conduit.TMChan
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Text (Text)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Network.JsonRpc
import Network.Haskoin.Stratum
import Network.Haskoin.Stratum.Arbitrary

tests :: [Test]
tests =
    [ testGroup "JSON-RPC Requests"
        [ testProperty "Check fields"
            (reqFields :: Request StratumRequest -> Bool)
        , testProperty "Encode/decode"
            (reqDecode :: Request StratumRequest -> Bool)
        ]
    , testGroup "JSON-RPC Notifications"
        [ testProperty "Check fields"
            (notifFields :: Notif StratumNotif -> Bool)
        , testProperty "Encode/decode"
            (notifDecode :: Notif StratumNotif -> Bool)
        ]
    , testGroup "JSON-RPC Responses"
        [ testProperty "Encode/decode"
            (resDecode :: ReqRes StratumRequest StratumResult -> Bool)
        , testProperty "Bad response id"
            (rpcBadResId :: ReqRes StratumRequest StratumResult -> Bool)
        , testProperty "Error response"
            (rpcErrRes
                :: (ReqRes StratumRequest StratumResult, ErrorObj) -> Bool)
        ]
    , testGroup "JSON-RPC Conduits"
        [ testProperty "Outgoing conduit"
            (newMsgConduit
                :: [Message StratumRequest StratumNotif StratumResult]
                -> Property)
        , testProperty "Decode requests"
            (decodeReqConduit :: ([Request StratumRequest], Ver) -> Property)
        , testProperty "Decode responses" 
            (decodeResConduit
                :: ([ReqRes StratumRequest StratumResult], Ver) -> Property)
        , testProperty "Bad responses" 
            (decodeErrConduit
                :: ([ReqRes StratumRequest StratumResult], Ver) -> Property)
        , testProperty "Sending messages" sendMsgNet
        ]
    ]

--
-- Requests
--

reqFields :: (ToRequest a, ToJSON a) => Request a -> Bool
reqFields rq = case rq of
    Request V1 m p i -> r1ks && vals m p i
    Request V2 m p i -> r2ks && vals m p i
  where
    (Object o) = toJSON rq
    r1ks = sort (M.keys o) == ["id", "method", "params"]
    r2ks = sort (M.keys o) == ["id", "jsonrpc", "method", "params"]
        || sort (M.keys o) == ["id", "jsonrpc", "method"]
    vals m p i = fromMaybe False $ parseMaybe (f m p i) o
    f m p i _ = do
        j <- o .:? "jsonrpc"
        guard $ fromMaybe True $ fmap (== ("2.0" :: Text)) j
        i' <- o .: "id"
        guard $ i == i'
        m' <- o .: "method"
        guard $ m == m'
        p' <- o .:? "params" .!= Null
        guard $ (toJSON p) == p'
        return True

reqDecode :: (Eq a, ToRequest a, ToJSON a, FromRequest a) => Request a -> Bool
reqDecode rq = case parseMaybe parseRequest (toJSON rq) of
    Nothing  -> False
    Just rqE -> either (const False) (rq ==) rqE

--
-- Notifications
--

notifFields :: (ToNotif a, ToJSON a) => Notif a -> Bool
notifFields rn = case rn of
    Notif V1 m p -> n1ks && vals m p
    Notif V2 m p -> n2ks && vals m p
  where
    (Object o) = toJSON rn
    n1ks = sort (M.keys o) == ["id", "method", "params"]
    n2ks = sort (M.keys o) == ["jsonrpc", "method", "params"]
        || sort (M.keys o) == ["jsonrpc", "method"]
    vals m p = fromMaybe False $ parseMaybe (f m p) o
    f m p _ = do
        i <- o .:? "id" .!= Null
        guard $ i == Null
        j <- o .:? "jsonrpc"
        guard $ fromMaybe True $ fmap (== ("2.0" :: Text)) j
        m' <- o .: "method"
        guard $ m == m'
        p' <- o .:? "params" .!= Null
        guard $ (toJSON p) == p'
        return True

notifDecode :: (Eq a, ToNotif a, ToJSON a, FromNotif a)
            => Notif a -> Bool
notifDecode rn = case parseMaybe parseNotif (toJSON rn) of
    Nothing  -> False
    Just rnE -> either (const False) (rn ==) rnE

--
-- Responses
--

resDecode :: (Eq r, ToJSON r, FromResponse r)
          => ReqRes q r -> Bool
resDecode (ReqRes rq rs) = case parseMaybe (parseResponse rq) (toJSON rs) of
    Nothing -> False
    Just rsE -> either (const False) (rs ==) rsE

rpcBadResId :: forall q r. (ToJSON r, FromResponse r)
            => ReqRes q r -> Bool
rpcBadResId (ReqRes rq rs) = case parseMaybe f (toJSON rs') of
    Nothing -> True
    _ -> False
  where
    f :: FromResponse r => Value -> Parser (Either ErrorObj (Response r))
    f = parseResponse rq
    rs' = rs { getResId = IdNull }

rpcErrRes :: forall q r. FromResponse r => (ReqRes q r, ErrorObj) -> Bool
rpcErrRes (ReqRes rq _, re) = case parseMaybe f (toJSON re') of
    Nothing -> False
    Just (Left _) -> True
    _ -> False
  where
    f :: FromResponse r => Value -> Parser (Either ErrorObj (Response r))
    f = parseResponse rq
    re' = re { getErrId = getReqId rq }

--
-- Conduit
--

newMsgConduit :: ( ToRequest q, ToJSON q, ToNotif n, ToJSON n
                 , ToJSON r, FromResponse r )
              => [Message q n r] -> Property
newMsgConduit (snds) = monadicIO $ do
    msgs <- run $ do
        qs <- atomically initSession
        CL.sourceList snds' $= msgConduit False qs $$ CL.consume
    assert $ length msgs == length snds'
    assert $ length (filter rqs msgs) == length (filter rqs snds')
    assert $ map idn (filter rqs msgs) == take (length (filter rqs msgs)) [1..]
  where
    rqs (MsgRequest _) = True
    rqs _ = False
    idn (MsgRequest rq) = getIdInt $ getReqId rq
    idn _ = error "Unexpected request"
    snds' = flip map snds $ \m -> case m of
        (MsgRequest rq) -> MsgRequest $ rq { getReqId = IdNull }
        _ -> m

decodeReqConduit :: forall q. (ToRequest q, FromRequest q, Eq q, ToJSON q)
                 => ([Request q], Ver) -> Property
decodeReqConduit (vs, ver) = monadicIO $ do
    inmsgs <- run $ do
        qs  <- atomically initSession
        qs' <- atomically initSession
        CL.sourceList vs
            $= CL.map f
            $= msgConduit False qs
            $= encodeConduit
            $= decodeConduit ver False qs'
            $$ CL.consume
    assert $ null $ filter unexpected inmsgs
    assert $ all (uncurry match) (zip vs inmsgs)
  where
    unexpected :: IncomingMsg () q () () -> Bool
    unexpected (IncomingMsg (MsgRequest _) Nothing) = False
    unexpected _ = True
    match rq (IncomingMsg (MsgRequest rq') _) =
        rq { getReqId = getReqId rq' } == rq'
    match _ _ = False
    f rq = MsgRequest $ rq { getReqId = IdNull } :: Message q () ()

decodeResConduit :: forall q r.
                    ( ToRequest q, FromRequest q, Eq q, ToJSON q, ToJSON r
                    , FromResponse r, Eq r )
                 => ([ReqRes q r], Ver) -> Property
decodeResConduit (rr, ver) = monadicIO $ do
    inmsgs <- run $ do
        qs  <- atomically initSession
        qs' <- atomically initSession
        CL.sourceList vs
            $= CL.map f
            $= msgConduit False qs
            $= encodeConduit
            $= decodeConduit ver False qs'
            $= CL.map respond
            $= encodeConduit
            $= decodeConduit ver False qs
            $$ CL.consume
    assert $ null $ filter unexpected inmsgs
    assert $ all (uncurry match) (zip vs inmsgs)
  where
    unexpected :: IncomingMsg q () () r -> Bool
    unexpected (IncomingMsg (MsgResponse _) (Just _)) = False
    unexpected _ = True

    match rq (IncomingMsg (MsgResponse rs) (Just rq')) =
        rq { getReqId = getReqId rq' } == rq'
            && rs == g rq'
    match _ _ = False

    respond :: IncomingMsg () q () () -> Response r
    respond (IncomingMsg (MsgRequest rq) Nothing) = g rq
    respond _ = undefined

    f rq = MsgRequest $ rq { getReqId = IdNull } :: Message q () ()
    vs = map (\(ReqRes rq _) -> rq) rr

    g rq = let (ReqRes _ rs) = fromJust $ find h rr
               h (ReqRes rq' _) = getReqParams rq == getReqParams rq'
           in  rs { getResId = getReqId rq }

decodeErrConduit :: forall q r.
                    ( ToRequest q, FromRequest q, Eq q, ToJSON q, ToJSON r
                    , FromResponse r, Eq r, Show r, Show q )
                 => ([ReqRes q r], Ver) -> Property
decodeErrConduit (vs, ver) = monadicIO $ do
    inmsgs <- run $ do
        qs  <- atomically initSession
        qs' <- atomically initSession
        CL.sourceList vs
            $= CL.map f
            $= msgConduit False qs
            $= encodeConduit
            $= decodeConduit ver False qs'
            $= CL.map respond
            $= encodeConduit
            $= decodeConduit ver False qs
            $$ CL.consume
    assert $ null $ filter unexpected inmsgs
    assert $ all (uncurry match) (zip vs inmsgs)
  where
    unexpected :: IncomingMsg q () () r -> Bool
    unexpected (IncomingMsg (MsgError _) (Just _)) = False
    -- unexpected _ = True
    unexpected i = error $ show i

    match (ReqRes rq _) (IncomingMsg (MsgError _) (Just rq')) =
        rq' { getReqId = getReqId rq } == rq
    match _ _ = False

    respond :: IncomingMsg () q () () -> ErrorObj
    respond (IncomingMsg (MsgRequest (Request ver' _ _ i)) Nothing) =
        ErrorObj ver' "test" (getIdInt i) Null i
    respond _ = undefined

    f (ReqRes rq _) = MsgRequest $ rq { getReqId = IdNull } :: Message q () ()

type ClientAppConduits =
    AppConduits StratumRequest StratumNotif StratumResult () () () IO
type ServerAppConduits =
    AppConduits () () () StratumRequest StratumNotif StratumResult IO

sendMsgNet :: ([Message StratumRequest StratumNotif StratumResult], Ver)
           -> Property
sendMsgNet (rs, ver) = monadicIO $ do
    rt <- run $ do
        mv <- newEmptyMVar
        to <- atomically $ newTBMChan 128
        ti <- atomically $ newTBMChan 128
        let tiSink   = sinkTBMChan ti True
            toSource = sourceTBMChan to
            toSink   = sinkTBMChan to True
            tiSource = sourceTBMChan ti
        withAsync (srv tiSink toSource mv) $ \_ ->
            runConduits ver False toSink tiSource (cliApp mv)
    assert $ length rt == length rs
    assert $ all (uncurry match) (zip rs rt)
  where
    srv tiSink toSource mv = runConduits ver False tiSink toSource (srvApp mv)

    srvApp :: MVar [IncomingMsg () StratumRequest StratumNotif StratumResult]
           -> ServerAppConduits -> IO ()
    srvApp mv (src, snk) =
        (CL.sourceNull $$ snk) >> (src $$ CL.consume) >>= putMVar mv

    cliApp :: MVar [IncomingMsg () StratumRequest StratumNotif StratumResult]
           -> ClientAppConduits
           -> IO [IncomingMsg () StratumRequest StratumNotif StratumResult]
    cliApp mv (src, snk) =
        (CL.sourceList rs $$ snk) >> (src $$ CL.sinkNull) >> readMVar mv

    match (MsgRequest rq) (IncomingMsg (MsgRequest rq') Nothing) =
        rq == rq'
    match (MsgNotif rn) (IncomingMsg (MsgNotif rn') Nothing) =
        rn == rn'
    match (MsgResponse _) (IncomingError e) =
        getErrMsg e == "Id not recognized"
    match (MsgError e) (IncomingMsg (MsgError e') Nothing) =
        getErrMsg e == getErrMsg e'
    match (MsgError _) (IncomingError e) =
        getErrMsg e == "Id not recognized"
    match _ _ = False

