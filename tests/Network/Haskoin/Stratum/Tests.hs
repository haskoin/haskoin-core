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
import Network.Haskoin.Stratum.RPC
import Network.Haskoin.Stratum.Types
import Network.Haskoin.Stratum.Conduit
import Network.Haskoin.Stratum.Arbitrary

type StratumReqRes = ReqRes StratumRequest StratumResult
type StratumMsg = RPCMsg StratumRequest StratumNotif StratumResult

tests :: [Test]
tests =
    [ testGroup "JSON-RPC Requests"
        [ testProperty "Check fields"
            (reqFields :: RPCReq Value -> Bool)
        , testProperty "Encode/decode"
            (reqDecode :: RPCReq Value -> Bool)
        ]
    , testGroup "JSON-RPC Notifications"
        [ testProperty "Check fields"
            (notifFields :: RPCNotif Value -> Bool)
        , testProperty "Encode/decode"
            (notifDecode :: RPCNotif Value -> Bool)
        ]
    , testGroup "JSON-RPC Responses"
        [ testProperty "Check fields"
            (resFields :: RPCRes Value -> Bool)
        , testProperty "Encode/decode"
            (resDecode :: ReqRes Value Value -> Bool)
        , testProperty "Bad response id"
            (rpcBadResId :: ReqRes Value Value -> Property)
        , testProperty "Error response"
            (rpcErrRes :: (ReqRes Value Value, RPCErr) -> Bool)
        ]
    , testGroup "JSON-RPC Conduits"
        [ testProperty "Outgoing conduit"
            (newMsgConduit :: [RPCMsg Value Value Value] -> Property)
        , testProperty "Decode requests"
            (decodeReqConduit :: ([RPCReq Value], Bool) -> Property)
        , testProperty "Decode responses" 
            (decodeResConduit :: ([ReqRes Value Value], Bool) -> Property)
        , testProperty "Bad responses" 
            (decodeErrConduit :: ([ReqRes Value Value], Bool) -> Property)
        , testProperty "Sending messages" sendMsgNet
        , testProperty "Two-way communication" twoWayNet
        ]
    , testGroup "Stratum Messages"
        [ testProperty "Encode/decode requests"
            (reqDecode :: RPCReq StratumRequest -> Bool)
        , testProperty "Encode/decode notifications"
            (notifDecode :: RPCNotif StratumNotif -> Bool)
        , testProperty "Encode/decode responses"
            (resDecode :: StratumReqRes -> Bool)
        , testProperty "Bad response id"
            (rpcBadResId :: StratumReqRes -> Property)
        , testProperty "Error response"
            (rpcErrRes :: (StratumReqRes, RPCErr) -> Bool)
        , testProperty "Outgoing conduit"
            (newMsgConduit :: [StratumMsg] -> Property)
        , testProperty "Decode requests via conduit"
            (decodeReqConduit :: ([RPCReq StratumRequest], Bool) -> Property)
        , testProperty "Decode responses via conduit" 
            (decodeResConduit :: ([StratumReqRes], Bool) -> Property)
        , testProperty "Bad responses via conduit" 
            (decodeErrConduit :: ([StratumReqRes], Bool) -> Property)
        ]
    ]

--
-- Requests
--

reqFields :: (ToRPCReq a, ToJSON a) => RPCReq a -> Bool
reqFields rq = case rq of
    RPCReq1 m p i -> r1ks && vals m p i
    RPCReq  m p i -> r2ks && vals m p i
  where
    (Object o) = toJSON rq
    r1ks = sort (M.keys o) == ["id", "method", "params"]
    r2ks = sort (M.keys o) == ["id", "jsonrpc", "method", "params"]
        || sort (M.keys o) == ["id", "jsonrpc", "method"]
    vals m p i = fromMaybe False $ parseMaybe (f m p i) o
    f m p i _ = do
        jM <- o .:? "jsonrpc"
        guard $ fromMaybe True $ fmap (== ("2.0" :: Text)) jM
        i' <- o .: "id"
        guard $ i == i'
        m' <- o .: "method"
        guard $ m == m'
        p' <- o .:? "params" .!= Null
        guard $ (toJSON p) == p'
        return True

reqDecode :: (Eq a, ToRPCReq a, ToJSON a, FromRPCReq a) => RPCReq a -> Bool
reqDecode rq = case parseMaybe parseRPCReq (toJSON rq) of
    Nothing  -> False
    Just rqE -> either (const False) (rq ==) rqE

--
-- Notifications
--

notifFields :: (ToRPCNotif a, ToJSON a) => RPCNotif a -> Bool
notifFields rn = case rn of
    RPCNotif1 m p -> n1ks && vals m p
    RPCNotif  m p -> n2ks && vals m p
  where
    (Object o) = toJSON rn
    n1ks = sort (M.keys o) == ["id", "method", "params"]
    n2ks = sort (M.keys o) == ["jsonrpc", "method", "params"]
        || sort (M.keys o) == ["jsonrpc", "method"]
    vals m p = fromMaybe False $ parseMaybe (f m p) o
    f m p _ = do
        i <- o .:? "id" .!= Null
        guard $ i == Null
        jM <- o .:? "jsonrpc"
        guard $ fromMaybe True $ fmap (== ("2.0" :: Text)) jM
        m' <- o .: "method"
        guard $ m == m'
        p' <- o .:? "params" .!= Null
        guard $ (toJSON p) == p'
        return True

notifDecode :: (Eq a, ToRPCNotif a, ToJSON a, FromRPCNotif a)
            => RPCNotif a -> Bool
notifDecode rn = case parseMaybe parseRPCNotif (toJSON rn) of
    Nothing  -> False
    Just rnE -> either (const False) (rn ==) rnE

--
-- Responses
--

resFields :: (Eq a, ToJSON a, FromJSON a) => RPCRes a -> Bool
resFields rs = case rs of
    RPCRes1 s i -> s1ks && vals s i
    RPCRes  s i -> s2ks && vals s i
  where
    (Object o) = toJSON rs
    s1ks = sort (M.keys o) == ["error", "id", "result"]
    s2ks = sort (M.keys o) == ["id", "jsonrpc", "result"]
    vals s i = fromMaybe False $ parseMaybe (f s i) o
    f s i _ = do
        i' <- o .: "id"
        guard $ i == i'
        jM <- o .:? "jsonrpc"
        guard $ fromMaybe True $ fmap (== ("2.0" :: Text)) jM
        s' <- o .: "result"
        guard $ s == s'
        e <- o .:? "error" .!= Null
        guard $ e == Null
        return True

resDecode :: (Eq r, ToJSON r, FromRPCResult r)
          => ReqRes q r -> Bool
resDecode (ReqRes rq rs) = case parseMaybe (parseRPCRes rq') (toJSON rs) of
    Nothing -> False
    Just rsE -> either (const False) (rs ==) rsE
  where
    rq' = rq { getReqId = getResId rs }

rpcBadResId :: forall q r. (ToJSON r, FromRPCResult r)
            => ReqRes q r -> Property
rpcBadResId (ReqRes rq rs) = getReqId rq /= getResId rs ==>
    case parseMaybe f (toJSON rs) of
        Nothing -> True
        _ -> False
  where
    f :: FromRPCResult r => Value -> Parser (RPCEither (RPCRes r))
    f = parseRPCRes rq

rpcErrRes :: forall q r. FromRPCResult r => (ReqRes q r, RPCErr) -> Bool
rpcErrRes (ReqRes rq _, re) = case parseMaybe f (toJSON re') of
    Nothing -> False
    Just (Left _) -> True
    _ -> False
  where
    f :: FromRPCResult r => Value -> Parser (RPCEither (RPCRes r))
    f = parseRPCRes rq
    re' = re { getErrId = Just $ getReqId rq }

--
-- Conduit
--

newMsgConduit :: ( ToRPCReq q, ToJSON q, ToRPCNotif n, ToJSON n
                 , ToJSON r, FromRPCResult r )
              => [RPCMsg q n r] -> Property
newMsgConduit (snds) = monadicIO $ do
    msgs <- run $ do
        qs <- atomically initRPCSession
        CL.sourceList snds' $= rpcNewMsg qs $$ CL.consume
    assert $ length msgs == length snds'
    assert $ length (filter rqs msgs) == length (filter rqs snds')
    assert $ map idn (filter rqs msgs) == take (length (filter rqs msgs)) [1..]
  where
    rqs (RPCMReq _) = True
    rqs _ = False
    idn (RPCMReq rq) = getIdInt $ getReqId rq
    idn _ = error "Unexpected request"
    snds' = flip map snds $ \m -> case m of
        (RPCMReq rq) -> RPCMReq $ rq { getReqId = RPCIdNull }
        _ -> m

decodeReqConduit :: forall q. (ToRPCReq q, FromRPCReq q, Eq q, ToJSON q)
                 => ([RPCReq q], Bool) -> Property
decodeReqConduit (vs, r1) = monadicIO $ do
    inmsgs <- run $ do
        qs  <- atomically initRPCSession
        qs' <- atomically initRPCSession
        CL.sourceList vs
            $= CL.map f
            $= rpcNewMsg qs
            $= rpcEncode
            $= rpcDecodeMsg r1 True qs'
            $$ CL.consume
    assert $ null $ filter unexpected inmsgs
    assert $ all (uncurry match) (zip vs inmsgs)
  where
    unexpected :: RPCInMsg () q () () -> Bool
    unexpected (RPCInMsg (RPCMReq _) Nothing) = False
    unexpected _ = True
    match rq (RPCInMsg (RPCMReq rq') _) = rq { getReqId = getReqId rq' } == rq'
    match _ _ = False
    f rq = RPCMReq $ rq { getReqId = RPCIdNull } :: RPCMsg q () ()

decodeResConduit :: forall q r.
                    ( ToRPCReq q, FromRPCReq q, Eq q, ToJSON q, ToJSON r
                    , FromRPCResult r, Eq r )
                 => ([ReqRes q r], Bool) -> Property
decodeResConduit (rr, r1) = monadicIO $ do
    inmsgs <- run $ do
        qs  <- atomically initRPCSession
        qs' <- atomically initRPCSession
        CL.sourceList vs
            $= CL.map f
            $= rpcNewMsg qs
            $= rpcEncode
            $= rpcDecodeMsg r1 True qs'
            $= CL.map respond
            $= rpcEncode
            $= rpcDecodeMsg r1 True qs
            $$ CL.consume
    assert $ null $ filter unexpected inmsgs
    assert $ all (uncurry match) (zip vs inmsgs)
  where
    unexpected :: RPCInMsg q () () r -> Bool
    unexpected (RPCInMsg (RPCMRes _) (Just _)) = False
    unexpected _ = True
    match rq (RPCInMsg (RPCMRes rs) (Just rq')) =
        rq { getReqId = getReqId rq' } == rq'
            && getResult rs == getResult (g rq)
    match _ _ = False
    respond :: RPCInMsg () q () () -> RPCRes r
    respond (RPCInMsg (RPCMReq rq) Nothing) = g rq
    respond _ = undefined
    f rq = RPCMReq $ rq { getReqId = RPCIdNull } :: RPCMsg q () ()
    vs = map (\(ReqRes rq _) -> rq) rr
    g rq = let (ReqRes _ rs) = fromJust $ find h rr
               h (ReqRes rq' _) = getReqParams rq == getReqParams rq'
           in  rs { getResId = getReqId rq }

decodeErrConduit :: forall q r.
                    ( ToRPCReq q, FromRPCReq q, Eq q, ToJSON q, ToJSON r
                    , FromRPCResult r, Eq r )
                 => ([ReqRes q r], Bool) -> Property
decodeErrConduit (rr, r1) = monadicIO $ do
    inmsgs <- run $ do
        qs  <- atomically initRPCSession
        qs' <- atomically initRPCSession
        CL.sourceList vs
            $= CL.map f
            $= rpcNewMsg qs
            $= rpcEncode
            $= rpcDecodeMsg r1 True qs'
            $= CL.map respond
            $= rpcEncode
            $= rpcDecodeMsg r1 True qs
            $$ CL.consume
    assert $ null $ filter unexpected inmsgs
    assert $ all (uncurry match) (zip vs inmsgs)
  where
    unexpected :: RPCInMsg q () () r -> Bool
    unexpected (RPCInMsg (RPCMErr _) (Just _)) = False
    unexpected _ = True

    match rq (RPCInMsg (RPCMErr _) (Just rq')) =
        rq' { getReqId = getReqId rq } == rq
    match _ _ = False

    respond :: RPCInMsg () q () () -> RPCErr
    respond (RPCInMsg (RPCMReq (RPCReq  _ _ i)) Nothing) =
        RPCErr (RPCErrObj "test" (getIdInt i) Null) (Just i)
    respond (RPCInMsg (RPCMReq (RPCReq1 _ _ i)) Nothing) =
        RPCErr1 "test" (Just i)
    respond _ = undefined

    f rq = RPCMReq $ rq { getReqId = RPCIdNull } :: RPCMsg q () ()
    vs = map (\(ReqRes rq _) -> rq) rr

type SendMsgConduitClient = RPCConduits Value Value Value () () () IO
type SendMsgConduitServer = RPCConduits () () () Value Value Value IO

sendMsgNet :: ([RPCMsg Value Value Value], Bool) -> Property
sendMsgNet (rs, r1) = monadicIO $ do
    rt <- run $ do
        mv <- newEmptyMVar
        to <- atomically $ newTBMChan 128
        ti <- atomically $ newTBMChan 128
        let tiSink   = sinkTBMChan ti True
            toSource = sourceTBMChan to
            toSink   = sinkTBMChan to True
            tiSource = sourceTBMChan ti
        withAsync (srv tiSink toSource mv) $ \_ -> do
        runRPCConduits r1 False toSink tiSource $ \c -> do
        CL.sourceList rs $$ rpcMsgSink (c :: SendMsgConduitClient)
        rpcMsgSource c $$ CL.sinkNull
        readMVar mv
    assert $ length rt == length rs
    assert $ all (uncurry match) (zip rs rt)
  where
    srv tiSink toSource mv =
        runRPCConduits r1 False tiSink toSource $ \c -> do
        msgs <- rpcMsgSource (c :: SendMsgConduitServer) $$ CL.consume
        CL.sourceNull $$ rpcMsgSink c
        putMVar mv msgs
    match (RPCMReq rq@(RPCReq _ _ _))
        (RPCInMsg (RPCMReq rq'@(RPCReq _ _ _)) Nothing) =
        rq == rq'
    match (RPCMReq rq@(RPCReq1 _ _ _))
        (RPCInMsg (RPCMReq rq'@(RPCReq1 _ _ _)) Nothing) =
        rq == rq'
    match (RPCMNotif rn@(RPCNotif _ _))
        (RPCInMsg (RPCMNotif rn'@(RPCNotif _ _)) Nothing) =
        rn == rn'
    match (RPCMNotif rn@(RPCNotif1 _ _))
        (RPCInMsg (RPCMNotif rn'@(RPCNotif1 _ _)) Nothing) =
        rn == rn'
    match (RPCMRes _)
        (RPCInErr (RPCErr1 e _)) =
        take 17 e == "Id not recognized"
    match (RPCMRes rs')
        (RPCInErr (RPCErr (RPCErrObj _ c i') Nothing)) =
        toJSON (getResId rs') == i' && c == (-32000)
    match (RPCMErr e@(RPCErr1 _ Nothing))
        (RPCInMsg (RPCMErr e'@(RPCErr1 _ _)) Nothing) =
        e == e'
    match (RPCMErr e@(RPCErr  _ Nothing))
        (RPCInMsg (RPCMErr e'@(RPCErr  _ _)) Nothing) =
        e == e'
    match (RPCMErr _)
        (RPCInErr (RPCErr1 e Nothing)) =
        take 17 e == "Id not recognized"
    match (RPCMErr e)
        (RPCInErr (RPCErr (RPCErrObj _ c i') Nothing)) =
        toJSON (getErrId e) == i' && c == (-32000)
    match v v' = error $ "Sent: " ++ show v ++ "\n" ++ "Received: " ++ show v'

type TwoWayConduit = RPCConduits Value Value Value Value Value Value IO

twoWayNet :: ([RPCMsg Value Value Value], Bool) -> Property
twoWayNet (rr, r1) = monadicIO $ do
    rt <- run $ do
        to <- atomically $ newTBMChan 128
        ti <- atomically $ newTBMChan 128
        let tiSink   = sinkTBMChan ti True
            toSource = sourceTBMChan to
            toSink   = sinkTBMChan to True
            tiSource = sourceTBMChan ti
        withAsync (srv tiSink toSource) $ \_ -> do
        runRPCConduits r1 False toSink tiSource $ \c -> do
        CL.sourceList rs $$ rpcMsgSink (c :: TwoWayConduit)
        rpcMsgSource c $$ CL.consume
    assert $ length rt == length rs
    assert $ all (uncurry match) (zip rs rt)
  where
    rs = map f rr where
        f (RPCMReq rq) = RPCMReq $ rq { getReqId = RPCIdNull }
        f m = m
    srv tiSink toSource =
        runRPCConduits r1 False tiSink toSource $ \c -> do
        rpcMsgSource (c :: TwoWayConduit) $= CL.map respond $$ rpcMsgSink c

    respond (RPCInErr e) =
        RPCMErr e
    respond (RPCInMsg (RPCMReq (RPCReq _ p i)) _) =
        RPCMRes (RPCRes p i)
    respond (RPCInMsg (RPCMReq (RPCReq1 _ p i)) _) =
        RPCMRes (RPCRes1 p i)
    respond (RPCInMsg (RPCMNotif rn) _) =
        RPCMNotif rn
    respond (RPCInMsg (RPCMErr e@(RPCErr _ _)) _) =
        RPCMNotif (RPCNotif "error" (toJSON e))
    respond (RPCInMsg (RPCMErr e@(RPCErr1 _ _)) _) =
        RPCMNotif (RPCNotif1 "error" (toJSON e))
    respond _ = undefined

    match (RPCMReq (RPCReq m p _))
        (RPCInMsg (RPCMRes (RPCRes p' _)) (Just (RPCReq m' p'' _))) =
        p == p' && p == p'' && m == m'
    match (RPCMReq (RPCReq1 m p _))
        (RPCInMsg (RPCMRes (RPCRes1 p' _)) (Just (RPCReq1 m' p'' _))) =
        p == p' && p == p'' && m == m'
    match (RPCMNotif (RPCNotif _ p))
        (RPCInMsg (RPCMNotif (RPCNotif _ p')) _) =
        p == p'
    match (RPCMNotif (RPCNotif1 _ p))
        (RPCInMsg (RPCMNotif (RPCNotif1 _ p')) _) =
        p == p'
    match (RPCMRes (RPCRes _ i))
        (RPCInMsg (RPCMErr (RPCErr (RPCErrObj _ c d) Nothing)) Nothing) =
        toJSON i == d && c == (-32000)
    match (RPCMRes (RPCRes1 _ _))
        (RPCInMsg (RPCMErr (RPCErr1 e Nothing)) Nothing) =
        take 17 e == "Id not recognized"
    match (RPCMErr (RPCErr _ (Just i)))
        (RPCInMsg (RPCMErr (RPCErr (RPCErrObj _ c d) Nothing)) Nothing) =
        c == (-32000) && toJSON i == d
    match (RPCMErr (RPCErr1 _ (Just _)))
        (RPCInMsg (RPCMErr (RPCErr1 e Nothing)) Nothing) =
        take 17 e == "Id not recognized"
    match (RPCMErr e@(RPCErr _ Nothing))
        (RPCInMsg (RPCMNotif (RPCNotif "error" (e'))) Nothing) =
        toJSON e == e'
    match (RPCMErr e@(RPCErr1 _ Nothing))
        (RPCInMsg (RPCMNotif (RPCNotif1 "error" (e'))) Nothing) =
        toJSON e == e'
    match _ _ = False
