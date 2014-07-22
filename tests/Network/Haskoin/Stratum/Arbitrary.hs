{-# LANGUAGE FlexibleInstances #-}
module Network.Haskoin.Stratum.Arbitrary where

import Control.Applicative
import Data.Aeson.Types
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Network.Haskoin.Crypto.Arbitrary ()
import Network.Haskoin.Protocol
import Network.Haskoin.Protocol.Arbitrary ()
import Network.Haskoin.Stratum.RPC
import Network.Haskoin.Stratum.Types
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data ReqRes q r = ReqRes !(RPCReq q) !(RPCRes r)
    deriving (Show, Eq)

instance Arbitrary (ReqRes Value Value) where
    arbitrary = ReqRes <$> arbitrary <*> arbitrary

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance (Arbitrary q, ToRPCReq q) => Arbitrary (RPCReq q) where
    arbitrary = do
        q <- arbitrary
        let m = rpcReqMethod q
        oneof [ RPCReq  m q <$> arbitrary
              , RPCReq1 m q <$> arbitrary
              ]

instance (Arbitrary n, ToRPCNotif n) => Arbitrary (RPCNotif n) where
    arbitrary = do
        n <- arbitrary
        let m = rpcNotifMethod n
        oneof $ map return [RPCNotif m n, RPCNotif1 m n]

instance Arbitrary r => Arbitrary (RPCRes r) where
    arbitrary = oneof [ RPCRes1 <$> arbitrary <*> arbitrary
                      , RPCRes  <$> arbitrary <*> arbitrary ]

instance Arbitrary RPCErr where
    arbitrary = oneof [ RPCErr1 <$> arbitrary <*> arbitrary
                      , RPCErr  <$> arbitrary <*> arbitrary ]

instance ( Arbitrary q, Arbitrary n, Arbitrary r
         , ToRPCReq q, ToRPCNotif n, ToJSON r )
    => Arbitrary (RPCMsg q n r)
  where
    arbitrary = oneof [ RPCMReq   <$> arbitrary
                      , RPCMNotif <$> arbitrary
                      , RPCMRes   <$> arbitrary
                      , RPCMErr   <$> arbitrary ]

instance Arbitrary RPCId where
    arbitrary = oneof [RPCIdInt <$> arbitrary, RPCIdTxt <$> arbitrary]

instance Arbitrary RPCErrObj where
    arbitrary = RPCErrObj <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Value where
    arbitrary = resize 10 $ oneof [val, lsn, objn] where
        val = oneof [ toJSON <$> (arbitrary :: Gen String)
                    , toJSON <$> (arbitrary :: Gen Int)
                    , toJSON <$> (arbitrary :: Gen Double)
                    , toJSON <$> (arbitrary :: Gen Bool) ]
        ls   = toJSON <$> listOf val
        obj  = toJSON . M.fromList <$> listOf ps
        ps   = (,) <$> (arbitrary :: Gen String) <*> oneof [val, ls]
        lsn  = toJSON <$> listOf (oneof [ls, obj, val])
        objn = toJSON . M.fromList <$> listOf psn
        psn  = (,) <$> (arbitrary :: Gen String) <*> oneof [val, ls, obj]

--
-- Stratum
--

data StratumPair = StratumPair !(RPCReq StratumRequest) !(RPCRes StratumResult)
    deriving (Show, Eq)

instance Arbitrary StratumTxInfo where
    arbitrary = StratumTxInfo <$> arbitrary <*> arbitrary

instance Arbitrary StratumCoin where
    arbitrary = do
        h <- arbitrary
        o@(OutPoint i _) <- arbitrary
        let t = StratumTxInfo h i
        StratumCoin o t <$> arbitrary

instance Arbitrary StratumRequest where
    arbitrary = oneof [ StratumReqVersion <$> arbitrary <*> arbitrary
                      , StratumReqHistory <$> arbitrary
                      , StratumReqBalance <$> arbitrary
                      , StratumReqUnspent <$> arbitrary
                      , StratumReqTx      <$> arbitrary
                      , StratumBcastTx    <$> arbitrary
                      , StratumSubAddr    <$> arbitrary ]

instance Arbitrary StratumNotif where
    arbitrary = StratumNotifAddr <$> arbitrary <*> arbitrary

instance Arbitrary StratumResult where
    arbitrary = oneof [ StratumSrvVersion  <$> arbitrary
                      , StratumAddrHistory <$> arbitrary
                      , StratumAddrBalance <$> arbitrary <*> arbitrary
                      , StratumAddrUnspent <$> arbitrary
                      , StratumAddrStatus  <$> arbitrary
                      , StratumTx          <$> arbitrary
                      , StratumBcastId     <$> arbitrary ]

instance Arbitrary (ReqRes StratumRequest StratumResult) where
    arbitrary = do
        (q, s) <- oneof
            [ (,) <$> (StratumReqVersion  <$> arbitrary <*> arbitrary)
                  <*> (StratumSrvVersion  <$> arbitrary)
            , (,) <$> (StratumReqHistory  <$> arbitrary)
                  <*> (StratumAddrHistory <$> arbitrary)
            , (,) <$> (StratumReqBalance  <$> arbitrary)
                  <*> (StratumAddrBalance <$> arbitrary <*> arbitrary)
            , (,) <$> (StratumReqUnspent  <$> arbitrary)
                  <*> (StratumAddrUnspent <$> arbitrary)
            , (,) <$> (StratumReqTx       <$> arbitrary)
                  <*> (StratumTx          <$> arbitrary)
            , (,) <$> (StratumBcastTx     <$> arbitrary)
                  <*> (StratumBcastId     <$> arbitrary) ]
        i <- arbitrary
        j <- arbitrary
        return $ ReqRes (RPCReq (rpcReqMethod q) q i) (RPCRes s j)
