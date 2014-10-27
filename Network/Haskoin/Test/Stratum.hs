{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Arbitrary instances and data types for use in test suites.
module Network.Haskoin.Test.Stratum
( -- * Arbitrary Data
  ReqRes(..)
) where

import Control.Applicative

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Aeson.Types hiding (Error)
import Data.Text (Text)
import qualified Data.Text as T

import Network.JsonRpc

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Test.Transaction

import Network.Haskoin.Transaction.Types
import Network.Haskoin.Stratum
import Network.Haskoin.Protocol hiding (Message)

-- | A pair of a request and its corresponding response.
-- Id and version should match.
data ReqRes q r = ReqRes !(Request q) !(Response r)
    deriving (Show, Eq)

instance Arbitrary (ReqRes Value Value) where
    arbitrary = do
        rq <- arbitrary
        rs <- arbitrary
        let rs' = rs { getResId = getReqId rq, getResVer = getReqVer rq }
        return $ ReqRes rq rs'

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary Ver where
    arbitrary = elements [V1, V2]

instance (Arbitrary q, ToRequest q) => Arbitrary (Request q) where
    arbitrary = do
        q <- arbitrary
        v <- arbitrary
        let m = requestMethod q
        Request v m q <$> arbitrary

instance (Arbitrary n, ToNotif n) => Arbitrary (Notif n) where
    arbitrary = do
        n <- arbitrary
        v <- arbitrary
        let m = notifMethod n
        return $ Notif v m n

instance Arbitrary r => Arbitrary (Response r) where
    arbitrary = Response <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ErrorObj where
    arbitrary = ErrorObj <$> arbitrary <*> arbitrary <*> arbitrary
                         <*> arbitrary <*> arbitrary

instance ( Arbitrary q, Arbitrary n, Arbitrary r
         , ToRequest q, ToNotif n, ToJSON r )
    => Arbitrary (Message q n r)
  where
    arbitrary = oneof [ MsgRequest  <$> arbitrary
                      , MsgNotif    <$> arbitrary
                      , MsgResponse <$> arbitrary
                      , MsgError    <$> arbitrary ]

instance Arbitrary Id where
    arbitrary = oneof [IdInt <$> arbitrary, IdTxt <$> arbitrary]

instance Arbitrary Value where
    arbitrary = resize 10 $ oneof [val, lsn, objn] where
        val = oneof [ toJSON <$> (arbitrary :: Gen String)
                    , toJSON <$> (arbitrary :: Gen Int)
                    , toJSON <$> (arbitrary :: Gen Double)
                    , toJSON <$> (arbitrary :: Gen Bool) ]
        ls   = toJSON <$> listOf val
        obj  = object . map (\(t, v) -> (T.pack t, v)) <$> listOf ps
        ps   = (,) <$> (arbitrary :: Gen String) <*> oneof [val, ls]
        lsn  = toJSON <$> listOf (oneof [ls, obj, val])
        objn = object . map (\(t, v) -> (T.pack t, v)) <$> listOf psn
        psn  = (,) <$> (arbitrary :: Gen String) <*> oneof [val, ls, obj]



--
-- Stratum
--

instance Arbitrary StratumTxInfo where
    arbitrary = StratumTxInfo <$> arbitrary <*> arbitrary

instance Arbitrary StratumCoin where
    arbitrary = do
        h <- arbitrary
        ArbitraryOutPoint o@(OutPoint i _) <- arbitrary
        let t = StratumTxInfo h i
        StratumCoin o t <$> arbitrary

instance Arbitrary StratumRequest where
    arbitrary = oneof 
        [ StratumReqVersion <$> arbitrary <*> arbitrary
        , StratumReqHistory <$> aAddr
        , StratumReqBalance <$> aAddr
        , StratumReqUnspent <$> aAddr
        , StratumReqTx      <$> arbitrary
        , StratumBcastTx    <$> aTx
        , StratumSubAddr    <$> aAddr
        ]
      where
        aAddr = arbitrary >>= \(ArbitraryAddress a) -> return a
        aTx   = arbitrary >>= \(ArbitraryTx tx) -> return tx

instance Arbitrary StratumNotif where
    arbitrary = 
        StratumNotifAddr <$> aAddr <*> arbitrary
      where
        aAddr = arbitrary >>= \(ArbitraryAddress a) -> return a

instance Arbitrary StratumResult where
    arbitrary = oneof 
        [ StratumSrvVersion  <$> arbitrary
        , StratumAddrHistory <$> arbitrary
        , StratumAddrBalance <$> arbitrary <*> arbitrary
        , StratumAddrUnspent <$> arbitrary
        , StratumAddrStatus  <$> arbitrary
        , StratumTx          <$> (arbitrary >>= \(ArbitraryTx tx) -> return tx)
        , StratumBcastId     <$> arbitrary 
        ]

instance Arbitrary (ReqRes StratumRequest StratumResult) where
    arbitrary = do
        (q, s) <- oneof
            [ (,) <$> (StratumReqVersion  <$> arbitrary <*> arbitrary)
                  <*> (StratumSrvVersion  <$> arbitrary)
            , (,) <$> (StratumReqHistory  <$> aAddr)
                  <*> (StratumAddrHistory <$> arbitrary)
            , (,) <$> (StratumReqBalance  <$> aAddr)
                  <*> (StratumAddrBalance <$> arbitrary <*> arbitrary)
            , (,) <$> (StratumReqUnspent  <$> aAddr)
                  <*> (StratumAddrUnspent <$> arbitrary)
            , (,) <$> (StratumReqTx       <$> arbitrary)
                  <*> (StratumTx          <$> aTx)
            , (,) <$> (StratumBcastTx     <$> aTx)
                  <*> (StratumBcastId     <$> arbitrary) 
            ]
        i <- arbitrary
        ver <- arbitrary
        return $ ReqRes (Request ver (requestMethod q) q i) (Response ver s i)
      where
        aAddr = arbitrary >>= \(ArbitraryAddress a) -> return a
        aTx   = arbitrary >>= \(ArbitraryTx tx) -> return tx

