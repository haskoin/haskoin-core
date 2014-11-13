module Network.Haskoin.Wallet.Arbitrary where

import Test.QuickCheck

import Data.Maybe

import Control.Monad
import Control.Applicative 

import Data.Bits (clearBit)
import qualified Data.ByteString as BS 
    ( ByteString
    , pack
    , drop
    )

import Network.Haskoin.Wallet.Types
import Network.Haskoin.REST.Types

import Network.Haskoin.Test

import Network.Haskoin.Script
import Network.Haskoin.Node
import Network.Haskoin.Transaction
import Network.Haskoin.Crypto

instance Arbitrary Wallet where
    arbitrary = do
        str <- arbitrary
        ArbitraryMasterKey m <- arbitrary
        return $ Wallet str m
            
instance Arbitrary Account where
    arbitrary = oneof [reg, ms, read, readms]
      where
        reg = do
            name <- arbitrary
            wallet <- arbitrary
            k <- arbitrary
            ArbitraryAccPubKey _ _ pub <- arbitrary
            return $ RegularAccount name wallet k pub
        ms = do
            name <- arbitrary
            wallet <- arbitrary
            k <- arbitrary
            ArbitraryMSParam m n <- arbitrary
            keys <- vectorOf n (arbitrary >>= \(ArbitraryXPubKey _ p) -> return p)
            return $ MultisigAccount name wallet k m n keys
        read = do
            name <- arbitrary
            ArbitraryAccPubKey _ _ key <- arbitrary
            return $ ReadAccount name key
        readms = do
            name <- arbitrary
            ArbitraryMSParam m n <- arbitrary
            keys <- vectorOf n (arbitrary >>= \(ArbitraryXPubKey _ p) -> return p)
            return $ ReadMSAccount name m n keys

instance Arbitrary PaymentAddress where
    arbitrary = do
        ArbitraryAddress addr <- arbitrary
        label <- arbitrary
        k <- arbitrary
        return $ PaymentAddress addr label k

instance Arbitrary AccTx where
    arbitrary = do
        tid <- arbitrary
        addrs <- listOf1 (arbitrary >>= \(ArbitraryAddress a) -> return a)
        v <- arbitrary
        conf <- arbitrary
        b <- arbitrary
        c <- abs <$> arbitrary
        return $ AccTx tid addrs v conf b c

instance Arbitrary TxConfidence where
    arbitrary = elements [ TxOffline, TxDead, TxPending, TxBuilding ]

instance Arbitrary TxSource where
    arbitrary = elements [ NetworkSource, WalletSource, UnknownSource ]

instance Arbitrary SigBlob where
    arbitrary = do
        dat <- listOf1 genDat
        ArbitraryTx tx <- arbitrary
        return $ SigBlob dat tx
      where
        genDat = do
            ArbitraryOutPoint op <- arbitrary
            ArbitraryScriptOutput so <- arbitrary
            b <- arbitrary
            k <- arbitrary
            return (op, so, b, k)

