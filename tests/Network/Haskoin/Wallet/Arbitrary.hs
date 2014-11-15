{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Arbitrary where

import Test.QuickCheck

import Control.Applicative 

import Network.Haskoin.Test
import Network.Haskoin.Wallet.Types

instance Arbitrary Wallet where
    arbitrary = do
        str <- arbitrary
        ArbitraryMasterKey m <- arbitrary
        return $ Wallet str m
            
instance Arbitrary Account where
    arbitrary = oneof [reg, ms, rd, rdms]
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
        rd = do
            name <- arbitrary
            ArbitraryAccPubKey _ _ key <- arbitrary
            return $ ReadAccount name key
        rdms = do
            name <- arbitrary
            ArbitraryMSParam m n <- arbitrary
            keys <- vectorOf n (arbitrary >>= \(ArbitraryXPubKey _ p) -> return p)
            return $ ReadMSAccount name m n keys

instance Arbitrary PaymentAddress where
    arbitrary = do
        ArbitraryAddress addr <- arbitrary
        l <- arbitrary
        k <- arbitrary
        b <- arbitrary
        hs <- arbitrary
        return $ PaymentAddress addr l k b hs

instance Arbitrary RecipientAddress where
    arbitrary = do
        ArbitraryAddress addr <- arbitrary
        l  <- arbitrary
        lo <- arbitrary
        return $ RecipientAddress addr l lo

instance Arbitrary AccTx where
    arbitrary = do
        tid <- arbitrary
        addrs <- listOf1 arbitrary
        v <- arbitrary
        conf <- arbitrary
        b <- arbitrary
        c <- abs <$> arbitrary
        ArbitraryUTCTime rd <- arbitrary
        cd <- arbitrary
        return $ AccTx tid addrs v conf b c rd cd

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

