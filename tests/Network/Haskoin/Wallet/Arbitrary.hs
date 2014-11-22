{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Arbitrary where

import Test.QuickCheck

import Control.Applicative 

import Network.Haskoin.Test
import Network.Haskoin.Wallet.Types
import Network.Haskoin.REST.Types

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

instance Arbitrary Balance where
    arbitrary = oneof
        [ return BalanceConflict
        , Balance <$> arbitrary
        ]

instance Arbitrary BalanceAddress where
    arbitrary = BalanceAddress <$> arbitrary 
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary

instance Arbitrary PaymentAddress where
    arbitrary = do
        ArbitraryAddress addr <- arbitrary
        l <- arbitrary
        k <- arbitrary
        return $ PaymentAddress addr l k

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

-- REST Types --

instance Arbitrary NewWallet where
    arbitrary = NewWallet <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary MnemonicRes where
    arbitrary = MnemonicRes <$> arbitrary

instance Arbitrary NewAccount where
    arbitrary = oneof
        [ NewAccount <$> arbitrary <*> arbitrary
        , NewReadAccount <$> arbitrary 
                         <*> ((\(ArbitraryXPubKey _ x) -> x) <$> arbitrary)
        , goms
        , goreadms
        ]
      where
        goms = do
            wallet <- arbitrary
            name <- arbitrary
            ArbitraryMSParam m n <- arbitrary
            keys <- vectorOf n $ (\(ArbitraryXPubKey _ x) -> x) <$> arbitrary
            return $ NewMSAccount wallet name m n keys
        goreadms = do
            name <- arbitrary
            ArbitraryMSParam m n <- arbitrary
            keys <- vectorOf n $ (\(ArbitraryXPubKey _ x) -> x) <$> arbitrary
            return $ NewReadMSAccount name m n keys
            

instance Arbitrary AddressPageRes where
    arbitrary = AddressPageRes <$> arbitrary <*> arbitrary

instance Arbitrary TxPageRes where
    arbitrary = TxPageRes <$> arbitrary <*> arbitrary

instance Arbitrary AddressData where
    arbitrary = AddressData <$> arbitrary

instance Arbitrary AccTxAction where
    arbitrary = oneof
        [ SendCoins <$> (listOf1 genaddr) <*> arbitrary <*> arbitrary
        , SignTx <$> ((\(ArbitraryTx x) -> x) <$> arbitrary) 
        , SignSigBlob <$> arbitrary
        ]
      where
        genaddr = (,) <$> ((\(ArbitraryAddress x) -> x) <$> arbitrary)
                      <*> arbitrary

instance Arbitrary TxAction where
    arbitrary = ImportTx <$> ((\(ArbitraryTx x) -> x) <$> arbitrary) 

instance Arbitrary TxHashStatusRes where
    arbitrary = TxHashStatusRes 
        <$> arbitrary 
        <*> arbitrary
        <*> oneof [ return Nothing
                  , (Just . (\(ArbitraryTx x) -> x) <$> arbitrary) 
                  ]

instance Arbitrary TxRes where
    arbitrary = TxRes <$> ((\(ArbitraryTx x) -> x) <$> arbitrary) 

instance Arbitrary TxStatusRes where
    arbitrary = TxStatusRes 
        <$> ((\(ArbitraryTx x) -> x) <$> arbitrary) 
        <*> arbitrary
        <*> oneof [ return Nothing
                  , (Just . (\(ArbitraryTx x) -> x) <$> arbitrary) 
                  ]

instance Arbitrary BalanceRes where
    arbitrary = BalanceRes <$> arbitrary <*> arbitrary

instance Arbitrary SpendableRes where
    arbitrary = SpendableRes <$> arbitrary

instance Arbitrary NodeAction where
    arbitrary = Rescan <$> arbitrary

instance Arbitrary RescanRes where
    arbitrary = RescanRes <$> arbitrary


