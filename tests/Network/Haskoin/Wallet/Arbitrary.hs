{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Arbitrary where

import Test.QuickCheck 
    ( Arbitrary
    , arbitrary
    , oneof
    , vectorOf
    , listOf1
    , elements
    )

import Control.Applicative ((<$>), (<*>))

import Network.Haskoin.Test
import Network.Haskoin.Wallet

{- Wallet types -}

instance Arbitrary Wallet where
    arbitrary = do
        str <- arbitrary
        ArbitraryMasterKey m <- arbitrary
        return $ Wallet str m
            
instance Arbitrary Account where
    arbitrary = oneof [reg, ms, rd, rdms]
      where
        reg = do
            wallet <- arbitrary
            name <- arbitrary
            k <- arbitrary
            ArbitraryAccPubKey _ _ pub <- arbitrary
            return $ AccountRegular wallet name k pub
        ms = do
            wallet <- arbitrary
            name <- arbitrary
            k <- arbitrary
            ArbitraryMSParam m n <- arbitrary
            keys <- vectorOf n (arbitrary >>= \(ArbitraryXPubKey _ p) -> return p)
            return $ AccountMultisig wallet name k m n keys
        rd = do
            wallet <- arbitrary
            name <- arbitrary
            ArbitraryAccPubKey _ _ key <- arbitrary
            return $ AccountRead wallet name key
        rdms = do
            wallet <- arbitrary
            name <- arbitrary
            ArbitraryMSParam m n <- arbitrary
            keys <- vectorOf n (arbitrary >>= \(ArbitraryXPubKey _ p) -> return p)
            return $ AccountReadMultisig wallet name m n keys

instance Arbitrary Balance where
    arbitrary = oneof
        [ return BalanceConflict
        , Balance <$> arbitrary
        ]

instance Arbitrary BalanceAddress where
    arbitrary = BalanceAddress <$> arbitrary <*> arbitrary <*> arbitrary
                               <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LabeledAddress where
    arbitrary = do
        ArbitraryAddress addr <- arbitrary
        LabeledAddress addr <$> arbitrary <*> arbitrary

instance Arbitrary RecipientAddress where
    arbitrary = do
        ArbitraryAddress addr <- arbitrary
        RecipientAddress addr <$> arbitrary <*> arbitrary

instance Arbitrary AccTx where
    arbitrary = do
        accTxTxId             <- arbitrary
        accTxRecipients       <- listOf1 arbitrary
        accTxValue            <- arbitrary
        accTxConfidence       <- arbitrary
        accTxIsCoinbase       <- arbitrary
        accTxConfirmations    <- abs <$> arbitrary
        ArbitraryTx accTxTx   <- arbitrary
        accTxReceivedDate     <- arbitrary
        accTxConfirmationDate <- arbitrary
        return AccTx{..}

instance Arbitrary TxConfidence where
    arbitrary = elements [ TxOffline, TxDead, TxPending, TxBuilding ]

instance Arbitrary TxSource where
    arbitrary = elements [ SourceNetwork, SourceWallet, SourceUnknown ]

instance Arbitrary OfflineTxData where
    arbitrary = do
        dat <- listOf1 genDat
        ArbitraryTx tx <- arbitrary
        return $ OfflineTxData dat tx
      where
        genDat = do
            ArbitraryOutPoint op <- arbitrary
            ArbitraryScriptOutput so <- arbitrary
            b <- arbitrary
            k <- arbitrary
            return (op, so, b, k)

{- Request types -}

instance Arbitrary PagedResult where
    arbitrary = PagedResult <$> arbitrary <*> arbitrary

instance Arbitrary NewWallet where
    arbitrary = NewWallet <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary NewAccount where
    arbitrary = oneof
        [ NewAccountRegular <$> arbitrary
        , NewAccountRead <$> arbitrary 
                         <*> ((\(ArbitraryXPubKey _ x) -> x) <$> arbitrary)
        , goms
        , goreadms
        ]
      where
        goms = do
            name <- arbitrary
            ArbitraryMSParam m n <- arbitrary
            keys <- vectorOf n $ (\(ArbitraryXPubKey _ x) -> x) <$> arbitrary
            return $ NewAccountMultisig name m n keys
        goreadms = do
            name <- arbitrary
            ArbitraryMSParam m n <- arbitrary
            keys <- vectorOf n $ (\(ArbitraryXPubKey _ x) -> x) <$> arbitrary
            return $ NewAccountReadMultisig name m n keys
            

instance Arbitrary AddressData where
    arbitrary = AddressData <$> arbitrary

instance Arbitrary AccTxAction where
    arbitrary = oneof
        [ CreateTx <$> (listOf1 genaddr) 
                   <*> arbitrary 
                   <*> arbitrary 
                   <*> arbitrary
        , SignTx <$> ((\(ArbitraryTx x) -> x) <$> arbitrary) <*> arbitrary
        , SignOfflineTxData <$> arbitrary <*> arbitrary
        , ImportTx <$> ((\(ArbitraryTx x) -> x) <$> arbitrary) 
        ]
      where
        genaddr = (,) <$> ((\(ArbitraryAddress x) -> x) <$> arbitrary)
                      <*> arbitrary

instance Arbitrary NodeAction where
    arbitrary = Rescan <$> arbitrary

{- Response types -}

instance Arbitrary MnemonicRes where
    arbitrary = MnemonicRes <$> arbitrary

instance Arbitrary AddressPageRes where
    arbitrary = AddressPageRes <$> arbitrary <*> arbitrary

instance Arbitrary TxPageRes where
    arbitrary = TxPageRes <$> arbitrary <*> arbitrary

instance Arbitrary TxHashStatusRes where
    arbitrary = TxHashStatusRes 
        <$> arbitrary 
        <*> arbitrary

instance Arbitrary TxStatusRes where
    arbitrary = TxStatusRes 
        <$> ((\(ArbitraryTx x) -> x) <$> arbitrary) 
        <*> arbitrary

instance Arbitrary TxRes where
    arbitrary = TxRes <$> ((\(ArbitraryTx x) -> x) <$> arbitrary) 

instance Arbitrary BalanceRes where
    arbitrary = BalanceRes <$> arbitrary <*> arbitrary

instance Arbitrary SpendableRes where
    arbitrary = SpendableRes <$> arbitrary

instance Arbitrary RescanRes where
    arbitrary = RescanRes <$> arbitrary


