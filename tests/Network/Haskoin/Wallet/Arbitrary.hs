{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Arbitrary where

import Test.QuickCheck (Arbitrary, arbitrary, oneof, listOf)

import Control.Applicative ((<$>))

import Network.Haskoin.Test
import Network.Haskoin.Wallet

instance Arbitrary AccountType where
    arbitrary = oneof
        [ return AccountRegular
        , do
            ArbitraryMSParam m n <- arbitrary
            return $ AccountMultisig m n
        , return AccountRead
        , do
            ArbitraryMSParam m n <- arbitrary
            return $ AccountReadMultisig m n
        ]

instance Arbitrary NodeAction where
    arbitrary = oneof [ NodeActionRescan <$> arbitrary
                      , NodeActionStatus
                      ]

instance Arbitrary TxAction where
    arbitrary = oneof
        [ do
            as' <- arbitrary
            let as = map (\(ArbitraryAddress a, x) -> (a, x)) as'
            fee <- arbitrary
            rcptFee <- arbitrary
            minConf <- arbitrary
            sign <- arbitrary
            return $ CreateTx as fee rcptFee minConf sign
        , do
            ArbitraryTx tx <- arbitrary
            return (ImportTx tx)
        , SignTx <$> arbitrary
        , do
            ArbitraryTx tx <- arbitrary
            sd <- listOf $ do
                ArbitraryOutPoint outPoint <- arbitrary
                ArbitraryScriptOutput scriptOutput <- arbitrary
                ArbitrarySoftPath deriv <- arbitrary
                let signData = CoinSignData outPoint scriptOutput deriv
                return signData
            return (SignOfflineTx tx sd)
        ]
