{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Arbitrary where

import Test.QuickCheck (Arbitrary, arbitrary, oneof)

import Network.Haskoin.Test
import Network.Haskoin.Wallet

instance Arbitrary AccountType where
    arbitrary = oneof
        [ return AccountRegular
        , do
            ArbitraryMSParam m n <- arbitrary
            return $ AccountMultisig m n
        ]

instance Arbitrary NodeAction where
    arbitrary = oneof [ NodeActionRescan <$> arbitrary
                      , return NodeActionStatus
                      ]

