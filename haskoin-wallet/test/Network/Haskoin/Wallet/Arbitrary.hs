{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Arbitrary where

import Test.QuickCheck (Arbitrary, arbitrary, oneof)

import Network.Haskoin.Test
import Network.Haskoin.Wallet

instance Arbitrary AccountType where
    arbitrary = oneof
        [ return AccountRegular
        , do
            (m, n) <- arbitraryMSParam
            return $ AccountMultisig m n
        ]

instance Arbitrary NodeAction where
    arbitrary = oneof [ NodeActionRescan <$> arbitrary
                      , return NodeActionStatus
                      ]

