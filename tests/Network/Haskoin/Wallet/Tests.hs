module Network.Haskoin.Wallet.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson (FromJSON, ToJSON, encode, decode)

import Network.Haskoin.Wallet.Arbitrary ()
import Network.Haskoin.Wallet

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize wallet types to JSON"
        [ testProperty "Wallet" (metaID :: Wallet -> Bool)
        , testProperty "Account" (metaID :: Account -> Bool)
        , testProperty "Balance" (metaID :: Balance -> Bool)
        , testProperty "LabeledAddress" (metaID :: LabeledAddress -> Bool)
        , testProperty "BalanceAddress" (metaID :: BalanceAddress -> Bool)
        , testProperty "RecipientAddress" (metaID :: RecipientAddress -> Bool)
        , testProperty "AccTx" (metaID :: AccTx -> Bool)
        , testProperty "TxConfidence" (metaID :: TxConfidence -> Bool)
        , testProperty "TxSource" (metaID :: TxSource -> Bool)
        , testProperty "OfflineTxData" (metaID :: OfflineTxData -> Bool)
        ]
    , testGroup "Serialize & de-serialize request types to JSON"
        [ testProperty "PagedResult" (metaID :: PagedResult -> Bool)
        , testProperty "NewWallet" (metaID :: NewWallet -> Bool)
        , testProperty "NewAccount" (metaID :: NewAccount -> Bool)
        , testProperty "AccTxAction" (metaID :: AccTxAction -> Bool)
        , testProperty "AddressData" (metaID :: AddressData -> Bool)
        , testProperty "NodeAction" (metaID :: NodeAction -> Bool)
        ]
    , testGroup "Serialize & de-serialize response types to JSON"
        [ testProperty "MnemonicRes" (metaID :: MnemonicRes -> Bool)
        , testProperty "AddressPageRes" (metaID :: AddressPageRes -> Bool)
        , testProperty "TxPageRes" (metaID :: TxPageRes -> Bool)
        , testProperty "TxHashStatusRes" (metaID :: TxHashStatusRes -> Bool)
        , testProperty "TxStatusRes" (metaID :: TxStatusRes -> Bool)
        , testProperty "TxRes" (metaID :: TxRes -> Bool)
        , testProperty "BalanceRes" (metaID :: BalanceRes -> Bool)
        , testProperty "SpendableRes" (metaID :: SpendableRes -> Bool)
        , testProperty "RescanRes" (metaID :: RescanRes -> Bool)
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

