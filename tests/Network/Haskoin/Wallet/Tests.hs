module Network.Haskoin.Wallet.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson

import Network.Haskoin.Wallet.Arbitrary ()
import Network.Haskoin.Wallet.Types
import Network.Haskoin.REST.Types

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize wallet types to JSON"
        [ testProperty "Wallet" (metaID :: Wallet -> Bool)
        , testProperty "Account" (metaID :: Account -> Bool)
        , testProperty "Balance" (metaID :: Balance -> Bool)
        , testProperty "PaymentAddress" (metaID :: PaymentAddress -> Bool)
        , testProperty "BalanceAddress" (metaID :: BalanceAddress -> Bool)
        , testProperty "RecipientAddress" (metaID :: RecipientAddress -> Bool)
        , testProperty "AccTx" (metaID :: AccTx -> Bool)
        , testProperty "TxConfidence" (metaID :: TxConfidence -> Bool)
        , testProperty "TxSource" (metaID :: TxSource -> Bool)
        , testProperty "SigBlob" (metaID :: SigBlob -> Bool)
        ]
    , testGroup "Serialize & de-serialize REST types to JSON"
        [ testProperty "NewWallet" (metaID :: NewWallet -> Bool)
        , testProperty "MnemonicRes" (metaID :: MnemonicRes -> Bool)
        , testProperty "NewAccount" (metaID :: NewAccount -> Bool)
        , testProperty "AddressPageRes" (metaID :: AddressPageRes -> Bool)
        , testProperty "TxPageRes" (metaID :: TxPageRes -> Bool)
        , testProperty "AddressData" (metaID :: AddressData -> Bool)
        , testProperty "TxAction" (metaID :: TxAction -> Bool)
        , testProperty "TxHashStatusRes" (metaID :: TxHashStatusRes -> Bool)
        , testProperty "TxRes" (metaID :: TxRes -> Bool)
        , testProperty "TxStatusRes" (metaID :: TxStatusRes -> Bool)
        , testProperty "BalanceRes" (metaID :: BalanceRes -> Bool)
        , testProperty "SpendableRes" (metaID :: SpendableRes -> Bool)
        , testProperty "NodeAction" (metaID :: NodeAction -> Bool)
        , testProperty "RescanRes" (metaID :: RescanRes -> Bool)
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

