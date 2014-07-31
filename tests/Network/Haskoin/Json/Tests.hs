module Network.Haskoin.Json.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson

import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Transaction
import Network.Haskoin.Crypto.Arbitrary()
import Network.Haskoin.Script.Arbitrary()
import Network.Haskoin.Protocol.Arbitrary()
import Network.Haskoin.Transaction.Arbitrary()


tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize haskoin types to JSON"
        [ testProperty "Coin" (metaID :: Coin -> Bool)
        , testProperty "ScriptOutput" (metaID :: ScriptOutput -> Bool)
        , testProperty "OutPoint" (metaID :: OutPoint -> Bool)
        , testProperty "Address" (metaID :: Address -> Bool)
        , testProperty "Tx" (metaID :: Tx -> Bool)
        , testProperty "TxHash" (metaID :: TxHash -> Bool)
        , testProperty "Word256" (metaID :: Word256 -> Bool)
        , testProperty "SigHash" (metaID :: SigHash -> Bool)
        , testProperty "SigInput" (metaID :: SigInput -> Bool)
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]
