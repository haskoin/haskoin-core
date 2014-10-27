module Network.Haskoin.Json.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Test.Script
import Network.Haskoin.Test.Transaction

import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Transaction


tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize haskoin types to JSON"
        [ testProperty "Coin" $ \(ArbitraryCoin x) -> metaID x
        , testProperty "ScriptOutput" $ \(ArbitraryScriptOutput x) -> metaID x
        , testProperty "OutPoint" $ \(ArbitraryOutPoint x) -> metaID x
        , testProperty "Address" $ \(ArbitraryAddress x) -> metaID x
        , testProperty "Tx" $ \(ArbitraryTx x) -> metaID x
        , testProperty "TxHash" (metaID :: TxHash -> Bool)
        , testProperty "Word256" (metaID :: Word256 -> Bool)
        , testProperty "SigHash" $ \(ArbitrarySigHash x) -> metaID x
        , testProperty "SigInput" $ \(ArbitrarySigInput x _) -> metaID x
        , testProperty "XPrvKey" $ \(ArbitraryXPrvKey x) -> metaID x
        , testProperty "XPubKey" $ \(ArbitraryXPubKey _ x) -> metaID x
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

