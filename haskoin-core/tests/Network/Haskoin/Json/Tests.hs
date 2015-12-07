module Network.Haskoin.Json.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson (FromJSON, ToJSON, decode, encode)

import Network.Haskoin.Test

tests :: [Test]
tests =
    [ testGroup "Serialize & de-serialize haskoin types to JSON"
        [ testProperty "ScriptOutput" $ \(ArbitraryScriptOutput x) -> metaID x
        , testProperty "OutPoint" $ \(ArbitraryOutPoint x) -> metaID x
        , testProperty "Address" $ \(ArbitraryAddress x) -> metaID x
        , testProperty "Tx" $ \(ArbitraryTx x) -> metaID x
        , testProperty "TxHash" $ \(ArbitraryTxHash x) -> metaID x
        , testProperty "BlockHash" $ \(ArbitraryBlockHash x) -> metaID x
        , testProperty "SigHash" $ \(ArbitrarySigHash x) -> metaID x
        , testProperty "SigInput" $ \(ArbitrarySigInput x _) -> metaID x
        , testProperty "PubKey" $ \(ArbitraryPubKey _ x) -> metaID x
        , testProperty "PubKeyC" $ \(ArbitraryPubKeyC _ x) -> metaID x
        , testProperty "PubKeyU" $ \(ArbitraryPubKeyU _ x) -> metaID x
        , testProperty "XPrvKey" $ \(ArbitraryXPrvKey x) -> metaID x
        , testProperty "XPubKey" $ \(ArbitraryXPubKey _ x) -> metaID x
        , testProperty "DerivPath" $ \(ArbitraryDerivPath x) -> metaID x
        , testProperty "ParsedPath" $ \(ArbitraryParsedPath x) -> metaID x
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

