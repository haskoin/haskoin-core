module Network.Haskoin.Json.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson (FromJSON, ToJSON, decode, encode)

import Network.Haskoin.Test
import Network.Haskoin.Crypto

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize haskoin types to JSON"
        [ testProperty "Coin" $ \(ArbitraryCoin x) -> metaID x
        , testProperty "ScriptOutput" $ \(ArbitraryScriptOutput x) -> metaID x
        , testProperty "OutPoint" $ \(ArbitraryOutPoint x) -> metaID x
        , testProperty "Address" $ \(ArbitraryAddress x) -> metaID x
        , testProperty "Tx" $ \(ArbitraryTx x) -> metaID x
        , testProperty "TxHash" (metaID :: TxHash -> Bool)
        , testProperty "BlockHash" (metaID :: BlockHash -> Bool)
        , testProperty "Word256" (metaID :: Word256 -> Bool)
        , testProperty "SigHash" $ \(ArbitrarySigHash x) -> metaID x
        , testProperty "SigInput" $ \(ArbitrarySigInput x _) -> metaID x
        , testProperty "XPrvKey" $ \(ArbitraryXPrvKey x) -> metaID x
        , testProperty "XPubKey" $ \(ArbitraryXPubKey _ x) -> metaID x
        , testProperty "DerivPath" $ \(ArbitraryDerivPath x) -> metaID x
        , testProperty "MasterKey" $ \(ArbitraryMasterKey x) -> metaID x
        , testProperty "AccPrvKey" $ \(ArbitraryAccPrvKey _ x) -> metaID x
        , testProperty "AccPubKey" $ \(ArbitraryAccPubKey _ _ x) -> metaID x
        , testProperty "AddrPrvKey" $ \(ArbitraryAddrPrvKey _ _ x) -> metaID x
        , testProperty "AddrPubKey" $ \(ArbitraryAddrPubKey _ _ _ x) -> metaID x
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

