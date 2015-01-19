module Network.Haskoin.Json.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson (FromJSON, ToJSON, decode, encode)

import Network.Haskoin.Network
import Network.Haskoin.Test
import Network.Haskoin.Crypto

type Net = Prodnet

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize haskoin types to JSON"
        [ testProperty "Coin" $
            \(ArbitraryCoin x :: ArbitraryCoin Net) -> metaID x
        , testProperty "ScriptOutput" $ \(ArbitraryScriptOutput x) -> metaID x
        , testProperty "OutPoint" $ \(ArbitraryOutPoint x) -> metaID x
        , testProperty "Address" $
            \(ArbitraryAddress x :: ArbitraryAddress Net) -> metaID x
        , testProperty "Tx" $
            \(ArbitraryTx x :: ArbitraryTx Net) -> metaID x
        , testProperty "TxHash" (metaID :: TxHash -> Bool)
        , testProperty "BlockHash" (metaID :: BlockHash -> Bool)
        , testProperty "Word256" (metaID :: Word256 -> Bool)
        , testProperty "SigHash" $ \(ArbitrarySigHash x) -> metaID x
        , testProperty "SigInput" $ \(ArbitrarySigInput x _) -> metaID x
        , testProperty "XPrvKey" $
            \(ArbitraryXPrvKey x :: ArbitraryXPrvKey Net) -> metaID x
        , testProperty "XPubKey" $
            \(ArbitraryXPubKey _ x :: ArbitraryXPubKey Net) -> metaID x
        , testProperty "DerivPath" $ \(ArbitraryDerivPath x) -> metaID x
        , testProperty "MasterKey" $
            \(ArbitraryMasterKey x :: ArbitraryMasterKey Net) -> metaID x
        , testProperty "AccPrvKey" $
            \(ArbitraryAccPrvKey _ x :: ArbitraryAccPrvKey Net) -> metaID x
        , testProperty "AccPubKey" $
            \(ArbitraryAccPubKey _ _ x :: ArbitraryAccPubKey Net) -> metaID x
        , testProperty "AddrPrvKey" $
            \(ArbitraryAddrPrvKey _ _ x :: ArbitraryAddrPrvKey Net) -> metaID x
        , testProperty "AddrPubKey" $
            \(ArbitraryAddrPubKey _ _ _ x :: ArbitraryAddrPubKey Net) -> metaID x
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

