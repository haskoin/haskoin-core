module Network.Haskoin.Json.Tests (tests) where

import           Data.Aeson
import           Data.HashMap.Strict                  (singleton)
import           Network.Haskoin.Test
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

tests :: [Test]
tests =
    [ testGroup
          "Serialize & de-serialize haskoin types to JSON"
          [ testProperty "ScriptOutput" $ forAll arbitraryScriptOutput testID
          , testProperty "OutPoint" $ forAll arbitraryOutPoint testID
          , testProperty "Address" $ forAll arbitraryAddress testID
          , testProperty "Tx" $ forAll arbitraryTx testID
          , testProperty "TxHash" $ forAll arbitraryTxHash testID
          , testProperty "BlockHash" $ forAll arbitraryBlockHash testID
          , testProperty "SigHash" $ forAll arbitrarySigHash testID
          , testProperty "SigInput" $ forAll arbitrarySigInput (testID . fst)
          , testProperty "PubKey" $ forAll arbitraryPubKey (testID . snd)
          , testProperty "PubKeyC" $ forAll arbitraryPubKeyC (testID . snd)
          , testProperty "PubKeyU" $ forAll arbitraryPubKeyU (testID . snd)
          , testProperty "XPrvKey" $ forAll arbitraryXPrvKey testID
          , testProperty "XPubKey" $ forAll arbitraryXPubKey (testID . snd)
          , testProperty "DerivPath" $ forAll arbitraryDerivPath testID
          , testProperty "ParsedPath" $ forAll arbitraryParsedPath testID
          ]
    ]

testID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
testID x =
    (decode . encode) (singleton ("object" :: String) x) ==
    Just (singleton ("object" :: String) x)
