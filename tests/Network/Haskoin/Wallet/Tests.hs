module Network.Haskoin.Wallet.Tests (tests) where

import Control.Monad
import Data.Maybe

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson
import Data.Aeson.Types

import Network.JsonRpc

import Network.Haskoin.Wallet.Arbitrary
import Network.Haskoin.Wallet.Types
import Network.Haskoin.REST.Types

encodeWalletRequest :: (ToRequest q, ToJSON q) => q -> Int -> Value
encodeWalletRequest wr i = toJSON $
    Request V2 (requestMethod wr) wr (IdInt i)

decodeWalletRequest :: (FromRequest q) => Value -> Maybe (Request q)
decodeWalletRequest v =
    parseMaybe parseRequest v >>= either (const Nothing) return

encodeWalletResponse :: ToJSON r => r -> Int -> Value
encodeWalletResponse res i = toJSON $ Response V2 res (IdInt i)

decodeWalletResponse :: FromResponse r => Request q -> Value -> Maybe (Response r)
decodeWalletResponse rq v =
    parseMaybe (parseResponse rq) v >>= either (const Nothing) return

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize wallet types to JSON"
        [ testProperty "Wallet" (metaID :: Wallet -> Bool)
        , testProperty "Account" (metaID :: Account -> Bool)
        , testProperty "PaymentAddress" (metaID :: PaymentAddress -> Bool)
        , testProperty "AccTx" (metaID :: AccTx -> Bool)
        , testProperty "TxConfidence" (metaID :: TxConfidence -> Bool)
        , testProperty "TxSource" (metaID :: TxSource -> Bool)
        , testProperty "SigBlob" (metaID :: SigBlob -> Bool)
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

