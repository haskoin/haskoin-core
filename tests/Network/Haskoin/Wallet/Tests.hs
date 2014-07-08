module Network.Haskoin.Wallet.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe
import Data.Aeson

import Network.Haskoin.Wallet.Arbitrary
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Server.Types

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize wallet types to JSON"
        [ testProperty "Wallet" (metaID :: Wallet -> Bool)
        , testProperty "Account" (metaID :: Account -> Bool)
        , testProperty "PaymentAddress" (metaID :: PaymentAddress -> Bool)
        , testProperty "AccTx" (metaID :: AccTx -> Bool)
        ]
    , testGroup "Serialize & de-serialize JSON-RPC types"
        [ testProperty "WalletRequest" testWalletRequest 
        , testProperty "WalletResponse" testWalletResponse
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) x == Just x

testWalletRequest :: WalletRequest -> Int -> Bool
testWalletRequest wr i = 
    decodeWalletRequest (encodeWalletRequest wr i) == Right (wr, i)

testWalletResponse :: RequestPair -> Int -> Bool
testWalletResponse (RequestPair req res) i =
    decodeWalletResponse req (encodeWalletResponse res i) == Right (res, i)

