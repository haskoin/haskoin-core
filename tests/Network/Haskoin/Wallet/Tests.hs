module Network.Haskoin.Wallet.Tests (tests) where

import Control.Monad
import Data.Maybe

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson
import Data.Aeson.Types

import Network.Haskoin.Stratum

import Network.Haskoin.Wallet.Arbitrary
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Server.Types

encodeWalletRequest :: (ToRPCReq q, ToJSON q) => q -> Int -> Value
encodeWalletRequest wr i = toJSON $
    RPCReq (rpcReqMethod wr) wr (RPCIdInt i)

decodeWalletRequest :: (FromRPCReq q) => Value -> Maybe (RPCReq q)
decodeWalletRequest v =
    parseMaybe parseRPCReq v >>= either (const Nothing) return

encodeWalletResponse :: ToJSON r => r -> Int -> Value
encodeWalletResponse res i = toJSON $ RPCRes res (RPCIdInt i)

decodeWalletResponse :: FromRPCResult r => RPCReq q -> Value -> Maybe (RPCRes r)
decodeWalletResponse rq v =
    parseMaybe (parseRPCRes rq) v >>= either (const Nothing) return

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
    , testGroup "Serialize & de-serialize JSON-RPC types"
        [ testProperty "WalletRequest" testWalletRequest 
        , testProperty "WalletResponse" testWalletResponse
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) [x] == Just [x]

testWalletRequest :: WalletRequest -> Int -> Bool
testWalletRequest wr i = fromMaybe False $ do
    rq <- decodeWalletRequest (encodeWalletRequest wr i)
    guard $ getReqId rq == RPCIdInt i
    return $ getReqParams rq == wr

testWalletResponse :: RequestPair -> Int -> Bool
testWalletResponse (RequestPair req res) i = fromMaybe False $ do
    let rq = RPCReq (rpcReqMethod req) req (RPCIdInt i)
    rs <- decodeWalletResponse rq (encodeWalletResponse res i)
    guard $ getResId rs == RPCIdInt i
    return $ getResult rs == res
