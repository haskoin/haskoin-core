module Network.Haskoin.Binary.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Binary (Binary)

import Network.Haskoin.Test
import Network.Haskoin.Util

tests :: [Test]
tests =
    [ testGroup "Binary encoding and decoding of utility types"
        [ testProperty "ByteString" $ \(ArbitraryByteString x) -> metaBinary x ]
    , testGroup "Binary encoding and decoding of hash types"
        [ testProperty "Hash160" $ \(ArbitraryHash160 x) -> metaBinary x
        , testProperty "Hash256" $ \(ArbitraryHash256 x) -> metaBinary x
        , testProperty "Hash512" $ \(ArbitraryHash512 x) -> metaBinary x
        ]
    , testGroup "Binary encoding and decoding of crypto types"
        [ testProperty "Signature" $ \(ArbitrarySignature _ _ x) -> metaBinary x
        , testProperty "PubKey" $ \(ArbitraryPubKey _ x) -> metaBinary x
        , testProperty "XPrvKey" $ \(ArbitraryXPrvKey x) -> metaBinary x
        , testProperty "XPubKey" $ \(ArbitraryXPubKey _ x) -> metaBinary x
        ]
    , testGroup "Binary encoding and decoding of protocol types"
        [ testProperty "VarInt" $ \(ArbitraryVarInt x) -> metaBinary x
        , testProperty "VarString" $ \(ArbitraryVarString x) -> metaBinary x
        , testProperty "NetworkAddress" $ \(ArbitraryNetworkAddress x) -> metaBinary x
        , testProperty "InvType" $ \(ArbitraryInvType x) -> metaBinary x
        , testProperty "InvVector" $ \(ArbitraryInvVector x) -> metaBinary x
        , testProperty "Inv" $ \(ArbitraryInv x) -> metaBinary x
        , testProperty "Version" $ \(ArbitraryVersion x) -> metaBinary x
        , testProperty "Addr" $ \(ArbitraryAddr x) -> metaBinary x
        , testProperty "Alert" $ \(ArbitraryAlert x) -> metaBinary x
        , testProperty "Reject" $ \(ArbitraryReject x) -> metaBinary x
        , testProperty "GetData" $ \(ArbitraryGetData x) -> metaBinary x
        , testProperty "NotFound" $ \(ArbitraryNotFound x) -> metaBinary x
        , testProperty "Ping" $ \(ArbitraryPing x) -> metaBinary x
        , testProperty "Pong" $ \(ArbitraryPong x) -> metaBinary x
        , testProperty "MessageCommand" $ \(ArbitraryMessageCommand x) -> metaBinary x
        , testProperty "MessageHeader" $ \(ArbitraryMessageHeader x) -> metaBinary x
        , testProperty "Message" $ \(ArbitraryMessage x) -> metaBinary x
        ]
    , testGroup "Binary encoding and decoding of script types"
        [ testProperty "ScriptOp" $ \(ArbitraryScriptOp x) -> metaBinary x
        , testProperty "Script" $ \(ArbitraryScript x) -> metaBinary x
        , testProperty "SigHash" $ \(ArbitrarySigHash x) -> metaBinary x
        ]
    , testGroup "Binary encoding and decoding of transaction types"
        [ testProperty "TxIn" $ \(ArbitraryTxIn x) -> metaBinary x
        , testProperty "TxOut" $ \(ArbitraryTxOut x) -> metaBinary x
        , testProperty "OutPoint" $ \(ArbitraryOutPoint x) -> metaBinary x
        , testProperty "Tx" $ \(ArbitraryTx x) -> metaBinary x
        , testProperty "CoinbaseTx" $ \(ArbitraryCoinbaseTx x) -> metaBinary x
        ]
    , testGroup "Binary encoding and decoding of block types"
        [ testProperty "Block" $ \(ArbitraryBlock x) -> metaBinary x
        , testProperty "BlockHeader" $ \(ArbitraryBlockHeader x) -> metaBinary x
        , testProperty "GetBlocks" $ \(ArbitraryGetBlocks x) -> metaBinary x
        , testProperty "GetHeaders" $ \(ArbitraryGetHeaders x) -> metaBinary x
        , testProperty "Headers" $ \(ArbitraryHeaders x) -> metaBinary x
        , testProperty "MerkleBlock" $ \(ArbitraryMerkleBlock x) -> metaBinary x
        ]
    , testGroup "Binary encoding and decoding of bloom types"
        [ testProperty "BloomFlags" $ \(ArbitraryBloomFlags x) -> metaBinary x
        , testProperty "BloomFilter" $ \(ArbitraryBloomFilter _ _ x) -> metaBinary x
        , testProperty "FilterLoad" $ \(ArbitraryFilterLoad x) -> metaBinary x
        , testProperty "FilterAdd" $ \(ArbitraryFilterAdd x) -> metaBinary x
        ]
    ]

metaBinary :: (Binary a, Eq a) => a -> Bool
metaBinary x = decode' (encode' x) == x
