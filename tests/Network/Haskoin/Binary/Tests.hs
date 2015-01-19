module Network.Haskoin.Binary.Tests (tests) where

import Test.QuickCheck (Property, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Binary (Binary)

import Network.Haskoin.Network
import Network.Haskoin.Test
import Network.Haskoin.Util
import Network.Haskoin.Crypto

type Net = Prodnet

tests :: [Test]
tests = 
    [ testGroup "Binary encoding and decoding of utility types"
        [ testProperty "ByteString" $ \(ArbitraryByteString x) -> metaBinary x ]
    , testGroup "Binary encoding and decoding of bigword types"
        [ testProperty "Word512" (metaBinary :: Word512 -> Bool)
        , testProperty "Word256" (metaBinary :: Word256 -> Bool)
        , testProperty "Word160" (metaBinary :: Word160 -> Bool)
        , testProperty "Word128" (metaBinary :: Word128 -> Bool)
        , testProperty "FieldP" (metaBinary :: FieldP -> Bool)
        , testProperty "FieldN" binaryFieldN
        ]
    , testGroup "Binary encoding and decoding of crypto types"
        [ testProperty "Signature" $ \(ArbitrarySignature _ _ _ x) -> metaBinary x
        , testProperty "Deterministic Signature" $ 
            \(ArbitraryDetSignature _ _ x) -> metaBinary x
        , testProperty "PubKey" $ \(ArbitraryPubKey _ x) -> metaBinary x
        , testProperty "XPrvKey" $
            \(ArbitraryXPrvKey x :: ArbitraryXPrvKey Net) -> metaBinary x
        , testProperty "XPubKey" $
            \(ArbitraryXPubKey _ x :: ArbitraryXPubKey Net) -> metaBinary x
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
        , testProperty "Message" $
            \(ArbitraryMessage x :: ArbitraryMessage Net) -> metaBinary x
        ]
    , testGroup "Binary encoding and decoding of script types"
        [ testProperty "ScriptOp" $ \(ArbitraryScriptOp x) -> metaBinary x
        , testProperty "Script" $ \(ArbitraryScript x) -> metaBinary x
        , testProperty "SigHash" $ \(ArbitrarySigHash x) -> metaBinary x
        ]
    , testGroup "Binary encoding and decoding of transaction types"
        [ testProperty "TxIn" $ \(ArbitraryTxIn x) -> metaBinary x
        , testProperty "TxOut" $
            \(ArbitraryTxOut x :: ArbitraryTxOut Net) -> metaBinary x
        , testProperty "OutPoint" $ \(ArbitraryOutPoint x) -> metaBinary x
        , testProperty "Tx" $
            \(ArbitraryTx x :: ArbitraryTx Net) -> metaBinary x
        , testProperty "CoinbaseTx" $
            \(ArbitraryCoinbaseTx x :: ArbitraryCoinbaseTx Net) -> metaBinary x
        ]
    , testGroup "Binary encoding and decoding of block types"
        [ testProperty "Block" $
            \(ArbitraryBlock x :: ArbitraryBlock Net) -> metaBinary x
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

binaryFieldN :: FieldN -> Property
binaryFieldN r = r > 0 ==> decode' (encode' r) == r

