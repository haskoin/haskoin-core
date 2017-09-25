module Network.Haskoin.Cereal.Tests (tests) where

import           Data.Serialize
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

tests :: [Test]
tests =
    [ testGroup "Binary encoding and decoding of utility types"
        [ testProperty "ByteString" $ forAll arbitraryBS testId ]
    , testGroup "Binary encoding and decoding of hash types"
        [ testProperty "Hash160" $ forAll arbitraryHash160 testId
        , testProperty "Hash256" $ forAll arbitraryHash256 testId
        , testProperty "Hash512" $ forAll arbitraryHash512 testId
        ]
    , testGroup "Binary encoding and decoding of crypto types"
        [ testProperty "Signature" $ forAll arbitrarySignature $ testId . lst3
        , testProperty "PubKey" $ forAll arbitraryPubKey $ testId . snd
        , testProperty "XPrvKey" $ forAll arbitraryXPrvKey testId
        , testProperty "XPubKey" $ forAll arbitraryXPubKey $ testId . snd
        ]
    , testGroup "Binary encoding and decoding of protocol types"
        [ testProperty "VarInt" $ forAll arbitraryVarInt testId
        , testProperty "VarString" $ forAll arbitraryVarString testId
        , testProperty "NetworkAddress" $ forAll arbitraryNetworkAddress testId
        , testProperty "InvType" $ forAll arbitraryInvType testId
        , testProperty "InvVector" $ forAll arbitraryInvVector testId
        , testProperty "Inv" $ forAll arbitraryInv1 testId
        , testProperty "Version" $ forAll arbitraryVersion testId
        , testProperty "Addr" $ forAll arbitraryAddr1 testId
        , testProperty "Alert" $ forAll arbitraryAlert testId
        , testProperty "Reject" $ forAll arbitraryReject testId
        , testProperty "GetData" $ forAll arbitraryGetData testId
        , testProperty "NotFound" $ forAll arbitraryNotFound testId
        , testProperty "Ping" $ forAll arbitraryPing testId
        , testProperty "Pong" $ forAll arbitraryPong testId
        , testProperty "MessageCommand" $ forAll arbitraryMessageCommand testId
        , testProperty "MessageHeader" $ forAll arbitraryMessageHeader testId
        , testProperty "Message" $ forAll arbitraryMessage testId
        ]
    , testGroup "Binary encoding and decoding of script types"
        [ testProperty "ScriptOp" $ forAll arbitraryScriptOp testId
        , testProperty "Script" $ forAll arbitraryScript testId
        , testProperty "SigHash" $ forAll arbitrarySigHash testId
        ]
    , testGroup "Binary encoding and decoding of transaction types"
        [ testProperty "TxIn" $ forAll arbitraryTxIn testId
        , testProperty "Witness" $ forAll arbitraryWitness testId
        , testProperty "TxOut" $ forAll arbitraryTxOut testId
        , testProperty "OutPoint" $ forAll arbitraryOutPoint testId
        , testProperty "Tx" $ forAll arbitraryTx testId
        ]
    , testGroup "Binary encoding and decoding of block types"
        [ testProperty "Block" $ forAll arbitraryBlock testId
        , testProperty "BlockHeader" $ forAll arbitraryBlockHeader testId
        , testProperty "GetBlocks" $ forAll arbitraryGetBlocks testId
        , testProperty "GetHeaders" $ forAll arbitraryGetHeaders testId
        , testProperty "Headers" $ forAll arbitraryHeaders testId
        , testProperty "MerkleBlock" $ forAll arbitraryMerkleBlock testId
        ]
    , testGroup "Binary encoding and decoding of bloom types"
        [ testProperty "BloomFlags" $ forAll arbitraryBloomFlags testId
        , testProperty "BloomFilter" $ forAll arbitraryBloomFilter $ testId . lst3
        , testProperty "FilterLoad" $ forAll arbitraryFilterLoad testId
        , testProperty "FilterAdd" $ forAll arbitraryFilterAdd testId
        ]
    ]

testId :: (Serialize a, Eq a) => a -> Bool
testId x = decode (encode x) == Right x
