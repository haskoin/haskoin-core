module Haskoin.Wallet.Tests (tests) where

import Test.QuickCheck.Property hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Control.Monad
import Control.Applicative

import Data.Bits
import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Wallet.Tx
import Haskoin.Wallet.ScriptParser
import Haskoin.Wallet.Arbitrary
import Haskoin.Crypto
import Haskoin.Crypto.Arbitrary
import Haskoin.Protocol
import Haskoin.Protocol.Arbitrary
import Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "HDW Extended Keys"
        [ testProperty "prvSubKey(k,c)*G = pubSubKey(k*G,c)" subkeyTest
        , testProperty "decode( encode(prvKey) ) = prvKey" binXPrvKey
        , testProperty "decode( encode(pubKey) ) = pubKey" binXPubKey
        , testProperty "fromB58( toB58(prvKey) ) = prvKey" b58PrvKey
        , testProperty "fromB58( toB58(pubKey) ) = pubKey" b58PubKey
        ]
    , testGroup "Script Parser"
        [ testProperty "canonical signatures" testCanonicalSig
        , testProperty "decode( encode(sighash) ) = sighash" binSigHash
        , testProperty "encodeSigHash32 is 4 bytes long" testEncodeSH32
        , testProperty "decode( encode(tsig) ) = tsig" binTxSig
        , testProperty "encode decode OP_1 .. OP_16" testScriptOpInt
        , testProperty "encode decode ScriptOutput" testScriptOutput
        , testProperty "encode decode ScriptInput" testScriptInput
        , testProperty "encode decode ScriptHashInput" testScriptHashInput
        ]
    , testGroup "Building Transactions"
        [ testProperty "building PKHash Tx" testBuildPKHashTx
        , testProperty "building ScriptHash Tx" testBuildScriptHashTx
        ]
    ]

{- HDW Extended Keys -}

subkeyTest :: XPrvKey -> Word32 -> Bool
subkeyTest k i = fromJust $ liftM2 (==) 
    (deriveXPubKey <$> prvSubKey k i') (pubSubKey (deriveXPubKey k) i')
    where i' = fromIntegral $ i .&. 0x7fffffff -- make it a public derivation

binXPrvKey :: XPrvKey -> Bool
binXPrvKey k = (decode' $ encode' k) == k

binXPubKey :: XPubKey -> Bool
binXPubKey k = (decode' $ encode' k) == k

b58PrvKey :: XPrvKey -> Bool
b58PrvKey k = (fromJust $ xPrvImport $ xPrvExport k) == k

b58PubKey :: XPubKey -> Bool
b58PubKey k = (fromJust $ xPubImport $ xPubExport k) == k

{- Script Parser -}

testCanonicalSig :: TxSignature -> Bool
testCanonicalSig ts = isCanonicalSig bs && isCanonicalEvenSig bs
    where bs = encode' ts

binSigHash :: SigHash -> Bool
binSigHash sh = (decode' $ encode' sh) == sh

testEncodeSH32 :: SigHash -> Bool
testEncodeSH32 sh = BS.length bs == 4 && BS.head bs /= 0 && BS.tail bs == zs
    where bs = encodeSigHash32 sh
          zs = BS.pack [0,0,0]

binTxSig :: TxSignature -> Bool
binTxSig ts = (decode' $ encode' ts) == ts

testScriptOpInt :: ScriptOpInt -> Bool
testScriptOpInt (ScriptOpInt i) = (scriptOpToInt i >>= intToScriptOp) == Just i

testScriptOutput :: ScriptOutput -> Bool
testScriptOutput so = (encodeOutput so >>= decodeOutput) == Just so

testScriptInput :: ScriptInput -> Bool
testScriptInput si = (encodeInput si >>= decodeInput) == Just si

testScriptHashInput :: ScriptHashInput -> Bool
testScriptHashInput sh = (encodeScriptHash sh >>= decodeScriptHash) == Just sh

{- Building Transactions -}

testBuildPKHashTx :: OutPoint -> Address -> Word64 -> Bool
testBuildPKHashTx o a v = case a of
    (PubKeyAddress _) -> 
        if v <= 2100000000000000 then isJust tx else isNothing tx
    (ScriptAddress _) -> isNothing tx
    where tx = buildPKHashTx [o] [(addrToBase58 a,v)]

testBuildScriptHashTx :: OutPoint -> Address -> Word64 -> Bool
testBuildScriptHashTx o a v = case a of
    (ScriptAddress _) -> 
        if v <= 2100000000000000 then isJust tx else isNothing tx
    (PubKeyAddress _) -> isNothing tx
    where tx = buildScriptHashTx [o] [(addrToBase58 a,v)]

