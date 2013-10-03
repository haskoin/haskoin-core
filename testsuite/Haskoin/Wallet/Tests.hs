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

import Haskoin.Wallet.Arbitrary
import Haskoin.Wallet
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
    , testGroup "Scripts"
        [ testProperty "ScriptOutput from/to [ScriptOp]" scriptOutputOps
        , testProperty "parse PubKey Script" testParsePK
        , testProperty "parse PubKeyHash Script" testParsePKHash
        , testProperty "parse Sig2 Script" testParseSig2
        , testProperty "parse Sig3 Script" testParseSig3
        , testProperty "parse ScriptHash Script" testParseSHash
        ]
    ]

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

scriptOutputOps :: ScriptOutput -> Bool
scriptOutputOps so = (scriptOpsToOutput $ outputToScriptOps so) == so

testParsePK :: TxSignature -> Bool
testParsePK s = (fromJust $ parsePK $ spendPK s) == s

testParsePKHash :: TxSignature -> PubKey -> Bool
testParsePKHash s p = 
    (fromJust $ parsePKHash $ spendPKHash s p) == (s,p)

testParseSig2 :: TxSignature -> TxSignature -> Bool
testParseSig2 s1 s2 = 
    (fromJust $ parseSig2 $ spendSig2 s1 s2) == (s1,s2)

testParseSig3 :: TxSignature -> TxSignature -> TxSignature -> Bool
testParseSig3 s1 s2 s3 = 
    (fromJust $ parseSig3 $ spendSig3 s1 s2 s3) == (s1,s2,s3)

testParseSHash :: ScriptInput -> ScriptOutput -> Bool
testParseSHash si so = 
    (fromJust $ parseSHash $ spendSHash si so) == (si,so)
