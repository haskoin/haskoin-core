module Haskoin.Wallet.Tests (tests) where

import Test.QuickCheck.Property hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Control.Monad
import Control.Applicative

import Data.Bits
import Data.Maybe
import Data.Binary
import qualified Data.ByteString as BS

import QuickCheckUtils

import Haskoin.Wallet
import Haskoin.Wallet.TxBuilder
import Haskoin.Wallet.Arbitrary
import Haskoin.Script
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
    , testGroup "Building Transactions"
        [ testProperty "building PKHash Tx" testBuildPKHashTx
        , testProperty "building ScriptHash Tx" testBuildScriptHashTx
        ]
    , testGroup "Signing Transactions"
        [ testProperty "Check signed transaction status" testSignTxBuild
        , testProperty "Sign and validate transactions" testSignTxValidate
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

{- Building Transactions -}

testBuildPKHashTx :: [OutPoint] -> Address -> Word64 -> Bool
testBuildPKHashTx os a v = case a of
    (PubKeyAddress _) -> 
        if v <= 2100000000000000 then isRight tx else isLeft tx
    (ScriptAddress _) -> isLeft tx
    where tx = buildPKHashTx os [(addrToBase58 a,v)]

testBuildScriptHashTx :: [OutPoint] -> Address -> Word64 -> Bool
testBuildScriptHashTx os a v = case a of
    (ScriptAddress _) -> 
        if v <= 2100000000000000 then isRight tx else isLeft tx
    (PubKeyAddress _) -> isLeft tx
    where tx = buildScriptHashTx os [(addrToBase58 a,v)]

{- Signing Transactions -}

testSignTxBuild :: PKHashSigTemplate -> Bool
testSignTxBuild (PKHashSigTemplate tx sigi prv)
    | null $ txIn tx = isBroken txSig && isBroken txSigP
    | otherwise = isComplete txSig && isPartial txSigP
    where txSig = detSignTx tx sigi prv
          txSigP = detSignTx tx (tail sigi) (tail prv)
         
testSignTxValidate :: PKHashSigTemplate -> Bool
testSignTxValidate (PKHashSigTemplate tx sigi prv) =
    case detSignTx tx sigi prv of
        (Broken s)    -> True
        (Partial tx)  -> not $ verifyTx tx $ map f sigi
        (Complete tx) -> verifyTx tx $ map f sigi
    where f si = (scriptOutput $ sigDataOut si, sigDataOP si)

-- todo: test p2sh transactions



