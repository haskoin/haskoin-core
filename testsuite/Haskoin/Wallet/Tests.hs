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

