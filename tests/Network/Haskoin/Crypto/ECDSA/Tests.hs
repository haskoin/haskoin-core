module Network.Haskoin.Crypto.ECDSA.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Property ((==>), Property)

import Data.Bits (testBit)
import qualified Data.ByteString as BS

import Network.Haskoin.Crypto.Arbitrary

import Network.Haskoin.Crypto.ECDSA
import Network.Haskoin.Crypto.Point
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Curve
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "ECDSA signatures"
        [ testProperty "verify( sign(msg) ) = True" signAndVerify
        , testProperty "verify( detSign(msg) ) = True" signAndVerifyD
        , testProperty "S component <= order/2" halfOrderSig
        ],
      testGroup "ECDSA Binary"
        [ testProperty "get( put(Sig) ) = Sig" getPutSig
        , testProperty "Encoded signature is canonical" testIsCanonical
        ]
    ]

{- ECDSA Signatures -}

signAndVerify :: Word256 -> FieldN -> FieldN -> Property
signAndVerify msg k n = k > 0 && n > 0 ==> case sM of
    (Just s) -> verifySig msg s (PubKey kP)
    Nothing  -> True -- very bad luck
    where kP = mulPoint k curveG
          nP = mulPoint n curveG
          sM = unsafeSignMsg msg k (n,nP)

signAndVerifyD :: Word256 -> TestPrvKeyC -> Bool
signAndVerifyD msg (TestPrvKeyC k) = verifySig msg (detSignMsg msg k) p
    where p = derivePubKey k
           
halfOrderSig :: Signature -> Bool
halfOrderSig sig@(Signature _ (BigWord s)) = 
    s <= (curveN `div` 2) && isCanonicalHalfOrder sig

{- ECDSA Binary -}

getPutSig :: Signature -> Bool
getPutSig sig = (decode' $ encode' sig) == sig

-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- from function IsCanonicalSignature
testIsCanonical :: Signature -> Bool
testIsCanonical sig = not $
    -- Non-canonical signature: too short
    (len < 8) ||
    -- Non-canonical signature: too long
    (len > 72) ||
    -- Non-canonical signature: wrong type
    (BS.index s 0 /= 0x30) ||
    -- Non-canonical signature: wrong length marker
    (BS.index s 1 /= len - 2) ||
    -- Non-canonical signature: S length misplaced
    (5 + rlen >= len) || 
    -- Non-canonical signature: R+S length mismatch
    (rlen + slen + 6 /= len) ||
    -- Non-canonical signature: R value type mismatch
    (BS.index s 2 /= 0x02) ||
    -- Non-canonical signature: R length is zero
    (rlen == 0) ||
    -- Non-canonical signature: R value negative
    (testBit (BS.index s 4) 7) ||
    -- Non-canonical signature: R value excessively padded
    (  rlen > 1 
    && BS.index s 4 == 0 
    && not (testBit (BS.index s 5) 7)
    ) ||
    -- Non-canonical signature: S value type mismatch
    (BS.index s (fromIntegral rlen+4) /= 0x02) ||
    -- Non-canonical signature: S length is zero
    (slen == 0) ||
    -- Non-canonical signature: S value negative
    (testBit (BS.index s (fromIntegral rlen+6)) 7) ||
    -- Non-canonical signature: S value excessively padded
    (  slen > 1
    && BS.index s (fromIntegral rlen+6) == 0 
    && not (testBit (BS.index s (fromIntegral rlen+7)) 7)
    ) 
    where s = encode' sig
          len = fromIntegral $ BS.length s
          rlen = BS.index s 3
          slen = BS.index s (fromIntegral rlen + 5)

