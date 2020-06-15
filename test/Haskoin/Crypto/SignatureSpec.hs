{-# LANGUAGE OverloadedStrings #-}
module Haskoin.Crypto.SignatureSpec (spec) where

import           Control.Monad
import           Data.Bits               (testBit)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.Serialize          as S
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Haskoin.Crypto
import           Haskoin.Util
import           Haskoin.Util.Arbitrary
import           Haskoin.UtilSpec        (readTestFile)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "Signature properties" $ do
        prop "verify signature" $
            forAll arbitrarySignature $ \(m, key, sig) ->
                verifyHashSig m sig (derivePubKey key)
        prop "s component less than half order" $
            forAll arbitrarySignature $ isCanonicalHalfOrder . lst3
        prop "encoded signature is canonical" $
            forAll arbitrarySignature $ testIsCanonical . lst3
        prop "decodeStrictSig . exportSig identity" $
            forAll arbitrarySignature $
            (\s -> decodeStrictSig (exportSig s) == Just s) . lst3
        prop "importSig . exportSig identity" $
            forAll arbitrarySignature $
            (\s -> importSig (exportSig s) == Just s) . lst3
        prop "getSig . putSig identity" $
            forAll arbitrarySignature $
            (\s -> runGet getSig (runPut $ putSig s) == Right s) . lst3
    describe "Signature vectors" $
        checkDistSig $ \file1 file2 -> do
            vectors <- runIO (readTestFile file1 :: IO [(Text, Text, Text)])
            vectorsDER <- runIO (readTestFile file2 :: IO [(Text, Text, Text)])
            it "Passes the trezor rfc6979 test vectors" $
                mapM_ (testRFC6979Vector . toVector) vectors
            it "Passes the rfc6979 DER test vectors" $
                mapM_ (testRFC6979DERVector . toVector) vectorsDER

-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- from function IsCanonicalSignature
testIsCanonical :: Sig -> Bool
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
    testBit (BS.index s 4) 7 ||
    -- Non-canonical signature: R value excessively padded
    (  rlen > 1
    && BS.index s 4 == 0
    && not (testBit (BS.index s 5) 7)
    ) ||
    -- Non-canonical signature: S value type mismatch
    (BS.index s (fromIntegral rlen + 4) /= 0x02) ||
    -- Non-canonical signature: S length is zero
    (slen == 0) ||
    -- Non-canonical signature: S value negative
    testBit (BS.index s (fromIntegral rlen+6)) 7 ||
    -- Non-canonical signature: S value excessively padded
    (  slen > 1
    && BS.index s (fromIntegral rlen + 6) == 0
    && not (testBit (BS.index s (fromIntegral rlen + 7)) 7)
    )
  where
    s = exportSig sig
    len = fromIntegral $ BS.length s
    rlen = BS.index s 3
    slen = BS.index s (fromIntegral rlen + 5)

-- RFC6979 note: Different libraries of libsecp256k1 use different constants
-- to produce a nonce. Thus, their deterministric signatures will be different.
-- We still want to test against fixed signatures so we need a way to switch
-- between implementations. We check the output of signMsg 1 0

distSig :: Text
distSig =
    encodeHex $
    exportSig $
    signMsg
        "0000000000000000000000000000000000000000000000000000000000000001"
        "0000000000000000000000000000000000000000000000000000000000000000"

-- We have test vectors for these cases
validDistSig :: Map Text (FilePath, FilePath)
validDistSig =
    Map.fromList
        [ ( "3045022100a0b37f8fba683cc68f6574cd43b39f0343a50008bf6ccea9d13231\
            \d9e7e2e1e4022011edc8d307254296264aebfc3dc76cd8b668373a072fd64665\
            \b50000e9fcce52"
          , ("rfc6979core.json", "rfc6979DERcore.json"))
        , ( "304402200581361d23e645be9e3efe63a9a2ac2e8dd0c70ba3ac8554c9befe06\
            \0ad0b36202207d8172f1e259395834793d81b17e986f1e6131e4734969d2f4ae\
            \3a9c8bc42965"
          , ("rfc6979abc.json", "rfc6979DERabc.json"))
        ]

checkDistSig :: (FilePath -> FilePath -> Spec) -> Spec
checkDistSig go =
    case distSig `Map.lookup` validDistSig of
        Just (file1, file2) -> go file1 file2
        _ ->
            it "Passes rfc6979 test vectors" $
            void $ assertFailure "Invalid rfc6979 signature"

{- Trezor RFC 6979 Test Vectors -}
-- github.com/trezor/python-ecdsa/blob/master/ecdsa/test_pyecdsa.py

toVector :: (Text, Text, Text) -> (SecKey, ByteString, Text)
toVector (prv, m, res) = (fromJust $ (secKey <=< decodeHex) prv, cs m, res)

testRFC6979Vector :: (SecKey, ByteString, Text) -> Assertion
testRFC6979Vector (prv, m, res) = do
    assertEqual "RFC 6979 Vector" res (encodeHex $ encode $ exportCompactSig s)
    assertBool "Signature is valid" $ verifyHashSig h s (derivePubKey prv)
    assertBool "Signature is canonical" $ testIsCanonical s
    assertBool "Signature is normalized" $ isCanonicalHalfOrder s
  where
    h = sha256 m
    s = signHash prv h

-- Test vectors from:
-- https://crypto.stackexchange.com/questions/20838/request-for-data-to-test-deterministic-ecdsa-signature-algorithm-for-secp256k1

testRFC6979DERVector :: (SecKey, ByteString, Text) -> Assertion
testRFC6979DERVector (prv, m, res) = do
    assertEqual "RFC 6979 DER Vector" res (encodeHex $ exportSig s)
    assertBool "DER Signature is valid" $ verifyHashSig h s (derivePubKey prv)
    assertBool "DER Signature is canonical" $ testIsCanonical s
    assertBool "DER Signature is normalized" $ isCanonicalHalfOrder s
  where
    h = sha256 m
    s = signHash prv h

