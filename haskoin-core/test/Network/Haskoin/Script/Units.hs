{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Script.Units (tests) where

import Test.HUnit (Assertion, assertBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import Data.Serialize (decode)

import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests =
    [ testGroup "Canonical signatures"
        (map canonicalVectorsMap $ zip canonicalVectors [0..])
    , testGroup "Non-canonical signatures"
        (map notCanonicalVectorsMap $ zip notCanonicalVectors [0..])
    , testGroup "Multi Signatures"
        (map mapMulSigVector $ zip mulSigVectors [0..])
    , testGroup "Signature decoding"
        (map sigDecodeMap $ zip scriptSigSignatures [0..])
    ]

canonicalVectorsMap :: (ByteString, Int) -> Test.Framework.Test
canonicalVectorsMap (_, i) =
    testCase ("Canonical Sig " ++ (show i)) func
  where
    func = testCanonicalSig $ canonicalVectors !! i

notCanonicalVectorsMap :: (ByteString, Int) -> Test.Framework.Test
notCanonicalVectorsMap (_, i) =
    testCase ("Not canonical Sig " ++ show i) func
  where
    func = testNotCanonicalSig $ notCanonicalVectors !! i

sigDecodeMap :: (ByteString, Int) -> Test.Framework.Test
sigDecodeMap (_, i) =
    testCase ("Signature " ++ show i) func
  where
    func = testSigDecode $ scriptSigSignatures !! i

testCanonicalSig :: ByteString -> Assertion
testCanonicalSig str =
    assertBool "    > Canonical Sig" $ isRight $ decodeCanonicalSig bs
  where
    bs = fromJust $ decodeHex str

testNotCanonicalSig :: ByteString -> Assertion
testNotCanonicalSig str =
    assertBool "    > Not canonical sig" $ isLeft $ decodeCanonicalSig bs
  where
    bs = fromJust $ decodeHex str

mapMulSigVector :: ((ByteString, ByteString), Int) -> Test.Framework.Test
mapMulSigVector (v, i) =
    testCase name $ runMulSigVector v
  where
    name = "MultiSignature vector " ++ (show i)

runMulSigVector :: (ByteString, ByteString) -> Assertion
runMulSigVector (a, ops) =
    assertBool "    >  MultiSig Vector" $ a == b
  where
    s = fromRight . decode $ fromJust $ decodeHex ops
    b = addrToBase58 $ scriptAddr $ fromRight $ decodeOutput s

testSigDecode :: ByteString -> Assertion
testSigDecode str =
  let bs = fromJust $ decodeHex str
      eitherSig = decodeSig bs
  in
  assertBool (unwords ["Decode failed:", fromLeft eitherSig]) $ isRight eitherSig

{- Canonical Signatures -}

-- Test vectors from bitcoind
-- http://github.com/bitcoin/bitcoin/blob/master/src/test/data/sig_canonical.json

canonicalVectors :: [ByteString]
canonicalVectors =
    [ "300602010102010101" -- Changed 0x00 to 0x01 as 0x00 is invalid
    , "3008020200ff020200ff01"
    , "304402203932c892e2e550f3af8ee4ce9c215a87f9bb831dcac87b2838e2c2eaa891df0c022030b61dd36543125d56b9f9f3a1f9353189e5af33cdda8d77a5209aec03978fa001"
    , "30450220076045be6f9eca28ff1ec606b833d0b87e70b2a630f5e3a496b110967a40f90a0221008fffd599910eefe00bc803c688c2eca1d2ba7f6b180620eaa03488e6585db6ba01"
    , "3046022100876045be6f9eca28ff1ec606b833d0b87e70b2a630f5e3a496b110967a40f90a0221008fffd599910eefe00bc803c688c2eca1d2ba7f6b180620eaa03488e6585db6ba01"
    ]

notCanonicalVectors :: [ByteString]
notCanonicalVectors =
    [ "30050201ff020001"
    , "30470221005990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba6105022200002d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed11"
    , "314402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "301f01205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb101"
    , "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed0001"
    , "304401205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "3024020002202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "304402208990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "30450221005990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610501202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "302402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba6105020001"
    , "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba61050220fd5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba61050221002d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    ]

mulSigVectors :: [(ByteString, ByteString)]
mulSigVectors =
    [ ( "3QJmV3qfvL9SuYo34YihAf3sRCW3qSinyC"
      , "52410491bba2510912a5bd37da1fb5b1673010e43d2c6d812c514e91bfa9f2eb129e1c183329db55bd868e209aac2fbc02cb33d98fe74bf23f0c235d6126b1d8334f864104865c40293a680cb9c020e7b1e106d8c1916d3cef99aa431a56d253e69256dac09ef122b1a986818a7cb624532f062c1d1f8722084861c5c3291ccffef4ec687441048d2455d2403e08708fc1f556002f1b6cd83f992d085097f9974ab08a28838f07896fbab08f39495e15fa6fad6edbfb1e754e35fa1c7844c41f322a1863d4621353ae"
      )
    ]

scriptSigSignatures :: [ByteString]
scriptSigSignatures =
     -- Signature in input of txid 1983a69265920c24f89aac81942b1a59f7eb30821a8b3fb258f88882b6336053
    [ "304402205ca6249f43538908151fe67b26d020306c0e59fa206cf9f3ccf641f33357119d02206c82f244d04ac0a48024fb9cc246b66e58598acf206139bdb7b75a2941a2b1e401"
      -- Signature in input of txid fb0a1d8d34fa5537e461ac384bac761125e1bfa7fec286fa72511240fa66864d  Strange DER sizes. But in Blockchain
    , "3048022200002b83d59c1d23c08efd82ee0662fec23309c3adbcbd1f0b8695378db4b14e736602220000334a96676e58b1bb01784cb7c556dd8ce1c220171904da22e18fe1e7d1510db501"
    ]
