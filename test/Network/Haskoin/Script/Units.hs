{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Script.Units (spec) where

import           Data.ByteString           (ByteString)
import           Data.Either               (fromLeft, fromRight, isRight)
import           Data.Maybe                (fromJust)
import           Data.Serialize            (decode)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Script
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.HUnit                (Assertion, assertBool)

spec :: Spec
spec = do
    describe "multi signatures" $
        sequence_ $ zipWith (curry mapMulSigVector) mulSigVectors [0 ..]
    describe "signature decoding" $
        sequence_ $ zipWith (curry sigDecodeMap) scriptSigSignatures [0 ..]

mapMulSigVector :: ((ByteString, ByteString), Int) -> Spec
mapMulSigVector (v, i) =
    it name $ runMulSigVector v
  where
    name = "check multisig vector " <> show i

runMulSigVector :: (ByteString, ByteString) -> Assertion
runMulSigVector (a, ops) = assertBool "multisig vector" $ Just a == b
  where
    s = do
        s <- decodeHex ops
        eitherToMaybe $ decode s
    b = do
        o <- s
        d <- eitherToMaybe $ decodeOutput o
        addrToString $ p2shAddr btc d

sigDecodeMap :: (ByteString, Int) -> Spec
sigDecodeMap (_, i) =
    it ("check signature " ++ show i) func
  where
    func = testSigDecode $ scriptSigSignatures !! i

testSigDecode :: ByteString -> Assertion
testSigDecode str =
    let bs = fromJust $ decodeHex str
        eitherSig = decodeTxLaxSig bs
    in assertBool
           (unwords
                [ "Decode failed:"
                , fromLeft (error "Decode did not fail") eitherSig
                ]) $
       isRight eitherSig

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
