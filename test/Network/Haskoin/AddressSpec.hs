{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.AddressSpec (spec) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS (append, empty, pack)
import           Data.Text                      (Text)
import           Network.Haskoin.Address
import           Network.Haskoin.Address.Base58
import           Network.Haskoin.Constants
import           Network.Haskoin.Test
import           Test.Hspec
import           Test.HUnit                     (Assertion, assertBool)
import           Test.QuickCheck

spec :: Spec
spec = do
    let net = btc
    describe "btc address" $
        it "base58 encodings" $ mapM_ runVector vectors
    describe "btc-test address" $ props btcTest
    describe "bch address" $ props bch
    describe "bch-test address" $ props bchTest
    describe "bch-regtest address" $ props bchRegTest
    describe "json serialization" $
        it "encodes and decodes address" $
            forAll arbitraryAddress (testCustom (addrFromJSON net) (addrToJSON net))

props :: Network -> Spec
props net = do
    it "encodes and decodes base58 bytestring" $
        property $
        forAll arbitraryBS $ \bs -> decodeBase58 (encodeBase58 bs) == Just bs
    it "encodes and decodes base58 bytestring with checksum" $
        property $
        forAll arbitraryBS $ \bs ->
            decodeBase58Check (encodeBase58Check bs) == Just bs
    it "encodes and decodes address" $
        property $
        forAll arbitraryAddress $ \a ->
            (stringToAddr net =<< addrToString net a) == Just a
    it "shows and reads address" $
        property $ forAll arbitraryAddress $ \a -> read (show a) == a

runVector :: (ByteString, Text, Text) -> Assertion
runVector (bs, e, chk) = do
    assertBool "encodeBase58" $ e == b58
    assertBool "encodeBase58Check" $ chk == b58Chk
    assertBool "decodeBase58" $ Just bs == decodeBase58 b58
    assertBool "decodeBase58Check" $ Just bs == decodeBase58Check b58Chk
  where
    b58    = encodeBase58 bs
    b58Chk = encodeBase58Check bs

vectors :: [(ByteString, Text, Text)]
vectors =
    [ ( BS.empty, "", "3QJmnh" )
    , ( BS.pack [0], "1", "1Wh4bh" )
    , ( BS.pack [0,0,0,0], "1111", "11114bdQda" )
    , ( BS.pack [0,0,1,0,0], "11LUw", "113CUwsFVuo" )
    , ( BS.pack [255], "5Q", "VrZDWwe" )
    , ( BS.pack [0,0,0,0] `BS.append` BS.pack [1..255]
      , "1111cWB5HCBdLjAuqGGReWE3R3CguuwSjw6RHn39s2yuDRTS5NsBgNiFpWgAnEx6VQi8csexkgYw3mdYrMHr8x9i7aEwP8kZ7vccXWqKDvGv3u1GxFKPuAkn8JCPPGDMf3vMMnbzm6Nh9zh1gcNsMvH3ZNLmP5fSG6DGbbi2tuwMWPthr4boWwCxf7ewSgNQeacyozhKDDQQ1qL5fQFUW52QKUZDZ5fw3KXNQJMcNTcaB723LchjeKun7MuGW5qyCBZYzA1KjofN1gYBV3NqyhQJ3Ns746GNuf9N2pQPmHz4xpnSrrfCvy6TVVz5d4PdrjeshsWQwpZsZGzvbdAdN8MKV5QsBDY"
      , "111151KWPPBRzdWPr1ASeu172gVgLf1YfUp6VJyk6K9t4cLqYtFHcMa2iX8S3NJEprUcW7W5LvaPRpz7UG7puBj5STE3nKhCGt5eckYq7mMn5nT7oTTic2BAX6zDdqrmGCnkszQkzkz8e5QLGDjf7KeQgtEDm4UER6DMSdBjFQVa6cHrrJn9myVyyhUrsVnfUk2WmNFZvkWv3Tnvzo2cJ1xW62XDfUgYz1pd97eUGGPuXvDFfLsBVd1dfdUhPwxW7pMPgdWHTmg5uqKGFF6vE4xXpAqZTbTxRZjCDdTn68c2wrcxApm8hq3JX65Hix7VtcD13FF8b7BzBtwjXq1ze6NMjKgUcqpJTN9vt"
      )
    ]

testCustom :: Eq a => (Value -> Parser a) -> (a -> Value) -> a -> Bool
testCustom f g x = parseMaybe f (g x) == Just x
