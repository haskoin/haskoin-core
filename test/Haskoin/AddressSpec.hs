{-# LANGUAGE OverloadedStrings #-}
module Haskoin.AddressSpec (spec) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS (append, empty, pack)
import           Data.Maybe             (fromJust, isJust)
import           Data.Text              (Text)
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Keys           (derivePubKeyI)
import           Haskoin.Util.Arbitrary
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

serialVals :: [SerialBox]
serialVals = [SerialBox arbitraryAddressAll]

readVals :: [ReadBox]
readVals = [ReadBox arbitraryAddressAll]

netVals :: [NetBox]
netVals =
    [NetBox (addrToJSON, addrToEncoding, addrFromJSON, arbitraryNetAddress)]

spec :: Spec
spec = do
    testIdentity serialVals readVals [] netVals
    describe "Address properties" $ do
        prop "encodes and decodes base58 bytestring" $
            forAll arbitraryBS $ \bs ->
                decodeBase58 (encodeBase58 bs) == Just bs
        prop "encodes and decodes base58 bytestring with checksum" $
            forAll arbitraryBS $ \bs ->
                decodeBase58Check (encodeBase58Check bs) == Just bs
        prop "textToAddr . addrToText identity" $
            forAll arbitraryNetAddress $ \(net, a) ->
                (textToAddr net =<< addrToText net a) == Just a
    describe "WIF key properties" $
        prop "encode and decode wif private keys" $
        forAll arbitraryNetwork $ \net ->
            forAll arbitraryKeyPair $ \(pk, _) ->
                fromWif net (toWif net pk) == Just pk
    describe "Address vectors" $ do
        it "Passes Base58 address vectors" $
            mapM_ runVector vectors
        it "Passes addresses witness p2sh(pwpkh) vectors" $
            mapM_ testCompatWitness compatWitnessVectors

runVector :: (ByteString, Text, Text) -> Assertion
runVector (bs, e, chk) = do
    assertEqual "encodeBase58" e b58
    assertEqual "encodeBase58Check" chk b58Chk
    assertEqual "decodeBase58" (Just bs) (decodeBase58 b58)
    assertEqual "decodeBase58Check" (Just bs) (decodeBase58Check b58Chk)
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

compatWitnessVectors :: [(Network, Text, Text)]
compatWitnessVectors =
    [ ( btcTest
      , "cNUnpYpMsJXYCERYBciJnsWBpcYEFjdcbq6dxj4SskGhs7uHuJ7Q"
      , "2N6PDTueBHvXzW61B4oe5SW1D3v2Z3Vpbvw")
    ]

testCompatWitness :: (Network, Text, Text) -> Assertion
testCompatWitness (net, seckey, addr) = do
    let seckeyM = fromWif net seckey
    assertBool "decode seckey" (isJust seckeyM)
    let pubkey = derivePubKeyI (fromJust seckeyM)
    let addrM = addrToText btcTest (pubKeyCompatWitnessAddr pubkey)
    assertBool "address can be encoded" (isJust addrM)
    assertEqual "witness address matches" addr (fromJust addrM)
