{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Address.Bech32Spec       ( spec )
where

import           Control.Monad
import           Data.ByteString                ( ByteString
                                                , uncons
                                                , append
                                                , snoc
                                                , pack
                                                )
import qualified Data.ByteString.Char8         as C
import           Data.Maybe
import           Data.Char                      ( toLower )
import           Data.Bits                      ( xor )
import           Data.Word                      ( Word8 )
import           Network.Haskoin.Address.Bech32
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.HUnit

spec = do
    describe "bech32 checksum" $ do
        forM_ validChecksums $ \checksum ->
            it "should have a valid checksum" $ testValidChecksum checksum
        forM_ invalidChecksums
            $ \checksum -> it "should have a invalid checksum"
                  $ testInvalidChecksum checksum
        forM_ validAddresses $ \vector ->
            it "should be a valid address" $ testValidAddress vector
        forM_ invalidAddresses $ \address ->
            it "should be an invalid address" $ testInvalidAddress address

    describe "More Encoding/Decoding Cases" $ do
        it "length > 90" $ assert $ isNothing $ bech32Encode
            (C.pack "bc")
            (replicate 82 (word5 (1 :: Word8)))
        it "segwit version bounds" $ assert $ isNothing $ segwitEncode
            (C.pack "bc")
            17
            []
        it "segwit prog len version 0" $ assert $ isNothing $ segwitEncode
            (C.pack "bc")
            0
            (replicate 30 1)
        it "segwit prog len version != 0" $ assert $ isJust $ segwitEncode
            (C.pack "bc")
            1
            (replicate 30 1)
        it "segwit prog len version != 0" $ assert $ isNothing $ segwitEncode
            (C.pack "bc")
            1
            (replicate 41 1)
        it "empty HRP encode" $ assert $ isNothing $ bech32Encode (C.pack "") []
        it "empty HRP encode" $ assert $ isNothing $ bech32Decode
            (C.pack "10a06t8")
        it "hrp lowercased"
            $          (Just $ C.pack "hrp1g9xj8m")
            `shouldBe` (bech32Encode (C.pack "HRP") [])


testValidChecksum :: Bech32 -> Assertion
testValidChecksum checksum = case bech32Decode checksum of
    Nothing                      -> assertFailure (show checksum)
    Just (resultHRP, resultData) -> do
        -- test that a corrupted checksum fails decoding.
        let (hrp, rest)         = C.breakEnd (== '1') checksum
            Just (first, rest') = uncons rest
            checksumCorrupted   = (hrp `snoc` (first `xor` 1)) `append` rest'
        assertBool (show checksum ++ " corrupted")
            $ isNothing (bech32Decode checksumCorrupted)
        -- test that re-encoding the decoded checksum results in the same checksum.
        let checksumEncoded  = bech32Encode resultHRP resultData
            expectedChecksum = Just $ C.map toLower checksum
        assertEqual (show checksum ++ " re-encode")
                    expectedChecksum
                    checksumEncoded

testInvalidChecksum :: Bech32 -> Assertion
testInvalidChecksum checksum =
    assertBool (show checksum) (isNothing $ bech32Decode checksum)

testValidAddress :: (ByteString, ByteString) -> Assertion
testValidAddress (address, hexscript) = do
    let address' = C.map toLower address
        hrp      = C.take 2 address'
    case segwitDecode hrp address of
        Nothing                -> assertFailure "decode failed"
        Just (witver, witprog) -> do
            assertEqual (show address)
                        (decodeHex hexscript)
                        (Just $ segwitScriptPubkey witver witprog)
            assertEqual (show address)
                        (Just address')
                        (segwitEncode hrp witver witprog)

testInvalidAddress :: ByteString -> Assertion
testInvalidAddress address = do
    assertBool (show address) (isNothing $ segwitDecode (C.pack "bc") address)
    assertBool (show address) (isNothing $ segwitDecode (C.pack "tb") address)

segwitScriptPubkey :: Word8 -> [Word8] -> ByteString
segwitScriptPubkey witver witprog =
    pack $ witver' : (fromIntegral $ length witprog) : witprog
    where witver' = if witver == 0 then 0 else witver + 0x50

validChecksums :: [ByteString]
validChecksums = map C.pack
    [ "A12UEL5L"
    , "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"
    , "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
    , "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
    , "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
    ]

invalidChecksums :: [ByteString]
invalidChecksums = map
    C.pack
    [ " 1nwldj5"
    , "\DEL1axkwrx"
    , "an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx"
    , "pzry9x0s0muk"
    , "1pzry9x0s0muk"
    , "x1b4n0q5v"
    , "li1dgmt3"
    , "de1lg7wt\xFF"
    ]

validAddresses :: [(ByteString, ByteString)]
validAddresses = map mapTuple
    [ ("BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4", "0014751e76e8199196d454941c45d1b3a323f1433bd6")
    , ("tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
      ,"00201863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262")
    , ("bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7k7grplx"
      ,"5128751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6")
    , ("BC1SW50QA3JX3S", "6002751e")
    , ("bc1zw508d6qejxtdg4y5r3zarvaryvg6kdaj", "5210751e76e8199196d454941c45d1b3a323")
    , ("tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy"
      ,"0020000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433")
    ]
  where
    mapTuple (a, b) = (C.pack a, C.pack b)

invalidAddresses :: [ByteString]
invalidAddresses = map C.pack
    [ "tc1qw508d6qejxtdg4y5r3zarvary0c5xw7kg3g4ty"
    , "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t5"
    , "BC13W508D6QEJXTDG4Y5R3ZARVARY0C5XW7KN40WF2"
    , "bc1rw5uspcuh"
    , "bc10w508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kw5rljs90"
    , "BC1QR508D6QEJXTDG4Y5R3ZARVARYV98GJ9P"
    , "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sL5k7"
    , "bc1zw508d6qejxtdg4y5r3zarvaryvqyzf3du"
    , "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3pjxtptv"
    , "bc1gmk9yu"
    ]
