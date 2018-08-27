module Network.Haskoin.Crypto.Base58.Tests
    ( spec
    ) where

import           Data.String               (fromString)
import           Data.String.Conversions   (cs)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Test
import           Test.Hspec
import           Test.QuickCheck

spec :: Network -> Spec
spec net =
    describe "base58 addresses" $ do
        it "encodes and decodes base58 bytestring" $
            property $
            forAll arbitraryBS $ \bs ->
                decodeBase58 (encodeBase58 bs) == Just bs
        it "encodes and decodes base58 bytestring with checksum" $
            property $
            forAll arbitraryBS $ \bs ->
                decodeBase58Check (encodeBase58Check bs) == Just bs
        it "encodes and decodes address" $
            property $
            forAll (arbitraryAddress net) $ \a ->
                (stringToAddr net =<< addrToString a) == Just a
