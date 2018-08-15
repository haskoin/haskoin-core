module Network.Haskoin.Crypto.Base58.Tests (tests) where

import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Network.Haskoin.Crypto
import           Network.Haskoin.Test
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

tests :: [Test]
tests =
    [ testGroup
          "Address and Base58"
          [ testProperty "decode58 . encode58 == id" $
            forAll arbitraryBS $ \bs ->
                decodeBase58 (encodeBase58 bs) == Just bs
          , testProperty "decode58Chk . encode58Chk == id" $
            forAll arbitraryBS $ \bs ->
                decodeBase58Check (encodeBase58Check bs) == Just bs
          , testProperty "base58ToAddr . addrToBase58 == id" $
            forAll arbitraryAddress $ \a ->
                base58ToAddr (addrToBase58 a) == Just a
          , testProperty "Read/Show address" $
            forAll arbitraryAddress $ \a -> read (show a) == a
          , testProperty "From string address" $
            forAll arbitraryAddress $ \a ->
                fromString (cs $ addrToBase58 a) == a
          ]
    ]

