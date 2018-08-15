module Network.Haskoin.Crypto.Keys.Tests (tests) where

import qualified Crypto.Secp256k1                     as EC
import qualified Data.ByteString                      as BS
import           Data.Serialize                       (encode, runGet, runPut)
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Network.Haskoin.Crypto
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

tests :: [Test]
tests =
    [ testGroup
          "PubKey Binary"
          [ testProperty "is public key canonical" $
            forAll arbitraryPubKey (isCanonicalPubKey . snd)
          , testProperty "makeKey . toKey" makeToKey
          , testProperty "makeKeyU . toKey" makeToKeyU
          ]
    , testGroup
          "Key formats"
          [ testProperty "fromWif . toWif PrvKey" $
            forAll arbitraryPrvKey $ \pk -> fromWif (toWif pk) == Just pk
          , testProperty "constant 32-byte encoding PrvKey" $
            forAll arbitraryPrvKey binaryPrvKey
          ]
    , testGroup
          "Key compression"
          [ testProperty "Compressed public key" testCompressed
          , testProperty "Uncompressed public key" testUnCompressed
          , testProperty "Compressed private key" testPrivateCompressed
          , testProperty "Uncompressed private key" testPrivateUnCompressed
          ]
    , testGroup
          "From/To strings"
          [ testProperty "Read/Show public key" $
            forAll arbitraryPubKey $ \(_, k) -> read (show k) == k
          , testProperty "Read/Show compressed public key" $
            forAll arbitraryPubKeyC $ \(_, k) -> read (show k) == k
          , testProperty "Read/Show uncompressed public key" $
            forAll arbitraryPubKeyU $ \(_, k) -> read (show k) == k
          , testProperty "Read/Show private key" $
            forAll arbitraryPrvKey $ \k -> read (show k) == k
          , testProperty "Read/Show compressed private key" $
            forAll arbitraryPrvKeyC $ \k -> read (show k) == k
          , testProperty "Read/Show uncompressed private key" $
            forAll arbitraryPrvKeyU $ \k -> read (show k) == k
          , testProperty "From string public key" $
            forAll arbitraryPubKey $ \(_, k) ->
                fromString (cs . encodeHex $ encode k) == k
          , testProperty "From string compressed public key" $
            forAll arbitraryPubKeyC $ \(_, k) ->
                fromString (cs . encodeHex $ encode k) == k
          , testProperty "From string uncompressed public key" $
            forAll arbitraryPubKeyU $ \(_, k) ->
                fromString (cs . encodeHex $ encode k) == k
          , testProperty "From string private key" $
            forAll arbitraryPrvKey $ \k -> fromString (cs $ toWif k) == k
          , testProperty "From string compressed private key" $
            forAll arbitraryPrvKeyC $ \k -> fromString (cs $ toWif k) == k
          , testProperty "From string uncompressed private key" $
            forAll arbitraryPrvKeyU $ \k -> fromString (cs $ toWif k) == k
          ]
    ]

-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- from function IsCanonicalPubKey
isCanonicalPubKey :: PubKey -> Bool
isCanonicalPubKey p = not $
    -- Non-canonical public key: too short
    (BS.length bs < 33) ||
    -- Non-canonical public key: invalid length for uncompressed key
    (BS.index bs 0 == 4 && BS.length bs /= 65) ||
    -- Non-canonical public key: invalid length for compressed key
    (BS.index bs 0 `elem` [2,3] && BS.length bs /= 33) ||
    -- Non-canonical public key: compressed nor uncompressed
    (BS.index bs 0 `notElem` [2,3,4])
  where
    bs = encode p

makeToKey :: EC.SecKey -> Bool
makeToKey i = prvKeySecKey (makePrvKey i) == i

makeToKeyU :: EC.SecKey -> Bool
makeToKeyU i = prvKeySecKey (makePrvKeyU i) == i

{- Key formats -}

binaryPrvKey :: PrvKey -> Bool
binaryPrvKey k =
    (Right k == runGet (prvKeyGetMonad f) (runPut $ prvKeyPutMonad k)) &&
    (Just k == decodePrvKey f (encodePrvKey k))
  where
    f = makePrvKeyG (prvKeyCompressed k)

{- Key Compression -}

testCompressed :: EC.SecKey -> Bool
testCompressed n =
    pubKeyCompressed (derivePubKey $ makePrvKey n) &&
    pubKeyCompressed (derivePubKey $ makePrvKeyG True n)

testUnCompressed :: EC.SecKey -> Bool
testUnCompressed n =
    not (pubKeyCompressed $ derivePubKey $ makePrvKeyG False n) &&
    not (pubKeyCompressed $ derivePubKey $ makePrvKeyU n)

testPrivateCompressed :: EC.SecKey -> Bool
testPrivateCompressed n =
    prvKeyCompressed (makePrvKey n) &&
    prvKeyCompressed (makePrvKeyC n)

testPrivateUnCompressed :: EC.SecKey -> Bool
testPrivateUnCompressed n =
    not (prvKeyCompressed $ makePrvKeyG False n) &&
    not (prvKeyCompressed $ makePrvKeyU n)
