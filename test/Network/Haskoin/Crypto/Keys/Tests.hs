module Network.Haskoin.Crypto.Keys.Tests (spec) where

import qualified Crypto.Secp256k1          as EC
import qualified Data.ByteString           as BS
import           Data.Serialize            (encode, runGet, runPut)
import           Data.String               (fromString)
import           Data.String.Conversions   (cs)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.QuickCheck

spec :: Network -> Spec
spec net = do
    describe "pubkey binary" $ do
        it "is public key canonical" $
            property $ forAll arbitraryPubKey (isCanonicalPubKey . snd)
        it "makeKey . toKey" $ property makeToKey
        it "makeKeyU . toKey" $ property makeToKeyU
    describe "key formats" $ do
        it "fromWif . toWif PrvKey" $
            property $
            forAll arbitraryPrvKey $ \pk ->
                fromWif net (toWif net pk) == Just pk
        it "constant 32-byte encoding PrvKey" $
            property $ forAll arbitraryPrvKey binaryPrvKey
    describe "key compression" $ do
        it "compressed public key" $ property testCompressed
        it "uncompressed public key" $ property testUnCompressed
        it "compressed private key" $ property testPrivateCompressed
        it "uncompressed private key" $ property testPrivateUnCompressed
    describe "keys from and to strings" $ do
        it "read and show public key" $
            property $ forAll arbitraryPubKey $ \(_, k) -> read (show k) == k
        it "read and show compressed public key" $
            property $ forAll arbitraryPubKeyC $ \(_, k) -> read (show k) == k
        it "read and show uncompressed public key" $
            property $ forAll arbitraryPubKeyU $ \(_, k) -> read (show k) == k
        it "read and show private key" $
            property $ forAll arbitraryPrvKey $ \k -> read (show k) == k
        it "read and show compressed private key" $
            property $ forAll arbitraryPrvKeyC $ \k -> read (show k) == k
        it "read and show uncompressed private key" $
            property $ forAll arbitraryPrvKeyU $ \k -> read (show k) == k
        it "from string public key" $
            property $
            forAll arbitraryPubKey $ \(_, k) ->
                fromString (cs . encodeHex $ encode k) == k
        it "from string compressed public key" $
            property $
            forAll arbitraryPubKeyC $ \(_, k) ->
                fromString (cs . encodeHex $ encode k) == k
        it "from string uncompressed public key" $
            property $
            forAll arbitraryPubKeyU $ \(_, k) ->
                fromString (cs . encodeHex $ encode k) == k

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
