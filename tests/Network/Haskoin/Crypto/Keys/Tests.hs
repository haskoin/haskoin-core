module Network.Haskoin.Crypto.Keys.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS (length, index)

import qualified Crypto.Secp256k1 as EC

import Network.Haskoin.Test
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Internals (PubKeyI(..), PrvKeyI(..))

tests :: [Test]
tests =
    [ testGroup "PubKey Binary"
        [ testProperty "is public key canonical" isCanonicalPubKey
        , testProperty "makeKey . toKey" makeToKey
        , testProperty "makeKeyU . toKey" makeToKeyU
        ]
    , testGroup "Key formats"
        [ testProperty "fromWif . toWif PrvKey" fromToWIF
        , testProperty "constant 32-byte encoding PrvKey" binaryPrvKey
        ]
    , testGroup "Key compression"
        [ testProperty "Compressed public key" testCompressed
        , testProperty "Uncompressed public key" testUnCompressed
        , testProperty "Compressed private key" testPrivateCompressed
        , testProperty "Uncompressed private key" testPrivateUnCompressed
        ]
    ]

-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- from function IsCanonicalPubKey
isCanonicalPubKey :: ArbitraryPubKey -> Bool
isCanonicalPubKey (ArbitraryPubKey _ p) = not $
    -- Non-canonical public key: too short
    (BS.length bs < 33) ||
    -- Non-canonical public key: invalid length for uncompressed key
    (BS.index bs 0 == 4 && BS.length bs /= 65) ||
    -- Non-canonical public key: invalid length for compressed key
    (BS.index bs 0 `elem` [2,3] && BS.length bs /= 33) ||
    -- Non-canonical public key: compressed nor uncompressed
    (not $ BS.index bs 0 `elem` [2,3,4])
  where
    bs = encode' p

makeToKey :: EC.SecKey -> Bool
makeToKey i = prvKeySecKey (makePrvKey i) == i

makeToKeyU :: EC.SecKey -> Bool
makeToKeyU i = prvKeySecKey (makePrvKeyU i) == i

{- Key formats -}

fromToWIF :: ArbitraryPrvKey -> Bool
fromToWIF (ArbitraryPrvKey pk) = (fromWif $ toWif pk) == Just pk

binaryPrvKey :: ArbitraryPrvKey -> Bool
binaryPrvKey (ArbitraryPrvKey k) =
    (k == runGet (prvKeyGetMonad f) (runPut $ prvKeyPutMonad k)) &&
    (Just k == decodePrvKey f (encodePrvKey k))
  where
    f = makePrvKeyG (prvKeyCompressed k)

{- Key Compression -}

testCompressed :: EC.SecKey -> Bool
testCompressed n =
    (pubKeyCompressed $ derivePubKey $ makePrvKey n) &&
    (pubKeyCompressed $ derivePubKey $ makePrvKeyG True n)

testUnCompressed :: EC.SecKey -> Bool
testUnCompressed n =
    (not $ pubKeyCompressed $ derivePubKey $ makePrvKeyG False n) &&
    (not $ pubKeyCompressed $ derivePubKey $ makePrvKeyU n)

testPrivateCompressed :: EC.SecKey -> Bool
testPrivateCompressed n =
    (prvKeyCompressed $ makePrvKey n) &&
    (prvKeyCompressed $ makePrvKeyC n)

testPrivateUnCompressed :: EC.SecKey -> Bool
testPrivateUnCompressed n =
    (not $ prvKeyCompressed $ makePrvKeyG False n) &&
    (not $ prvKeyCompressed $ makePrvKeyU n)
