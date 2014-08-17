module Network.Haskoin.Crypto.Keys.Tests (tests) where

import Test.QuickCheck.Property (Property, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe (fromJust, isNothing)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS (ByteString, length, index)

import Network.Haskoin.Crypto.Arbitrary

import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Point
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.Curve
import Network.Haskoin.Crypto.ExtendedKeys
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "PubKey Binary"
        [ testProperty "get( put(PubKey) ) = PubKey" getPutPubKey
        , testProperty "is public key canonical" isCanonicalPubKey
        , testProperty "makeKey( toKey(k) ) = k" makeToKey
        , testProperty "makeKeyU( toKey(k) ) = k" makeToKeyU
        , testProperty "decoded PubKey is always valid" decodePubKey
        ],
      testGroup "Key formats"
        [ testProperty "fromWIF( toWIF(i) ) = i" fromToWIF
        , testProperty "get( put(PrvKey) )" getPutPrv
        ],
      testGroup "Key compression"
        [ testProperty "Compressed public key" testCompressed
        , testProperty "Uncompressed public key" testUnCompressed
        , testProperty "Compressed private key" testPrivateCompressed
        , testProperty "Uncompressed private key" testPrivateUnCompressed
        ],
      testGroup "Public Key"
        [ testProperty "Derived public key valid" testDerivedPubKey
        , testProperty "Derived public key from Integer valid" deriveFromInt
        ],
      testGroup "Key properties"
        [ testProperty "PubKey addition" testAddPubKey
        , testProperty "PrvKey addition" testAddPrvKey
        ]
    ]

{- Public Key Binary -}

getPutPubKey :: PubKey -> Bool
getPutPubKey p = p == (decode' $ encode' p)

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
    (not $ BS.index bs 0 `elem` [2,3,4])
    where bs = encode' p

makeToKey :: FieldN -> Property
makeToKey i = i /= 0 ==> 
    (fromPrvKey $ makeKey (fromIntegral i)) == (fromIntegral i)
    where makeKey = fromJust . makePrvKey

makeToKeyU :: FieldN -> Property
makeToKeyU i = i /= 0 ==> 
    (fromPrvKey $ makeKey (fromIntegral i)) == (fromIntegral i)
    where makeKey = fromJust . makePrvKeyU

decodePubKey :: BS.ByteString -> Bool
decodePubKey bs = fromDecode bs True isValidPubKey

{- Key formats -}

fromToWIF :: PrvKey -> Bool
fromToWIF pk = pk == (fromJust $ fromWIF $ toWIF pk)

getPutPrv :: PrvKey -> Bool
getPutPrv k@(PrvKey  _) = k == runGet getPrvKey  (runPut $ putPrvKey k)
getPutPrv k@(PrvKeyU _) = k == runGet getPrvKeyU (runPut $ putPrvKey k)

{- Key Compression -}

testCompressed :: FieldN -> Property
testCompressed n = n > 0 ==> 
    not $ isPubKeyU $ derivePubKey $ fromJust $ makePrvKey $ fromIntegral n

testUnCompressed :: FieldN -> Property
testUnCompressed n = n > 0 ==> 
    isPubKeyU $ derivePubKey $ fromJust $ makePrvKeyU $ fromIntegral n

testPrivateCompressed :: FieldN -> Property
testPrivateCompressed n = n > 0 ==> 
    not $ isPrvKeyU $ fromJust $ makePrvKey $ fromIntegral n

testPrivateUnCompressed :: FieldN -> Property
testPrivateUnCompressed n = n > 0 ==> 
    isPrvKeyU $ fromJust $ makePrvKeyU $ fromIntegral n

testDerivedPubKey :: PrvKey -> Bool
testDerivedPubKey k = isValidPubKey $ derivePubKey k

deriveFromInt :: Integer -> Bool
deriveFromInt i = maybe True (isValidPubKey . derivePubKey) $ makePrvKey i

{- Key properties -}

testAddPubKey :: TestPrvKeyC -> Word256 -> Bool
testAddPubKey (TestPrvKeyC key) i 
    | toInteger i >= curveN = isNothing res
    | model == InfPoint     = isNothing res
    | otherwise             = PubKey model == fromJust res
    where pub   = derivePubKey key
          pt    = mulPoint (fromIntegral i :: FieldN) curveG
          model = addPoint (pubKeyPoint pub) pt
          res   = addPubKeys pub i

testAddPrvKey :: TestPrvKeyC -> Word256 -> Bool
testAddPrvKey (TestPrvKeyC key) i
    | toInteger i >= curveN = isNothing res
    | model == 0  = isNothing res
    | otherwise   = PrvKey model == fromJust res
    where model = (prvKeyFieldN key) + (fromIntegral i :: FieldN)
          res   = addPrvKeys key i

