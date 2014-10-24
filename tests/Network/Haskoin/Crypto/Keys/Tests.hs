module Network.Haskoin.Crypto.Keys.Tests (tests) where

import Test.QuickCheck.Property (Property, (==>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe (fromJust, isNothing)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS (ByteString, length, index)

import Network.Haskoin.Test.Crypto

import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Point
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.Curve
import Network.Haskoin.Crypto.ExtendedKeys
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "PubKey Binary"
        [ testProperty "decode . encode PubKey" binaryPubKey
        , testProperty "is public key canonical" isCanonicalPubKey
        , testProperty "makeKey . toKey" makeToKey
        , testProperty "makeKeyU . toKey" makeToKeyU
        ],
      testGroup "Key formats"
        [ testProperty "fromWIF . toWIF PrvKey" fromToWIF
        , testProperty "decode . encode PrvKey" binaryPrvKey
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
        [ testProperty "PrvKey and PubKey are valid" validKeys
        ]
    ]

{- Public Key Binary -}

binaryPubKey :: ArbitraryPubKey -> Bool
binaryPubKey (ArbitraryPubKey _ p) = p == (decode' $ encode' p)

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

makeToKey :: ArbitraryFieldN -> Property
makeToKey (ArbitraryBigWord i) = i /= 0 ==> 
    (fromPrvKey $ makeKey (fromIntegral i)) == (fromIntegral i)
  where 
    makeKey = fromJust . makePrvKey

makeToKeyU :: ArbitraryFieldN -> Property
makeToKeyU (ArbitraryBigWord i) = i /= 0 ==> 
    (fromPrvKey $ makeKey (fromIntegral i)) == (fromIntegral i)
  where 
    makeKey = fromJust . makePrvKeyU

{- Key formats -}

fromToWIF :: ArbitraryPrvKey -> Bool
fromToWIF (ArbitraryPrvKey pk) = (fromWIF $ toWIF pk) == Just pk

binaryPrvKey :: ArbitraryPrvKey -> Bool
binaryPrvKey (ArbitraryPrvKey k) = case k of
    PrvKey _  -> k == runGet getPrvKey  (runPut $ putPrvKey k)
    PrvKeyU _ -> k == runGet getPrvKeyU (runPut $ putPrvKey k)

{- Key Compression -}

testCompressed :: ArbitraryFieldN -> Property
testCompressed (ArbitraryBigWord n) = n > 0 ==> 
    not $ isPubKeyU $ derivePubKey $ fromJust $ makePrvKey $ fromIntegral n

testUnCompressed :: ArbitraryFieldN -> Property
testUnCompressed (ArbitraryBigWord n) = n > 0 ==> 
    isPubKeyU $ derivePubKey $ fromJust $ makePrvKeyU $ fromIntegral n

testPrivateCompressed :: ArbitraryFieldN -> Property
testPrivateCompressed (ArbitraryBigWord n) = n > 0 ==> 
    not $ isPrvKeyU $ fromJust $ makePrvKey $ fromIntegral n

testPrivateUnCompressed :: ArbitraryFieldN -> Property
testPrivateUnCompressed (ArbitraryBigWord n) = n > 0 ==> 
    isPrvKeyU $ fromJust $ makePrvKeyU $ fromIntegral n

testDerivedPubKey :: ArbitraryPrvKey -> Bool
testDerivedPubKey (ArbitraryPrvKey k) = isValidPubKey $ derivePubKey k

deriveFromInt :: Integer -> Bool
deriveFromInt i = maybe True (isValidPubKey . derivePubKey) $ makePrvKey i

{- Key properties -}

validKeys :: ArbitraryPubKey -> Bool
validKeys (ArbitraryPubKey prv pub) = 
    isValidPubKey pub && isValidPrvKey (fromPrvKey prv)


