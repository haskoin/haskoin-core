module Haskoin.KeysSpec (spec) where

import           Data.Aeson              as A
import qualified Data.ByteString         as BS
import           Data.Map.Strict         (singleton)
import           Data.Serialize          as S
import           Data.String             (fromString)
import           Data.String.Conversions (cs)
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Crypto
import           Haskoin.Keys
import           Haskoin.Util.Arbitrary
import           Haskoin.Util
import           Test.Hspec
import           Test.QuickCheck

serialVals :: [SerialBox]
serialVals =
    [ SerialBox (arbitrary :: Gen SecKey)
    , SerialBox (snd <$> arbitraryKeyPair) -- PubKeyI
    ]

readVals :: [ReadBox]
readVals =
    [ ReadBox (arbitrary :: Gen SecKey)
    , ReadBox arbitrarySecKeyI
    , ReadBox (snd <$> arbitraryKeyPair) -- PubKeyI
    ]

jsonVals :: [JsonBox]
jsonVals =
    [ JsonBox (snd <$> arbitraryKeyPair) -- PubKeyI
    ]

spec :: Spec
spec =
    describe "keys" $ do
        testIdentity serialVals readVals jsonVals []
        it "is public key canonical" $
            property $ forAll arbitraryKeyPair (isCanonicalPubKey . snd)
        it "from string public key" $
            property $
            forAll arbitraryKeyPair $ \(_, k) ->
                fromString (cs . encodeHex $ S.encode k) == k

-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- from function IsCanonicalPubKey
isCanonicalPubKey :: PubKeyI -> Bool
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
    bs = S.encode p

