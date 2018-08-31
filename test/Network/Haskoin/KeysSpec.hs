module Network.Haskoin.KeysSpec (spec) where

import           Data.Aeson                as A
import qualified Data.ByteString           as BS
import           Data.Map.Strict           (singleton)
import           Data.Serialize            as S
import           Data.String               (fromString)
import           Data.String.Conversions   (cs)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Keys
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
    describe "keys" $ do
        let net = btc
        it "is public key canonical" $
            property $ forAll arbitraryKeyPair (isCanonicalPubKey . snd)
        it "encode and decode wif private keys" $
            property $
            forAll arbitraryKeyPair $ \(pk, _) ->
                fromWif net (toWif net pk) == Just pk
        it "encode and decode serialized private key" $
            property $ forAll arbitrary binaryPrvKey
        it "read and show public key" $
            property $ forAll arbitraryKeyPair $ \(_, k) -> read (show k) == k
        it "read and show private key" $
            property $ forAll arbitrarySecKeyI $ \k -> read (show k) == k
        it "from string public key" $
            property $
            forAll arbitraryKeyPair $ \(_, k) ->
                fromString (cs . encodeHex $ S.encode k) == k
        it "encode and decode json public key" $
            property $ forAll arbitraryKeyPair (testID . snd)
        it "encodes and decodes serialized public key" $
            property $ forAll arbitraryKeyPair $ cerealID . snd


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

{- Key formats -}

binaryPrvKey :: SecKey -> Bool
binaryPrvKey k =
    Right k == runGet secKeyGet (runPut (secKeyPut k)) &&
    Just k == secKey (getSecKey k)

testID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
testID x =
    (A.decode . A.encode) (singleton ("object" :: String) x) ==
    Just (singleton ("object" :: String) x)

cerealID :: (Serialize a, Eq a) => a -> Bool
cerealID x = S.decode (S.encode x) == Right x
