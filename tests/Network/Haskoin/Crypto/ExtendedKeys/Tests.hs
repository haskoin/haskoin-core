module Network.Haskoin.Crypto.ExtendedKeys.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import Data.Word (Word32)
import Data.Bits ((.&.))
import Data.Maybe (fromJust)

import Network.Haskoin.Crypto
import Network.Haskoin.Crypto.Arbitrary()
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "HDW Extended Keys"
        [ testProperty "prvSubKey(k,c)*G = pubSubKey(k*G,c)" subkeyTest
        , testProperty "decode . encode prvKey" binXPrvKey
        , testProperty "decode . encode pubKey" binXPubKey
        , testProperty "fromB58 . toB58 prvKey" b58PrvKey
        , testProperty "fromB58 . toB58 pubKey" b58PubKey
        ]
    , testGroup "HDW Normalized Keys"
        [ testProperty "decode . encode masterKey" decEncMaster
        , testProperty "decode . encode prvAccKey" decEncPrvAcc
        , testProperty "decode . encode pubAccKey" decEncPubAcc
        ]
    ]

{- HDW Extended Keys -}

subkeyTest :: XPrvKey -> Word32 -> Bool
subkeyTest k i = fromJust $ liftM2 (==) 
    (deriveXPubKey <$> prvSubKey k i') (pubSubKey (deriveXPubKey k) i')
    where i' = fromIntegral $ i .&. 0x7fffffff -- make it a public derivation

binXPrvKey :: XPrvKey -> Bool
binXPrvKey k = (decode' $ encode' k) == k

binXPubKey :: XPubKey -> Bool
binXPubKey k = (decode' $ encode' k) == k

b58PrvKey :: XPrvKey -> Bool
b58PrvKey k = (fromJust $ xPrvImport $ xPrvExport k) == k

b58PubKey :: XPubKey -> Bool
b58PubKey k = (fromJust $ xPubImport $ xPubExport k) == k

{- HDW Normalized Keys -}

decEncMaster :: MasterKey -> Bool
decEncMaster k = (fromJust $ loadMasterKey $ decode' bs) == k
    where bs = encode' $ masterKey k

decEncPrvAcc :: AccPrvKey -> Bool
decEncPrvAcc k = (fromJust $ loadPrvAcc $ decode' bs) == k
    where bs = encode' $ getAccPrvKey k

decEncPubAcc :: AccPubKey -> Bool
decEncPubAcc k = (fromJust $ loadPubAcc $ decode' bs) == k
    where bs = encode' $ getAccPubKey k


