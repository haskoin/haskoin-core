module Network.Haskoin.Crypto.ExtendedKeys.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import Data.Word (Word32)
import Data.Bits ((.&.))
import Data.Maybe (fromJust)

import Network.Haskoin.Test.Crypto

import Network.Haskoin.Crypto
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

subkeyTest :: ArbitraryXPrvKey -> Word32 -> Bool
subkeyTest (ArbitraryXPrvKey k) i = fromJust $ liftM2 (==) 
    (deriveXPubKey <$> prvSubKey k i') (pubSubKey (deriveXPubKey k) i')
  where 
    i' = fromIntegral $ i .&. 0x7fffffff -- make it a public derivation

binXPrvKey :: ArbitraryXPrvKey -> Bool
binXPrvKey (ArbitraryXPrvKey k) = (decode' $ encode' k) == k

binXPubKey :: ArbitraryXPubKey -> Bool
binXPubKey (ArbitraryXPubKey _ k) = (decode' $ encode' k) == k

b58PrvKey :: ArbitraryXPrvKey -> Bool
b58PrvKey (ArbitraryXPrvKey k) = (xPrvImport $ xPrvExport k) == Just k

b58PubKey :: ArbitraryXPubKey -> Bool
b58PubKey (ArbitraryXPubKey _ k) = (xPubImport $ xPubExport k) == Just k

{- HDW Normalized Keys -}

decEncMaster :: ArbitraryMasterKey -> Bool
decEncMaster (ArbitraryMasterKey k) = 
    (loadMasterKey $ decode' bs) == Just k
  where 
    bs = encode' $ masterKey k

decEncPrvAcc :: ArbitraryAccPrvKey -> Bool
decEncPrvAcc (ArbitraryAccPrvKey _ k) = 
    (loadPrvAcc $ decode' bs) == Just k
  where 
    bs = encode' $ getAccPrvKey k

decEncPubAcc :: ArbitraryAccPubKey -> Bool
decEncPubAcc (ArbitraryAccPubKey _ _ k) = 
    (loadPubAcc $ decode' bs) == Just k
  where 
    bs = encode' $ getAccPubKey k


