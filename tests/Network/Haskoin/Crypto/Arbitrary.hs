{-# LANGUAGE EmptyDataDecls #-}
{-| 
  QuickCheck Arbitrary instances for Haskoin.Crypto types.
-}
module Network.Haskoin.Crypto.Arbitrary 
( Test32
, TestPrvKeyC(..)
, TestPrvKeyU(..)
, genPrvKeyC
, genPrvKeyU
)  where

import Test.QuickCheck
import Network.Haskoin.Util.Arbitrary()

import Control.Applicative ((<$>), (<*>))

import Data.Maybe

import Network.Haskoin.Crypto.Point
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.ECDSA
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.Curve
import Network.Haskoin.Crypto.ExtendedKeys
import Network.Haskoin.Crypto.NormalizedKeys
import Network.Haskoin.Crypto.Merkle

data Mod32
type Test32 = BigWord Mod32

instance BigWordMod Mod32 where
    rFromInteger i = BigWord $ i `mod` 2 ^ (32 :: Integer)
    rBitSize     _ = 32

instance BigWordMod n => Arbitrary (BigWord n) where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary Point where
    arbitrary = frequency
        [ (1, return makeInfPoint)
        , (9, (flip mulPoint $ curveG) <$> (arbitrary :: Gen FieldN))
        ]

-- | Generate an arbitrary compressed private key
genPrvKeyC :: Gen PrvKey
genPrvKeyC = do
    i <- fromInteger <$> choose (1, curveN-1)
    return $ fromJust $ makePrvKey i

-- | Generate an arbitrary uncompressed private key
genPrvKeyU :: Gen PrvKey
genPrvKeyU = do
    i <- fromInteger <$> choose (1, curveN-1)
    return $ fromJust $ makePrvKeyU i

newtype TestPrvKeyC = TestPrvKeyC { getTestPrvKeyC :: PrvKey }
    deriving (Eq, Show)

newtype TestPrvKeyU = TestPrvKeyU { getTestPrvKeyU :: PrvKey }
    deriving (Eq, Show)

instance Arbitrary TestPrvKeyC where
    arbitrary = TestPrvKeyC <$> genPrvKeyC

instance Arbitrary TestPrvKeyU where
    arbitrary = TestPrvKeyU <$> genPrvKeyU

instance Arbitrary PrvKey where
    arbitrary = oneof [genPrvKeyC, genPrvKeyU]

instance Arbitrary PubKey where
    arbitrary = derivePubKey <$> arbitrary

instance Arbitrary Address where
    arbitrary = do
        i <- fromInteger <$> choose (1,2^(160-1 :: Int))
        elements [ PubKeyAddress i
                 , ScriptAddress i
                 ]

instance Arbitrary Signature where
    arbitrary = do
        msg <- arbitrary
        prv <- prvKeyFieldN <$> arbitrary
        non <- prvKeyFieldN <$> arbitrary
        let pub  = mulPoint non curveG
        case unsafeSignMsg msg prv (non,pub) of
            (Just sig) -> return sig
            Nothing    -> arbitrary 

instance Arbitrary XPrvKey where
    arbitrary = XPrvKey <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> genPrvKeyC

instance Arbitrary XPubKey where
    arbitrary = deriveXPubKey <$> arbitrary

instance Arbitrary MasterKey where
    arbitrary = fromJust . makeMasterKey <$> arbitrary

instance Arbitrary AccPrvKey where
    arbitrary = do
        master <- arbitrary
        index  <- choose (0,0x7fffffff)
        return $ fromJust $ accPrvKey master index

instance Arbitrary AccPubKey where
    arbitrary = do
        master <- arbitrary
        index  <- choose (0,0x7fffffff)
        return $ fromJust $ accPubKey master index

instance Arbitrary AddrPrvKey where
    arbitrary = do
        accKey <- arbitrary
        index  <- arbitrary
        elements [ fromJust $ extPrvKey accKey index
                 , fromJust $ intPrvKey accKey index
                 ]

instance Arbitrary AddrPubKey where
    arbitrary = do
        accKey <- arbitrary
        index  <- arbitrary
        elements [ fromJust $ extPubKey accKey index
                 , fromJust $ intPubKey accKey index
                 ]

