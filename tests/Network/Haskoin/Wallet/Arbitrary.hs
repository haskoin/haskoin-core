{-|
  Arbitrary instances for wallet data types.
-}
module Network.Haskoin.Wallet.Arbitrary 
( genPubKeyC
, genMulSigInput
, genRegularInput 
, genAddrOutput
, RegularTx(..)
, MSParam(..)
) where

import Test.QuickCheck 
    ( Gen
    , Arbitrary
    , arbitrary
    , vectorOf
    , oneof
    , choose
    , elements
    )

import Control.Monad (liftM)
import Control.Applicative ((<$>),(<*>))

import Data.Maybe (fromJust)

import Network.Haskoin.Wallet
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto

-- | Data type for generating arbitrary valid multisignature parameters (m of n)
data MSParam = MSParam Int Int deriving (Eq, Show)

instance Arbitrary MSParam where
    arbitrary = do
        n <- choose (1,16)
        m <- choose (1,n)
        return $ MSParam m n

-- | Data type for generating arbitrary transaction with inputs and outputs
-- consisting only of script hash or pub key hash scripts.
data RegularTx = RegularTx Tx deriving (Eq, Show)

-- | Generate an arbitrary compressed public key.
genPubKeyC :: Gen PubKey
genPubKeyC = derivePubKey <$> genPrvKeyC

-- | Generate an arbitrary script hash input spending a multisignature
-- pay to script hash.
genMulSigInput :: Gen ScriptHashInput
genMulSigInput = do
    (MSParam m n) <- arbitrary
    rdm <- PayMulSig <$> (vectorOf n genPubKeyC) <*> (return m)
    inp <- SpendMulSig <$> (vectorOf m arbitrary) <*> (return m)
    return $ ScriptHashInput inp rdm

-- | Generate an arbitrary transaction input spending a public key hash or
-- script hash output.
genRegularInput :: Gen TxIn
genRegularInput = do
    op <- arbitrary
    sq <- arbitrary
    sc <- oneof [ encodeScriptHash <$> genMulSigInput
                , encodeInput <$> (SpendPKHash <$> arbitrary <*> genPubKeyC)
                ]
    return $ TxIn op sc sq

-- | Generate an arbitrary output paying to a public key hash or script hash
-- address.
genAddrOutput :: Gen TxOut
genAddrOutput = do
    v  <- arbitrary
    sc <- oneof [ (PayPKHash . pubKeyAddr) <$> arbitrary
                , (PayScriptHash . scriptAddr) <$> arbitrary
                ]
    return $ TxOut v $ encodeOutput sc

instance Arbitrary RegularTx where
    arbitrary = do
        x <- choose (1,10)
        y <- choose (1,10)
        liftM RegularTx $ Tx <$> arbitrary 
                             <*> (vectorOf x genRegularInput) 
                             <*> (vectorOf y genAddrOutput) 
                             <*> arbitrary

