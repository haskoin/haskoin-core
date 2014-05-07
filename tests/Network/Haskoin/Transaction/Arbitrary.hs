{-|
  Arbitrary instances for transaction package
-}
module Network.Haskoin.Transaction.Arbitrary 
( genPubKeyC
, genMulSigInput
, genRegularInput 
, genAddrOutput
, RegularTx(..)
, MSParam(..)
, PKHashSigTemplate(..)
) where

import Test.QuickCheck 
    ( Gen
    , Arbitrary
    , arbitrary
    , vectorOf
    , oneof
    , choose
    )

import Control.Monad (liftM)
import Control.Applicative ((<$>),(<*>))

import Data.List (permutations)

import Network.Haskoin.Crypto.Arbitrary 
import Network.Haskoin.Protocol.Arbitrary ()
import Network.Haskoin.Script.Arbitrary ()

import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util

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

instance Arbitrary Coin where
    arbitrary = Coin <$> arbitrary <*> arbitrary <*> arbitrary
        
data PKHashSigTemplate = PKHashSigTemplate Tx [SigInput] [PrvKey]
    deriving (Eq, Show)

-- Generates data for signing a PKHash transaction
instance Arbitrary PKHashSigTemplate where
    arbitrary = do
        inCount   <- choose (0,10)
        perm      <- choose (0,max 0 $ inCount-1)
        outPoints <- vectorOf inCount arbitrary
        prvKeys   <- vectorOf inCount arbitrary
        sigHashes <- vectorOf inCount arbitrary
        payTo <- choose (0,10) >>= \n -> do
            h <- (map (addrToBase58 . PubKeyAddress)) <$> vectorOf n arbitrary    
            v <- vectorOf n $ choose (1,2100000000000000)
            return $ zip h v
        let pubKeys   = map derivePubKey prvKeys
            scriptOut = map (PayPKHash . pubKeyAddr) pubKeys
            scripts   = map encodeOutput scriptOut
            sigInputs = map (\(s,o,h) -> SigInput s o h) 
                            (zip3 scripts outPoints sigHashes)
            perInputs = (permutations sigInputs) !! perm
            perKeys   = (permutations prvKeys) !! perm
            tx        = fromRight $ buildAddrTx outPoints payTo
        return $ PKHashSigTemplate tx perInputs perKeys

