{-|
  Arbitrary instances for transaction package
-}
module Network.Haskoin.Transaction.Arbitrary 
( genPubKeyC
, genMulSigP2SH
, genSpendAddrInput
, genAddrOutput
, SpendAddrTx(..)
, MSParam(..)
, PKHashSigTemplate(..)
, MulSigTemplate(..)
) where

import Test.QuickCheck 
    ( Gen
    , Arbitrary
    , arbitrary
    , vectorOf
    , oneof
    , choose
    )

import Control.Applicative ((<$>),(<*>))

import Data.Word (Word64)
import Data.List (permutations, nubBy)

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

-- | Generate an arbitrary compressed public key.
genPubKeyC :: Gen PubKey
genPubKeyC = derivePubKey <$> genPrvKeyC

-- | Generate an arbitrary script hash input spending a multisignature
-- pay to script hash.
genMulSigP2SH :: Gen ScriptInput
genMulSigP2SH = do
    (MSParam m n) <- arbitrary
    rdm <- PayMulSig <$> (vectorOf n genPubKeyC) <*> (return m)
    inp <- SpendMulSig <$> (vectorOf m arbitrary)
    return $ ScriptHashInput inp rdm

-- | Generate an arbitrary transaction input spending a public key hash or
-- script hash output.
genSpendAddrInput :: Gen TxIn
genSpendAddrInput = do
    op <- arbitrary
    sq <- arbitrary
    si <- oneof 
        [ RegularInput <$> (SpendPKHash <$> arbitrary <*> genPubKeyC)
        , genMulSigP2SH
        ]
    return $ TxIn op (encodeInputBS si) sq

-- | Generate an arbitrary output paying to a public key hash or script hash
-- address.
genAddrOutput :: Gen TxOut
genAddrOutput = do
    v  <- choose (1,2100000000000000)
    sc <- oneof [ (PayPKHash . pubKeyAddr) <$> arbitrary
                , (PayScriptHash . scriptAddr) <$> arbitrary
                ]
    return $ TxOut v $ encodeOutputBS sc

-- | Data type for generating arbitrary transaction with inputs and outputs
-- consisting only of script hash or pub key hash scripts.
data SpendAddrTx = SpendAddrTx Tx deriving (Eq, Show)

instance Arbitrary SpendAddrTx where
    arbitrary = do
        x  <- choose (1,5)
        y  <- choose (1,10)
        xs <- vectorOf x genSpendAddrInput
        ys <- vectorOf y genAddrOutput
        let tx = Tx 1 xs ys 0
        return $ SpendAddrTx tx

instance Arbitrary Coin where
    arbitrary = Coin <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        
data PKHashSigTemplate = PKHashSigTemplate Tx [SigInput] [PrvKey]
    deriving (Eq, Show)

data MulSigTemplate = MulSigTemplate Tx [(ScriptOutput, SigInput)] [PrvKey]
    deriving (Eq, Show)

-- Generates a private key that can sign a input using the OutPoint and SigInput
genPKHashData :: Gen (OutPoint, SigInput, PrvKey)
genPKHashData = do
    op  <- arbitrary
    prv <- arbitrary
    sh  <- arbitrary
    let pub    = derivePubKey prv
        script = PayPKHash $ pubKeyAddr pub
        sigi   = SigInput script op sh Nothing
    return (op, sigi, prv)

-- Generates private keys that can sign an input using the OutPoint and SigInput
genMSData :: Gen (OutPoint, ScriptOutput, SigInput, [PrvKey])
genMSData = do
    (MSParam m n) <- arbitrary
    prv     <- vectorOf n arbitrary
    perm    <- choose (0,n-1)
    op      <- arbitrary
    sh      <- arbitrary
    let pub    = map derivePubKey prv
        rdm    = PayMulSig pub m
        script = PayScriptHash $ scriptAddr rdm
        sigi   = SigInput script op sh (Just rdm)
        perPrv = permutations prv !! perm
    return (op, script, sigi, take m perPrv)

genPayTo :: Gen (String, Word64)
genPayTo = do
    v  <- choose (1,2100000000000000)
    sc <- oneof [ PubKeyAddress <$> arbitrary
                , ScriptAddress <$> arbitrary
                ]
    return (addrToBase58 sc, v)

-- Generates data for signing a PKHash transaction
instance Arbitrary PKHashSigTemplate where
    arbitrary = do
        inC   <- choose (0,5)
        outC  <- choose (0,10)
        dat   <- nubBy (\a b -> fst3 a == fst3 b) <$> vectorOf inC genPKHashData
        perm  <- choose (0,max 0 $ length dat - 1)
        payTo <- vectorOf outC genPayTo
        let tx   = fromRight $ buildAddrTx (map fst3 dat) payTo
            perI = permutations (map snd3 dat) !! perm
        return $ PKHashSigTemplate tx perI (map lst3 dat)

-- Generates data for signing a P2SH transactions
instance Arbitrary MulSigTemplate where
    arbitrary = do
        inC   <- choose (0,5)
        outC  <- choose (0,10)
        dat   <- nubBy (\a b -> f1 a == f1 b) <$> vectorOf inC genMSData
        perm  <- choose (0,max 0 $ length dat - 1)
        payTo <- vectorOf outC genPayTo
        let tx   = fromRight $ buildAddrTx (map f1 dat) payTo
            perI = permutations (map (\(_,a,b,_) -> (a,b)) dat) !! perm
        return $ MulSigTemplate tx perI (concat $ map f4 dat)
      where
        f1 (a,_,_,_) = a
        f4 (_,_,_,d) = d

