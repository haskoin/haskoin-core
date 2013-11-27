module Haskoin.Wallet.Arbitrary where

import Test.QuickCheck
import Haskoin.Crypto.Arbitrary
import Haskoin.Protocol.Arbitrary
import Haskoin.Script.Arbitrary

import Control.Monad
import Control.Applicative

import Data.Word
import Data.Maybe

import Haskoin.Wallet
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Store
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto

data MSParam = MSParam Int Int deriving (Eq, Show)

instance Arbitrary MSParam where
    arbitrary = do
        n <- choose (1,16)
        m <- choose (1,n)
        return $ MSParam m n
        

data RegularTx = RegularTx { runRegularTx :: Tx }
    deriving (Eq, Show)

genPubKeyC :: Gen PubKey
genPubKeyC = derivePubKey <$> genPrvKeyC

genMulSigInput :: Gen ScriptHashInput
genMulSigInput = do
    (MSParam m n) <- arbitrary
    rdm <- PayMulSig <$> (vectorOf n genPubKeyC) <*> (return m)
    inp <- SpendMulSig <$> (vectorOf m arbitrary) <*> (return m)
    return $ ScriptHashInput inp rdm

genRegularInput :: Gen TxIn
genRegularInput = do
    op <- arbitrary
    sq <- arbitrary
    sc <- oneof [ encodeScriptHash <$> genMulSigInput
                , encodeInput <$> (SpendPKHash <$> arbitrary <*> genPubKeyC)
                ]
    return $ TxIn op sc sq

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

instance Arbitrary Coin where
    arbitrary = Coin <$> arbitrary <*> arbitrary
        
