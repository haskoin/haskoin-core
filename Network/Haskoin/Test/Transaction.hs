{-| 
  Arbitrary types for Network.Haskoin.Transaction
-}
module Network.Haskoin.Test.Transaction
( ArbitrarySatoshi(..)
, ArbitraryTx(..)
, ArbitraryTxIn(..)
, ArbitraryTxOut(..)
, ArbitraryOutPoint(..) 
, ArbitraryAddrOnlyTx(..)
, ArbitraryAddrOnlyTxIn(..)
, ArbitraryAddrOnlyTxOut(..)
, ArbitraryCoin(..)
) where

import Test.QuickCheck 
    ( Gen
    , Arbitrary
    , arbitrary
    , vectorOf
    , oneof
    , choose
    , listOf
    , frequency
    )

import Control.Applicative ((<$>),(<*>))

import Data.Word (Word64)
import Data.List (permutations, nubBy)

import Network.Haskoin.Test.Script

import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Constants
import Network.Haskoin.Util

-- | Arbitrary amount of Satoshi as Word64 (Between 1 and 21e14)
newtype ArbitrarySatoshi = ArbitrarySatoshi Word64
    deriving (Eq, Show, Read)

instance Arbitrary ArbitrarySatoshi where
    arbitrary = ArbitrarySatoshi <$> choose (1, maxSatoshi)

-- | Arbitrary OutPoint
newtype ArbitraryOutPoint = ArbitraryOutPoint OutPoint
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryOutPoint where
    arbitrary = ArbitraryOutPoint <$> (OutPoint <$> arbitrary <*> arbitrary)

-- | Arbitrary TxOut
newtype ArbitraryTxOut = ArbitraryTxOut TxOut
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryTxOut where
    arbitrary = do
        ArbitrarySatoshi v <- arbitrary
        ArbitraryScriptOutput out <- arbitrary
        return $ ArbitraryTxOut $ TxOut v $ encodeOutputBS out

-- | Arbitrary TxIn
newtype ArbitraryTxIn = ArbitraryTxIn TxIn
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryTxIn where
    arbitrary = do
        ArbitraryOutPoint o <- arbitrary 
        ArbitraryScriptInput inp <- arbitrary
        seq <- arbitrary
        return $ ArbitraryTxIn $ TxIn o (encodeInputBS inp) seq

-- | Arbitrary Tx
newtype ArbitraryTx = ArbitraryTx Tx
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryTx where
    arbitrary = do
        v <- arbitrary
        inps <- listOf $ arbitrary >>= \(ArbitraryTxIn i) -> return i
        outs <- listOf $ arbitrary >>= \(ArbitraryTxOut o) -> return o
        t <- arbitrary
        return $ ArbitraryTx $ Tx v inps outs t

-- | Arbitrary Tx containing only inputs of type SpendPKHash, SpendScriptHash
-- (multisig) and outputs of type PayPKHash and PaySH
newtype ArbitraryAddrOnlyTx = ArbitraryAddrOnlyTx Tx
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAddrOnlyTx where
    arbitrary = do
        v <- arbitrary
        inps <- listOf $ arbitrary >>= \(ArbitraryAddrOnlyTxIn i) -> return i
        outs <- listOf $ arbitrary >>= \(ArbitraryAddrOnlyTxOut o) -> return o
        t <- arbitrary
        return $ ArbitraryAddrOnlyTx $ Tx v inps outs t

-- | Arbitrary TxIn that can only be of type SpendPKHash or
-- SpendScriptHash (multisig)
newtype ArbitraryAddrOnlyTxIn = ArbitraryAddrOnlyTxIn TxIn
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAddrOnlyTxIn where
    arbitrary = do
        ArbitraryOutPoint o <- arbitrary 
        inp <- oneof
            [ arbitrary >>= \(ArbitraryPKHashInput i) -> return i
            , arbitrary >>= \(ArbitraryMulSigSHInput i) -> return i
            ]
        seq <- arbitrary
        return $ ArbitraryAddrOnlyTxIn $ TxIn o (encodeInputBS inp) seq

-- | Arbitrary TxOut that can only be of type PayPKHash or PaySH
newtype ArbitraryAddrOnlyTxOut = ArbitraryAddrOnlyTxOut TxOut
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAddrOnlyTxOut where
    arbitrary = do
        ArbitrarySatoshi v <- arbitrary
        out <- oneof
            [ arbitrary >>= \(ArbitraryPKHashOutput o) -> return o
            , arbitrary >>= \(ArbitrarySHOutput o) -> return o
            ]
        return $ ArbitraryAddrOnlyTxOut $ TxOut v $ encodeOutputBS out

newtype ArbitraryCoin = ArbitraryCoin Coin
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryCoin where
    arbitrary = do
        ArbitrarySatoshi v <- arbitrary
        ArbitraryScriptOutput out <- arbitrary
        ArbitraryOutPoint op <- arbitrary
        rdm <- oneof 
            [ arbitrary >>= \(ArbitrarySimpleOutput r) -> return $ Just r
            , return Nothing
            ]
        return $ ArbitraryCoin $ Coin v out op rdm
            
newtype ArbitrarySigInput = ArbitrarySigInput SigInput
    deriving (Eq, Show, Read)
    
instance Arbitrary ArbitrarySigInput where
    arbitrary = do
        ArbitraryScriptOutput out <- arbitrary
        ArbitraryOutPoint op <- arbitrary
        ArbitrarySigHash sh <- arbitrary
        rdm <- oneof 
            [ arbitrary >>= \(ArbitrarySimpleOutput r) -> return $ Just r
            , return Nothing
            ]
        return $ ArbitrarySigInput $ SigInput out op sh rdm

