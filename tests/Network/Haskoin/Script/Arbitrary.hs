{-|
  This module provides arbitrary instances for data types in
  'Network.Haskoin.Script'.
-}
module Network.Haskoin.Script.Arbitrary where

import Test.QuickCheck 
    ( Gen
    , Arbitrary
    , arbitrary
    , oneof
    , choose
    , vectorOf
    , elements
    )
import Network.Haskoin.Crypto.Arbitrary()

import Control.Monad (liftM2)
import Control.Applicative ((<$>),(<*>))

import Data.Bits (testBit, clearBit)
import Data.Word (Word8)

import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto

instance Arbitrary TxSignature where
    arbitrary = liftM2 TxSignature arbitrary arbitrary

instance Arbitrary SigHash where
    arbitrary = do
        w <- arbitrary :: Gen Word8
        let acp = testBit w 7
        return $ case clearBit w 7 of
            1 -> SigAll acp
            2 -> SigNone acp
            3 -> SigSingle acp
            _ -> SigUnknown acp w

instance Arbitrary ScriptOutput where
    arbitrary = oneof 
        [ PayPK <$> arbitrary
        , (PayPKHash . pubKeyAddr) <$> arbitrary 
        , genPayMulSig
        , (PayScriptHash . scriptAddr) <$> arbitrary
        ]

-- | Generate an arbitrary 'ScriptOutput' of value PayMulSig.
genPayMulSig :: Gen ScriptOutput
genPayMulSig = do
    n <- choose (1,16)
    m <- choose (1,n)
    PayMulSig <$> (vectorOf n arbitrary) <*> (return m)

instance Arbitrary ScriptInput where
    arbitrary = oneof
        [ SpendPK <$> arbitrary
        , SpendPKHash <$> arbitrary <*> arbitrary
        , genSpendMulSig
        ]

-- | Generate an arbitrary 'ScriptInput' of value SpendMulSig.
genSpendMulSig :: Gen ScriptInput
genSpendMulSig = do
    m <- choose (1,16)
    s <- choose (1,m)
    SpendMulSig <$> (vectorOf s arbitrary) <*> (return m)

instance Arbitrary ScriptHashInput where
    arbitrary = ScriptHashInput <$> arbitrary <*> arbitrary

-- | Data type for generating an arbitrary 'ScriptOp' with a value in
-- [OP_1 .. OP_16]
data ScriptOpInt = ScriptOpInt ScriptOp
    deriving (Eq, Show)

instance Arbitrary ScriptOpInt where
    arbitrary = ScriptOpInt <$> elements 
                    [ OP_1,  OP_2,  OP_3,  OP_4
                    , OP_5,  OP_6,  OP_7,  OP_8
                    , OP_9,  OP_10, OP_11, OP_12
                    , OP_13, OP_14, OP_15, OP_16
                    ]

