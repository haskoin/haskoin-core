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

import Network.Haskoin.Util.Arbitrary (nonEmptyBS)
import Network.Haskoin.Script
import Network.Haskoin.Crypto

instance Arbitrary Script where
    arbitrary = do
        i <- choose (1,10)
        Script <$> (vectorOf i arbitrary)

instance Arbitrary ScriptOp where
    arbitrary = oneof [ opPushData <$> nonEmptyBS
                      , return OP_0
                      , return OP_1NEGATE
                      , return OP_1
                      , return OP_2, return OP_3, return OP_4, return OP_5
                      , return OP_6, return OP_7, return OP_8, return OP_9
                      , return OP_10, return OP_11, return OP_12, return OP_13
                      , return OP_14, return OP_15, return OP_16
                      , return OP_VERIFY
                      , return OP_DUP
                      , return OP_EQUAL
                      , return OP_EQUALVERIFY
                      , return OP_HASH160
                      , return OP_CHECKSIG
                      , return OP_CHECKMULTISIG
                      , return $ OP_INVALIDOPCODE 0xff
                      ]

instance Arbitrary PushDataType where
    arbitrary = elements [ OPCODE, OPDATA1, OPDATA2, OPDATA4 ]

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
        , genSpendMulSig =<< choose (1,16)
        ]

-- | Generate an arbitrary 'ScriptInput' of value SpendMulSig.
genSpendMulSig :: Int -> Gen ScriptInput
genSpendMulSig r = do
    s <- choose (1,r)
    flip SpendMulSig r <$> (vectorOf s arbitrary)

instance Arbitrary ScriptHashInput where
    arbitrary = do
        out <- oneof 
            [ PayPK <$> arbitrary
            , (PayPKHash . pubKeyAddr) <$> arbitrary 
            , genPayMulSig
            ]
        inp <- case out of
            (PayPK _)         -> SpendPK <$> arbitrary
            (PayPKHash _)     -> SpendPKHash <$> arbitrary <*> arbitrary
            (PayMulSig _ r)   -> genSpendMulSig r
            _                 -> error "Won't happen"
        return $ ScriptHashInput inp out

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

