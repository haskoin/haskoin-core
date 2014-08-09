{-|
  This module provides arbitrary instances for data types in
  'Network.Haskoin.Script'.
-}
module Network.Haskoin.Script.Arbitrary (ScriptOpInt(..))
where

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

import Data.Bits (testBit, clearBit, (.&.))
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
                      , return $ OP_1NEGATE
                      , return $ OP_0
                      , return $ OP_1
                      , return $ OP_2
                      , return $ OP_3
                      , return $ OP_4
                      , return $ OP_5
                      , return $ OP_6
                      , return $ OP_7
                      , return $ OP_8
                      , return $ OP_9
                      , return $ OP_10
                      , return $ OP_11
                      , return $ OP_12
                      , return $ OP_13
                      , return $ OP_14
                      , return $ OP_15
                      , return $ OP_16

                      -- Flow control
                      , return $ OP_NOP
                      , return $ OP_IF
                      , return $ OP_NOTIF
                      , return $ OP_ELSE
                      , return $ OP_ENDIF
                      , return $ OP_VERIFY
                      , return $ OP_RETURN

                      -- Stack
                      , return $ OP_TOALTSTACK
                      , return $ OP_FROMALTSTACK
                      , return $ OP_2DROP
                      , return $ OP_2DUP
                      , return $ OP_3DUP
                      , return $ OP_2OVER
                      , return $ OP_2ROT
                      , return $ OP_2SWAP
                      , return $ OP_IFDUP
                      , return $ OP_DEPTH
                      , return $ OP_DROP
                      , return $ OP_DUP
                      , return $ OP_NIP
                      , return $ OP_OVER
                      , return $ OP_PICK
                      , return $ OP_ROLL
                      , return $ OP_ROT
                      , return $ OP_SWAP
                      , return $ OP_TUCK

                      -- Splice
                      , return $ OP_CAT
                      , return $ OP_SUBSTR
                      , return $ OP_LEFT
                      , return $ OP_RIGHT
                      , return $ OP_SIZE

                      -- Bitwise logic
                      , return $ OP_INVERT
                      , return $ OP_AND
                      , return $ OP_OR
                      , return $ OP_XOR
                      , return $ OP_EQUAL
                      , return $ OP_EQUALVERIFY

                      -- Arithmetic
                      , return $ OP_1ADD
                      , return $ OP_1SUB
                      , return $ OP_2MUL
                      , return $ OP_2DIV
                      , return $ OP_NEGATE
                      , return $ OP_ABS
                      , return $ OP_NOT
                      , return $ OP_0NOTEQUAL
                      , return $ OP_ADD
                      , return $ OP_SUB
                      , return $ OP_MUL
                      , return $ OP_DIV
                      , return $ OP_MOD
                      , return $ OP_LSHIFT
                      , return $ OP_RSHIFT
                      , return $ OP_BOOLAND
                      , return $ OP_BOOLOR
                      , return $ OP_NUMEQUAL
                      , return $ OP_NUMEQUALVERIFY
                      , return $ OP_NUMNOTEQUAL
                      , return $ OP_LESSTHAN
                      , return $ OP_GREATERTHAN
                      , return $ OP_LESSTHANOREQUAL
                      , return $ OP_GREATERTHANOREQUAL
                      , return $ OP_MIN
                      , return $ OP_MAX
                      , return $ OP_WITHIN

                      -- Crypto
                      , return $ OP_RIPEMD160
                      , return $ OP_SHA1
                      , return $ OP_SHA256
                      , return $ OP_HASH160
                      , return $ OP_HASH256
                      , return $ OP_CODESEPARATOR
                      , return $ OP_CHECKSIG
                      , return $ OP_CHECKSIGVERIFY
                      , return $ OP_CHECKMULTISIG
                      , return $ OP_CHECKMULTISIGVERIFY

                      -- More NOPs
                      , return $ OP_NOP1
                      , return $ OP_NOP2
                      , return $ OP_NOP3
                      , return $ OP_NOP4
                      , return $ OP_NOP5
                      , return $ OP_NOP6
                      , return $ OP_NOP7
                      , return $ OP_NOP8
                      , return $ OP_NOP9
                      , return $ OP_NOP10

                      , return $ OP_INVALIDOPCODE 0xff
                      ]

instance Arbitrary PushDataType where
    arbitrary = elements [ OPCODE, OPDATA1, OPDATA2, OPDATA4 ]

instance Arbitrary TxSignature where
    arbitrary = liftM2 TxSignature arbitrary arbitrary

instance Arbitrary SigHash where
    arbitrary = do
        oneof [ SigAll    <$> arbitrary
              , SigNone   <$> arbitrary
              , SigSingle <$> arbitrary
              , f
              ]
      where
        f = do
            wGen <- arbitrary :: Gen Word8
            -- Make sure we don't have an unknown which is actually known
            let w = if clearBit wGen 7 <= 3 then wGen .&. 0xfc else wGen
            return $ SigUnknown (testBit w 7) w

instance Arbitrary ScriptOutput where
    arbitrary = oneof [ genSimpleOutput
                      , genPaySHOutput
                      ]

genSimpleOutput :: Gen ScriptOutput
genSimpleOutput = oneof
    [ PayPK <$> arbitrary
    , (PayPKHash . pubKeyAddr) <$> arbitrary 
    , genPayMulSig =<< choose (1,16)
    ]
    
genPaySHOutput :: Gen ScriptOutput
genPaySHOutput = (PayScriptHash . scriptAddr) <$> genSimpleOutput

-- | Generate an arbitrary 'ScriptOutput' of value PayMulSig.
genPayMulSig :: Int -> Gen ScriptOutput
genPayMulSig m = do
    n <- choose (m,16)
    PayMulSig <$> (vectorOf n arbitrary) <*> (return m)

instance Arbitrary SimpleInput where
    arbitrary = oneof
        [ SpendPK <$> arbitrary
        , SpendPKHash <$> arbitrary <*> arbitrary
        , genSpendMulSig =<< choose (1,16)
        ]

instance Arbitrary ScriptInput where
    arbitrary = oneof
        [ RegularInput <$> arbitrary
        , genScriptHashInput
        ]

-- | Generate an arbitrary 'SimpleInput of value SpendMulSig.
genSpendMulSig :: Int -> Gen SimpleInput
genSpendMulSig r = do
    s <- choose (1,r)
    SpendMulSig <$> (vectorOf s arbitrary)

genScriptHashInput :: Gen ScriptInput
genScriptHashInput = do
    inp <- arbitrary :: Gen SimpleInput
    out <- case inp of
        SpendPK _        -> PayPK <$> arbitrary
        SpendPKHash _ _  -> (PayPKHash . pubKeyAddr) <$> arbitrary
        SpendMulSig sigs -> genPayMulSig $ length sigs
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

