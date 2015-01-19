{-| 
  Arbitrary types for Network.Haskoin.Script
-}
module Network.Haskoin.Test.Script
( ArbitraryScriptOp(..)
, ArbitraryScript(..)
, ArbitraryIntScriptOp(..)
, ArbitraryPushDataType(..)
, ArbitraryTxSignature(..)
, ArbitraryDetTxSignature(..)
, ArbitrarySigHash(..)
, ArbitraryValidSigHash(..)
, ArbitraryMSParam(..)
, ArbitraryScriptOutput(..)
, ArbitrarySimpleOutput(..)
, ArbitraryPKOutput(..)
, ArbitraryPKHashOutput(..)
, ArbitraryMSOutput(..)
, ArbitraryMSCOutput(..)
, ArbitrarySHOutput(..)
, ArbitraryScriptInput(..)
, ArbitrarySimpleInput(..)
, ArbitraryPKInput(..)
, ArbitraryPKHashInput(..)
, ArbitraryPKHashCInput(..)
, ArbitraryMSInput(..)
, ArbitrarySHInput(..)
, ArbitraryMulSigSHCInput(..)
) where

import Test.QuickCheck 
    ( Arbitrary
    , arbitrary
    , oneof
    , choose
    , vectorOf
    , elements
    )

import Control.Applicative ((<$>))

import Data.Bits (testBit)

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Crypto

-- | Arbitrary Script with random script ops
newtype ArbitraryScript = ArbitraryScript Script
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryScript where
    arbitrary = do
        vs <- arbitrary
        return $ ArbitraryScript $ Script $ map f vs
      where
        f (ArbitraryScriptOp op) = op

-- | Arbitrary ScriptOp (push operations have random data)
newtype ArbitraryScriptOp = ArbitraryScriptOp ScriptOp
    deriving (Eq, Show, Read)
    
instance Arbitrary ArbitraryScriptOp where
    arbitrary = ArbitraryScriptOp <$> oneof 
        [ -- Pushing Data
         arbitrary >>= \(ArbitraryNotNullByteString bs) -> 
            return $ opPushData bs
        ,return OP_0
        ,return OP_1NEGATE
        ,return OP_RESERVED
        ,return OP_1 , return OP_2 , return OP_3 , return OP_4
        ,return OP_5 , return OP_6 , return OP_7 , return OP_8
        ,return OP_9 , return OP_10, return OP_11, return OP_12
        ,return OP_13, return OP_14, return OP_15, return OP_16

        -- Flow control
        ,return OP_NOP
        ,return OP_VER        
        ,return OP_IF
        ,return OP_NOTIF
        ,return OP_VERIF      
        ,return OP_VERNOTIF   
        ,return OP_ELSE
        ,return OP_ENDIF
        ,return OP_VERIFY
        ,return OP_RETURN

        -- Stack operations
        ,return OP_TOALTSTACK
        ,return OP_FROMALTSTACK
        ,return OP_IFDUP
        ,return OP_DEPTH
        ,return OP_DROP
        ,return OP_DUP
        ,return OP_NIP
        ,return OP_OVER
        ,return OP_PICK
        ,return OP_ROLL
        ,return OP_ROT
        ,return OP_SWAP
        ,return OP_TUCK
        ,return OP_2DROP
        ,return OP_2DUP
        ,return OP_3DUP
        ,return OP_2OVER
        ,return OP_2ROT
        ,return OP_2SWAP

        -- Splice
        ,return OP_CAT
        ,return OP_SUBSTR
        ,return OP_LEFT
        ,return OP_RIGHT
        ,return OP_SIZE

        -- Bitwise logic
        ,return OP_INVERT
        ,return OP_AND
        ,return OP_OR
        ,return OP_XOR
        ,return OP_EQUAL
        ,return OP_EQUALVERIFY
        ,return OP_RESERVED1
        ,return OP_RESERVED2

        -- Arithmetic
        ,return OP_1ADD
        ,return OP_1SUB
        ,return OP_2MUL
        ,return OP_2DIV
        ,return OP_NEGATE
        ,return OP_ABS
        ,return OP_NOT
        ,return OP_0NOTEQUAL
        ,return OP_ADD
        ,return OP_SUB
        ,return OP_MUL
        ,return OP_DIV
        ,return OP_MOD
        ,return OP_LSHIFT
        ,return OP_RSHIFT
        ,return OP_BOOLAND
        ,return OP_BOOLOR
        ,return OP_NUMEQUAL
        ,return OP_NUMEQUALVERIFY
        ,return OP_NUMNOTEQUAL
        ,return OP_LESSTHAN
        ,return OP_GREATERTHAN
        ,return OP_LESSTHANOREQUAL
        ,return OP_GREATERTHANOREQUAL
        ,return OP_MIN
        ,return OP_MAX
        ,return OP_WITHIN

        -- Crypto
        ,return OP_RIPEMD160
        ,return OP_SHA1
        ,return OP_SHA256
        ,return OP_HASH160
        ,return OP_HASH256
        ,return OP_CODESEPARATOR
        ,return OP_CHECKSIG
        ,return OP_CHECKSIGVERIFY
        ,return OP_CHECKMULTISIG
        ,return OP_CHECKMULTISIGVERIFY

        -- Expansion
        ,return OP_NOP1, return OP_NOP2 
        ,return OP_NOP3, return OP_NOP4 
        ,return OP_NOP5, return OP_NOP6
        ,return OP_NOP7, return OP_NOP8
        ,return OP_NOP9, return OP_NOP10

        -- Other
        ,return OP_PUBKEYHASH
        ,return OP_PUBKEY
        ,return $ OP_INVALIDOPCODE 0xff
        ]

-- | Arbtirary ScriptOp with a value in [OP_1 .. OP_16]
newtype ArbitraryIntScriptOp = ArbitraryIntScriptOp ScriptOp
    deriving (Eq, Show)

instance Arbitrary ArbitraryIntScriptOp where
    arbitrary = ArbitraryIntScriptOp <$> elements 
        [ OP_1,  OP_2,  OP_3,  OP_4
        , OP_5,  OP_6,  OP_7,  OP_8
        , OP_9,  OP_10, OP_11, OP_12
        , OP_13, OP_14, OP_15, OP_16
        ]

-- | Arbitrary PushDataType
newtype ArbitraryPushDataType = ArbitraryPushDataType PushDataType
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPushDataType where
    arbitrary = ArbitraryPushDataType <$> elements 
        [ OPCODE, OPDATA1, OPDATA2, OPDATA4 ]

-- | Arbitrary SigHash (including invalid/unknown sighash codes)
newtype ArbitrarySigHash = ArbitrarySigHash SigHash
    deriving (Eq, Show, Read)

instance Arbitrary ArbitrarySigHash where
    arbitrary = ArbitrarySigHash <$> oneof 
        [ SigAll    <$> arbitrary
        , SigNone   <$> arbitrary
        , SigSingle <$> arbitrary
        , f
        ]
      where
        f = do
            -- avoid valid SigHash bytes
            w <- elements $ 0x00 : 0x80 : [0x04..0x7f] ++ [0x84..0xff]
            return $ SigUnknown (testBit w 7) w

-- | Arbitrary valid SigHash
newtype ArbitraryValidSigHash = ArbitraryValidSigHash SigHash
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryValidSigHash where
    arbitrary = ArbitraryValidSigHash <$> oneof
        [ SigAll    <$> arbitrary
        , SigNone   <$> arbitrary
        , SigSingle <$> arbitrary
        ]

-- | Arbitrary message hash, private key, nonce and corresponding TxSignature.
-- The signature is generated with a random message, random private key and
-- a random nonce.
data ArbitraryTxSignature = 
    ArbitraryTxSignature Word256 PrvKey FieldN TxSignature
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryTxSignature where
    arbitrary = do
        ArbitrarySignature msg key nonce sig <- arbitrary
        ArbitrarySigHash sh <- arbitrary
        return $ ArbitraryTxSignature msg key nonce $ TxSignature sig sh

-- | Arbitrary message hash, private key and corresponding TxSignature. The
-- signature is generated deterministically using a random message and a
-- random private key.
data ArbitraryDetTxSignature = 
    ArbitraryDetTxSignature Word256 PrvKey TxSignature
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryDetTxSignature where
    arbitrary = do
        ArbitraryDetSignature msg key sig <- arbitrary
        ArbitrarySigHash sh <- arbitrary
        return $ ArbitraryDetTxSignature msg key $ TxSignature sig sh

-- | Arbitrary m of n parameters
data ArbitraryMSParam = ArbitraryMSParam Int Int
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryMSParam where
    arbitrary = do
        m <- choose (1,16)
        n <- choose (m,16)
        return $ ArbitraryMSParam m n

-- | Arbitrary ScriptOutput (Can by any valid type)
newtype ArbitraryScriptOutput a = ArbitraryScriptOutput (ScriptOutput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryScriptOutput a) where
    arbitrary = ArbitraryScriptOutput <$> oneof 
        [ arbitrary >>= \(ArbitraryPKOutput o) -> return o
        , arbitrary >>= \(ArbitraryPKHashOutput o) -> return o
        , arbitrary >>= \(ArbitraryMSOutput o) -> return o
        , arbitrary >>= \(ArbitrarySHOutput o) -> return o
        ]

-- | Arbitrary ScriptOutput of type PayPK, PayPKHash or PayMS
-- (Not PayScriptHash)
newtype ArbitrarySimpleOutput a = ArbitrarySimpleOutput (ScriptOutput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitrarySimpleOutput a) where
    arbitrary = ArbitrarySimpleOutput <$> oneof 
        [ arbitrary >>= \(ArbitraryPKOutput o) -> return o
        , arbitrary >>= \(ArbitraryPKHashOutput o) -> return o
        , arbitrary >>= \(ArbitraryMSOutput o) -> return o
        ]

-- | Arbitrary ScriptOutput of type PayPK
newtype ArbitraryPKOutput a = ArbitraryPKOutput (ScriptOutput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryPKOutput a) where
    arbitrary = do
        ArbitraryPubKey _ key <- arbitrary
        return $ ArbitraryPKOutput $ PayPK key

-- | Arbitrary ScriptOutput of type PayPKHash
newtype ArbitraryPKHashOutput a = ArbitraryPKHashOutput (ScriptOutput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryPKHashOutput a) where
    arbitrary = do
        ArbitraryPubKeyAddress a <- arbitrary
        return $ ArbitraryPKHashOutput $ PayPKHash a

-- | Arbitrary ScriptOutput of type PayMS
newtype ArbitraryMSOutput a = ArbitraryMSOutput (ScriptOutput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryMSOutput a) where
    arbitrary = do
        ArbitraryMSParam m n <- arbitrary
        keys <- map f <$> vectorOf n arbitrary
        return $ ArbitraryMSOutput $ PayMulSig keys m
      where
        f (ArbitraryPubKey _ key) = key

-- | Arbitrary ScriptOutput of type PayMS containing only compressed keys
newtype ArbitraryMSCOutput a = ArbitraryMSCOutput (ScriptOutput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryMSCOutput a) where
    arbitrary = do
        ArbitraryMSParam m n <- arbitrary
        keys <- map f <$> vectorOf n arbitrary
        return $ ArbitraryMSCOutput $ PayMulSig keys m
      where
        f (ArbitraryPubKeyC _ key) = toPubKeyG key

-- | Arbitrary ScriptOutput of type PayScriptHash
newtype ArbitrarySHOutput a = ArbitrarySHOutput (ScriptOutput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitrarySHOutput a) where
    arbitrary = do
        ArbitraryScriptAddress a <- arbitrary
        return $ ArbitrarySHOutput $ PayScriptHash a

-- | Arbitrary ScriptInput 
newtype ArbitraryScriptInput a = ArbitraryScriptInput (ScriptInput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryScriptInput a) where
    arbitrary = ArbitraryScriptInput <$> oneof
        [ arbitrary >>= \(ArbitraryPKInput i) -> return i
        , arbitrary >>= \(ArbitraryPKHashInput i) -> return i
        , arbitrary >>= \(ArbitraryMSInput i) -> return i
        , arbitrary >>= \(ArbitrarySHInput i) -> return i
        ]

-- | Arbitrary ScriptInput of type SpendPK, SpendPKHash or SpendMulSig
-- (not ScriptHashInput)
newtype ArbitrarySimpleInput a = ArbitrarySimpleInput (ScriptInput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitrarySimpleInput a) where
    arbitrary = ArbitrarySimpleInput <$> oneof
        [ arbitrary >>= \(ArbitraryPKInput i) -> return i
        , arbitrary >>= \(ArbitraryPKHashInput i) -> return i
        , arbitrary >>= \(ArbitraryMSInput i) -> return i
        ]

-- | Arbitrary ScriptInput of type SpendPK
newtype ArbitraryPKInput a = ArbitraryPKInput (ScriptInput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryPKInput a) where
    arbitrary = ArbitraryPKInput . RegularInput . SpendPK <$> oneof 
        [ arbitrary >>= \(ArbitraryTxSignature _ _ _ sig) -> return sig
        , arbitrary >>= \(ArbitraryDetTxSignature _ _ sig) -> return sig
        ]

-- | Arbitrary ScriptInput of type SpendPK
newtype ArbitraryPKHashInput a = ArbitraryPKHashInput (ScriptInput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryPKHashInput a) where
    arbitrary = do
        sig <- oneof
            [ arbitrary >>= \(ArbitraryTxSignature _ _ _ sig) -> return sig
            , arbitrary >>= \(ArbitraryDetTxSignature _ _ sig) -> return sig
            ]
        ArbitraryPubKey _ key <- arbitrary
        return $ ArbitraryPKHashInput $ RegularInput $ SpendPKHash sig key

-- | Arbitrary ScriptInput of type SpendPK with a compressed public key
newtype ArbitraryPKHashCInput a = ArbitraryPKHashCInput (ScriptInput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryPKHashCInput a) where
    arbitrary = do
        sig <- oneof
            [ arbitrary >>= \(ArbitraryTxSignature _ _ _ sig) -> return sig
            , arbitrary >>= \(ArbitraryDetTxSignature _ _ sig) -> return sig
            ]
        ArbitraryPubKeyC _ key <- arbitrary
        return $ ArbitraryPKHashCInput $ RegularInput $ 
            SpendPKHash sig $ toPubKeyG key

-- | Arbitrary ScriptInput of type SpendMulSig
newtype ArbitraryMSInput a = ArbitraryMSInput (ScriptInput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryMSInput a) where
    arbitrary = do
        ArbitraryMSParam m _ <- arbitrary
        sigs <- vectorOf m f
        return $ ArbitraryMSInput $ RegularInput $ SpendMulSig sigs
      where
        f = oneof
            [ arbitrary >>= \(ArbitraryTxSignature _ _ _ sig) -> return sig
            , arbitrary >>= \(ArbitraryDetTxSignature _ _ sig) -> return sig
            ]

-- | Arbitrary ScriptInput of type ScriptHashInput
newtype ArbitrarySHInput a = ArbitrarySHInput (ScriptInput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitrarySHInput a) where
    arbitrary = do
        ArbitrarySimpleInput i <- arbitrary
        ArbitrarySimpleOutput o <- arbitrary
        return $ ArbitrarySHInput $ ScriptHashInput (getRegularInput i) o

-- | Arbitrary ScriptInput of type ScriptHashInput containing a RedeemScript
-- of type PayMulSig and an input of type SpendMulSig. Only compressed keys
-- are used.
newtype ArbitraryMulSigSHCInput a = ArbitraryMulSigSHCInput (ScriptInput a)
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryMulSigSHCInput a) where
    arbitrary = do
        ArbitraryMSCOutput rdm@(PayMulSig _ m) <- arbitrary
        sigs <- vectorOf m f
        return $ ArbitraryMulSigSHCInput
               $ ScriptHashInput (SpendMulSig sigs) rdm
      where
        f = oneof
            [ arbitrary >>= \(ArbitraryTxSignature _ _ _ sig) -> return sig
            , arbitrary >>= \(ArbitraryDetTxSignature _ _ sig) -> return sig
            ]

