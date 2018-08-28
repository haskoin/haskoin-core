{-|
  Arbitrary types for Network.Haskoin.Script
-}
module Network.Haskoin.Test.Script where

import           Data.Word
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Keys.Types
import           Network.Haskoin.Script
import           Network.Haskoin.Test.Address
import           Network.Haskoin.Test.Crypto
import           Network.Haskoin.Test.Keys
import           Network.Haskoin.Test.Util
import           Network.Haskoin.Transaction.Types
import           Network.Haskoin.Util
import           Test.QuickCheck

-- | Arbitrary Script with random script ops
arbitraryScript :: Gen Script
arbitraryScript = Script <$> listOf arbitraryScriptOp

-- | Arbitrary ScriptOp (push operations have random data)
arbitraryScriptOp :: Gen ScriptOp
arbitraryScriptOp =
    oneof
          -- Pushing Data
        [ opPushData <$> arbitraryBS1
        , return OP_0
        , return OP_1NEGATE
        , return OP_RESERVED
        , return OP_1
        , return OP_2
        , return OP_3
        , return OP_4
        , return OP_5
        , return OP_6
        , return OP_7
        , return OP_8
        , return OP_9
        , return OP_10
        , return OP_11
        , return OP_12
        , return OP_13
        , return OP_14
        , return OP_15
        , return OP_16
        -- Flow control
        , return OP_NOP
        , return OP_VER
        , return OP_IF
        , return OP_NOTIF
        , return OP_VERIF
        , return OP_VERNOTIF
        , return OP_ELSE
        , return OP_ENDIF
        , return OP_VERIFY
        , return OP_RETURN
        -- Stack operations
        , return OP_TOALTSTACK
        , return OP_FROMALTSTACK
        , return OP_IFDUP
        , return OP_DEPTH
        , return OP_DROP
        , return OP_DUP
        , return OP_NIP
        , return OP_OVER
        , return OP_PICK
        , return OP_ROLL
        , return OP_ROT
        , return OP_SWAP
        , return OP_TUCK
        , return OP_2DROP
        , return OP_2DUP
        , return OP_3DUP
        , return OP_2OVER
        , return OP_2ROT
        , return OP_2SWAP
        -- Splice
        , return OP_CAT
        , return OP_SUBSTR
        , return OP_LEFT
        , return OP_RIGHT
        , return OP_SIZE
        -- Bitwise logic
        , return OP_INVERT
        , return OP_AND
        , return OP_OR
        , return OP_XOR
        , return OP_EQUAL
        , return OP_EQUALVERIFY
        , return OP_RESERVED1
        , return OP_RESERVED2
        -- Arithmetic
        , return OP_1ADD
        , return OP_1SUB
        , return OP_2MUL
        , return OP_2DIV
        , return OP_NEGATE
        , return OP_ABS
        , return OP_NOT
        , return OP_0NOTEQUAL
        , return OP_ADD
        , return OP_SUB
        , return OP_MUL
        , return OP_DIV
        , return OP_MOD
        , return OP_LSHIFT
        , return OP_RSHIFT
        , return OP_BOOLAND
        , return OP_BOOLOR
        , return OP_NUMEQUAL
        , return OP_NUMEQUALVERIFY
        , return OP_NUMNOTEQUAL
        , return OP_LESSTHAN
        , return OP_GREATERTHAN
        , return OP_LESSTHANOREQUAL
        , return OP_GREATERTHANOREQUAL
        , return OP_MIN
        , return OP_MAX
        , return OP_WITHIN
        -- Crypto
        , return OP_RIPEMD160
        , return OP_SHA1
        , return OP_SHA256
        , return OP_HASH160
        , return OP_HASH256
        , return OP_CODESEPARATOR
        , return OP_CHECKSIG
        , return OP_CHECKSIGVERIFY
        , return OP_CHECKMULTISIG
        , return OP_CHECKMULTISIGVERIFY
        -- Expansion
        , return OP_NOP1
        , return OP_NOP2
        , return OP_NOP3
        , return OP_NOP4
        , return OP_NOP5
        , return OP_NOP6
        , return OP_NOP7
        , return OP_NOP8
        , return OP_NOP9
        , return OP_NOP10
        -- Other
        , return OP_PUBKEYHASH
        , return OP_PUBKEY
        , return $ OP_INVALIDOPCODE 0xff
        ]

-- | Arbtirary ScriptOp with a value in [OP_1 .. OP_16]
arbitraryIntScriptOp :: Gen ScriptOp
arbitraryIntScriptOp =
    elements
        [ OP_1,  OP_2,  OP_3,  OP_4
        , OP_5,  OP_6,  OP_7,  OP_8
        , OP_9,  OP_10, OP_11, OP_12
        , OP_13, OP_14, OP_15, OP_16
        ]

-- | Arbitrary PushDataType
arbitraryPushDataType :: Gen PushDataType
arbitraryPushDataType = elements [OPCODE, OPDATA1, OPDATA2, OPDATA4]

-- | Arbitrary SigHash (including invalid/unknown sighash codes)
arbitrarySigHash :: Gen SigHash
arbitrarySigHash = fromIntegral <$> (arbitrary :: Gen Word32)

-- | Arbitrary valid SigHash
arbitraryValidSigHash :: Gen SigHash
arbitraryValidSigHash = do
    sh <- elements [sigHashAll, sigHashNone, sigHashSingle]
    f1 <- elements [id, setForkIdFlag]
    f2 <- elements [id, setAnyoneCanPayFlag]
    return $ f1 $ f2 sh

-- | Arbitrary message hash, private key and corresponding TxSignature. The
-- signature is generated deterministically using a random message and a
-- random private key.
arbitraryTxSignature :: Gen (TxHash, PrvKey, TxSignature)
arbitraryTxSignature = do
    (msg, key, sig) <- arbitrarySignature
    sh <- fromIntegral <$> (arbitrary :: Gen Word8)
    let txsig = TxSignature sig sh
    return (TxHash msg, key, txsig)

-- | Arbitrary transaction signature that could also be empty
arbitraryTxSignatureEmpty :: Gen TxSignature
arbitraryTxSignatureEmpty =
    frequency [ (1, return TxSignatureEmpty)
              , (10, lst3 <$> arbitraryTxSignature)
              ]

-- | Arbitrary m of n parameters
arbitraryMSParam :: Gen (Int, Int)
arbitraryMSParam = do
    m <- choose (1,16)
    n <- choose (m,16)
    return (m, n)

-- | Arbitrary ScriptOutput (Can by any valid type)
arbitraryScriptOutput :: Network -> Gen ScriptOutput
arbitraryScriptOutput net =
    oneof
        [ arbitraryPKOutput
        , arbitraryPKHashOutput
        , arbitraryMSOutput
        , arbitrarySHOutput net
        , arbitraryDCOutput
        ]

-- | Arbitrary ScriptOutput of type PayPK, PayPKHash or PayMS
-- (Not PayScriptHash or DataCarrier)
arbitrarySimpleOutput ::Gen ScriptOutput
arbitrarySimpleOutput =
    oneof
        [ arbitraryPKOutput
        , arbitraryPKHashOutput
        , arbitraryMSOutput
        ]

-- | Arbitrary ScriptOutput of type PayPK
arbitraryPKOutput :: Gen ScriptOutput
arbitraryPKOutput =  PayPK . snd <$> arbitraryPubKey

-- | Arbitrary ScriptOutput of type PayPKHash
arbitraryPKHashOutput :: Gen ScriptOutput
arbitraryPKHashOutput = PayPKHash <$> arbitraryHash160

-- | Arbitrary ScriptOutput of type PayMS
arbitraryMSOutput :: Gen ScriptOutput
arbitraryMSOutput = do
    (m, n) <- arbitraryMSParam
    keys <- map snd <$> vectorOf n arbitraryPubKey
    return $ PayMulSig keys m

-- | Arbitrary ScriptOutput of type PayMS containing only compressed keys
arbitraryMSCOutput :: Gen ScriptOutput
arbitraryMSCOutput = do
    (m, n) <- arbitraryMSParam
    keys <- map (toPubKeyG . snd) <$> vectorOf n arbitraryPubKeyC
    return $ PayMulSig keys m

-- | Arbitrary ScriptOutput of type PayScriptHash
arbitrarySHOutput :: Network -> Gen ScriptOutput
arbitrarySHOutput net =
    PayScriptHash . getAddrHash160 <$> arbitraryScriptAddress net

-- | Arbitrary ScriptOutput of type DataCarrier
arbitraryDCOutput :: Gen ScriptOutput
arbitraryDCOutput = DataCarrier <$> arbitraryBS1

-- | Arbitrary ScriptInput
arbitraryScriptInput :: Gen ScriptInput
arbitraryScriptInput =
    oneof
        [ arbitraryPKInput
        , arbitraryPKHashInput
        , arbitraryMSInput
        , arbitrarySHInput
        ]

-- | Arbitrary ScriptInput of type SpendPK, SpendPKHash or SpendMulSig
-- (not ScriptHashInput)
arbitrarySimpleInput :: Gen ScriptInput
arbitrarySimpleInput =
    oneof
        [ arbitraryPKInput
        , arbitraryPKHashInput
        , arbitraryMSInput
        ]

-- | Arbitrary ScriptInput of type SpendPK
arbitraryPKInput :: Gen ScriptInput
arbitraryPKInput = RegularInput . SpendPK <$> arbitraryTxSignatureEmpty

-- | Arbitrary ScriptInput of type SpendPK
arbitraryPKHashInput :: Gen ScriptInput
arbitraryPKHashInput = do
    sig <- arbitraryTxSignatureEmpty
    key <- snd <$> arbitraryPubKey
    return $ RegularInput $ SpendPKHash sig key

-- | Arbitrary ScriptInput of type SpendPK with a compressed public key
arbitraryPKHashCInput :: Gen ScriptInput
arbitraryPKHashCInput = do
    sig <- arbitraryTxSignatureEmpty
    key <- snd <$> arbitraryPubKeyC
    return $ RegularInput $ SpendPKHash sig $ toPubKeyG key

-- | Like 'arbitraryPKHashCInput' without empty signatures
arbitraryPKHashCInputFull :: Gen ScriptInput
arbitraryPKHashCInputFull = do
    sig <- lst3 <$> arbitraryTxSignature
    key <- snd <$> arbitraryPubKeyC
    return $ RegularInput $ SpendPKHash sig $ toPubKeyG key

-- | Arbitrary ScriptInput of type SpendMulSig
arbitraryMSInput :: Gen ScriptInput
arbitraryMSInput = do
    m    <- fst <$> arbitraryMSParam
    sigs <- vectorOf m arbitraryTxSignatureEmpty
    return $ RegularInput $ SpendMulSig sigs

-- | Arbitrary ScriptInput of type ScriptHashInput
arbitrarySHInput :: Gen ScriptInput
arbitrarySHInput = do
    i <- arbitrarySimpleInput
    o <- arbitrarySimpleOutput
    return $ ScriptHashInput (getRegularInput i) o

-- | Arbitrary ScriptInput of type ScriptHashInput containing a RedeemScript
-- of type PayMulSig and an input of type SpendMulSig. Only compressed keys
-- are used.
arbitraryMulSigSHCInput :: Gen ScriptInput
arbitraryMulSigSHCInput = do
    rdm@(PayMulSig _ m) <- arbitraryMSCOutput
    sigs <- vectorOf m arbitraryTxSignatureEmpty
    return $ ScriptHashInput (SpendMulSig sigs) rdm

-- | Like 'arbitraryMulSigSHCInput' with no empty signatures
arbitraryMulSigSHCInputFull :: Gen ScriptInput
arbitraryMulSigSHCInputFull = do
    rdm@(PayMulSig _ m) <- arbitraryMSCOutput
    sigs <- map lst3 <$> vectorOf m arbitraryTxSignature
    return $ ScriptHashInput (SpendMulSig sigs) rdm
