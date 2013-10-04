module Haskoin.Wallet.ScriptParser
( ScriptOutput(..)
, ScriptInput(..)
, MulSig2Type(..)
, MulSig3Type(..)
, ScriptHashInput(..)
, SigHash(..)
, TxSignature(..)
, scriptAddr
, encodeInput
, decodeInput
, encodeOutput
, decodeOutput
, encodeScriptHash
, decodeScriptHash
, encodeSigHash32
) where

import Control.Monad

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Util

data SigHash = SigAll    
             | SigNone   
             | SigSingle 

             -- Anyone Can Pay
             | SigAllAcp
             | SigNoneAcp
             | SigSingleAcp 
             deriving (Eq, Show)

instance Binary SigHash where

    get = do
        w <- getWord8
        case w of 0x01 -> return SigAll
                  0x02 -> return SigNone
                  0x03 -> return SigSingle
                  0x81 -> return SigAllAcp
                  0x82 -> return SigNoneAcp
                  0x83 -> return SigSingleAcp
                  _    -> fail "Non-canonical signature: unknown hashtype byte"

    put sh = putWord8 $ case sh of
        SigAll       -> 0x01
        SigNone      -> 0x02
        SigSingle    -> 0x03
        SigAllAcp    -> 0x81
        SigNoneAcp   -> 0x82
        SigSingleAcp -> 0x83

encodeSigHash32 :: SigHash -> BS.ByteString
encodeSigHash32 sh = encode' sh `BS.append` BS.pack [0,0,0]

-- Signatures in scripts contain the signature hash type byte
data TxSignature = TxSignature 
    { txSignature :: Signature 
    , sigHashType :: SigHash
    } deriving (Eq, Show)

instance Binary TxSignature where
    get = liftM2 TxSignature get get
    put (TxSignature s h) = put s >> put h

data MulSig2Type = OneOfTwo | TwoOfTwo
    deriving (Eq, Show)

data MulSig3Type = OneOfThree | TwoOfThree | ThreeOfThree
    deriving (Eq, Show)

data ScriptOutput = 
      PayPK         { runPayPubKey     :: !PubKey }
    | PayPKHash     { runPayPubKeyHash :: !Address }
    | PayMulSig1    { runPayMulSig1    :: !PubKey }
    | PayMulSig2    { mulSig2Type      :: !MulSig2Type
                    , fstMulSigKey     :: !PubKey
                    , sndMulSigKey     :: !PubKey
                    }
    | PayMulSig3    { mulSig3Type      :: !MulSig3Type
                    , fstMulSigKey     :: !PubKey
                    , sndMulSigKey     :: !PubKey
                    , trdMulSigKey     :: !PubKey
                    }
    | PayScriptHash { runPayScriptHash :: !Address }
    | PayNonStd     { runPayNonStd     :: !Script }
    deriving (Eq, Show)

scriptAddr :: ScriptOutput -> Address
scriptAddr = ScriptAddress . hash160 . hash256BS . encode' . encodeOutput
    
encodeOutput :: ScriptOutput -> Script
encodeOutput s = Script $ case s of
    (PayPK k) -> 
        [OP_PUSHDATA $ encode' k, OP_CHECKSIG]
    (PayPKHash a) ->
        [ OP_DUP, OP_HASH160
        , OP_PUSHDATA $ encode' $ runAddress a
        , OP_EQUALVERIFY, OP_CHECKSIG
        ] 
    (PayMulSig1 k) ->
        [OP_1, OP_PUSHDATA $ encode' k, OP_1, OP_CHECKMULTISIG] 
    (PayMulSig2 t k1 k2) ->
        [ case t of
            OneOfTwo -> OP_1
            TwoOfTwo -> OP_2
        , OP_PUSHDATA $ encode' k1
        , OP_PUSHDATA $ encode' k2
        , OP_2, OP_CHECKMULTISIG
        ]
    (PayMulSig3 t k1 k2 k3) ->
        [ case t of
            OneOfThree   -> OP_1
            TwoOfThree   -> OP_2
            ThreeOfThree -> OP_3
        , OP_PUSHDATA $ encode' k1
        , OP_PUSHDATA $ encode' k2
        , OP_PUSHDATA $ encode' k3
        , OP_3, OP_CHECKMULTISIG
        ]
    (PayScriptHash a) ->
        [OP_HASH160, OP_PUSHDATA $ encode' $ runAddress a, OP_EQUAL]
    (PayNonStd s) -> runScript s

decodeOutput :: Script -> ScriptOutput
decodeOutput s = case runScript s of
    [OP_PUSHDATA k, OP_CHECKSIG] -> 
        decodeEither k def PayPK
    [OP_DUP, OP_HASH160, OP_PUSHDATA h, OP_EQUALVERIFY, OP_CHECKSIG] -> 
        decodeEither h def (PayPKHash . PubKeyAddress)
    [OP_1, OP_PUSHDATA k, OP_1, OP_CHECKMULTISIG] -> 
        decodeEither k def PayMulSig1
    [t, OP_PUSHDATA k1, OP_PUSHDATA k2, OP_2, OP_CHECKMULTISIG] -> 
        decodeEither k1 def $ \r1 -> decodeEither k2 def $ \r2 ->
            case t of OP_1 -> PayMulSig2 OneOfTwo r1 r2
                      OP_2 -> PayMulSig2 TwoOfTwo r1 r2
                      _    -> def
    [t, OP_PUSHDATA k1, OP_PUSHDATA k2, OP_PUSHDATA k3
      , OP_3, OP_CHECKMULTISIG] -> 
        decodeEither k1 def $ \r1 -> 
        decodeEither k2 def $ \r2 -> 
        decodeEither k3 def $ \r3 -> 
            case t of OP_1 -> PayMulSig3 OneOfThree   r1 r2 r3
                      OP_2 -> PayMulSig3 TwoOfThree   r1 r2 r3
                      OP_3 -> PayMulSig3 ThreeOfThree r1 r2 r3
                      _    -> def
    [OP_HASH160, OP_PUSHDATA h, OP_EQUAL] -> 
        decodeEither h def (PayScriptHash . ScriptAddress)
    _ -> def
    where def = PayNonStd s

data ScriptInput = 
      SpendPK      { runSpendSig1      :: !TxSignature }
    | SpendPKHash  { runSpendPKHashSig :: !TxSignature 
                   , runSpendPKHashKey :: !PubKey
                   }
    | SpendMulSig1 { runSpendSig1      :: !TxSignature }
    | SpendMulSig2 { runSpendSig1      :: !TxSignature 
                   , runSpendSig2      :: !TxSignature
                   }
    | SpendMulSig3 { runSpendSig1      :: !TxSignature 
                   , runSpendSig2      :: !TxSignature 
                   , runSpendSig3      :: !TxSignature
                   }
    | SpendNonStd  { runSpendNonStd    :: !Script }
    deriving (Eq, Show)

encodeInput :: ScriptInput -> Script
encodeInput s = Script $ case s of
    (SpendPK s) -> [OP_PUSHDATA $ encode' s]
    (SpendPKHash ts p) -> 
        [ OP_PUSHDATA $ encode' ts
        , OP_PUSHDATA $ encode' p
        ]
    (SpendMulSig1 s) -> 
        [ OP_FALSE -- OP_CHECKMULTISIG bug
        , OP_PUSHDATA $ encode' s
        ]
    (SpendMulSig2 ts1 ts2) -> 
        [ OP_FALSE -- OP_CHECKMULTISIG bug
        , OP_PUSHDATA $ encode' ts1
        , OP_PUSHDATA $ encode' ts2
        ]
    (SpendMulSig3 ts1 ts2 ts3) -> 
        [ OP_FALSE -- OP_CHECKMULTISIG bug
        , OP_PUSHDATA $ encode' ts1
        , OP_PUSHDATA $ encode' ts2
        , OP_PUSHDATA $ encode' ts3
        ]
    (SpendNonStd (Script ops)) -> ops

decodeInput :: Script -> ScriptInput
decodeInput s = case runScript s of
    [OP_PUSHDATA s] -> decodeEither s def SpendPK
    [OP_PUSHDATA a, OP_PUSHDATA b] -> 
        decodeEither a def $ \s -> 
        decodeEither b def $ \p -> SpendPKHash s p
    [OP_FALSE, OP_PUSHDATA s] -> decodeEither s def SpendMulSig1
    [OP_FALSE, OP_PUSHDATA a, OP_PUSHDATA b] ->
        decodeEither a def $ \s1 -> 
        decodeEither b def $ \s2 -> SpendMulSig2 s1 s2
    [OP_FALSE, OP_PUSHDATA a, OP_PUSHDATA b, OP_PUSHDATA c] ->
        decodeEither a def $ \s1 ->
        decodeEither b def $ \s2 ->
        decodeEither c def $ \s3 -> SpendMulSig3 s1 s2 s3
    _ -> def
    where def = SpendNonStd s

data ScriptHashInput = ScriptHashInput 
    { spendSHInput  :: ScriptInput 
    , spendSHOutput :: ScriptOutput
    } deriving (Eq, Show)

encodeScriptHash :: ScriptHashInput -> Script
encodeScriptHash (ScriptHashInput i o) = 
    Script $ iops ++ [OP_PUSHDATA outBS]
    where (Script iops) = encodeInput i
          out           = encodeOutput o
          -- Encode the script without the initial VarInt length
          outBS         = toStrictBS $ runPut $ putScriptOps $ runScript out

decodeScriptHash :: Script -> Maybe ScriptHashInput
decodeScriptHash s@(Script ops)
    | length ops < 2 = Nothing
    | otherwise = case last ops of
        (OP_PUSHDATA o) -> case runGetOrFail getScriptOps (toLazyBS o) of
            (Left _)          -> Nothing
            (Right (_,_,res)) -> 
                Just $ ScriptHashInput i $ decodeOutput $ Script res
        _ -> Nothing
    where i = decodeInput $ Script $ init ops

