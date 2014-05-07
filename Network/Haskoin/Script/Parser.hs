module Network.Haskoin.Script.Parser
( ScriptOutput(..)
, ScriptInput(..)
, RedeemScript
, ScriptHashInput(..)
, scriptAddr
, scriptRecipient
, scriptSender
, encodeInput
, decodeInput
, encodeOutput
, decodeOutput
, encodeScriptHash
, decodeScriptHash
, sortMulSig
, intToScriptOp
, scriptOpToInt
, isPayPK
, isPayPKHash
, isPayMulSig
, isPayScriptHash
, isSpendPK
, isSpendPKHash
, isSpendMulSig
) where

import Control.Monad (liftM2)
import Control.Applicative ((<$>),(<*>))

import Data.List (sortBy)
import qualified Data.ByteString as BS (head, singleton)

import Network.Haskoin.Util
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Protocol.Script
import Network.Haskoin.Script.SigHash

-- | Data type describing standard transaction output scripts. Output scripts
-- provide the conditions that must be fulfilled for someone to spend the
-- output coins. 
data ScriptOutput = 
      -- | Pay to a public key.
      PayPK         { getOutputPubKey   :: !PubKey }
      -- | Pay to a public key hash.
    | PayPKHash     { getOutputAddress  :: !Address }
      -- | Pay to multiple public keys.
    | PayMulSig     { getOutputMulSigKeys     :: ![PubKey]
                    , getOutputMulSigRequired :: !Int
                    }
      -- | Pay to a script hash.
    | PayScriptHash { getOutputAddress  :: !Address }
    deriving (Eq, Show)

-- | Returns True if the script is a pay to public key output.
isPayPK :: ScriptOutput -> Bool
isPayPK (PayPK _) = True
isPayPK _ = False

-- | Returns True if the script is a pay to public key hash output.
isPayPKHash :: ScriptOutput -> Bool
isPayPKHash (PayPKHash _) = True
isPayPKHash _ = False

-- | Returns True if the script is a pay to multiple public keys output.
isPayMulSig :: ScriptOutput -> Bool
isPayMulSig (PayMulSig _ _) = True
isPayMulSig _ = False

-- | Returns true if the script is a pay to script hash output.
isPayScriptHash :: ScriptOutput -> Bool
isPayScriptHash (PayScriptHash _) = True
isPayScriptHash _ = False

-- | Computes a script address from a script output. This address can be used
-- in a pay to script hash output.
scriptAddr :: ScriptOutput -> Address
scriptAddr = ScriptAddress . hash160 . hash256BS . toBS
  where 
    toBS = encodeScriptOps . encodeOutput 

-- | Sorts the public keys of a multisignature output in ascending order by
-- comparing their serialized representations. This feature allows for easier
-- multisignature account management as participants in a multisignature wallet
-- will blindly agree on an ordering of the public keys without having to
-- communicate. 
sortMulSig :: ScriptOutput -> ScriptOutput
sortMulSig out = case out of
    PayMulSig keys r -> PayMulSig (sortBy f keys) r
    _ -> error "Can only call orderMulSig on PayMulSig scripts"
  where 
    f a b = encode' a `compare` encode' b

-- | Computes a 'Script' from a 'ScriptOutput'. The 'Script' is a list of 
-- 'ScriptOp' can can be used to build a 'Tx'.
encodeOutput :: ScriptOutput -> Script
encodeOutput s = Script $ case s of
    -- Pay to PubKey
    (PayPK k) -> [opPushData $ encode' k, OP_CHECKSIG]
    -- Pay to PubKey Hash Address
    (PayPKHash a) -> case a of
        (PubKeyAddress h) -> [ OP_DUP, OP_HASH160, opPushData $ encode' h
                             , OP_EQUALVERIFY, OP_CHECKSIG 
                             ] 
        (ScriptAddress _) -> 
            error "encodeOutput: ScriptAddress is invalid in PayPKHash"
    -- Pay to MultiSig Keys
    (PayMulSig ps r)
      | r <= length ps ->
        let opM = intToScriptOp r
            opN = intToScriptOp $ length ps
            keys = map (opPushData . encode') ps
            in opM : keys ++ [opN, OP_CHECKMULTISIG]
      | otherwise -> error "encodeOutput: PayMulSig r must be <= than pkeys"
    -- Pay to Script Hash Address
    (PayScriptHash a) -> case a of
        (ScriptAddress h) -> [ OP_HASH160
                             , opPushData $ encode' h, OP_EQUAL
                             ]
        (PubKeyAddress _) -> 
            error "encodeOutput: PubKeyAddress is invalid in PayScriptHash"

-- | Tries to decode a 'ScriptOutput' from a 'Script'. This can fail if the
-- script is not recognized as any of the standard output types.
decodeOutput :: Script -> Either String ScriptOutput
decodeOutput s = case scriptOps s of
    -- Pay to PubKey
    [OP_PUSHDATA bs _, OP_CHECKSIG] -> PayPK <$> decodeToEither bs
    -- Pay to PubKey Hash
    [OP_DUP, OP_HASH160, OP_PUSHDATA bs _, OP_EQUALVERIFY, OP_CHECKSIG] -> 
        (PayPKHash . PubKeyAddress) <$> decodeToEither bs
    -- Pay to Script Hash
    [OP_HASH160, OP_PUSHDATA bs _, OP_EQUAL] -> 
        (PayScriptHash . ScriptAddress) <$> decodeToEither bs
    -- Pay to MultiSig Keys
    _ -> matchPayMulSig s

-- Match [ OP_N, PubKey1, ..., PubKeyM, OP_M, OP_CHECKMULTISIG ]
matchPayMulSig :: Script -> Either String ScriptOutput
matchPayMulSig (Script ops) = case splitAt (length ops - 2) ops of
    (m:xs,[n,OP_CHECKMULTISIG]) -> do
        (intM,intN) <- liftM2 (,) (scriptOpToInt m) (scriptOpToInt n)
        if intM <= intN && length xs == intN 
            then liftM2 PayMulSig (go xs) (return intM)
            else Left "matchPayMulSig: Invalid M or N parameters"
    _ -> Left "matchPayMulSig: script did not match output template"
  where 
    go (OP_PUSHDATA bs _:xs) = liftM2 (:) (decodeToEither bs) (go xs)
    go [] = return []
    go  _ = Left "matchPayMulSig: invalid multisig opcode"

-- | Transforms integers [1 .. 16] to 'ScriptOp' [OP_1 .. OP_16]
intToScriptOp :: Int -> ScriptOp
intToScriptOp i
    | i `elem` [1..16] = op
    |        otherwise = error $ "intToScriptOp: Invalid integer " ++ (show i)
  where 
    op = decode' $ BS.singleton $ fromIntegral $ i + 0x50

-- | Decode 'ScriptOp' [OP_1 .. OP_16] to integers [1 .. 16]. This functions
-- fails for other values of 'ScriptOp'
scriptOpToInt :: ScriptOp -> Either String Int
scriptOpToInt s 
    | res `elem` [1..16] = return res
    | otherwise          = Left $ "scriptOpToInt: invalid opcode " ++ (show s)
  where 
    res = (fromIntegral $ BS.head $ encode' s) - 0x50

-- | Computes the recipient address of a script. This function fails if the
-- script could not be decoded as a pay to public key hash or pay to script
-- hash. 
scriptRecipient :: Script -> Either String Address
scriptRecipient s = case decodeOutput s of
    Right (PayPKHash a)     -> return a
    Right (PayScriptHash a) -> return a
    Right _                 -> Left "scriptRecipient: bad output script type"
    _                       -> Left "scriptRecipient: non-standard script type"

-- | Computes the sender address of a script. This function fails if the
-- script could not be decoded as a spend public key hash or script hash
-- input. 
scriptSender :: Script -> Either String Address
scriptSender s = case decodeInput s of
    Right (SpendPKHash _ key) -> return $ pubKeyAddr key
    Right _ -> Left "scriptSender: bad input script type"
    _ -> case decodeScriptHash s of
        Right (ScriptHashInput _ rdm) -> return $ scriptAddr rdm
        _ -> Left "scriptSender: non-standard script type"

-- | Data type describing standard transaction input scripts. Input scripts
-- provide the signing data required to unlock the coins of the output they are
-- trying to spend. 
data ScriptInput = 
      -- | Spend the coins of a PayPK output.
      SpendPK     { getInputSig       :: !TxSignature }
      -- | Spend the coins of a PayPKHash output.
    | SpendPKHash { getInputSig :: !TxSignature 
                  , getInputKey :: !PubKey
                  }
      -- | Spend the coins of a PayMulSig output.
    | SpendMulSig { getInputMulSigKeys     :: ![TxSignature] 
                  , getInputMulSigRequired :: !Int
                  }
    deriving (Eq, Show)

-- | Returns True if the input script is spending a public key.
isSpendPK :: ScriptInput -> Bool
isSpendPK (SpendPK _) = True
isSpendPK _ = False

-- | Returns True if the input script is spending a public key hash.
isSpendPKHash :: ScriptInput -> Bool
isSpendPKHash (SpendPKHash _ _) = True
isSpendPKHash _ = False

-- | Returns True if the input script is spending a multisignature output.
isSpendMulSig :: ScriptInput -> Bool
isSpendMulSig (SpendMulSig _ _) = True
isSpendMulSig _ = False

-- | Computes a 'Script' from a 'ScriptInput'. The 'Script' is a list of 
-- 'ScriptOp' can can be used to build a 'Tx'.
encodeInput :: ScriptInput -> Script
encodeInput s = Script $ case s of
    SpendPK ts        -> [ opPushData $ encodeSig ts ]
    SpendPKHash ts p  -> [ opPushData $ encodeSig ts
                         , opPushData $ encode' p
                         ]
    SpendMulSig ts r 
        | length ts <= 16 && r >= 1 && r <= 16 ->
            let sigs = map (opPushData . encodeSig) ts
                in OP_0 : sigs ++ replicate (r - length ts) OP_0
        | otherwise -> error "SpendMulSig: Bad multisig parameters"

-- | Decodes a 'ScriptInput' from a 'Script'. This function fails if the 
-- script can not be parsed as a standard script input.
decodeInput :: Script -> Either String ScriptInput
decodeInput s = case scriptOps s of
    [OP_PUSHDATA bs _] -> SpendPK <$> decodeSig bs 
    [OP_PUSHDATA sig _, OP_PUSHDATA p _] -> 
        liftM2 SpendPKHash (decodeSig sig) (decodeToEither p)
    (OP_0 : xs) -> matchSpendMulSig $ Script xs
    _ -> Left "decodeInput: Script did not match input templates"

matchSpendMulSig :: Script -> Either String ScriptInput
matchSpendMulSig (Script ops) = 
    liftM2 SpendMulSig (go ops) (return $ length ops)
  where 
    go (OP_PUSHDATA bs _:xs) = liftM2 (:) (decodeSig bs) (go xs)
    go (OP_0:xs)
        | all (== OP_0) xs = return []
        | otherwise = Left "matchSpendMulSig: invalid opcode after OP_0"
    go [] = return []
    go _  = Left "matchSpendMulSig: invalid multisig opcode"

type RedeemScript = ScriptOutput

-- | Data type describing an input script spending a pay to script hash
-- output. To spend a script hash output, an input script must provide
-- both a redeem script and a regular input script spending the redeem 
-- script.
data ScriptHashInput = ScriptHashInput 
    { -- | Input script spending the redeem script
      spendSHInput  :: ScriptInput   
      -- | Redeem script
    , spendSHOutput :: RedeemScript
    } deriving (Eq, Show)

-- | Compute a 'Script' from a 'ScriptHashInput'. The 'Script' is a list of 
-- 'ScriptOp' can can be used to build a 'Tx'.
encodeScriptHash :: ScriptHashInput -> Script
encodeScriptHash (ScriptHashInput i o) =
    Script $ (scriptOps si) ++ [opPushData $ encodeScriptOps so]
  where 
    si = encodeInput i
    so = encodeOutput o

-- | Tries to decode a 'ScriptHashInput' from a 'Script'. This function fails
-- if the script can not be parsed as a script hash input.
decodeScriptHash :: Script -> Either String ScriptHashInput
decodeScriptHash (Script ops) = case splitAt (length ops - 1) ops of
    (is,[OP_PUSHDATA bs _]) -> 
        ScriptHashInput <$> (decodeInput $ Script is) 
                        <*> (decodeOutput =<< decodeScriptOps bs)
    _ -> Left "decodeScriptHash: Script did not match input template"

