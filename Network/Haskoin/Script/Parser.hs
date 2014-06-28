module Network.Haskoin.Script.Parser
( ScriptOutput(..)
, ScriptInput(..)
, RedeemScript
, scriptAddr
, scriptRecipient
, scriptSender
, encodeInput
, encodeInputBS
, decodeInput
, decodeInputBS
, encodeOutput
, encodeOutputBS
, decodeOutput
, decodeOutputBS
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
, getRedeem
, appendRedeem
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (liftM, liftM2, unless, when)
import Control.Applicative ((<$>))

import Data.List (sortBy)
import qualified Data.ByteString as BS 
    ( ByteString
    , head
    , singleton
    )

import Network.Haskoin.Util
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Script.Types
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
    deriving (Eq, Show, Read)

instance NFData ScriptOutput where
    rnf (PayPK k) = rnf k
    rnf (PayPKHash a) = rnf a
    rnf (PayMulSig k r) = rnf k `seq` rnf r
    rnf (PayScriptHash a) = rnf a

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
scriptAddr = ScriptAddress . hash160 . hash256BS . encodeOutputBS

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

-- | Similar to 'encodeOutput' but encodes to a ByteString
encodeOutputBS :: ScriptOutput -> BS.ByteString
encodeOutputBS = encode' . encodeOutput

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

-- | Similar to 'decodeOutput' but decodes from a ByteString
decodeOutputBS :: BS.ByteString -> Either String ScriptOutput
decodeOutputBS = (decodeOutput =<<) . decodeToEither

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
scriptSender :: ScriptOutput -> Script -> Either String Address
scriptSender so s = case decodeInput so s of
    Right (_, _, True) -> return $ getOutputAddress so
    Right (_, _, False) -> Left "scriptSender: not a P2SH output"
    Left _ -> Left "scriptSender: unable to determine address"

-- | Data type describing standard transaction input scripts. Input scripts
-- provide the signing data required to unlock the coins of the output they are
-- trying to spend. 
data ScriptInput = 
      -- | Spend the coins of a PayPK output.
      SpendPK     { getInputSig :: !TxSignature }
      -- | Spend the coins of a PayPKHash output.
    | SpendPKHash { getInputSig :: !TxSignature 
                  , getInputKey :: !PubKey
                  }
      -- | Spend the coins of a PayMulSig output.
    | SpendMulSig { getInputMulSigKeys :: ![TxSignature] 
                  , getInputRequired   :: Int
                  }
    deriving (Eq, Show, Read)

instance NFData ScriptInput where
    rnf (SpendPK i) = rnf i
    rnf (SpendPKHash i k) = rnf i `seq` rnf k
    rnf (SpendMulSig k r) = rnf k `seq` rnf r

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
-- 'ScriptOp' that can be used to build a 'Tx'.
encodeInput :: ScriptInput -> Script
encodeInput s = Script $ case s of
    SpendPK ts        -> [ opPushData $ encodeSig ts ]
    SpendPKHash ts p  -> [ opPushData $ encodeSig ts
                         , opPushData $ encode' p
                         ]
    SpendMulSig ts r
        | length ts <= 16 && r <= 16 && length ts <= r -> 
            OP_0 : map (opPushData . encodeSig) ts
        | otherwise -> error "SpendMulSig: Too many signatures"

-- | Similar to 'encodeInput' but encodes to a ByteString
encodeInputBS :: ScriptInput -> BS.ByteString
encodeInputBS = encode' . encodeInput

-- | Decode a 'ScriptInput' from a 'Script', given the corresponding
-- 'ScriptOutput'. Return ScriptInput and P2SH recursion flag.
decodeInput :: ScriptOutput   -- ^ Output that this input spends
            -> Script         -- ^ Sigscript being parsed
            -> Either String (ScriptInput, ScriptOutput, Bool)
            -- ^ (PkScript, SigScript, P2SH)
decodeInput pks sgs = go False pks sgs
  where
    go p o i = case o of
        PayPK _ -> case scriptOps i of
            [OP_PUSHDATA sig _] -> do
                dsig <- decodeSig sig
                return (SpendPK dsig, o, p)
            _ -> Left "decodeInput: could not decode script as SpendPK"
        PayPKHash _ -> case scriptOps i of
            [OP_PUSHDATA sig _, OP_PUSHDATA pub _] -> do
                dsig <- decodeSig sig
                dpub <- decodeToEither pub
                return (SpendPKHash dsig dpub, o, p)
            _ -> Left "decodeInput: could not decode script as SpendPKHash"
        PayMulSig _ r -> case scriptOps i of
            x : xs -> do
                unless (isPushOp x) $ Left "decodeInput: invalid SpendMulSig"
                ms <- matchSpendMulSig (Script xs) r
                return (ms, o, p)
            -- _ -> Left "decodeInput: could not decode script as SpendMulSig"
            _ -> Left $ show $ scriptOps i
        PayScriptHash a -> do
            when p $ Left "decodeInput: nested P2SH scrpt"
            out <- getRedeem $ Script (scriptOps i)
            unless (scriptAddr out == a) $
                Left "decodeInput: address doesn't match redeem script"
            go True out $ Script (init $ scriptOps i)

-- | Similar to 'decodeInput' but decodes from a ByteString.
decodeInputBS :: ScriptOutput    -- ^ Output that this input spends
              -> BS.ByteString   -- ^ ByteString with sigscript
              -> Either String (ScriptInput, ScriptOutput, Bool)
                 -- ^ Output parsed sigscript and P2SH flag
decodeInputBS pks = (decodeInput pks =<<) . decodeToEither 

matchSpendMulSig :: Script -> Int -> Either String ScriptInput
matchSpendMulSig (Script ops) r = 
    liftM (flip SpendMulSig r) $ go ops
  where 
    go (OP_PUSHDATA bs _:xs) = liftM2 (:) (decodeSig bs) (go xs)
    go [] = return []
    go _  = Left "matchSpendMulSig: invalid multisig opcode"

type RedeemScript = ScriptOutput

-- | Get parsed redeem script from sigscript.
getRedeem :: Script                      -- ^ Sigscript
          -> Either String RedeemScript
getRedeem (Script ops) = do
    when (null ops) $ Left "getRedeem: empty sigscript"
    out <- decodeOutputBS bs
    when (isPayScriptHash out) $ Left "getRedeem: nested P2SH script"
    return out
  where
    (OP_PUSHDATA bs _) = last ops

-- | Append redeem script to end of sigscript.
appendRedeem :: ScriptInput
             -> RedeemScript
             -> Script
appendRedeem i r = Script $ si ++ [rdm]
  where
    si = scriptOps $ encodeInput i
    rdm = opPushData $ encodeOutputBS r
