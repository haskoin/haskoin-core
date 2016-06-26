module Network.Haskoin.Script.Parser
( ScriptOutput(..)
, ScriptInput(..)
, SimpleInput(..)
, RedeemScript
, scriptAddr
, outputAddress
, inputAddress
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
, isScriptHashInput
) where

import           Control.Applicative            ((<|>))
import           Control.DeepSeq                (NFData, rnf)
import           Control.Monad                  (guard, liftM2, (<=<))
import           Data.Aeson                     (FromJSON, ToJSON,
                                                 Value (String), parseJSON,
                                                 toJSON, withText)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS (head, singleton)
import           Data.Foldable                  (foldrM)
import           Data.List                      (sortBy)
import           Data.Serialize                 (encode, decode)
import           Data.String.Conversions        (cs)
import           Network.Haskoin.Crypto.Base58
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Crypto.Keys
import           Network.Haskoin.Script.SigHash
import           Network.Haskoin.Script.Types
import           Network.Haskoin.Util

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

instance FromJSON ScriptOutput where
    parseJSON = withText "scriptoutput" $ \t -> either fail return $
        maybeToEither "scriptoutput not hex" (decodeHex $ cs t) >>=
        decodeOutputBS

instance ToJSON ScriptOutput where
    toJSON = String . cs . encodeHex . encodeOutputBS

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
scriptAddr = ScriptAddress . hash160 . getHash256 . hash256 . encodeOutputBS

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
    f a b = encode a `compare` encode b

-- | Computes a 'Script' from a 'ScriptOutput'. The 'Script' is a list of
-- 'ScriptOp' can can be used to build a 'Tx'.
encodeOutput :: ScriptOutput -> Script
encodeOutput s = Script $ case s of
    -- Pay to PubKey
    (PayPK k) -> [opPushData $ encode k, OP_CHECKSIG]
    -- Pay to PubKey Hash Address
    (PayPKHash a) -> case a of
        (PubKeyAddress h) -> [ OP_DUP, OP_HASH160, opPushData $ encode h
                             , OP_EQUALVERIFY, OP_CHECKSIG
                             ]
        (ScriptAddress _) ->
            error "encodeOutput: ScriptAddress is invalid in PayPKHash"
    -- Pay to MultiSig Keys
    (PayMulSig ps r)
      | r <= length ps ->
        let opM = intToScriptOp r
            opN = intToScriptOp $ length ps
            keys = map (opPushData . encode) ps
            in opM : keys ++ [opN, OP_CHECKMULTISIG]
      | otherwise -> error "encodeOutput: PayMulSig r must be <= than pkeys"
    -- Pay to Script Hash Address
    (PayScriptHash a) -> case a of
        (ScriptAddress h) -> [ OP_HASH160
                             , opPushData $ encode h, OP_EQUAL
                             ]
        (PubKeyAddress _) ->
            error "encodeOutput: PubKeyAddress is invalid in PayScriptHash"

-- | Similar to 'encodeOutput' but encodes to a ByteString
encodeOutputBS :: ScriptOutput -> ByteString
encodeOutputBS = encode . encodeOutput

-- | Tries to decode a 'ScriptOutput' from a 'Script'. This can fail if the
-- script is not recognized as any of the standard output types.
decodeOutput :: Script -> Either String ScriptOutput
decodeOutput s = case scriptOps s of
    -- Pay to PubKey
    [OP_PUSHDATA bs _, OP_CHECKSIG] -> PayPK <$> decode bs
    -- Pay to PubKey Hash
    [OP_DUP, OP_HASH160, OP_PUSHDATA bs _, OP_EQUALVERIFY, OP_CHECKSIG] ->
        (PayPKHash . PubKeyAddress) <$> decode bs
    -- Pay to Script Hash
    [OP_HASH160, OP_PUSHDATA bs _, OP_EQUAL] ->
        (PayScriptHash . ScriptAddress) <$> decode bs
    -- Pay to MultiSig Keys
    _ -> matchPayMulSig s

-- | Similar to 'decodeOutput' but decodes from a ByteString
decodeOutputBS :: ByteString -> Either String ScriptOutput
decodeOutputBS = decodeOutput <=< decode

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
    go (OP_PUSHDATA bs _:xs) = liftM2 (:) (decode bs) (go xs)
    go [] = return []
    go  _ = Left "matchPayMulSig: invalid multisig opcode"

-- | Transforms integers [1 .. 16] to 'ScriptOp' [OP_1 .. OP_16]
intToScriptOp :: Int -> ScriptOp
intToScriptOp i
    | i `elem` [1..16] = either (const err) id op
    | otherwise = err
  where
    op  = decode $ BS.singleton $ fromIntegral $ i + 0x50
    err = error $ "intToScriptOp: Invalid integer " ++ (show i)

-- | Decode 'ScriptOp' [OP_1 .. OP_16] to integers [1 .. 16]. This functions
-- fails for other values of 'ScriptOp'
scriptOpToInt :: ScriptOp -> Either String Int
scriptOpToInt s
    | res `elem` [1..16] = return res
    | otherwise          = Left $ "scriptOpToInt: invalid opcode " ++ (show s)
  where
    res = (fromIntegral $ BS.head $ encode s) - 0x50

-- | Get the address of a `ScriptOutput`
outputAddress :: ScriptOutput -> Either String Address
outputAddress s = case s of
    PayPKHash a -> return a
    PayScriptHash a -> return a
    PayPK k -> return $ pubKeyAddr k
    _ -> Left "outputAddress: bad output script type"

-- | Get the address of a `ScriptInput`
inputAddress :: ScriptInput -> Either String Address
inputAddress s = case s of
    RegularInput (SpendPKHash _ key) -> return $ pubKeyAddr key
    ScriptHashInput _ rdm -> return $ scriptAddr rdm
    _ -> Left "inputAddress: bad input script type"

-- | Data type describing standard transaction input scripts. Input scripts
-- provide the signing data required to unlock the coins of the output they are
-- trying to spend.
data SimpleInput
      -- | Spend the coins of a PayPK output.
    = SpendPK     { getInputSig :: !TxSignature }
      -- | Spend the coins of a PayPKHash output.
    | SpendPKHash { getInputSig :: !TxSignature
                  , getInputKey :: !PubKey
                  }
      -- | Spend the coins of a PayMulSig output.
    | SpendMulSig { getInputMulSigKeys :: ![TxSignature] }
    deriving (Eq, Show, Read)

instance NFData SimpleInput where
    rnf (SpendPK i) = rnf i
    rnf (SpendPKHash i k) = rnf i `seq` rnf k
    rnf (SpendMulSig k) = rnf k

-- | Returns True if the input script is spending a public key.
isSpendPK :: ScriptInput -> Bool
isSpendPK (RegularInput (SpendPK _)) = True
isSpendPK _ = False

-- | Returns True if the input script is spending a public key hash.
isSpendPKHash :: ScriptInput -> Bool
isSpendPKHash (RegularInput (SpendPKHash _ _)) = True
isSpendPKHash _ = False

-- | Returns True if the input script is spending a multisignature output.
isSpendMulSig :: ScriptInput -> Bool
isSpendMulSig (RegularInput (SpendMulSig _)) = True
isSpendMulSig _ = False

isScriptHashInput :: ScriptInput -> Bool
isScriptHashInput (ScriptHashInput _ _) = True
isScriptHashInput _ = False

type RedeemScript = ScriptOutput

data ScriptInput
    = RegularInput    { getRegularInput     :: SimpleInput }
    | ScriptHashInput { getScriptHashInput  :: SimpleInput
                      , getScriptHashRedeem :: RedeemScript
                      }
    deriving (Eq, Show, Read)

instance NFData ScriptInput where
    rnf (RegularInput i) = rnf i
    rnf (ScriptHashInput i o) = rnf i `seq` rnf o

-- | Computes a 'Script' from a 'SimpleInput'. The 'Script' is a list of
-- 'ScriptOp' that can be used to build a 'Tx'.
encodeSimpleInput :: SimpleInput -> Script
encodeSimpleInput s = Script $ case s of
    SpendPK ts       -> [ opPushData $ encodeSig ts ]
    SpendPKHash ts p -> [ opPushData $ encodeSig ts
                        , opPushData $ encode p
                        ]
    SpendMulSig ts   -> OP_0 : map (opPushData . encodeSig) ts

decodeSimpleInput :: Script -> Either String SimpleInput
decodeSimpleInput (Script ops) = maybeToEither errMsg $
    matchPK ops <|> matchPKHash ops <|> matchMulSig ops
  where
    matchPK [OP_PUSHDATA bs _] = SpendPK <$> eitherToMaybe (decodeSig bs)
    matchPK _ = Nothing
    matchPKHash [OP_PUSHDATA sig _, OP_PUSHDATA pub _] =
        liftM2 SpendPKHash (eitherToMaybe $ decodeSig sig) (decodeToMaybe pub)
    matchPKHash _ = Nothing
    matchMulSig (x:xs) = do
        guard $ isPushOp x
        SpendMulSig <$> foldrM f [] xs
    matchMulSig _ = Nothing
    f (OP_PUSHDATA bs _) acc =
        liftM2 (:) (eitherToMaybe $ decodeSig bs) (Just acc)
    f _ _ = Nothing
    errMsg = "decodeInput: Could not decode script input"

encodeInput :: ScriptInput -> Script
encodeInput s = case s of
    RegularInput ri -> encodeSimpleInput ri
    ScriptHashInput i o -> Script $
        (scriptOps $ encodeSimpleInput i) ++ [opPushData $ encodeOutputBS o]

-- | Similar to 'encodeInput' but encodes to a ByteString
encodeInputBS :: ScriptInput -> ByteString
encodeInputBS = encode . encodeInput

-- | Decodes a 'ScriptInput' from a 'Script'. This function fails if the
-- script can not be parsed as a standard script input.
decodeInput :: Script -> Either String ScriptInput
decodeInput s@(Script ops) = maybeToEither errMsg $
    matchSimpleInput <|> matchPayScriptHash
  where
    matchSimpleInput = RegularInput <$> (eitherToMaybe $ decodeSimpleInput s)
    matchPayScriptHash = case splitAt (length (scriptOps s) - 1) ops of
        (is, [OP_PUSHDATA bs _]) -> do
            rdm <- eitherToMaybe $ decodeOutputBS bs
            inp <- eitherToMaybe $ decodeSimpleInput $ Script is
            return $ ScriptHashInput inp rdm
        _ -> Nothing
    errMsg = "decodeInput: Could not decode script input"

-- | Similar to 'decodeInput' but decodes from a ByteString
decodeInputBS :: ByteString -> Either String ScriptInput
decodeInputBS = decodeInput <=< decode

