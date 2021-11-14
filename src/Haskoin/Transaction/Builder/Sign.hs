{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskoin.Transaction.Builder.Sign
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Types and logic for signing transactions.
-}
module Haskoin.Transaction.Builder.Sign (
    SigInput (..),
    makeSignature,
    makeSigHash,
    signTx,
    findInputIndex,
    signInput,
    buildInput,
    sigKeys,
) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM, when)
import Data.Aeson (
    FromJSON,
    ToJSON (..),
    object,
    pairs,
    parseJSON,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Either (rights)
import Data.Hashable (Hashable)
import Data.List (find, nub)
import Data.Maybe (
    catMaybes,
    fromMaybe,
    mapMaybe,
    maybeToList,
 )
import Data.Word (Word64)
import GHC.Generics (Generic)
import Haskoin.Address (getAddrHash160, pubKeyAddr)
import Haskoin.Crypto (Hash256, SecKey)
import Haskoin.Crypto.Signature (signHash, verifyHashSig)
import Haskoin.Data (Network)
import Haskoin.Keys.Common (
    PubKeyI (..),
    SecKeyI (..),
    derivePubKeyI,
    wrapSecKey,
 )
import Haskoin.Script
import Haskoin.Transaction.Common
import Haskoin.Transaction.Segwit
import Haskoin.Util (matchTemplate, updateIndex)

{- | Data type used to specify the signing parameters of a transaction input.
 To sign an input, the previous output script, outpoint and sighash are
 required. When signing a pay to script hash output, an additional redeem
 script is required.
-}
data SigInput = SigInput
    { -- | output script to spend
      -- ^ output script value
      sigInputScript :: !ScriptOutput
    , -- | output script value
      -- ^ outpoint to spend
      sigInputValue :: !Word64
    , -- | outpoint to spend
      -- ^ signature type
      sigInputOP :: !OutPoint
    , -- | signature type
      -- ^ redeem script
      sigInputSH :: !SigHash
    , -- | redeem script
      sigInputRedeem :: !(Maybe RedeemScript)
    }
    deriving (Eq, Show, Read, Generic, Hashable, NFData)

instance ToJSON SigInput where
    toJSON (SigInput so val op sh rdm) =
        object $
            [ "pkscript" .= so
            , "value" .= val
            , "outpoint" .= op
            , "sighash" .= sh
            ]
                ++ ["redeem" .= r | r <- maybeToList rdm]
    toEncoding (SigInput so val op sh rdm) =
        pairs $
            "pkscript" .= so
                <> "value" .= val
                <> "outpoint" .= op
                <> "sighash" .= sh
                <> maybe mempty ("redeem" .=) rdm

instance FromJSON SigInput where
    parseJSON =
        withObject "SigInput" $ \o ->
            SigInput <$> o .: "pkscript"
                <*> o .: "value"
                <*> o .: "outpoint"
                <*> o .: "sighash"
                <*> o .:? "redeem"

{- | Sign a transaction by providing the 'SigInput' signing parameters and a
 list of private keys. The signature is computed deterministically as defined
 in RFC-6979.
-}
signTx ::
    Network ->
    -- | transaction to sign
    Tx ->
    -- | signing parameters, with nesting flag
    [(SigInput, Bool)] ->
    -- | private keys to sign with
    [SecKey] ->
    -- | signed transaction
    Either String Tx
signTx net otx sigis allKeys
    | null ti = Left "signTx: Transaction has no inputs"
    | otherwise = foldM go otx $ findInputIndex (sigInputOP . fst) sigis ti
  where
    ti = txIn otx
    go tx (sigi@(SigInput so _ _ _ rdmM, _), i) = do
        keys <- sigKeys so rdmM allKeys
        foldM (\t k -> signInput net t i sigi k) tx keys

{- | Sign a single input in a transaction deterministically (RFC-6979).  The
 nesting flag only affects the behavior of segwit inputs.
-}
signInput ::
    Network ->
    Tx ->
    Int ->
    -- | boolean flag: nest input
    (SigInput, Bool) ->
    SecKeyI ->
    Either String Tx
signInput net tx i (sigIn@(SigInput so val _ _ rdmM), nest) key = do
    let sig = makeSignature net tx i sigIn key
    si <- buildInput net tx i so val rdmM sig $ derivePubKeyI key
    w <- updatedWitnessData tx i so si
    return
        tx
            { txIn = nextTxIn so si
            , txWitness = w
            }
  where
    f si x = x{scriptInput = encodeInputBS si}
    g so' x = x{scriptInput = runPutS . serialize . opPushData $ encodeOutputBS so'}
    txis = txIn tx
    nextTxIn so' si
        | isSegwit so' && nest = updateIndex i txis (g so')
        | isSegwit so' = txIn tx
        | otherwise = updateIndex i txis (f si)

{- | Add the witness data of the transaction given segwit parameters for an input.

 @since 0.11.0.0
-}
updatedWitnessData :: Tx -> Int -> ScriptOutput -> ScriptInput -> Either String WitnessData
updatedWitnessData tx i so si
    | isSegwit so = updateWitness . toWitnessStack =<< calcWitnessProgram so si
    | otherwise = return $ txWitness tx
  where
    updateWitness w
        | null $ txWitness tx = return $ updateIndex i defaultStack (const w)
        | length (txWitness tx) /= n = Left "Invalid number of witness stacks"
        | otherwise = return $ updateIndex i (txWitness tx) (const w)
    defaultStack = replicate n $ toWitnessStack EmptyWitnessProgram
    n = length $ txIn tx

-- | Associate an input index to each value in a list
findInputIndex ::
    -- | extract an outpoint
    (a -> OutPoint) ->
    -- | input list
    [a] ->
    -- | reference list of inputs
    [TxIn] ->
    [(a, Int)]
findInputIndex getOutPoint as ti =
    mapMaybe g $ zip (matchTemplate as ti f) [0 ..]
  where
    f s txin = getOutPoint s == prevOutput txin
    g (Just s, i) = Just (s, i)
    g (Nothing, _) = Nothing

{- | Find from the list of provided private keys which one is required to sign
 the 'ScriptOutput'.
-}
sigKeys ::
    ScriptOutput ->
    Maybe RedeemScript ->
    [SecKey] ->
    Either String [SecKeyI]
sigKeys so rdmM keys =
    case (so, rdmM) of
        (PayPK p, Nothing) ->
            return . map fst . maybeToList $ find ((== p) . snd) zipKeys
        (PayPKHash h, Nothing) -> return $ keyByHash h
        (PayMulSig ps r, Nothing) ->
            return $ map fst $ take r $ filter ((`elem` ps) . snd) zipKeys
        (PayScriptHash _, Just rdm) -> sigKeys rdm Nothing keys
        (PayWitnessPKHash h, _) -> return $ keyByHash h
        (PayWitnessScriptHash _, Just rdm) -> sigKeys rdm Nothing keys
        _ -> Left "sigKeys: Could not decode output script"
  where
    zipKeys =
        [ (prv, pub)
        | k <- keys
        , t <- [True, False]
        , let prv = wrapSecKey t k
        , let pub = derivePubKeyI prv
        ]
    keyByHash h = fmap fst . maybeToList . findKey h $ zipKeys
    findKey h = find $ (== h) . getAddrHash160 . pubKeyAddr . snd

{- | Construct an input for a transaction given a signature, public key and data
 about the previous output.
-}
buildInput ::
    Network ->
    -- | transaction where input will be added
    Tx ->
    -- | input index where signature will go
    Int ->
    -- | output script being spent
    ScriptOutput ->
    -- | amount of previous output
    Word64 ->
    -- | redeem script if pay-to-script-hash
    Maybe RedeemScript ->
    TxSignature ->
    PubKeyI ->
    Either String ScriptInput
buildInput net tx i so val rdmM sig pub = do
    when (i >= length (txIn tx)) $ Left "buildInput: Invalid input index"
    case (so, rdmM) of
        (PayScriptHash _, Just rdm) -> buildScriptHashInput rdm
        (PayWitnessScriptHash _, Just rdm) -> buildScriptHashInput rdm
        (PayWitnessPKHash _, Nothing) -> return . RegularInput $ SpendPKHash sig pub
        (_, Nothing) -> buildRegularInput so
        _ -> Left "buildInput: Invalid output/redeem script combination"
  where
    buildRegularInput = \case
        PayPK _ -> return $ RegularInput $ SpendPK sig
        PayPKHash _ -> return $ RegularInput $ SpendPKHash sig pub
        PayMulSig msPubs r -> do
            let mSigs = take r $ catMaybes $ matchTemplate allSigs msPubs f
                allSigs = nub $ sig : parseExistingSigs net tx so i
            return $ RegularInput $ SpendMulSig mSigs
        _ -> Left "buildInput: Invalid output/redeem script combination"
    buildScriptHashInput rdm = do
        inp <- buildRegularInput rdm
        return $ ScriptHashInput (getRegularInput inp) rdm
    f (TxSignature x sh) p =
        verifyHashSig (makeSigHash net tx i so val sh rdmM) x (pubKeyPoint p)
    f TxSignatureEmpty _ = False

{- | Apply heuristics to extract the signatures for a particular input that are
 embedded in the transaction.

 @since 0.11.0.0
-}
parseExistingSigs :: Network -> Tx -> ScriptOutput -> Int -> [TxSignature]
parseExistingSigs net tx so i = insSigs <> witSigs
  where
    insSigs = case decodeInputBS net scp of
        Right (ScriptHashInput (SpendMulSig xs) _) -> xs
        Right (RegularInput (SpendMulSig xs)) -> xs
        _ -> []
    scp = scriptInput $ txIn tx !! i
    witSigs
        | not $ isSegwit so = []
        | null $ txWitness tx = []
        | otherwise = rights $ decodeTxSig net <$> (txWitness tx !! i)

-- | Produce a structured representation of a deterministic (RFC-6979) signature over an input.
makeSignature :: Network -> Tx -> Int -> SigInput -> SecKeyI -> TxSignature
makeSignature net tx i (SigInput so val _ sh rdmM) key =
    TxSignature (signHash (secKeyData key) m) sh
  where
    m = makeSigHash net tx i so val sh rdmM

{- | A function which selects the digest algorithm and parameters as appropriate

 @since 0.11.0.0
-}
makeSigHash ::
    Network ->
    Tx ->
    Int ->
    ScriptOutput ->
    Word64 ->
    SigHash ->
    Maybe RedeemScript ->
    Hash256
makeSigHash net tx i so val sh rdmM = h net tx (encodeOutput so') val i sh
  where
    so' = case so of
        PayWitnessPKHash h' -> PayPKHash h'
        _ -> fromMaybe so rdmM
    h
        | isSegwit so = txSigHashForkId
        | otherwise = txSigHash
