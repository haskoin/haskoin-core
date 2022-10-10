{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- Types to represent segregated witness data and auxilliary functions to
-- manipulate it.  See [BIP 141](https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki)
-- and [BIP 143](https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki) for
-- details.
module Bitcoin.Transaction.Segwit (
    -- * Segwit
    WitnessProgram (..),
    WitnessProgramPKH (..),
    WitnessProgramSH (..),
    isSegwit,
    viewWitnessProgram,
    decodeWitnessInput,
    calcWitnessProgram,
    simpleInputStack,
    toWitnessStack,
) where

import Bitcoin.Data (Network)
import Bitcoin.Keys.Common (PubKeyI)
import Bitcoin.Script (
    Script,
    ScriptInput (..),
    ScriptOutput (..),
    SimpleInput (..),
    TxSignature (TxSignatureEmpty),
    decodeOutput,
    decodeTxSig,
    encodeOutput,
    encodeTxSig,
 )
import Bitcoin.Transaction.Common (WitnessStack)
import qualified Bitcoin.Util as U
import qualified Data.Binary as Bin
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL


-- | Test if a 'ScriptOutput' is P2WPKH or P2WSH
isSegwit :: ScriptOutput -> Bool
isSegwit = \case
    PayWitnessPKHash{} -> True
    PayWitnessScriptHash{} -> True
    _ -> False


-- | High level represenation of a (v0) witness program
data WitnessProgram
    = P2WPKH WitnessProgramPKH
    | P2WSH WitnessProgramSH
    | EmptyWitnessProgram
    deriving (Eq, Show)


-- | Encode a witness program
toWitnessStack :: WitnessProgram -> WitnessStack
toWitnessStack = \case
    P2WPKH (WitnessProgramPKH sig key) ->
        [ encodeTxSig sig
        , U.encodeS key
        ]
    P2WSH (WitnessProgramSH stack scr) -> stack <> [U.encodeS scr]
    EmptyWitnessProgram -> mempty


-- | High level representation of a P2WPKH witness
data WitnessProgramPKH = WitnessProgramPKH
    { witnessSignature :: !TxSignature
    , witnessPubKey :: !PubKeyI
    }
    deriving (Eq, Show)


-- | High-level representation of a P2WSH witness
data WitnessProgramSH = WitnessProgramSH
    { witnessScriptHashStack :: ![ByteString]
    , witnessScriptHashScript :: !Script
    }
    deriving (Eq, Show)


-- | Calculate the witness program from the transaction data
viewWitnessProgram ::
    Network -> ScriptOutput -> WitnessStack -> Either String WitnessProgram
viewWitnessProgram net so witness = case so of
    PayWitnessPKHash _ | length witness == 2 -> do
        sig <- decodeTxSig net $ head witness
        pubkey <- U.decode . BSL.fromStrict $ witness !! 1
        return . P2WPKH $ WitnessProgramPKH sig pubkey
    PayWitnessScriptHash _ | not (null witness) -> do
        redeemScript <- U.decode . BSL.fromStrict $ last witness
        return . P2WSH $ WitnessProgramSH (init witness) redeemScript
    _
        | null witness -> return EmptyWitnessProgram
        | otherwise -> Left "viewWitnessProgram: Invalid witness program"


-- | Analyze the witness, trying to match it with standard input structures
decodeWitnessInput ::
    Network ->
    WitnessProgram ->
    Either String (Maybe ScriptOutput, SimpleInput)
decodeWitnessInput net = \case
    P2WPKH (WitnessProgramPKH sig key) -> return (Nothing, SpendPKHash sig key)
    P2WSH (WitnessProgramSH st scr) -> do
        so <- decodeOutput scr
        fmap (Just so,) $ case (so, st) of
            (PayPK _, [sigBS]) ->
                SpendPK <$> decodeTxSig net sigBS
            (PayPKHash _, [sigBS, keyBS]) ->
                SpendPKHash <$> decodeTxSig net sigBS <*> U.decode (BSL.fromStrict keyBS)
            (PayMulSig _ _, "" : sigsBS) ->
                SpendMulSig <$> traverse (decodeTxSig net) sigsBS
            _ -> Left "decodeWitnessInput: Non-standard script output"
    EmptyWitnessProgram -> Left "decodeWitnessInput: Empty witness program"


-- | Create the witness program for a standard input
calcWitnessProgram :: ScriptOutput -> ScriptInput -> Either String WitnessProgram
calcWitnessProgram so si = case (so, si) of
    (PayWitnessPKHash{}, RegularInput (SpendPKHash sig pk)) -> p2wpkh sig pk
    (PayScriptHash{}, RegularInput (SpendPKHash sig pk)) -> p2wpkh sig pk
    (PayWitnessScriptHash{}, ScriptHashInput i o) -> p2wsh i o
    (PayScriptHash{}, ScriptHashInput i o) -> p2wsh i o
    _ -> Left "calcWitnessProgram: Invalid segwit SigInput"
  where
    p2wpkh sig = return . P2WPKH . WitnessProgramPKH sig
    p2wsh i o = return . P2WSH $ WitnessProgramSH (simpleInputStack i) (encodeOutput o)


-- | Create the witness stack required to spend a standard P2WSH input
simpleInputStack :: SimpleInput -> [ByteString]
simpleInputStack = \case
    SpendPK sig -> [f sig]
    SpendPKHash sig k -> [f sig, U.encodeS k]
    SpendMulSig sigs -> "" : fmap f sigs
  where
    f TxSignatureEmpty = ""
    f sig = encodeTxSig sig
