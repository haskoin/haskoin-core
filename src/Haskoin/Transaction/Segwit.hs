{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Haskoin.Transaction.Segwit
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Types to represent segregated witness data and auxilliary functions to
manipulate it.  See [BIP 141](https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki)
and [BIP 143](https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki) for
details.
-}
module Haskoin.Transaction.Segwit (
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

import Data.ByteString (ByteString)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Haskoin.Data
import Haskoin.Keys.Common
import Haskoin.Script
import Haskoin.Transaction.Common

{- | Test if a 'ScriptOutput' is P2WPKH or P2WSH

 @since 0.11.0.0
-}
isSegwit :: ScriptOutput -> Bool
isSegwit = \case
    PayWitnessPKHash{} -> True
    PayWitnessScriptHash{} -> True
    _ -> False

{- | High level represenation of a (v0) witness program

 @since 0.11.0.0
-}
data WitnessProgram
    = P2WPKH WitnessProgramPKH
    | P2WSH WitnessProgramSH
    | EmptyWitnessProgram
    deriving (Eq, Show)

{- | Encode a witness program

 @since 0.11.0.0
-}
toWitnessStack :: WitnessProgram -> WitnessStack
toWitnessStack = \case
    P2WPKH (WitnessProgramPKH sig key) -> [encodeTxSig sig, runPutS (serialize key)]
    P2WSH (WitnessProgramSH stack scr) -> stack <> [runPutS (serialize scr)]
    EmptyWitnessProgram -> mempty

{- | High level representation of a P2WPKH witness

 @since 0.11.0.0
-}
data WitnessProgramPKH = WitnessProgramPKH
    { witnessSignature :: !TxSignature
    , witnessPubKey :: !PubKeyI
    }
    deriving (Eq, Show)

{- | High-level representation of a P2WSH witness

 @since 0.11.0.0
-}
data WitnessProgramSH = WitnessProgramSH
    { witnessScriptHashStack :: ![ByteString]
    , witnessScriptHashScript :: !Script
    }
    deriving (Eq, Show)

{- | Calculate the witness program from the transaction data

 @since 0.11.0.0
-}
viewWitnessProgram ::
    Network -> ScriptOutput -> WitnessStack -> Either String WitnessProgram
viewWitnessProgram net so witness = case so of
    PayWitnessPKHash _ | length witness == 2 -> do
        sig <- decodeTxSig net $ head witness
        pubkey <- runGetS deserialize $ witness !! 1
        return . P2WPKH $ WitnessProgramPKH sig pubkey
    PayWitnessScriptHash _ | not (null witness) -> do
        redeemScript <- runGetS deserialize $ last witness
        return . P2WSH $ WitnessProgramSH (init witness) redeemScript
    _
        | null witness -> return EmptyWitnessProgram
        | otherwise -> Left "viewWitnessProgram: Invalid witness program"

{- | Analyze the witness, trying to match it with standard input structures

 @since 0.11.0.0
-}
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
                SpendPKHash <$> decodeTxSig net sigBS <*> runGetS deserialize keyBS
            (PayMulSig _ _, "" : sigsBS) ->
                SpendMulSig <$> traverse (decodeTxSig net) sigsBS
            _ -> Left "decodeWitnessInput: Non-standard script output"
    EmptyWitnessProgram -> Left "decodeWitnessInput: Empty witness program"

{- | Create the witness program for a standard input

 @since 0.11.0.0
-}
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

{- | Create the witness stack required to spend a standard P2WSH input

 @since 0.11.0.0
-}
simpleInputStack :: SimpleInput -> [ByteString]
simpleInputStack = \case
    SpendPK sig -> [f sig]
    SpendPKHash sig k -> [f sig, runPutS (serialize k)]
    SpendMulSig sigs -> "" : fmap f sigs
  where
    f TxSignatureEmpty = ""
    f sig = encodeTxSig sig
