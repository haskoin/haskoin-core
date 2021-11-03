{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskoin.Transaction.Builder
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Code to simplify transaction creation, signing, fee calculation and coin
selection.
-}
module Haskoin.Transaction.Builder (
    -- * Transaction Builder
    buildAddrTx,
    buildTx,
    buildInput,
    SigInput (..),
    signTx,
    signNestedWitnessTx,
    makeSignature,
    signInput,
    signNestedInput,
    verifyStdTx,
    mergeTxs,
    sigKeys,
    mergeTxInput,
    findSigInput,
    verifyStdInput,

    -- * Coin Selection
    Coin (..),
    chooseCoins,
    chooseCoinsSink,
    chooseMSCoins,
    chooseMSCoinsSink,
    countMulSig,
    greedyAddSink,
    guessTxFee,
    guessMSTxFee,
    guessTxSize,
    guessMSSize,
) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad (foldM, unless)
import Control.Monad.Identity (runIdentity)
import Crypto.Secp256k1
import qualified Data.ByteString as B
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Conduit (
    ConduitT,
    Void,
    await,
    runConduit,
    (.|),
 )
import Data.Conduit.List (sourceList)
import Data.Either (fromRight)
import Data.List (nub)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word64)
import Haskoin.Address
import Haskoin.Constants
import Haskoin.Crypto.Hash (Hash256, addressHash)
import Haskoin.Crypto.Signature
import Haskoin.Keys.Common
import Haskoin.Network.Common
import Haskoin.Script
import Haskoin.Transaction.Builder.Sign (
    SigInput (..),
    buildInput,
    makeSignature,
    sigKeys,
 )
import qualified Haskoin.Transaction.Builder.Sign as S
import Haskoin.Transaction.Common
import Haskoin.Transaction.Segwit (
    decodeWitnessInput,
    isSegwit,
    viewWitnessProgram,
 )
import Haskoin.Util

{- | Any type can be used as a Coin if it can provide a value in Satoshi.
 The value is used in coin selection algorithms.
-}
class Coin c where
    coinValue :: c -> Word64

{- | Coin selection algorithm for normal (non-multisig) transactions. This
 function returns the selected coins together with the amount of change to
 send back to yourself, taking the fee into account.
-}
chooseCoins ::
    Coin c =>
    -- | value to send
    Word64 ->
    -- | fee per byte
    Word64 ->
    -- | number of outputs (including change)
    Int ->
    -- | try to find better solutions
    Bool ->
    -- | list of ordered coins to choose from
    [c] ->
    -- | coin selection and change
    Either String ([c], Word64)
chooseCoins target fee nOut continue coins =
    runIdentity . runConduit $
        sourceList coins .| chooseCoinsSink target fee nOut continue

{- | Coin selection algorithm for normal (non-multisig) transactions. This
 function returns the selected coins together with the amount of change to
 send back to yourself, taking the fee into account. This version uses a Sink
 for conduit-based coin selection.
-}
chooseCoinsSink ::
    (Monad m, Coin c) =>
    -- | value to send
    Word64 ->
    -- | fee per byte
    Word64 ->
    -- | number of outputs (including change)
    Int ->
    -- | try to find better solution
    Bool ->
    -- | coin selection and change
    ConduitT c Void m (Either String ([c], Word64))
chooseCoinsSink target fee nOut continue
    | target > 0 =
        maybeToEither err
            <$> greedyAddSink target (guessTxFee fee nOut) continue
    | otherwise = return $ Left "chooseCoins: Target must be > 0"
  where
    err = "chooseCoins: No solution found"

{- | Coin selection algorithm for multisig transactions. This function returns
 the selected coins together with the amount of change to send back to
 yourself, taking the fee into account. This function assumes all the coins
 are script hash outputs that send funds to a multisignature address.
-}
chooseMSCoins ::
    Coin c =>
    -- | value to send
    Word64 ->
    -- | fee per byte
    Word64 ->
    -- | m of n multisig
    (Int, Int) ->
    -- | number of outputs (including change)
    Int ->
    -- | try to find better solution
    Bool ->
    [c] ->
    -- | coin selection change amount
    Either String ([c], Word64)
chooseMSCoins target fee ms nOut continue coins =
    runIdentity . runConduit $
        sourceList coins .| chooseMSCoinsSink target fee ms nOut continue

{- | Coin selection algorithm for multisig transactions. This function returns
 the selected coins together with the amount of change to send back to
 yourself, taking the fee into account. This function assumes all the coins
 are script hash outputs that send funds to a multisignature address. This
 version uses a Sink if you need conduit-based coin selection.
-}
chooseMSCoinsSink ::
    (Monad m, Coin c) =>
    -- | value to send
    Word64 ->
    -- | fee per byte
    Word64 ->
    -- | m of n multisig
    (Int, Int) ->
    -- | number of outputs (including change)
    Int ->
    -- | try to find better solution
    Bool ->
    -- | coin selection and change
    ConduitT c Void m (Either String ([c], Word64))
chooseMSCoinsSink target fee ms nOut continue
    | target > 0 =
        maybeToEither err
            <$> greedyAddSink target (guessMSTxFee fee ms nOut) continue
    | otherwise = return $ Left "chooseMSCoins: Target must be > 0"
  where
    err = "chooseMSCoins: No solution found"

{- | Select coins greedily by starting from an empty solution. If the 'continue'
 flag is set, the algorithm will try to find a better solution in the stream
 after a solution is found. If the next solution found is not strictly better
 than the previously found solution, the algorithm stops and returns the
 previous solution. If the continue flag is not set, the algorithm will return
 the first solution it finds in the stream.
-}
greedyAddSink ::
    (Monad m, Coin c) =>
    -- | value to send
    Word64 ->
    -- | coin count to fee function
    (Int -> Word64) ->
    -- | try to find better solutions
    Bool ->
    -- | coin selection and change
    ConduitT c Void m (Maybe ([c], Word64))
greedyAddSink target guessFee continue =
    go [] 0 [] 0
  where
    -- The goal is the value we must reach (including the fee) for a certain
    -- amount of selected coins.
    goal c = target + guessFee c
    go acc aTot ps pTot =
        await >>= \case
            -- A coin is available in the stream
            Just coin -> do
                let val = coinValue coin
                -- We have reached the goal using this coin
                if val + aTot >= goal (length acc + 1)
                    then -- If we want to continue searching for better solutions

                        if continue
                            then -- This solution is the first one or
                            -- This solution is better than the previous one

                                if pTot == 0 || val + aTot < pTot
                                    then -- Continue searching for better solutions in the stream
                                        go [] 0 (coin : acc) (val + aTot)
                                    else -- Otherwise, we stop here and return the previous
                                    -- solution
                                        return $ Just (ps, pTot - goal (length ps))
                            else -- Otherwise, return this solution

                                return $
                                    Just (coin : acc, val + aTot - goal (length acc + 1))
                    else -- We have not yet reached the goal. Add the coin to the
                    -- accumulator
                        go (coin : acc) (val + aTot) ps pTot
            -- We reached the end of the stream
            Nothing ->
                return $
                    if null ps
                        then -- If no solution was found, return Nothing
                            Nothing
                        else -- If we have a solution, return it
                            Just (ps, pTot - goal (length ps))

-- | Estimate tranasction fee to pay based on transaction size estimation.
guessTxFee :: Word64 -> Int -> Int -> Word64
guessTxFee byteFee nOut nIn =
    byteFee * fromIntegral (guessTxSize nIn [] nOut 0)

-- | Same as 'guessTxFee' but for multisig transactions.
guessMSTxFee :: Word64 -> (Int, Int) -> Int -> Int -> Word64
guessMSTxFee byteFee ms nOut nIn =
    byteFee * fromIntegral (guessTxSize 0 (replicate nIn ms) nOut 0)

{- | Computes an upper bound on the size of a transaction based on some known
 properties of the transaction.
-}
guessTxSize ::
    -- | number of regular transaction inputs
    Int ->
    -- | multisig m of n for each input
    [(Int, Int)] ->
    -- | number of P2PKH outputs
    Int ->
    -- | number of P2SH outputs
    Int ->
    -- | upper bound on transaction size
    Int
guessTxSize pki msi pkout msout =
    8 + inpLen + inp + outLen + out
  where
    inpLen =
        B.length
            . runPutS
            . serialize
            . VarInt
            . fromIntegral
            $ length msi + pki
    outLen =
        B.length
            . runPutS
            . serialize
            . VarInt
            . fromIntegral
            $ pkout + msout
    inp = pki * 148 + sum (map guessMSSize msi)
    -- (20: hash160) + (5: opcodes) +
    -- (1: script len) + (8: Word64)
    out =
        pkout * 34
            +
            -- (20: hash160) + (3: opcodes) +
            -- (1: script len) + (8: Word64)
            msout * 32

-- | Size of a multisig P2SH input.
guessMSSize :: (Int, Int) -> Int
guessMSSize (m, n) =
    -- OutPoint (36) + Sequence (4) + Script
    40
        + fromIntegral (B.length $ runPutS . serialize $ VarInt $ fromIntegral scp)
        + scp
  where
    -- OP_M + n*PubKey + OP_N + OP_CHECKMULTISIG

    rdm =
        fromIntegral $
            B.length $ runPutS $ serialize $ opPushData $ B.replicate (n * 34 + 3) 0
    -- Redeem + m*sig + OP_0
    scp = rdm + m * 73 + 1

{- Build a new Tx -}

{- | Build a transaction by providing a list of outpoints as inputs
 and a list of recipient addresses and amounts as outputs.
-}
buildAddrTx :: Network -> [OutPoint] -> [(Text, Word64)] -> Either String Tx
buildAddrTx net ops rcps =
    buildTx ops <$> mapM f rcps
  where
    f (aTxt, v) =
        maybeToEither ("buildAddrTx: Invalid address " <> cs aTxt) $ do
            a <- textToAddr net aTxt
            let o = addressToOutput a
            return (o, v)

{- | Build a transaction by providing a list of outpoints as inputs
 and a list of 'ScriptOutput' and amounts as outputs.
-}
buildTx :: [OutPoint] -> [(ScriptOutput, Word64)] -> Tx
buildTx ops rcpts =
    Tx 1 (toIn <$> ops) (toOut <$> rcpts) [] 0
  where
    toIn op = TxIn op B.empty maxBound
    toOut (o, v) = TxOut v $ encodeOutputBS o

{- | Sign a transaction by providing the 'SigInput' signing parameters and a
 list of private keys. The signature is computed deterministically as defined
 in RFC-6979.

 Example: P2SH-P2WKH

 > sigIn = SigInput (PayWitnessPKHash h) 100000 op sigHashAll Nothing
 > signedTx = signTx btc unsignedTx [sigIn] [key]

 Example: P2SH-P2WSH multisig

 > sigIn = SigInput (PayWitnessScriptHash h) 100000 op sigHashAll (Just $ PayMulSig [p1,p2,p3] 2)
 > signedTx = signTx btc unsignedTx [sigIn] [k1,k3]
-}
signTx ::
    Network ->
    -- | transaction to sign
    Tx ->
    -- | signing parameters
    [SigInput] ->
    -- | private keys to sign with
    [SecKey] ->
    -- | signed transaction
    Either String Tx
signTx net tx si = S.signTx net tx $ notNested <$> si
  where
    notNested s = (s, False)

{- | This function differs from 'signTx' by assuming all segwit inputs are
 P2SH-nested.  Use the same signing parameters for segwit inputs as in 'signTx'.
-}
signNestedWitnessTx ::
    Network ->
    -- | transaction to sign
    Tx ->
    -- | signing parameters
    [SigInput] ->
    -- | private keys to sign with
    [SecKey] ->
    -- | signed transaction
    Either String Tx
signNestedWitnessTx net tx si = S.signTx net tx $ nested <$> si
  where
    -- NOTE: the nesting flag is ignored for non-segwit inputs
    nested s = (s, True)

-- | Sign a single input in a transaction deterministically (RFC-6979).
signInput :: Network -> Tx -> Int -> SigInput -> SecKeyI -> Either String Tx
signInput net tx i si = S.signInput net tx i (si, False)

-- | Like 'signInput' but treat segwit inputs as nested
signNestedInput :: Network -> Tx -> Int -> SigInput -> SecKeyI -> Either String Tx
signNestedInput net tx i si = S.signInput net tx i (si, True)

{- | Order the 'SigInput' with respect to the transaction inputs. This allows
 the user to provide the 'SigInput' in any order. Users can also provide only
 a partial set of 'SigInput' entries.
-}
findSigInput :: [SigInput] -> [TxIn] -> [(SigInput, Int)]
findSigInput = S.findInputIndex sigInputOP

{- Merge multisig transactions -}

{- | Merge partially-signed multisig transactions.  This function does not
 support segwit and P2SH-segwit inputs.  Use PSBTs to merge transactions with
 segwit inputs.
-}
mergeTxs ::
    Network -> [Tx] -> [(ScriptOutput, Word64, OutPoint)] -> Either String Tx
mergeTxs net txs os
    | null txs = Left "Transaction list is empty"
    | length (nub emptyTxs) /= 1 = Left "Transactions do not match"
    | length txs == 1 = return $ head txs
    | otherwise = foldM (mergeTxInput net txs) (head emptyTxs) outs
  where
    zipOp = zip (matchTemplate os (txIn $ head txs) f) [0 ..]
    outs =
        map (first $ (\(o, v, _) -> (o, v)) . fromJust) $
            filter (isJust . fst) zipOp
    f (_, _, o) txin = o == prevOutput txin
    emptyTxs = map (\tx -> foldl clearInput tx outs) txs
    ins is i = updateIndex i is (\ti -> ti{scriptInput = B.empty})
    clearInput tx (_, i) =
        Tx (txVersion tx) (ins (txIn tx) i) (txOut tx) [] (txLockTime tx)

{- | Merge input from partially-signed multisig transactions.  This function
 does not support segwit and P2SH-segwit inputs.
-}
mergeTxInput ::
    Network ->
    [Tx] ->
    Tx ->
    ((ScriptOutput, Word64), Int) ->
    Either String Tx
mergeTxInput net txs tx ((so, val), i) = do
    -- Ignore transactions with empty inputs
    let ins = map (scriptInput . (!! i) . txIn) txs
    sigRes <- mapM extractSigs $ filter (not . B.null) ins
    let rdm = snd $ head sigRes
    unless (all ((== rdm) . snd) sigRes) $ Left "Redeem scripts do not match"
    si <- encodeInputBS <$> go (nub $ concatMap fst sigRes) so rdm
    let ins' = updateIndex i (txIn tx) (\ti -> ti{scriptInput = si})
    return $ Tx (txVersion tx) ins' (txOut tx) [] (txLockTime tx)
  where
    go allSigs out rdmM =
        case out of
            PayMulSig msPubs r ->
                let sigs =
                        take r $
                            catMaybes $ matchTemplate allSigs msPubs $ f out
                 in return $ RegularInput $ SpendMulSig sigs
            PayScriptHash _ ->
                case rdmM of
                    Just rdm -> do
                        si <- go allSigs rdm Nothing
                        return $ ScriptHashInput (getRegularInput si) rdm
                    _ -> Left "Invalid output script type"
            _ -> Left "Invalid output script type"
    extractSigs si =
        case decodeInputBS net si of
            Right (RegularInput (SpendMulSig sigs)) -> Right (sigs, Nothing)
            Right (ScriptHashInput (SpendMulSig sigs) rdm) ->
                Right (sigs, Just rdm)
            _ -> Left "Invalid script input type"
    f out (TxSignature x sh) p =
        verifyHashSig
            (txSigHash net tx (encodeOutput out) val i sh)
            x
            (pubKeyPoint p)
    f _ TxSignatureEmpty _ = False

{- Tx verification -}

-- | Verify if a transaction is valid and all of its inputs are standard.
verifyStdTx :: Network -> Tx -> [(ScriptOutput, Word64, OutPoint)] -> Bool
verifyStdTx net tx xs =
    not (null (txIn tx)) && all go (zip (matchTemplate xs (txIn tx) f) [0 ..])
  where
    f (_, _, o) txin = o == prevOutput txin
    go (Just (so, val, _), i) = verifyStdInput net tx i so val
    go _ = False

-- | Verify if a transaction input is valid and standard.
verifyStdInput :: Network -> Tx -> Int -> ScriptOutput -> Word64 -> Bool
verifyStdInput net tx i so0 val
    | isSegwit so0 =
        fromRight False $ (inp == mempty &&) . verifySegwitInput so0 <$> wp so0
    | otherwise =
        fromRight False $
            (verifyLegacyInput so0 <$> decodeInputBS net inp)
                <|> (nestedScriptOutput >>= \so -> verifyNestedInput so0 so <$> wp so)
  where
    inp = scriptInput $ txIn tx !! i
    theTxSigHash so = S.makeSigHash net tx i so val

    ws :: WitnessStack
    ws
        | length (txWitness tx) > i = txWitness tx !! i
        | otherwise = []

    wp :: ScriptOutput -> Either String (Maybe ScriptOutput, SimpleInput)
    wp so = decodeWitnessInput net =<< viewWitnessProgram net so ws

    nestedScriptOutput :: Either String ScriptOutput
    nestedScriptOutput =
        scriptOps <$> runGetS deserialize inp >>= \case
            [OP_PUSHDATA bs _] -> decodeOutputBS bs
            _ -> Left "nestedScriptOutput: not a nested output"

    verifyLegacyInput :: ScriptOutput -> ScriptInput -> Bool
    verifyLegacyInput so si = case (so, si) of
        (PayPK pub, RegularInput (SpendPK (TxSignature sig sh))) ->
            verifyHashSig (theTxSigHash so sh Nothing) sig (pubKeyPoint pub)
        (PayPKHash h, RegularInput (SpendPKHash (TxSignature sig sh) pub)) ->
            pubKeyAddr pub == p2pkhAddr h
                && verifyHashSig (theTxSigHash so sh Nothing) sig (pubKeyPoint pub)
        (PayMulSig pubs r, RegularInput (SpendMulSig sigs)) ->
            countMulSig net tx out val i (pubKeyPoint <$> pubs) sigs == r
        (PayScriptHash h, ScriptHashInput si' rdm) ->
            payToScriptAddress rdm == p2shAddr h && verifyLegacyInput rdm (RegularInput si')
        _ -> False
      where
        out = encodeOutput so

    verifySegwitInput ::
        ScriptOutput -> (Maybe ScriptOutput, SimpleInput) -> Bool
    verifySegwitInput so (rdm, si) = case (so, rdm, si) of
        (PayWitnessPKHash h, Nothing, SpendPKHash (TxSignature sig sh) pub) ->
            pubKeyWitnessAddr pub == p2wpkhAddr h
                && verifyHashSig (theTxSigHash so sh Nothing) sig (pubKeyPoint pub)
        (PayWitnessScriptHash h, Just rdm'@(PayPK pub), SpendPK (TxSignature sig sh)) ->
            payToWitnessScriptAddress rdm' == p2wshAddr h
                && verifyHashSig (theTxSigHash so sh $ Just rdm') sig (pubKeyPoint pub)
        (PayWitnessScriptHash h, Just rdm'@(PayPKHash kh), SpendPKHash (TxSignature sig sh) pub) ->
            payToWitnessScriptAddress rdm' == p2wshAddr h
                && addressHash (runPutS (serialize pub)) == kh
                && verifyHashSig (theTxSigHash so sh $ Just rdm') sig (pubKeyPoint pub)
        (PayWitnessScriptHash h, Just rdm'@(PayMulSig pubs r), SpendMulSig sigs) ->
            payToWitnessScriptAddress rdm' == p2wshAddr h
                && countMulSig' (\sh -> theTxSigHash so sh $ Just rdm') (pubKeyPoint <$> pubs) sigs == r
        _ -> False

    verifyNestedInput ::
        ScriptOutput -> ScriptOutput -> (Maybe RedeemScript, SimpleInput) -> Bool
    verifyNestedInput so so' x = case so of
        PayScriptHash h -> payToScriptAddress so' == p2shAddr h && verifySegwitInput so' x
        _ -> False

-- | Count the number of valid signatures for a multi-signature transaction.
countMulSig ::
    Network ->
    Tx ->
    Script ->
    Word64 ->
    Int ->
    [PubKey] ->
    [TxSignature] ->
    Int
countMulSig net tx out val i =
    countMulSig' h
  where
    h = txSigHash net tx out val i

countMulSig' :: (SigHash -> Hash256) -> [PubKey] -> [TxSignature] -> Int
countMulSig' _ [] _ = 0
countMulSig' _ _ [] = 0
countMulSig' h (_ : pubs) (TxSignatureEmpty : sigs) = countMulSig' h pubs sigs
countMulSig' h (pub : pubs) sigs@(TxSignature sig sh : sigs')
    | verifyHashSig (h sh) sig pub = 1 + countMulSig' h pubs sigs'
    | otherwise = countMulSig' h pubs sigs
