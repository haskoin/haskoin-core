module Haskoin.Wallet.Tx where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans (MonadIO)

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Wallet.ScriptParser
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

{- Build a new Tx -}

-- Helper for pay to pubkey hash transactions
-- Returns Nothing if the address String is badly formatted
buildPKHashTx :: [OutPoint] -> [(String,Word64)] -> Maybe Tx
buildPKHashTx xs ys = mapM f ys >>= buildTx xs
    where f (s,v) = liftM2 (,) (base58ToAddr s >>= checkAddrType) (Just v)
          checkAddrType a@(PubKeyAddress _) = Just $ PayPKHash a
          checkAddrType   (ScriptAddress _) = Nothing

-- Helper for pay to script hash transactions
-- Returns Nothing if the address String is badly formatted
buildScriptHashTx :: [OutPoint] -> [(String,Word64)] -> Maybe Tx
buildScriptHashTx xs ys = mapM f ys >>= buildTx xs
    where f (s,v) = liftM2 (,) (base58ToAddr s >>= checkAddrType) (Just v)
          checkAddrType a@(ScriptAddress _) = Just $ PayScriptHash a
          checkAddrType   (PubKeyAddress _) = Nothing

buildTx :: [OutPoint] -> [(ScriptOutput,Word64)] -> Maybe Tx
buildTx xs ys = mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os 0
    where fi outPoint = TxIn outPoint (Script []) maxBound
          fo (o,v) | v <= 2100000000000000 = Just $ TxOut v (encodeOutput o)
                   | otherwise             = Nothing

{- Sign a pubKeyHash tx -}

type RedeemScript = ScriptOutput

-- Data used for building the siganture hash
newtype SigData = SigData Tx ScriptOutput i SigHash
    deriving (Eq, Show)

signTx :: MonadIO m => SigData -> PrvKey -> SecretT m (Maybe Tx)
signTx sd@(SigData tx _ i sh) prv = do
    sig <- liftM2 TxSignature (signMsg (txSigHash sd) prv) (return sh)
    return $ do
        s <- encodeInput =<< buildSigInput sd sig (derivePubKey prv)
        return $ updateTxInput tx i s

detSignTx :: SigData -> PrvKey -> Maybe Tx
detSignTx sd@(SigData tx _ i sh) prv = do
    s <- encodeInput =<< buildSigInput sd sig (derivePubKey prv)
    return $ updateTxInput tx i s
    where sig = TxSignature (detSignMsg (txSigHash sd) prv) sh

signScriptHash :: MonadIO m => SigData -> PrvKey -> RedeemScript 
               -> SecretT m (Maybe Tx)
signScriptHash sd@(SigData tx _ i sh) prv rdm = 
    sig <- liftM2 TxSignature (signMsg (txSigHash sd') prv) (return sh)
    return $ buildScriptHashTx sd sig pub rdm
    where sd' = SigData tx rdm i sh -- build new SigData with redeem script
          pub = derivePubKey prv

detSignScriptHash :: SigData -> PrvKey -> RedeemScript -> Maybe Tx
detSignScriptHash sd@(SigData tx _ i sh) prv rdm = 
    buildScriptHashTx sd sig pub rdm
    where sd' = SigData tx rdm i sh -- build new SigData with redeem script
          sig = TxSignature (detSignMsg (txSigHash sd') prv) sh
          pub = derivePubKey prv

{- Helpers for signing transactions -}

updateTxInput :: Tx -> Int -> Script -> Tx
updateTxInput tx i s = tx{ txIn = updateIndex i (txIn tx) f }
    where f txin = txin{ scriptInput = s }

buildScriptHashTx :: SigData -> TxSignature -> PubKey -> RedeemScript
                  -> Maybe Tx
buildScriptHashTx (SigData tx out i sh) sig pub rdm = case out of
    (PayScriptHash a) -> do
        guard $ scriptAddr rdm == a 
        shi <- liftM2 ScriptHashInput (buildSigInput sd' sig pub) (Just rdm)
        (updateTxInput tx i) <$> encodeScriptHash shi
    _ -> Nothing
    where sd' = SigData tx rdm i sh 
        
buildSigInput :: SigData -> TxSignature -> PubKey -> Maybe ScriptInput
buildSigInput sd@(SigData tx out i _) sig pub = case out of
    (PayPK p) -> 
        guard $ p == pub
        return $ SpendPK sig
    (PayPKHash a) -> do
        guard $ a == pubKeyAddr pub
        return $ SpendPKHash sig pub
    (PayMulSig pubs r) -> do
        guard $ pub `elem` pubs
        let sigs = sig : case txIn tx !! i of (SpendMulSig xs _) -> xs
                                              _                  -> []
            res  = combineSigs sd sigs pubs
        guard $ length res > 0
        return $ SpendMulSig (take r res) r
    _ -> Nothing -- Don't sign PayScriptHash here

-- Signatures need to be sorted with respect to the public keys in a script
combineSigs :: SigData -> [TxSignature] -> [PubKey] -> [TxSignature]
combineSigs _ [] _  = []
combineSigs _ _  [] = []
combineSigs sd sigs (pub:ps) = case break f sigs of
    (l,(r:rs)) -> r : combineSigs sd (l ++ rs) ps
    _          -> combineSigs sd sigs ps
    where f sig = verifySig (txSigHash sd) sig pub

{- Build tx signature hashes -}

txSigHashes :: Tx -> [ScriptOutput] -> SigHash -> [Hash256]
txSigHashes tx os sh 
    | length os /= length (txIn tx) = error $
        "txSigHashes: Invalid [ScriptOutput] length: " ++ (show $ length os)
    | otherwise = [txSigHash tx o sh i | (o,i) <- zip os [0..]]

txSigHash :: SigData -> Hash256
txSigHash sd@(SigData tx out i sh) = 
    doubleHash256 $ encode' newTx `BS.append` encodeSigHash32 sh
    where newTx = tx {txIn = buildInputs sd, txOut = buildOutputs sd}

-- Builds transaction inputs for computing SigHashes
buildInputs :: SigData -> [TxIn]
buildInputs (SigData (Tx _ txins _ _) out i sh)
    | i < 0 || i >= length txins = error $ 
        "buildInputs: index out of range: " ++ (show i)
    | sh `elem` [SigAllAcp, SigNoneAcp, SigSingleAcp] =
            (txins !! i) { scriptInput = encodeOutput out } : []
    | sh == SigAll                   = map f $ zip txins [0..]
    | sh `elem` [SigNone, SigSingle] = map (f . g) $ zip txins [0..]
    where f (ti,j) | i == j    = ti{ scriptInput = encodeOutput out }
                   | otherwise = ti{ scriptInput = Script [] }
          g (ti,j) | i == j    = (ti,j)
                   | otherwise = (ti{ txInSequence = 0 },j)

-- Build transaction outputs for computing SigHashes
buildOutputs :: SigData -> [TxOut]
buildOutputs (SigData (Tx _ _ os _) _ i sh)
    | i < 0 || i >= length os = error $ 
        "buildOutputs: index out of range: " ++ (show i)
    | sh `elem` [SigAll, SigAllAcp]       = os
    | sh `elem` [SigNone, SigNoneAcp]     = []
    | sh `elem` [SigSingle, SigSingleAcp] = buffer ++ [os !! i]
    where buffer = replicate i $ TxOut (-1) $ Script []

