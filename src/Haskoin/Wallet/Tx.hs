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

detSignTx :: SigData -> PrvKey -> Maybe Tx
detSignTx sd@(SigData tx _ i _) prv = do
    s <- encodeInput =<< detSignInput sd prv
    return tx{ txIn = updateIndex i (txIn tx) (const s) }

detSignSHTx :: SigData -> PrvKey -> RedeemScript -> Maybe Tx
detSignSHTx (SigData tx out i sh) prv rdm = case out of
    (PayScriptHash a) -> do
        guard $ scriptAddr rdm == a 
        -- build new SigData with the redeem script
        inp <- detSignInput (SigData tx rdm i sh) prv
        s   <- encodeScriptHash $ ScriptHashInput inp rdm
        return tx{ txIn = updateIndex i (txIn tx) (const s) }
    _ -> Nothing
    
detSignInput :: SigData -> PrvKey -> Maybe ScriptInput
detSignInput sd@(SigData tx out i sh) prv = case out of
    (PayPK p) -> 
        guard $ p == pub
        return $ SpendPK sig
    (PayPKHash a) -> do
        guard $ a == pubKeyAddr pub
        return $ SpendPKHash sig pub
    (PayMulSig pubs r) -> do
        let sigs = sig : case txIn tx !! i of (SpendMulSig xs _) -> xs
                                              _                  -> []
            res  = combineSigs sd sigs pubs
        guard (length res > 0) >> return $ SpendMulSig (take r res) r
    _ -> Nothing -- Don't sign PayScriptHash here
    where pub  = derivePubKey prv
          sig  = TxSignature (detSignMsg (txSigHash sd) prv) sh

combineSigs :: SigData -> [TxSignature] -> [PubKey] -> [TxSignature]
combineSigs _ _ _ [] _  = []
combineSigs _ _ _ _  [] = []
combineSigs (SigData tx out i _) sigs (p:ps) = case break f sigs of
    (l,(r:rs)) -> r : combineSigs tx out i (l ++ rs) ps
    _          -> combineSigs tx out i sigs ps
    where f s = verifyTxSig tx out i s p

verifyTxSig :: Tx -> ScriptOutput -> Int -> TxSignature -> PubKey -> Bool
verifyTxSig tx out i (TxSignature sig sh) pub = verifySig h sig pub
    where h = txSigHash tx out sh i

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

