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

detSignTx :: Tx -> ScriptOutput -> Int -> PrvKey -> SigHash -> Maybe Tx
detSignTx tx out i prv sh = 
    tx{ txIn = updateIndex i (txIn tx)
    where pub = derivePubKey prv
          sig = detSignMsg (txSigHash tx out sh i) prv
          txi = dispatchSig out (TxSignature sig sh) pub

dispatchSig :: ScriptOutput -> TxSignature -> PubKey -> Maybe ScriptInput
dispatchSig out sig pub = case out of
    (PayPK p) -> guard (p == pub) >> return $ SpendPK sig
    (PayPKHash a) -> guard (a == pubKeyAddr pub) >> return $ SpendPKHash sig pub
    (PayMulSig1 p) -> guard (p == pub) >> return $ SpendMulSig1 sig
    (PayMulSig2 t p1 p2) -> case t of
        OneOfTwo -> guard (pub == p1 || pub == p2) >> return $ SpendMulSig1 sig
        TwoOfTwo -> f2 p1 p2
    (PayMulSig3 t p1 p2 p3) -> case t of
        OneOfThree -> do
            guard $ pub == p1 || pub == p2 || pub == p3
            return $ SpendMulSig1 sig
        TwoOfThree ->
    where f2 p1 p2 | pub == p1 = Just $ SpendMulSig2 sig Nothing
                   | pub == p2 = Just $ SpendMulSig2 Nothing sig
                   | otherwise = Nothing
        

combineSigs :: Script -> ScriptOutput -> TxSignature -> Maybe ScriptInput
combineSigs inp out sig = case out of
    (PayMulSig2 p1 p2) -> case inp of
        [OP_0, OP_PUSHDATA s1, OP_PUSHDATA s2] -> Just $ SpendMulSig s1 s2
        [OP_0, OP_PUSHDATA s1, OP0] -> Just $ SpendMulSig2 s1 sig
        [OP_0, OP_0, OP_0] -> SpendMulSig2 s1 s2


signPKHash :: MonadIO m => Tx -> ScriptOutput -> SigHash -> PrvKey -> Int 
           -> SecretT m Tx
signPKHash tx out sh prv i = do
    sig <- signMsg (txSigHash tx out sh i) prv
    return tx{ txIn = updateIndex i (txIn tx) (f $ TxSignature sig sh) }
    where f s ti = ti{ scriptInput = encodeInput $ SpendPKHash s pub }
          pub = derivePubKey prv
    
{- Build tx signature hashes -}

txSigHashes :: Tx -> [ScriptOutput] -> SigHash -> [Hash256]
txSigHashes tx os sh 
    | length os /= length (txIn tx) = error $
        "txSigHashes: Invalid [ScriptOutput] length: " ++ (show $ length os)
    | otherwise = [txSigHash tx o sh i | (o,i) <- zip os [0..]]

txSigHash :: Tx -> ScriptOutput -> SigHash -> Int -> Hash256
txSigHash tx@(Tx _ is os _) out sh i = 
    doubleHash256 $ encode' newTx `BS.append` encodeSigHash32 sh
    where newTx = tx { txIn  = buildInputs is out sh i
                     , txOut = buildOutputs os sh i
                     }

-- Builds transaction inputs for computing SigHashes
buildInputs :: [TxIn] -> ScriptOutput -> SigHash -> Int -> [TxIn]
buildInputs is out sh i 
    | i < 0 || i >= length is = error $ 
        "buildInputs: index out of range: " ++ (show i)
    | sh `elem` [SigAllAcp, SigNoneAcp, SigSingleAcp] =
            current{ scriptInput = encodeOutput out } : []
    | sh == SigAll                   = map f $ zip is [0..]
    | sh `elem` [SigNone, SigSingle] = map (f . g) $ zip is [0..]
    where current = is !! i
          f (ti,j) | i == j    = ti{ scriptInput = encodeOutput out }
                   | otherwise = ti{ scriptInput = Script [] }
          g (ti,j) | i == j    = (ti,j)
                   | otherwise = (ti{ txInSequence = 0 },j)

-- Build transaction outputs for computing SigHashes
buildOutputs :: [TxOut] -> SigHash -> Int -> [TxOut]
buildOutputs os sh i 
    | i < 0 || i >= length os = error $ 
        "buildOutputs: index out of range: " ++ (show i)
    | sh `elem` [SigAll, SigAllAcp]       = os
    | sh `elem` [SigNone, SigNoneAcp]     = []
    | sh `elem` [SigSingle, SigSingleAcp] = buffer ++ [os !! i]
    where buffer = replicate i $ TxOut (-1) $ Script []

