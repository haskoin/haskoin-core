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
buildTx xs ys = Tx <$> (Just 1)
                   <*> (Just $ map fi xs)
                   <*> (mapM fo ys)
                   <*> (Just 0)
    where fi outPoint = TxIn outPoint (Script []) maxBound
          fo (o,v) | v <= 2100000000000000 = Just $ TxOut v (encodeOutput o)
                   | otherwise             = Nothing

{- Sign a pubKeyHash tx -}

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

