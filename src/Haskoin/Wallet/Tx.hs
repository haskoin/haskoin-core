module Haskoin.Wallet.Tx where

import Control.Monad.Trans (MonadIO)

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

data SigHash = SigAll    
             | SigNone   
             | SigSingle 

             -- Anyone Can Pay
             | SigAllAcp
             | SigNoneAcp
             | SigSingleAcp 
             deriving (Eq, Show)

instance Binary SigHash where

    get = do
        w <- getWord32be
        case w of 0x01 -> return SigAll
                  0x02 -> return SigNone
                  0x03 -> return SigSingle
                  0x81 -> return SigAllAcp
                  0x82 -> return SigNoneAcp
                  0x83 -> return SigSingleAcp
                  _    -> fail $ "Invalid SigHash: " ++ (show w)

    put sh = putWord32be $ case sh of
        SigAll       -> 0x01
        SigNone      -> 0x02
        SigSingle    -> 0x03
        SigAllAcp    -> 0x81
        SigNoneAcp   -> 0x82
        SigSingleAcp -> 0x83

{- Build a new Tx -}

buildTx :: [OutPoint] -> [(Address,Word64)] -> Tx
buildTx xs ys = Tx 1 is os 0
     where is = map fi xs
           fi outPoint = TxIn outPoint (ScriptInput []) maxBound
           os = map fo ys
           fo (a,v) = TxOut v (PayPubKeyHash a)

{- Build tx signature hashes -}

txSigHashes :: Tx -> [ScriptOutput] -> SigHash -> [Hash256]
txSigHashes tx os sh 
    | length os /= length (txIn tx) = error $
        "txSigHashes: Invalid [ScriptOutput] length: " ++ (show $ length os)
    | otherwise = map (\(o,i) -> txSigHash tx o sh i) $ zip os [0..]

txSigHash :: Tx -> ScriptOutput -> SigHash -> Int -> Hash256
txSigHash tx@(Tx _ is os _) out sh i = doubleHash256 $ encode' newTx
    where newTx = tx { txIn  = buildInputs is out sh i
                     , txOut = buildOutputs os sh i
                     }

-- Builds transaction inputs for computing SigHashes
buildInputs :: [TxIn] -> ScriptOutput -> SigHash -> Int -> [TxIn]
buildInputs is out sh i 
    | i < 0 || i >= length is = error $ 
        "buildInputs: index out of range: " ++ (show i)
    | sh `elem` [SigAllAcp, SigNoneAcp, SigSingleAcp] =
            [current{ scriptInput = toScriptInput out }]
    | sh == SigAll                   = map f $ zip is [0..]
    | sh `elem` [SigNone, SigSingle] = map (f . g) $ zip is [0..]
    where current = is !! i
          f (ti,j) | i == j    = ti{ scriptInput = toScriptInput out }
                   | otherwise = ti{ scriptInput = ScriptInput [] }
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
    where buffer = replicate i $ TxOut (-1) $ PayNonStd []

{- Sign a pubKeyHash tx -}

signPKHash :: MonadIO m => Tx -> ScriptOutput -> SigHash -> PrvKey -> Int 
           -> SecretT m Tx
signPKHash tx out sh prv i = do
    sig <- signMsg (txSigHash tx out sh i) prv
    return $ tx{ txIn = updateIndex i (txIn tx) (f sig) }
    where f sig ti = ti{ scriptInput = spendPKHash sig $ derivePubKey prv }
    
