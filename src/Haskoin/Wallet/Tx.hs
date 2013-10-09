module Haskoin.Wallet.Tx 
( buildPKHashTx
, buildScriptHashTx
, buildTx
, SigInput(..)
, signTx
) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans (MonadIO)

import Data.Maybe
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

{- Signed Monad -}

data Signed a = SigFull    { runSigned :: a } 
              | SigPartial { runSigned :: a }
              | SigError   { sigError :: String }
              deriving (Eq, Show)

instance Functor Signed where
    fmap f (SigFull x)    = SigFull (f x)
    fmap f (SigPartial x) = SigPartial (f x)
    fmap _ (SigError s)   = SigError s

instance Monad Signed where
    return = SigFull
    (SigFull x) >>= f = f x
    (SigPartial x) >>= f = case f x of
        e@(SigError _) -> e
        a              -> SigPartial $ runSigned a
    (SigError s) >>= _ = SigError s

isSigFull :: Signed a -> Bool
isSigFull (SigFull _) = True
isSigFull _           = False

isSigPartial :: Signed a -> Bool
isSigPartial (SigPartial _) = True
isSigPartial _              = False

isSigError :: Signed a -> Bool
isSigError (SigError s) = True
isSigError _            = False

{- Build a new Tx -}

-- Helper for pay to pubkey hash transactions
-- Returns Nothing if the address String is badly formatted
buildPKHashTx :: [OutPoint] -> [(String,Word64)] -> Signed Tx
buildPKHashTx xs ys = mapM f ys >>= buildTx xs
    where f (s,v) = liftM2 (,) (checkAddrType $ base58ToAddr s) (SigPartial v)
          checkAddrType (Just a@(PubKeyAddress _)) = SigPartial $ PayPKHash a
          checkAddrType (Just   (ScriptAddress _)) = SigError
              "buildPKHashTx: script hash address is invalid for pkhash"
          checkAddrType Nothing = SigError
              "buildPKHash: invalid address format"

-- Helper for pay to script hash transactions
-- Returns Nothing if the address String is badly formatted
buildScriptHashTx :: [OutPoint] -> [(String,Word64)] -> Signed Tx
buildScriptHashTx xs ys = mapM f ys >>= buildTx xs
    where f (s,v) = liftM2 (,) (checkAddrType $ base58ToAddr s) (SigPartial v)
          checkAddrType (Just a@(ScriptAddress _)) = 
               SigPartial $ PayScriptHash a
          checkAddrType (Just   (PubKeyAddress _)) = SigError
               "buildScriptHashTx: pkhash address is invalid for script hash"
          checkAddrType Nothing = SigError
              "buildPKHash: invalid address format"

buildTx :: [OutPoint] -> [(ScriptOutput,Word64)] -> Signed Tx
buildTx xs ys = mapM fo ys >>= \os -> SigPartial $ Tx 1 (map fi xs) os 0
    where fi outPoint = TxIn outPoint (Script []) maxBound
          fo (o,v) | v <= 2100000000000000 = fromMaybe 
                        (SigError "buildTx: Error encoding output")
                        (encodeOutput o >>= return . SigPartial . (TxOut v))
                   | otherwise = SigError $
                       "buildTx: Invalid amount: " ++ (show v)

{- Sign a pubKeyHash tx -}

type RedeemScript = ScriptOutput

-- Data used for building the siganture hash
data SigInput = SigInput
    { sigDataOut :: TxOut 
    , sigDataOP  :: OutPoint
    , sigDataSH  :: SigHash
    } deriving (Eq, Show)

type SigData = (Tx, ScriptOutput, Int, SigHash)

signTx :: MonadIO m => Tx -> [SigInput] -> [PrvKey] -> SecretT m (Signed Tx)
signTx tx si keys = do
    newIn <- mapM sign $ orderSigInput tx si
    return $ sequence newIn >>= \x -> return tx{ txIn = x }
    where sign (maybeSI,txin,i) = case maybeSI of
              -- User input match. We need to sign this input
              (Just (SigInput (TxOut _ s) _ sh)) -> case decodeOutput s of
                  (Just out) -> signTxIn txin (tx,out,i,sh) keys
                  Nothing    -> return $ SigError $
                      "signTx: Could not decode output for index " ++ (show i)
              -- No user input match. Check if input is already signed
              Nothing -> return $ toSignedTxIn txin i

signTxIn :: MonadIO m => TxIn -> SigData -> [PrvKey] -> SecretT m (Signed TxIn)
signTxIn txin@(TxIn _ s _) sd@(tx,out,i,sh) keys = do
    let (vKeys,pubs) = unzip $ sigKeys out keys
    (mapM getSig vKeys) >>= return . (buildTxInSig txin sd pubs)
    where getSig k = liftM (flip TxSignature sh) $ signMsg (txSigHash sd) k

{- Helpers for signing transactions -}

toSignedTxIn :: TxIn -> Int -> Signed TxIn
toSignedTxIn txin@(TxIn _ s _) i = case decodeInput s of
    Just (SpendPK _)        -> SigFull txin
    Just (SpendPKHash _ _)  -> SigFull txin
    Just (SpendMulSig xs r) -> 
        if length xs == r then SigFull txin else SigPartial txin
    Nothing -> SigError $ 
        "signedTxIn: Could not decode input for index " ++ (show i)

orderSigInput :: Tx -> [SigInput] -> [(Maybe SigInput, TxIn, Int)]
orderSigInput tx si = zip3 (matchOrder si (txIn tx) f) (txIn tx) [0..]
    where f (SigInput _ o1 _) (TxIn o2 _ _) = o1 == o2

buildTxInSig :: TxIn -> SigData -> [PubKey] -> [TxSignature] -> Signed TxIn
buildTxInSig txin@(TxIn _ s _) (tx,out,i,_) pubs sigs
    | null sigs = SigPartial $ txin{ scriptInput = Script [] }
    | otherwise = do
        res <- case out of
            (PayPK _) -> fromMaybe def $
                SigFull <$> (encodeInput $ SpendPK $ head sigs)
            (PayPKHash _) -> fromMaybe def $ 
                SigFull <$> (encodeInput $ SpendPKHash (head sigs) (head pubs))
            (PayMulSig pubs r) -> do
                let sigs' = sigs ++ case decodeInput s of 
                                        Just (SpendMulSig xs _) -> xs
                                        _                       -> []
                    cSigs = take r $ catMaybes $ matchOrder sigs' pubs g
                    tag   = if length cSigs == r then SigFull else SigPartial
                fromMaybe def $ tag <$> (encodeInput $ SpendMulSig cSigs r)
            _ -> SigError $ "sigInput: Can't sign a P2SH output here"
        return txin{ scriptInput = res }
    where def = SigPartial $ Script []
          g (TxSignature sig txsh) pub = 
              verifySig (txSigHash (tx,out,i,txsh)) sig pub

sigKeys :: ScriptOutput -> [PrvKey] -> [(PrvKey,PubKey)]
sigKeys out keys = case out of
    (PayPK p) -> maybeToList $ find ((== p) . snd) zipKeys
    (PayPKHash a) -> maybeToList $ find ((== a) . pubKeyAddr . snd) zipKeys
    (PayMulSig ps r) -> take r $ filter ((`elem` ps) . snd) zipKeys
    _ -> []
    where zipKeys = zip keys $ map derivePubKey keys

{- Build tx signature hashes -}

txSigHash :: SigData -> Hash256
txSigHash sd@(tx,out,i,sh) = 
    doubleHash256 $ encode' newTx `BS.append` encodeSigHash32 sh
    where newTx = tx {txIn = buildInputs sd, txOut = buildOutputs sd}

-- Builds transaction inputs for computing SigHashes
buildInputs :: SigData -> [TxIn]
buildInputs ((Tx _ txins _ _),out,i,sh)
    | i < 0 || i >= length txins = error $ 
        "buildInputs: index out of range: " ++ (show i)
    | sh `elem` [SigAllAcp, SigNoneAcp, SigSingleAcp] =
            (txins !! i) { scriptInput = outScript } : []
    | sh == SigAll                   = map f $ zip txins [0..]
    | sh `elem` [SigNone, SigSingle] = map (f . g) $ zip txins [0..]
    where outScript = fromMaybe (Script []) $ encodeOutput out
          f (ti,j) | i == j    = ti{ scriptInput = outScript }
                   | otherwise = ti{ scriptInput = Script [] }
          g (ti,j) | i == j    = (ti,j)
                   | otherwise = (ti{ txInSequence = 0 },j)

-- Build transaction outputs for computing SigHashes
buildOutputs :: SigData -> [TxOut]
buildOutputs ((Tx _ _ os _),_,i,sh)
    | i < 0 || i >= length os = error $ 
        "buildOutputs: index out of range: " ++ (show i)
    | sh `elem` [SigAll, SigAllAcp]       = os
    | sh `elem` [SigNone, SigNoneAcp]     = []
    | sh `elem` [SigSingle, SigSingleAcp] = buffer ++ [os !! i]
    where buffer = replicate i $ TxOut (-1) $ Script []

