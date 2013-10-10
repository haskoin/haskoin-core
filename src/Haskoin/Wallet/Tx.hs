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
              | SigError   { sigError  :: String }
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
buildPKHashTx :: [OutPoint] -> [(String,Word64)] -> Either String Tx
buildPKHashTx xs ys = mapM f ys >>= buildTx xs
    where f (s,v) = liftM2 (,) (checkAddrType $ base58ToAddr s) (Right v)
          checkAddrType (Just a@(PubKeyAddress _)) = Right $ PayPKHash a
          checkAddrType (Just   (ScriptAddress _)) = Left
              "buildPKHashTx: script hash address is invalid for pkhash"
          checkAddrType Nothing = Left
              "buildPKHash: invalid address format"

-- Helper for pay to script hash transactions
-- Returns Nothing if the address String is badly formatted
buildScriptHashTx :: [OutPoint] -> [(String,Word64)] -> Either String Tx
buildScriptHashTx xs ys = mapM f ys >>= buildTx xs
    where f (s,v) = liftM2 (,) (checkAddrType $ base58ToAddr s) (Right v)
          checkAddrType (Just a@(ScriptAddress _)) = Right $ PayScriptHash a
          checkAddrType (Just   (PubKeyAddress _)) = Left
               "buildScriptHashTx: pkhash address is invalid for script hash"
          checkAddrType Nothing = Left
              "buildPKHash: invalid address format"

buildTx :: [OutPoint] -> [(ScriptOutput,Word64)] -> Either String Tx
buildTx xs ys = mapM fo ys >>= \os -> Right $ Tx 1 (map fi xs) os 0
    where fi outPoint = TxIn outPoint (Script []) maxBound
          fo (o,v) | v <= 2100000000000000 = fromMaybe 
                        (Left "buildTx: Error encoding output")
                        (encodeOutput o >>= return . Right . (TxOut v))
                   | otherwise = Left $
                       "buildTx: Invalid amount: " ++ (show v)

{- Sign a pubKeyHash tx -}

-- Data used for building the siganture hash
data SigInput = SigInput   { sigDataOut :: TxOut 
                           , sigDataOP  :: OutPoint
                           , sigDataSH  :: SigHash
                           } 
              | SigInputSH { sigDataOut :: TxOut
                           , sigDataOP  :: OutPoint
                           , sigRedeem  :: Script
                           , sigDataSH  :: SigHash
                           }
              deriving (Eq, Show)

type SigData = (Tx, ScriptOutput, Int, SigHash)

signTx :: MonadIO m => Tx -> [SigInput] -> [PrvKey] -> SecretT m (Signed Tx)
signTx tx sigis keys = do
    newIn <- mapM sign $ orderSigInput tx sigis
    return $ sequence newIn >>= \x -> return tx{ txIn = x }
    where sign (maybeSI,txin,i) = case maybeSI of
              Just sigi -> signTxIn txin sigi tx i keys
              _         -> return $ toSignedTxIn txin i

signTxIn :: MonadIO m => TxIn -> SigInput -> Tx -> Int -> [PrvKey] 
         -> SecretT m (Signed TxIn)
signTxIn txin sigi tx i keys = case decodeSigInput sigi keys of
    Right (out,vKeys,pubs,buildF) -> do
        let sd = (tx,out,i,sh)
        sigs <- mapM (signMsg $ txSigHash sd) vKeys
        return $ buildF txin sd pubs $ map (flip TxSignature sh) sigs
    Left err -> return $ SigError err
    where sh = sigDataSH sigi

detSignTx :: Tx -> [SigInput] -> [PrvKey] -> Signed Tx
detSignTx tx sigis keys = do
    newIn <- mapM sign $ orderSigInput tx sigis
    return tx{ txIn = newIn }
    where sign (maybeSI,txin,i) = case maybeSI of
            Just sigi -> detSignTxIn txin sigi tx i keys
            _         -> toSignedTxIn txin i

detSignTxIn :: TxIn -> SigInput -> Tx -> Int -> [PrvKey] -> Signed TxIn
detSignTxIn txin sigi tx i keys = case decodeSigInput sigi keys of
    Right (out,vKeys,pubs,buildF) -> do
        let sd   = (tx,out,i,sh)
            sigs = map (detSignMsg (txSigHash sd)) vKeys
        buildF txin sd pubs $ map (flip TxSignature sh) sigs
    Left err -> SigError err
    where sh = sigDataSH sigi

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
    where f si txin = sigDataOP si == prevOutput txin

type BuildFunction = 
    (TxIn -> SigData -> [PubKey] -> [TxSignature] -> Signed TxIn)

decodeSigInput :: SigInput -> [PrvKey] -> 
    Either String (ScriptOutput, [PrvKey], [PubKey], BuildFunction)
decodeSigInput sigi keys = case sigi of
    SigInput (TxOut _ s) _ _ -> case decodeOutput s of
        Just out -> do
            (vKeys,pubs) <- sigKeys out keys
            return (out,vKeys,pubs,buildTxIn)
        _ -> Left "decodeSigInput: Could not decode script output"
    SigInputSH (TxOut _ s) _ sr _ -> case decodeOutput s of
        Just out -> case decodeOutput sr of
            Just rdm -> do
                (vKeys,pubs) <- sigKeysSH out rdm keys
                return (rdm,vKeys,pubs,buildTxInSH)
            _ -> Left "decodeSigInput: Could not decode redeem script"
        _ -> Left "decodeSigInput: Could not decode script output"

buildTxInSH :: BuildFunction
buildTxInSH txin sd@(tx,rdm,i,_) pubs sigs = do
    s <- scriptInput <$> buildTxIn txin sd pubs sigs
    buildRes <$> case decodeInput s of
        Just si -> do
            let shi = ScriptHashInput si rdm
            fromMaybe emptySig $ SigFull <$> (encodeScriptHash shi)
        _ -> emptySig
    where emptySig = SigPartial $ Script []
          buildRes res = txin{ scriptInput = res }

buildTxIn :: BuildFunction
buildTxIn txin (tx,out,i,_) pubs sigs
    | null sigs = buildRes <$> emptySig
    | otherwise = buildRes <$> case out of
        (PayPK _) -> fromMaybe emptySig $
            SigFull <$> (encodeInput $ SpendPK $ head sigs)
        (PayPKHash _) -> fromMaybe emptySig $ 
            SigFull <$> (encodeInput $ SpendPKHash (head sigs) (head pubs))
        (PayMulSig msPubs r) -> do
            let mSigs = take r $ catMaybes $ matchOrder allSigs msPubs g
                tag   = if length mSigs == r then SigFull else SigPartial
            fromMaybe emptySig $ tag <$> (encodeInput $ SpendMulSig mSigs r)
        _ -> SigError "buildTxIn: Can't sign a P2SH script here"
    where emptySig = SigPartial $ Script []
          buildRes res = txin{ scriptInput = res }
          g (TxSignature sig txsh) pub = 
              verifySig (txSigHash (tx,out,i,txsh)) sig pub
          allSigs = sigs ++ case decodeInput $ scriptInput txin of
              Just (SpendMulSig xs _) -> xs
              _                       -> []

sigKeysSH :: ScriptOutput -> RedeemScript -> [PrvKey]
          -> Either String ([PrvKey],[PubKey])
sigKeysSH out rdm keys = case out of
    (PayScriptHash a) -> if scriptAddr rdm == a
        then sigKeys out keys
        else Left "sigKeys: Redeem script does not match P2SH script"
    _ -> Left "sigKeys: Can only decode P2SH script here"

sigKeys :: ScriptOutput -> [PrvKey] -> Either String ([PrvKey],[PubKey])
sigKeys out keys = unzip <$> case out of
    (PayPK p) -> Right $ 
        maybeToList $ find ((== p) . snd) zipKeys
    (PayPKHash a) -> Right $ 
        maybeToList $ find ((== a) . pubKeyAddr . snd) zipKeys
    (PayMulSig ps r) -> Right $ 
        take r $ filter ((`elem` ps) . snd) zipKeys
    _  -> Left "sigKeys: Can't decode P2SH here" 
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

