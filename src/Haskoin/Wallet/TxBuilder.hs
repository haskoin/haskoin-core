module Haskoin.Wallet.TxBuilder 
( buildPKHashTx
, buildScriptHashTx
, buildTx
, SigInput(..)
, signTx
, detSignTx
) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans 

import Data.Maybe
import Data.List
import Data.Word
import qualified Data.ByteString as BS

import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

{- Build a new Tx -}

-- Helper for pay to pubkey hash transactions
-- Returns Nothing if the address String is badly formatted
buildPKHashTx :: [OutPoint] -> [(String,Word64)] -> Either String Tx
buildPKHashTx xs ys = mapM f ys >>= buildTx xs
    where f (s,v) = liftM2 (,) (checkAddrType $ base58ToAddr s) (return v)
          checkAddrType (Just a@(PubKeyAddress _)) = return $ PayPKHash a
          checkAddrType (Just   (ScriptAddress _)) = Left
              "buildPKHashTx: script hash address is invalid for pkhash"
          checkAddrType Nothing = Left "buildPKHash: invalid address format"

-- Helper for pay to script hash transactions
-- Returns Nothing if the address String is badly formatted
buildScriptHashTx :: [OutPoint] -> [(String,Word64)] -> Either String Tx
buildScriptHashTx xs ys = mapM f ys >>= buildTx xs
    where f (s,v) = liftM2 (,) (checkAddrType $ base58ToAddr s) (return v)
          checkAddrType (Just a@(ScriptAddress _)) = return $ PayScriptHash a
          checkAddrType (Just   (PubKeyAddress _)) = Left
               "buildScriptHashTx: pkhash address is invalid for script hash"
          checkAddrType Nothing = Left "buildPKHash: invalid address format"

buildTx :: [OutPoint] -> [(ScriptOutput,Word64)] -> Either String Tx
buildTx xs ys = mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os 0
    where fi outPoint = TxIn outPoint (Script []) maxBound
          fo (o,v) | v <= 2100000000000000 = fromMaybe 
                        (Left "buildTx: Error encoding output")
                        (return . (TxOut v) <$> encodeOutput o)
                   | otherwise = Left $ "buildTx: Invalid amount: " ++ (show v)

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

liftSecret :: MonadIO m => Build a -> SecretT (BuildT m) a
liftSecret = lift . liftBuild

signTx :: MonadIO m => Tx -> [SigInput] -> [PrvKey] -> SecretT (BuildT m) Tx
signTx tx sigis keys = do
    newIn <- mapM sign $ orderSigInput tx sigis
    return tx{ txIn = newIn }
    where sign (maybeSI,txin,i) = case maybeSI of
              Just sigi -> signTxIn txin sigi tx i keys
              _         -> liftSecret $ toBuildTxIn txin i

signTxIn :: MonadIO m => TxIn -> SigInput -> Tx -> Int -> [PrvKey] 
         -> SecretT (BuildT m) TxIn
signTxIn txin sigi tx i keys = do
    (out,vKeys,pubs,buildF) <- liftSecret $ decodeSigInput sigi keys
    let sd = (tx,out,i,sh)
    sigs <- mapM (((liftSecret $ txSigHash sd) >>=) . (flip signMsg)) vKeys
    liftSecret $ buildF txin sd pubs $ map (flip TxSignature sh) sigs
    where sh = sigDataSH sigi

detSignTx :: Tx -> [SigInput] -> [PrvKey] -> Build Tx
detSignTx tx sigis keys = do
    newIn <- mapM sign $ orderSigInput tx sigis
    return tx{ txIn = newIn }
    where sign (maybeSI,txin,i) = case maybeSI of
            Just sigi -> detSignTxIn txin sigi tx i keys
            _         -> toBuildTxIn txin i

detSignTxIn :: TxIn -> SigInput -> Tx -> Int -> [PrvKey] -> Build TxIn
detSignTxIn txin sigi tx i keys = do
    (out,vKeys,pubs,buildF) <- decodeSigInput sigi keys
    let sd   = (tx,out,i,sh)
    sigs <- mapM ((<$> txSigHash sd) . (flip detSignMsg)) vKeys
    buildF txin sd pubs $ map (flip TxSignature sh) sigs
    where sh = sigDataSH sigi

{- Helpers for signing transactions -}

toBuildTxIn :: TxIn -> Int -> Build TxIn
toBuildTxIn txin@(TxIn _ s _) i
    | null $ runScript s = Partial txin
    | otherwise = case decodeInput s of
        Just (SpendPK _)        -> Complete txin
        Just (SpendPKHash _ _)  -> Complete txin
        Just (SpendMulSig xs r) -> 
            if length xs == r then Complete txin else Partial txin
        Nothing -> Broken $ 
            "toBuildTxIn: Could not decode input for index " ++ (show i)

orderSigInput :: Tx -> [SigInput] -> [(Maybe SigInput, TxIn, Int)]
orderSigInput tx si = zip3 (matchTemplate si (txIn tx) f) (txIn tx) [0..]
    where f si txin = sigDataOP si == prevOutput txin

type BuildFunction = 
    (TxIn -> SigData -> [PubKey] -> [TxSignature] -> Build TxIn)

decodeSigInput :: SigInput -> [PrvKey] -> 
    Build (ScriptOutput, [PrvKey], [PubKey], BuildFunction)
decodeSigInput sigi keys = case sigi of
    SigInput (TxOut _ s) _ _ -> case decodeOutput s of
        Just out -> do
            (vKeys,pubs) <- sigKeys out keys
            return (out,vKeys,pubs,buildTxIn)
        _ -> Broken "decodeSigInput: Could not decode script output"
    SigInputSH (TxOut _ s) _ sr _ -> case decodeOutput s of
        Just out -> case decodeOutput sr of
            Just rdm -> do
                (vKeys,pubs) <- sigKeysSH out rdm keys
                return (rdm,vKeys,pubs,buildTxInSH)
            _ -> Broken "decodeSigInput: Could not decode redeem script"
        _ -> Broken "decodeSigInput: Could not decode script output"

buildTxInSH :: BuildFunction
buildTxInSH txin sd@(tx,rdm,i,_) pubs sigs = do
    s <- scriptInput <$> buildTxIn txin sd pubs sigs
    buildRes <$> case decodeInput s of
        Just si -> do
            let shi = ScriptHashInput si rdm
            fromMaybe emptySig $ Complete <$> (encodeScriptHash shi)
        _ -> emptySig
    where emptySig = Partial $ Script []
          buildRes res = txin{ scriptInput = res }

buildTxIn :: BuildFunction
buildTxIn txin (tx,out,i,_) pubs sigs
    | null sigs = buildRes <$> emptySig
    | otherwise = buildRes <$> case out of
        (PayPK _) -> fromMaybe emptySig $
            Complete <$> (encodeInput $ SpendPK $ head sigs)
        (PayPKHash _) -> fromMaybe emptySig $ 
            Complete <$> (encodeInput $ SpendPKHash (head sigs) (head pubs))
        (PayMulSig msPubs r) -> do
            let mSigs = take r $ catMaybes $ matchTemplate allSigs msPubs g
                tag   = if length mSigs == r then Complete else Partial
            fromMaybe emptySig $ tag <$> (encodeInput $ SpendMulSig mSigs r)
        _ -> Broken "buildTxIn: Can't sign a P2SH script here"
    where emptySig = Partial $ Script []
          buildRes res = txin{ scriptInput = res }
          g (TxSignature sig txsh) pub = 
              let msg = txSigHash (tx,out,i,txsh)
                  in isComplete msg && verifySig (runBuild msg) sig pub
          allSigs = sigs ++ case decodeInput $ scriptInput txin of
              Just (SpendMulSig xs _) -> xs
              _                       -> []

sigKeysSH :: ScriptOutput -> RedeemScript -> [PrvKey]
          -> Build ([PrvKey],[PubKey])
sigKeysSH out rdm keys = case out of
    (PayScriptHash a) -> if scriptAddr rdm == a
        then sigKeys out keys
        else Broken "sigKeys: Redeem script does not match P2SH script"
    _ -> Broken "sigKeys: Can only decode P2SH script here"

sigKeys :: ScriptOutput -> [PrvKey] -> Build ([PrvKey],[PubKey])
sigKeys out keys = unzip <$> case out of
    (PayPK p) -> return $ 
        maybeToList $ find ((== p) . snd) zipKeys
    (PayPKHash a) -> return $ 
        maybeToList $ find ((== a) . pubKeyAddr . snd) zipKeys
    (PayMulSig ps r) -> return $ 
        take r $ filter ((`elem` ps) . snd) zipKeys
    _  -> Broken "sigKeys: Can't decode P2SH here" 
    where zipKeys = zip keys $ map derivePubKey keys

{- Build tx signature hashes -}

txSigHash :: SigData -> Build Hash256
txSigHash sd@(tx,out,i,sh) = do
    (newIn,newOut) <- liftM2 (,) (buildInputs sd) (buildOutputs sd)
    let newTx = tx{ txIn = newIn, txOut = newOut }
    return $ doubleHash256 $ encode' newTx `BS.append` encodeSigHash32 sh

-- Builds transaction inputs for computing SigHashes
buildInputs :: SigData -> Build [TxIn]
buildInputs ((Tx _ txins _ _),out,i,sh)
    | sh `elem` [SigAllAcp, SigNoneAcp, SigSingleAcp] =
            return $ (txins !! i) { scriptInput = outScript } : []
    | sh == SigAll                   = return $ map f $ zip txins [0..]
    | sh `elem` [SigNone, SigSingle] = return $ map (f . g) $ zip txins [0..]
    where outScript = fromMaybe (Script []) $ encodeOutput out
          f (ti,j) | i == j    = ti{ scriptInput = outScript }
                   | otherwise = ti{ scriptInput = Script [] }
          g (ti,j) | i == j    = (ti,j)
                   | otherwise = (ti{ txInSequence = 0 },j)

-- Build transaction outputs for computing SigHashes
buildOutputs :: SigData -> Build [TxOut]
buildOutputs ((Tx _ _ os _),_,i,sh)
    | sh `elem` [SigAll, SigAllAcp]       = return os
    | sh `elem` [SigNone, SigNoneAcp]     = return []
    | sh `elem` [SigSingle, SigSingleAcp] = 
        if i < 0 || i >= length os
            then Broken $ "buildOutputs: index out of range: " ++ (show i)
            else return $ buffer ++ [os !! i]
    where buffer = replicate i $ TxOut (-1) $ Script []

