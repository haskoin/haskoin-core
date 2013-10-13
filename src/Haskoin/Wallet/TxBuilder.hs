module Haskoin.Wallet.TxBuilder 
( buildPKHashTx
, buildScriptHashTx
, buildTx
, SigInput(..)
, signTx
, detSignTx
, verifyTx
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


liftSecret :: MonadIO m => Build a -> SecretT (BuildT m) a
liftSecret = lift . liftBuild

signTx :: MonadIO m => Tx -> [SigInput] -> [PrvKey] -> SecretT (BuildT m) Tx
signTx tx sigis keys 
    | null $ txIn tx = liftSecret $ Broken "signTx: Transaction has no inputs"
    | otherwise = do
        newIn <- mapM sign $ orderSigInput tx sigis
        return tx{ txIn = newIn }
    where sign (maybeSI,txin,i) = case maybeSI of
              Just sigi -> signTxIn txin sigi tx i keys
              _         -> liftSecret $ toBuildTxIn txin i

signTxIn :: MonadIO m => TxIn -> SigInput -> Tx -> Int -> [PrvKey] 
         -> SecretT (BuildT m) TxIn
signTxIn txin sigi tx i keys = do
    (out,vKeys,pubs,buildf) <- liftSecret $ decodeSigInput sigi keys
    let sigh = txSigHash tx (fromJust $ encodeOutput out) i sh
        f k  = either (liftSecret . Broken) (flip signMsg k) sigh
    sigs <- mapM f vKeys
    liftSecret $ buildf txin tx out i pubs $ map (flip TxSignature sh) sigs
    where sh = sigDataSH sigi

detSignTx :: Tx -> [SigInput] -> [PrvKey] -> Build Tx
detSignTx tx sigis keys 
    | null $ txIn tx = Broken "detSignTx: Transaction has no inputs"
    | otherwise = do
        newIn <- mapM sign $ orderSigInput tx sigis
        return tx{ txIn = newIn }
    where sign (maybeSI,txin,i) = case maybeSI of
            Just sigi -> detSignTxIn txin sigi tx i keys
            _         -> toBuildTxIn txin i

detSignTxIn :: TxIn -> SigInput -> Tx -> Int -> [PrvKey] -> Build TxIn
detSignTxIn txin sigi tx i keys = do
    (out,vKeys,pubs,buildf) <- decodeSigInput sigi keys
    let sigh = txSigHash tx (fromJust $ encodeOutput out) i sh
        f k  = either Broken (return . (flip detSignMsg k)) sigh
    sigs <- mapM f vKeys
    buildf txin tx out i pubs $ map (flip TxSignature sh) sigs
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

type BuildFunction =  TxIn -> Tx -> ScriptOutput -> Int 
                   -> [PubKey] -> [TxSignature] -> Build TxIn

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
buildTxInSH txin tx rdm i pubs sigs = do
    s <- scriptInput <$> buildTxIn txin tx rdm i pubs sigs
    buildRes <$> case decodeInput s of
        Just si -> do
            let shi = ScriptHashInput si rdm
            fromMaybe emptySig $ Complete <$> (encodeScriptHash shi)
        _ -> emptySig
    where emptySig = Partial $ Script []
          buildRes res = txin{ scriptInput = res }

buildTxIn :: BuildFunction
buildTxIn txin tx out i pubs sigs
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
          so = fromJust $ encodeOutput out
          g (TxSignature sig txsh) pub = case txSigHash tx so i txsh of
              (Right msg) -> verifySig msg sig pub
              _ -> False
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

{- Tx verification -}

verifyTx :: Tx -> [(Script,OutPoint)] -> Bool
verifyTx tx xs = all v $ zip3 m (txIn tx) [0..]
    where m = map (fst <$>) $ matchTemplate xs (txIn tx) f
          f (s,o) txin = o == prevOutput txin
          v (maybeS,txin,i) = fromMaybe False $ do
              s         <- maybeS
              (out,inp) <- decodeVerifySigInput s txin
              let so = fromJust $ encodeOutput out
              case (out,inp) of
                  (PayPK pub,SpendPK (TxSignature sig sh)) -> do
                      msg <- eitherToMaybe $ txSigHash tx so i sh
                      return $ verifySig msg sig pub
                  (PayPKHash a,SpendPKHash (TxSignature sig sh) pub) -> do
                      guard $ pubKeyAddr pub == a
                      msg <- eitherToMaybe $ txSigHash tx so i sh
                      return $ verifySig msg sig pub
                  (PayMulSig pubs r,SpendMulSig sigs _) ->
                      (== r) <$> countMulSig tx so i pubs sigs 

-- Count the number of valid signatures
countMulSig :: Tx -> Script -> Int -> [PubKey] -> [TxSignature] -> Maybe Int
countMulSig tx so i [] sigs = Just 0
countMulSig tx so i pubs [] = Just 0
countMulSig tx so i (pub:pubs) sigs@(TxSignature sig sh:rest) = do
    msg <- eitherToMaybe $ txSigHash tx so i sh
    if verifySig msg sig pub then (+1) <$> countMulSig tx so i pubs rest
                             else          countMulSig tx so i pubs sigs
                  
decodeVerifySigInput :: Script -> TxIn -> Maybe (ScriptOutput, ScriptInput)
decodeVerifySigInput so (TxIn _ si _ ) = case decodeOutput so of
    Just (PayScriptHash a) -> do
        (ScriptHashInput inp rdm) <- decodeScriptHash si
        guard $ scriptAddr rdm == a
        return (rdm,inp)
    out -> liftM2 (,) out $ decodeInput si

