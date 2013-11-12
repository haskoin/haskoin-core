module Haskoin.Wallet.TxBuilder 
( buildTx
, buildAddrTx
, SigInput(..)
, signTx
, detSignTx
, verifyTx
, guessTxSize
, chooseCoins
, chooseMSCoins
, getFee
, getMSFee
) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans 

import Data.Maybe
import Data.List
import Data.Word
import qualified Data.ByteString as BS

import Haskoin.Wallet.Store.DBCoin
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

-- |Select coins to spend based on a target amount and a fee per KB
chooseCoins :: Word64 -> Word64 -> [DBCoin] -> Either String ([DBCoin],Word64)
chooseCoins target kbfee xs 
    | target > 0 = maybeToEither err $ greedyAdd target (getFee kbfee) xs
    | otherwise  = Left "chooseCoins: Target must be > 0"
    where err = "chooseCoins: No solution found"

-- |Select coins to spend from a multisignature account
chooseMSCoins :: Word64 -> Word64 -> (Int,Int) -> [DBCoin] 
              -> Either String ([DBCoin],Word64)
chooseMSCoins target kbfee ms xs 
    | target > 0 = maybeToEither err $ greedyAdd target (getMSFee kbfee ms) xs
    | otherwise  = Left "chooseMSCoins: Target must be > 0"
    where err = "chooseMSCoins: No solution found"

-- |Select coins greedily by starting from an empty solution
greedyAdd :: Word64 -> (Int -> Word64) -> [DBCoin] -> Maybe ([DBCoin],Word64)
greedyAdd target fee xs = go [] 0 [] 0 $ sortBy desc xs
    where desc a b = compare (outValue $ coinTxOut b) (outValue $ coinTxOut a)
          goal c = target + fee c
          go _ _ [] _ []    = Nothing
          go _ _ ps pTot [] = return (ps,pTot - (goal $ length ps))
          go acc aTot ps pTot (y:ys)
            | val + aTot >= (goal $ length acc + 1) =
                if aTot + val - target < pTot - target
                    then go [] 0 (y:acc) (aTot + val) ys
                    else return (ps,pTot - (goal $ length ps))
            | otherwise = go (y:acc) (aTot + val) ps pTot ys
            where val = outValue $ coinTxOut y

-- |Start from a solution containing all coins and greedily remove them
greedyRem :: Word64 -> (Int -> Word64) -> [DBCoin] -> Maybe ([DBCoin],Word64)
greedyRem target fee xs 
    | s < goal (length xs) = Nothing
    | otherwise = return $ go [] s $ sortBy desc xs
    where desc a b = compare (outValue $ coinTxOut b) (outValue $ coinTxOut a)
          s        = sum $ map (outValue . coinTxOut) xs
          goal   c = target + fee c
          go acc tot [] = (acc,tot - (goal $ length acc))
          go acc tot (y:ys) 
            | tot - val >= (goal $ length ys + length acc) = 
                go acc (tot - val) ys
            | otherwise = go (y:acc) tot ys
            where val = outValue $ coinTxOut y

getFee :: Word64 -> Int -> Word64
getFee kbfee count = kbfee*((len + 999) `div` 1000)
    where len = fromIntegral $ guessTxSize count [] 2 0

getMSFee :: Word64 -> (Int,Int) -> Int -> Word64
getMSFee kbfee ms count = kbfee*((len + 999) `div` 1000)
    where len = fromIntegral $ guessTxSize 0 (replicate count ms) 2 0

guessTxSize :: Int -> [(Int,Int)] -> Int -> Int -> Int
guessTxSize pki msi pkout msout = 8 + inpLen + inp + outLen + out
    where inpLen = BS.length $ encode' $ 
            VarInt $ fromIntegral $ (length msi) + pki
          outLen = BS.length $ encode' $ 
            VarInt $ fromIntegral $ pkout + msout
          inp    = pki*148 + (sum $ map guessMSSize msi)
                   -- (20: hash160) + (5: opcodes) + 
                   -- (1: script len) + (8: Word64)
          out    = pkout*34 + 
                   -- (20: hash160) + (3: opcodes) + 
                   -- (1: script len) + (8: Word64)
                   msout*32

-- Size of a multisig pay2sh input
guessMSSize :: (Int,Int) -> Int
          -- OutPoint (36) + Sequence (4) + Script
guessMSSize (m,n) = 40 + (BS.length $ encode' $ VarInt $ fromIntegral scp) + scp
          -- OP_M + n*PubKey + OP_N + OP_CHECKMULTISIG
    where rdm = BS.length $ encode' $ OP_PUSHDATA $ BS.replicate (n*34 + 3) 0
          -- Redeem + m*sig + OP_0
          scp = rdm + m*73 + 1 

{- Build a new Tx -}

-- Helper for paying to a base58 encoded address
buildAddrTx :: [OutPoint] -> [(String,Word64)] -> Either String Tx
buildAddrTx xs ys = buildTx xs =<< mapM f ys
    where f (s,v) = case base58ToAddr s of
            Just a@(PubKeyAddress _) -> return (PayPKHash a,v)
            Just a@(ScriptAddress _) -> return (PayScriptHash a,v)
            _ -> Left $ "buildAddrTx: Invalid address " ++ s

buildTx :: [OutPoint] -> [(ScriptOutput,Word64)] -> Either String Tx
buildTx xs ys = mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os 0
    where fi outPoint = TxIn outPoint (Script []) maxBound
          fo (o,v) | v <= 2100000000000000 = return $ TxOut v $ encodeOutput o
                   | otherwise = Left $ "buildTx: Invalid amount " ++ (show v)

-- Data used for building the siganture hash
data SigInput = SigInput   { sigDataOut :: Script 
                           , sigDataOP  :: OutPoint
                           , sigDataSH  :: SigHash
                           } 
              | SigInputSH { sigDataOut :: Script
                           , sigDataOP  :: OutPoint
                           , sigRedeem  :: Script
                           , sigDataSH  :: SigHash
                           } deriving (Eq, Show)

liftSecret :: Monad m => Build a -> SecretT (BuildT m) a
liftSecret = lift . liftBuild

signTx :: Monad m => Tx -> [SigInput] -> [PrvKey] -> SecretT (BuildT m) Tx
signTx tx@(Tx _ ti _ _) sigis keys = do
    liftSecret $ when (null ti) $ Broken "signTx: Transaction has no inputs"
    newIn <- mapM sign $ orderSigInput ti sigis
    return tx{ txIn = newIn }
    where sign (maybeSI,txin,i) = case maybeSI of
              Just sigi -> signTxIn txin sigi tx i keys
              _         -> liftSecret $ toBuildTxIn txin

signTxIn :: Monad m => TxIn -> SigInput -> Tx -> Int -> [PrvKey] 
         -> SecretT (BuildT m) TxIn
signTxIn txin sigi tx i keys = do
    (out,vKeys,pubs,buildf) <- liftSecret $ decodeSigInput sigi keys
    let msg = txSigHash tx (encodeOutput out) i sh
    sigs <- mapM (signMsg msg) vKeys
    liftSecret $ buildf txin tx out i pubs $ map (flip TxSignature sh) sigs
    where sh = sigDataSH sigi

detSignTx :: Tx -> [SigInput] -> [PrvKey] -> Build Tx
detSignTx tx@(Tx _ ti _ _) sigis keys = do
    when (null ti) $ Broken "detSignTx: Transaction has no inputs"
    newIn <- mapM sign $ orderSigInput ti sigis
    return tx{ txIn = newIn }
    where sign (maybeSI,txin,i) = case maybeSI of
              Just sigi -> detSignTxIn txin sigi tx i keys
              _         -> toBuildTxIn txin

detSignTxIn :: TxIn -> SigInput -> Tx -> Int -> [PrvKey] -> Build TxIn
detSignTxIn txin sigi tx i keys = do
    (out,vKeys,pubs,buildf) <- decodeSigInput sigi keys
    let msg  = txSigHash tx (encodeOutput out) i sh
        sigs = map (detSignMsg msg) vKeys
    buildf txin tx out i pubs $ map (flip TxSignature sh) sigs
    where sh = sigDataSH sigi

{- Helpers for signing transactions -}

toBuildTxIn :: TxIn -> Build TxIn
toBuildTxIn txin@(TxIn _ s _)
    | null $ runScript s = Partial txin
    | otherwise = eitherToBuild (decodeInput s) >>= \si -> case si of
        SpendMulSig xs r -> guardPartial (length xs == r) >> return txin
        _                -> return txin

orderSigInput :: [TxIn] -> [SigInput] -> [(Maybe SigInput, TxIn, Int)]
orderSigInput ti si = zip3 (matchTemplate si ti f) ti [0..]
    where f si txin = sigDataOP si == prevOutput txin

type BuildFunction =  TxIn -> Tx -> ScriptOutput -> Int 
                   -> [PubKey] -> [TxSignature] -> Build TxIn

decodeSigInput :: SigInput -> [PrvKey] -> 
    Build (ScriptOutput, [PrvKey], [PubKey], BuildFunction)
decodeSigInput sigi keys = case sigi of
    SigInput s _ _ -> do
        out          <- eitherToBuild $ decodeOutput s
        (vKeys,pubs) <- sigKeys out keys
        return (out,vKeys,pubs,buildTxIn)
    SigInputSH s _ sr _ -> do
        out          <- eitherToBuild $ decodeOutput s
        rdm          <- eitherToBuild $ decodeOutput sr
        (vKeys,pubs) <- sigKeysSH out rdm keys
        return (rdm,vKeys,pubs,buildTxInSH)

buildTxInSH :: BuildFunction
buildTxInSH txin tx rdm i pubs sigs = do
    s   <- scriptInput <$> buildTxIn txin tx rdm i pubs sigs
    res <- either empty return $ 
        encodeScriptHash . (flip ScriptHashInput rdm) <$> decodeInput s
    return txin{ scriptInput = res }
    where empty = const $ Partial $ Script []

buildTxIn :: BuildFunction
buildTxIn txin tx out i pubs sigs 
    | null sigs = Partial txin{ scriptInput = Script [] }
    | otherwise = buildRes <$> case out of
        PayPK _     -> return $ SpendPK $ head sigs
        PayPKHash _ -> return $ SpendPKHash (head sigs) (head pubs)
        PayMulSig msPubs r -> do
            let mSigs = take r $ catMaybes $ matchTemplate aSigs msPubs f
            guardPartial $ length mSigs == r
            return $ SpendMulSig mSigs r
        _ -> Broken "buildTxIn: Can't sign a P2SH script here"
    where buildRes res = txin{ scriptInput = encodeInput res }
          aSigs = sigs ++ case decodeInput $ scriptInput txin of
              Right (SpendMulSig xs _) -> xs
              _ -> []
          f (TxSignature sig sh) pub = 
              verifySig (txSigHash tx (encodeOutput out) i sh) sig pub

sigKeysSH :: ScriptOutput -> RedeemScript -> [PrvKey]
          -> Build ([PrvKey],[PubKey])
sigKeysSH out rdm keys = case out of
    PayScriptHash a -> if scriptAddr rdm == a
        then sigKeys rdm keys
        else Broken "sigKeys: Redeem script does not match P2SH script"
    _ -> Broken "sigKeys: Can only decode P2SH script here"

sigKeys :: ScriptOutput -> [PrvKey] -> Build ([PrvKey],[PubKey])
sigKeys out keys = unzip <$> case out of
    PayPK p        -> return $ maybeToList $ 
        find ((== p) . snd) zipKeys
    PayPKHash a    -> return $ maybeToList $ 
        find ((== a) . pubKeyAddr . snd) zipKeys
    PayMulSig ps r -> return $ take r $ 
        filter ((`elem` ps) . snd) zipKeys
    _ -> Broken "sigKeys: Can't decode P2SH here" 
    where zipKeys = zip keys $ map derivePubKey keys

{- Tx verification -}

verifyTx :: Tx -> [(Script,OutPoint)] -> Bool
verifyTx tx xs = flip all z3 $ \(maybeS,txin,i) -> fromMaybe False $ do
    (out,inp) <- maybeS >>= flip decodeVerifySigInput txin
    let so = encodeOutput out
    case (out,inp) of
        (PayPK pub,SpendPK (TxSignature sig sh)) -> 
            return $ verifySig (txSigHash tx so i sh) sig pub
        (PayPKHash a,SpendPKHash (TxSignature sig sh) pub) -> do
            guard $ pubKeyAddr pub == a
            return $ verifySig (txSigHash tx so i sh) sig pub
        (PayMulSig pubs r,SpendMulSig sigs _) ->
            (== r) <$> countMulSig tx so i pubs sigs 
        _ -> Nothing
    where m = map (fst <$>) $ matchTemplate xs (txIn tx) f
          f (s,o) txin = o == prevOutput txin
          z3 = zip3 m (txIn tx) [0..]
                      
-- Count the number of valid signatures
countMulSig :: Tx -> Script -> Int -> [PubKey] -> [TxSignature] -> Maybe Int
countMulSig tx so i [] sigs = return 0
countMulSig tx so i pubs [] = return 0
countMulSig tx so i (pub:pubs) sigs@(TxSignature sig sh:rest)
    | verifySig (txSigHash tx so i sh) sig pub = 
         (+1) <$> countMulSig tx so i pubs rest
    | otherwise = countMulSig tx so i pubs sigs
                  
decodeVerifySigInput :: Script -> TxIn -> Maybe (ScriptOutput, ScriptInput)
decodeVerifySigInput so (TxIn _ si _ ) = case decodeOutput so of
    Right (PayScriptHash a) -> do
        (ScriptHashInput inp rdm) <- eitherToMaybe $ decodeScriptHash si
        guard $ scriptAddr rdm == a
        return (rdm,inp)
    out -> eitherToMaybe $ liftM2 (,) out (decodeInput si)

