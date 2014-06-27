module Network.Haskoin.Transaction.Builder 
( Coin(..)
, buildTx
, buildAddrTx
, SigInput(..)
, SigStatus(..)
, isSigInvalid
, signTx
, detSignTx
, verifyTx
, guessTxSize
, chooseCoins
, chooseMSCoins
, getFee
, getMSFee
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (guard, liftM2)
import Control.Applicative ((<$>))

import Data.Maybe (catMaybes, maybeToList, fromMaybe)
import Data.List (sortBy, find, nub, foldl')
import Data.Word (Word64)
import qualified Data.ByteString as BS (length, replicate, empty, null)

import Network.Haskoin.Util
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Script

-- | A Coin is something that can be spent by a transaction and is
-- represented by a transaction output, an outpoint and optionally a
-- redeem script.
data Coin = 
    Coin { coinTxOut    :: !TxOut          -- ^ Transaction output
         , coinOutPoint :: !OutPoint       -- ^ Previous outpoint
         , coinRedeem   :: !(Maybe Script) -- ^ Redeem script
         } deriving (Eq, Show)

instance NFData Coin where
    rnf (Coin t p r) = rnf t `seq` rnf p `seq` rnf r

-- | Coin selection algorithm for normal (non-multisig) transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account.
chooseCoins :: Word64 -- ^ Target price to pay.
            -> Word64 -- ^ Fee price per 1000 bytes.
            -> [Coin] -- ^ List of coins to choose from.
            -> Either String ([Coin],Word64) 
               -- ^ Coin selection result and change amount.
chooseCoins target kbfee xs 
    | target > 0 = maybeToEither err $ greedyAdd target (getFee kbfee) xs
    | otherwise  = Left "chooseCoins: Target must be > 0"
    where err = "chooseCoins: No solution found"

-- | Coin selection algorithm for multisignature transactions. This function
-- returns the selected coins together with the amount of change to send back
-- to yourself, taking the fee into account. This function assumes all the 
-- coins are script hash outputs that send funds to a multisignature address.
chooseMSCoins :: Word64    -- ^ Target price to pay.
              -> Word64    -- ^ Fee price per 1000 bytes.
              -> (Int,Int) -- ^ Multisig parameters m of n (m,n).
              -> [Coin]    -- ^ List of coins to choose from.
              -> Either String ([Coin],Word64) 
                 -- ^ Coin selection result and change amount.
chooseMSCoins target kbfee ms xs 
    | target > 0 = maybeToEither err $ greedyAdd target (getMSFee kbfee ms) xs
    | otherwise  = Left "chooseMSCoins: Target must be > 0"
    where err = "chooseMSCoins: No solution found"

-- Select coins greedily by starting from an empty solution
greedyAdd :: Word64 -> (Int -> Word64) -> [Coin] -> Maybe ([Coin],Word64)
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

{-
-- Start from a solution containing all coins and greedily remove them
greedyRem :: Word64 -> (Int -> Word64) -> [Coin] -> Maybe ([Coin],Word64)
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
-}

getFee :: Word64 -> Int -> Word64
getFee kbfee count = kbfee*((len + 999) `div` 1000)
    where len = fromIntegral $ guessTxSize count [] 2 0

getMSFee :: Word64 -> (Int,Int) -> Int -> Word64
getMSFee kbfee ms count = kbfee*((len + 999) `div` 1000)
    where len = fromIntegral $ guessTxSize 0 (replicate count ms) 2 0

-- | Computes an upper bound on the size of a transaction based on some known
-- properties of the transaction.
guessTxSize :: Int         -- ^ Number of regular transaction inputs.
            -> [(Int,Int)] 
               -- ^ For every multisig input in the transaction, provide
               -- the multisig parameters m of n (m,n) for that input.
            -> Int         -- ^ Number of pay to public key hash outputs.
            -> Int         -- ^ Number of pay to script hash outputs.
            -> Int         -- ^ Upper bound on the transaction size.
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
    where rdm = BS.length $ encode' $ opPushData $ BS.replicate (n*34 + 3) 0
          -- Redeem + m*sig + OP_0
          scp = rdm + m*73 + 1 

{- Build a new Tx -}

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of recipients addresses and amounts as outputs. 
buildAddrTx :: [OutPoint] -> [(String,Word64)] -> Either String Tx
buildAddrTx xs ys = buildTx xs =<< mapM f ys
    where f (s,v) = case base58ToAddr s of
            Just a@(PubKeyAddress _) -> return (PayPKHash a,v)
            Just a@(ScriptAddress _) -> return (PayScriptHash a,v)
            _ -> Left $ "buildAddrTx: Invalid address " ++ s

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of 'ScriptOutput' and amounts as outputs.
buildTx :: [OutPoint] -> [(ScriptOutput,Word64)] -> Either String Tx
buildTx xs ys = mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os 0
    where fi outPoint = TxIn outPoint BS.empty maxBound
          fo (o,v) | v <= 2100000000000000 = return $ TxOut v $ encodeOutputBS o
                   | otherwise = Left $ "buildTx: Invalid amount " ++ (show v)

-- | Data type used to specify the signing parameters of a transaction input.
-- To sign an input, the previous output script, outpoint and sighash are
-- required. When signing a pay to script hash output, an additional redeem
-- script is required.
data SigInput 
    -- | Parameters for signing a pay to public key hash output.
    = SigInput   { sigDataOut :: !Script   -- ^ Output script to spend.
                 , sigDataOP  :: !OutPoint 
                   -- ^ Reference to the transaction output to spend.
                 , sigDataSH  :: !SigHash  -- ^ Signature type.
                 } 
    -- | Parameters for signing a pay to script hash output.
    | SigInputSH { sigDataOut :: !Script   
                 , sigDataOP  :: !OutPoint 
                 , sigRedeem  :: !Script   -- ^ Redeem script.
                 , sigDataSH  :: !SigHash  
                 } deriving (Eq, Show)

instance NFData SigInput where
    rnf (SigInput o p h) = rnf o `seq` rnf p `seq` rnf h
    rnf (SigInputSH o p r h) = rnf o `seq` rnf p `seq` rnf r `seq` rnf h

isSigInputSH :: SigInput -> Bool
isSigInputSH (SigInputSH _ _ _ _) = True
isSigInputSH _                    = False

data SigStatus
    = SigComplete
    | SigPartial
    | SigInvalid String
    deriving (Eq, Read, Show)

isSigInvalid :: SigStatus -> Bool
isSigInvalid (SigInvalid _) = True
isSigInvalid _              = False

instance NFData SigStatus where
    rnf (SigInvalid s) = rnf s
    rnf _ = ()

lcdStatus :: [SigStatus] -> SigStatus
lcdStatus []     = error "lcdStatus can not be called on an empty list"
lcdStatus (x:xs) = foldl' go x xs
  where
    go p n = case (p, n) of
        (SigInvalid s, _)   -> SigInvalid s
        (_, SigInvalid s)   -> SigInvalid s
        (SigPartial, _)     -> SigPartial
        (_, SigPartial)     -> SigPartial
        _                   -> SigComplete

-- | Sign a transaction by providing the 'SigInput' signing parameters and a
-- list of private keys. The signature is computed within the 'SecretT' monad
-- to generate the random signing nonce and within the 'BuildT' monad to add
-- information on wether the result was fully or partially signed.
signTx :: Monad m 
       => Tx                        -- ^ Transaction to sign
       -> [SigInput]                -- ^ SigInput signing parameters
       -> [PrvKey]                  -- ^ List of private keys to use for signing
       -> SecretT m (Tx, SigStatus) -- ^ (Signed transaction, Status)
signTx tx@(Tx _ ti _ _) sigis keys 
    | null ti   = return (tx, SigInvalid "signTx: Transaction has no inputs")
    | otherwise = do
        inResults <- mapM sign $ orderSigInput ti sigis
        return (tx{ txIn = map fst inResults }, lcdStatus $ map snd inResults)
  where 
    sign (sigiM, i) = case sigiM of
        Just sigi -> signInput tx i sigi keys
        Nothing   -> return (txIn tx !! i, SigComplete)

signInput :: Monad m 
          => Tx -> Int -> SigInput -> [PrvKey] 
          -> SecretT m (TxIn, SigStatus)
signInput tx i sigi keys
    | status == SigComplete = return (txin, SigComplete)
    | isLeft resE = return (txin, SigInvalid $ fromLeft resE)
    | otherwise   = do
        sigs <- mapM (signMsg msg) vKeys
        return $ f $ (map $ flip TxSignature sh) sigs
  where 
    txin               = txIn tx !! i
    status             = getInputStatus txin sigi
    resE               = decodeSigInput sigi keys
    (out, vKeys, pubs) = fromRight resE
    sh                 = sigDataSH sigi
    msg                = txSigHash tx (encodeOutput out) i sh
    f | isSigInputSH sigi = buildInputSH tx i out pubs
      | otherwise         = buildInput   tx i out pubs

-- | Sign a transaction by providing the 'SigInput' signing paramters and 
-- a list of private keys. The signature is computed deterministically as
-- defined in RFC-6979. The signature is computed within the 'Build' monad
-- to add information on wether the result was fully or partially signed.
detSignTx :: Tx              -- ^ Transaction to sign
          -> [SigInput]      -- ^ SigInput signing parameters
          -> [PrvKey]        -- ^ List of private keys to use for signing
          -> (Tx, SigStatus) -- ^ Signed transaction
detSignTx tx@(Tx _ ti _ _) sigis keys
    | null ti   = 
        (tx, SigInvalid "signTx: Transaction has no inputs")
    | otherwise = 
        (tx{ txIn = map fst inResults }, lcdStatus $ map snd inResults)
  where 
    inResults             = map sign $ orderSigInput ti sigis
    sign (sigiM, i) = case sigiM of
        Just sigi -> detSignInput tx i sigi keys
        Nothing   -> (txIn tx !! i, SigComplete)

detSignInput :: Tx -> Int -> SigInput -> [PrvKey] -> (TxIn, SigStatus)
detSignInput tx i sigi keys 
    | status == SigComplete = (txin, SigComplete)
    | isLeft resE = (txin, SigInvalid $ fromLeft resE)
    | otherwise = f $ (map $ flip TxSignature sh) sigs
  where
    txin               = txIn tx !! i
    status             = getInputStatus txin sigi
    resE               = decodeSigInput sigi keys
    (out, vKeys, pubs) = fromRight resE
    sh                 = sigDataSH sigi
    msg                = txSigHash tx (encodeOutput out) i sh
    sigs               = map (detSignMsg msg) vKeys
    f | isSigInputSH sigi = buildInputSH tx i out pubs
      | otherwise         = buildInput   tx i out pubs

{- Helpers for signing transactions -}

-- Parse an input and return its completion status. If the input is
-- non-standard, we return SigComplete.
getInputStatus :: TxIn -> SigInput -> SigStatus
getInputStatus (TxIn _ s _) sigi
    | BS.null s = SigPartial
    | otherwise = case decodeScriptHashBS s of
        Right (ScriptHashInput (SpendMulSig xs) (PayMulSig _ r)) -> 
            if length xs >= r then SigComplete else SigPartial
        Right _ -> SigComplete
        Left _  -> case (decodeInputBS s, decodeOutput $ sigDataOut sigi) of
            (Right (SpendMulSig xs), Right (PayMulSig _ r)) ->
                if length xs >= r then SigComplete else SigPartial
            -- If we can not decode an input, we consider it complete
            _ -> SigComplete

-- Order the SigInput with respect to the transaction inputs. This allow the
-- users to provide the SigInput in any order. Users can also provide only a
-- partial set of SigInputs.
orderSigInput :: [TxIn] -> [SigInput] -> [(Maybe SigInput, Int)]
orderSigInput ti si = 
    zip (matchTemplate si ti f) [0..]
  where 
    f s txin = sigDataOP s == prevOutput txin

decodeSigInput :: SigInput 
               -> [PrvKey] 
               -> Either String (ScriptOutput, [PrvKey], [PubKey])
decodeSigInput sigi keys = case sigi of
    SigInput s _ _ -> do
        out           <- decodeOutput s
        (vKeys, pubs) <- sigKeys out keys
        -- The input is signed using the output
        return (out, vKeys, pubs)
    SigInputSH s _ sr _ -> do
        out           <- decodeOutput s
        rdm           <- decodeOutput sr
        (vKeys, pubs) <- sigKeysSH out rdm keys
        -- The inputs is signed using the redeem script
        return (rdm, vKeys, pubs)

buildInputSH :: Tx -> Int -> ScriptOutput -> [PubKey] -> [TxSignature] 
             -> (TxIn, SigStatus)
buildInputSH tx i rdm pubs sigs
    | isSigInvalid status = (txin, status)
    | BS.null $ scriptInput newIn = (txin, SigPartial)
    | otherwise = (txin{ scriptInput = fromRight newSH }, status)
  where 
    txin            = txIn tx !! i
    (newIn, status) = buildInput tx i rdm pubs sigs
    newSH           = toP2SH <$> decodeInputBS (scriptInput newIn)
    toP2SH          = encodeScriptHashBS . (flip ScriptHashInput rdm)

buildInput :: Tx -> Int -> ScriptOutput -> [PubKey] -> [TxSignature] 
           -> (TxIn, SigStatus)
buildInput tx i out pubs sigs 
    | null sigs = (txin, SigPartial)
    | otherwise = case out of
        PayPK _     -> 
            (buildRes $ SpendPK $ head sigs, SigComplete)
        PayPKHash _ -> 
            (buildRes $ SpendPKHash (head sigs) (head pubs), SigComplete)
        PayMulSig msPubs r -> 
            -- We need to order the existing sigs and new sigs with respect
            -- to the order of the public keys
            let mSigs = take r $ catMaybes $ matchTemplate allSigs msPubs f
                inp   = buildRes $ SpendMulSig mSigs
                in if length mSigs == r 
                    then (inp, SigComplete) 
                    else (inp, SigPartial)
        _ -> (txin, SigInvalid "buildInput: Invalid P2SH input")
    where 
      txin = txIn tx !! i
      buildRes res = txin{ scriptInput = encodeInputBS res }
      allSigs = nub $ concat
        [ sigs 
        , case decodeScriptHashBS $ scriptInput txin of
            Right (ScriptHashInput (SpendMulSig xs) _) -> xs
            _ -> case decodeInputBS $ scriptInput txin of
                    Right (SpendMulSig xs) -> xs
                    _ -> []
        ]
      f (TxSignature sig sh) pub = 
          verifySig (txSigHash tx (encodeOutput out) i sh) sig pub

sigKeysSH :: ScriptOutput -> RedeemScript -> [PrvKey]
          -> Either String ([PrvKey], [PubKey])
sigKeysSH out rdm keys = case out of
    PayScriptHash a -> if scriptAddr rdm == a
        then sigKeys rdm keys
        else Left "sigKeys: Redeem script does not match P2SH script"
    _ -> Left "sigKeys: Invalid PayScriptHash output"

-- Find from the list of private keys which one is required to sign the 
-- provided ScriptOutput. It also return the public key of that private key
-- if one could be found.
sigKeys :: ScriptOutput -> [PrvKey] -> Either String ([PrvKey], [PubKey])
sigKeys out keys = unzip <$> case out of
    PayPK p        -> Right $ maybeToList $ 
        find ((== p) . snd) zipKeys
    PayPKHash a    -> Right $ maybeToList $ 
        find ((== a) . pubKeyAddr . snd) zipKeys
    PayMulSig ps r -> Right $ take r $ 
        filter ((`elem` ps) . snd) zipKeys
    _ -> Left "sigKeys: Invalid output" 
  where 
    zipKeys = zip keys $ map derivePubKey keys

{- Tx verification -}

-- This is not the final transaction verification function. It is here mainly
-- as a helper for tests. It can only validates standard inputs.
verifyTx :: Tx -> [(Script,OutPoint)] -> Bool
verifyTx tx xs = flip all z3 $ \(maybeS,txin,i) -> fromMaybe False $ do
    (out,inp) <- maybeS >>= flip decodeVerifySigInput txin
    let so = encodeOutput out
    case (out,inp) of
        (PayPK pub, SpendPK (TxSignature sig sh)) -> 
            return $ verifySig (txSigHash tx so i sh) sig pub
        (PayPKHash a, SpendPKHash (TxSignature sig sh) pub) -> do
            guard $ pubKeyAddr pub == a
            return $ verifySig (txSigHash tx so i sh) sig pub
        (PayMulSig pubs r, SpendMulSig sigs) ->
            (== r) <$> countMulSig tx so i pubs sigs 
        _ -> Nothing
    where m = map (fst <$>) $ matchTemplate xs (txIn tx) f
          f (_,o) txin = o == prevOutput txin
          z3 = zip3 m (txIn tx) [0..]
                      
-- Count the number of valid signatures
countMulSig :: Tx -> Script -> Int -> [PubKey] -> [TxSignature] -> Maybe Int
countMulSig _ _ _ [] _  = return 0
countMulSig _ _ _ _  [] = return 0
countMulSig tx so i (pub:pubs) sigs@(TxSignature sig sh:rest)
    | verifySig (txSigHash tx so i sh) sig pub = 
         (+1) <$> countMulSig tx so i pubs rest
    | otherwise = countMulSig tx so i pubs sigs
                  
decodeVerifySigInput :: Script -> TxIn -> Maybe (ScriptOutput, ScriptInput)
decodeVerifySigInput so (TxIn _ si _ ) = case decodeOutput so of
    Right (PayScriptHash a) -> do
        (ScriptHashInput inp rdm) <- eitherToMaybe $ decodeScriptHashBS si
        guard $ scriptAddr rdm == a
        return (rdm,inp)
    out -> eitherToMaybe $ liftM2 (,) out (decodeInputBS si)

