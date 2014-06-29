module Network.Haskoin.Transaction.Builder 
( Coin(..)
, buildTx
, buildAddrTx
, SigInput(..)
, signTx
, signInput
, detSignTx
, detSignInput
, sigStatusTx
, verifyInput
, verifyTx
, guessTxSize
, chooseCoins
, chooseMSCoins
, getFee
, getMSFee
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (liftM, foldM, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT, hoistEither)

import Data.Maybe (catMaybes, isJust, fromJust, maybeToList)
import Data.List (sortBy, find)
import Data.Word (Word64)
import qualified Data.ByteString as BS (length, replicate, empty)

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

-- Size of a multisig P2SH input
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
-- required. In P2SH mode, the output script will be appended serialized at the
-- end of the SigScript.
data SigInput 
    -- | Parameters for signing a pay to public key hash output.
    = SigInput   { sigDataOut  :: !Script   -- ^ Output script to spend.
                 , sigDataOP   :: !OutPoint 
                   -- ^ Reference to the transaction output to spend.
                 , sigDataSH   :: !SigHash  -- ^ Signature type.
                 , sigIsP2SH   :: !Bool     -- ^ P2SH mode.
                 } 
    deriving (Eq, Show)

instance NFData SigInput where
    rnf (SigInput o p h b) = rnf o `seq` rnf p `seq` rnf h `seq` rnf b

-- | Prepare input for signing.
signInputParams :: Tx                 -- ^ Transaction to sign
                -> Int                -- ^ Input index to sign
                -> PrvKey             -- ^ Key to sign input with
                -> SigInput           -- ^ Signature parameters
                -> Either String (PubKey, ScriptOutput, Script, SigHash, Bool)
                -- ^ (Public key, PkScript, Sigscript, HashType, P2SH)
signInputParams tx ini prv si = do
    when (ini < 0 || ini >= length (txIn tx)) $
        Left "signInputParams: index out of bounds"
    sgs <- decodeToEither $ scriptInput (txIn tx !! ini)
    pks <- decodeOutput $ sigDataOut si
    return (pub, pks, sgs, sh, p2s)
  where
    pub = derivePubKey prv
    sh  = sigDataSH si
    p2s = sigIsP2SH si

-- | Replace input script in a transaction. Used internally by signing code.
replaceInput :: Tx           -- ^ Transaction
             -> Int          -- ^ Input index
             -> ScriptOutput -- ^ PkScript
             -> ScriptInput  -- ^ SigScript
             -> Bool         -- ^ P2SH flag
             -> Tx           -- ^ Modified transaction
replaceInput tx ini pks sgs p2s = tx { txIn = updateIndex ini (txIn tx) f }
  where
    f txi = txi { scriptInput = encode' $ rdm sgs }
    rdm i = if p2s then appendRedeem i pks else encodeInput i

-- | Add signature to multisig input. Used internally by signing code.
msAddSig :: Tx             -- ^ Signed transaction
         -> Int            -- ^ Signed input
         -> ScriptOutput   -- ^ Multisig output script
         -> ScriptInput    -- ^ Transaction input
         -> TxSignature    -- ^ Signature
         -> Either String ScriptInput
         -- ^ Input with added signature
msAddSig tx ini pks@(PayMulSig pubs r) (SpendMulSig sigs _) sig = do
    when (not $ sig `elem` sigs') $ Left "msAddSig: invalid signature provided"
    return $ SpendMulSig (take r sigs') r
  where
    msg h = txSigHash tx (encodeOutput pks) ini h
    sigs' = catMaybes $ matchTemplate (sig:sigs) pubs f
    f (TxSignature s h) p = verifySig (msg h) s p
msAddSig _ _ _ _ _ = Left "No multisig PkScript or SigScript provided"

-- | Add signature to input. Used internally by signing code.
addSig :: Tx            -- ^ Signed transaction
       -> Int           -- ^ Signed input
       -> ScriptOutput  -- ^ Output script from previous transaction
       -> Script        -- ^ Input script (could be an empty script)
       -> PubKey        -- ^ Public key
       -> TxSignature   -- ^ Transaction signature
       -> Bool          -- ^ P2SH
       -> Either String ScriptInput
       -- ^ SigScript
addSig tx ini pks scr pub ts p2s = do
    scr' <- if p2s && not (null ops) then removeRedeem scr else return scr
    case pks of
        PayPKHash _     -> return $ SpendPKHash ts pub
        PayPK _         -> return $ SpendPK ts
        PayMulSig _ r   -> if null (scriptOps scr')
          then
            msAddSig tx ini pks (SpendMulSig [] r) ts
          else do
            (sgs, _, _) <- decodeInput pks scr'
            msAddSig tx ini pks sgs ts
        PayScriptHash _ -> Left "addSig: cannot sign P2SH directly"
  where
    ops = scriptOps scr

-- | Sign a transaction input with a private key. Monadic version.
signInput :: Monad m
          => Tx              -- ^ Transaction to sign
          -> Int             -- ^ Input index to sign
          -> SigInput        -- ^ Signature parameters
          -> PrvKey          -- ^ Key to sign input with
          -> EitherT String (SecretT m) Tx
          -- ^ Resulting transaction
signInput tx ini si prv = do
    (pub, pks, sgs, sh, p2s) <- hoistEither $ signInputParams tx ini prv si
    sig <- lift $ signMsg (msg pks sh) prv
    ipt <- hoistEither $ addSig tx ini pks sgs pub (TxSignature sig sh) p2s
    return $ replaceInput tx ini pks ipt p2s
  where
    msg p h = txSigHash tx (encodeOutput p) ini h

-- | Sign a transaction input with a private key. Deterministic version.
detSignInput :: Tx                 -- ^ Transaction to sign
             -> Int                -- ^ Input index to sign
             -> SigInput           -- ^ Signature parameters
             -> PrvKey             -- ^ Key to sign input with
             -> Either String Tx   -- ^ Resulting transaction
detSignInput tx ini si prv = do
    (pub, pks, sgs, sh, p2s) <- signInputParams tx ini prv si
    ipt <- addSig tx ini pks sgs pub (TxSignature (sig pks sh) sh) p2s
    return $ replaceInput tx ini pks ipt p2s
  where
    msg p h = txSigHash tx (encodeOutput p) ini h
    sig p h = detSignMsg (msg p h) prv

-- | Obtain parameters required internally by signTx and detSignTx. Resulting
-- list contains as many tuples as combinations of inputs and private keys
-- there are. For input parameters lacking a private key, there will be a tuple
-- with no private key to perform signature validation on the input only.
getSigParms :: Tx           -- ^ Transaction to sign
            -> [SigInput]   -- ^ Signing parameters
            -> [PrvKey]     -- ^ Private keys
            -> Either String [(SigInput, Int, PrvKey)]
getSigParms (Tx _ ti _ _) sigis keys = do
    ps <- sequence pEs
    return $ concat ps
  where
    f si ini prvs = map (\prv -> (si, ini, prv)) prvs
    pEs = [ fmap (f si ini) $ sigKeys (sigDataOut si) keys
          | (si, ini) <- findSigInput sigis ti ]

-- | Sign a transaction by providing signing parameters and corresponding
-- private keys. The signature is computed within the 'SecretT' monad to
-- generate the random signing nonce.
signTx :: Monad m 
       => Tx                             -- ^ Transaction to sign
       -> [SigInput]                     -- ^ Signing parameters
       -> [PrvKey]                       -- ^ Private keys
       -> EitherT String (SecretT m) (Tx, Bool)
       -- ^ (Signed transaction, Complete?)
signTx tx@(Tx _ ti _ _) sigis keys = do
    when (null ti) . hoistEither $ Left "signTx: transaction has no inputs"
    sigp <- hoistEither $ getSigParms tx sigis keys
    tx' <- foldM msign tx sigp
    return (tx', sigStatusTx tx' sigis)
  where
    msign t (si, ini, prv) = signInput t ini si prv

-- | Sign a transaction by providing signing parameters and corresponding
-- private keys. This version is deterministic and does not require context.
detSignTx :: Tx          -- ^ Transaction to sign
          -> [SigInput]  -- ^ Signing parameters
          -> [PrvKey]    -- ^ Private keys
          -> Either String (Tx, Bool)
          -- ^ (Signed transaction, Complete?)
detSignTx tx@(Tx _ ti _ _) sigis keys = do
    when (null ti) $ Left "detSignTx: transaction has no inputs"
    sigp <- getSigParms tx sigis keys
    tx' <- foldM msign tx sigp
    return (tx', sigStatusTx tx' sigis)
  where 
    msign t (si, ini, prv) = detSignInput t ini si prv

-- Order 'SigInput' list with respect to the transaction inputs. This allows
-- users to provide list of 'SigInput' in any order. Users can also provide
-- only a partial list for signing or verification. Other inputs will be
-- ignored.
findSigInput :: [SigInput] -> [TxIn] -> [(SigInput, Int)]
findSigInput si ti =
    catMaybes $ map g $ zip (matchTemplate si ti f) [0..]
  where
    f s txin = sigDataOP s == prevOutput txin
    g (Just s, i) = Just (s,i)
    g (Nothing, _) = Nothing

-- Find from the list of private keys which one is required to sign the 
-- provided 'ScriptOutput'.
sigKeys :: Script -> [PrvKey] -> Either String [PrvKey]
sigKeys out keys = liftM (map fst) $ case decodeOutput out of
    Right (PayPK p)        -> 
        return $ maybeToList $ find ((== p) . snd) zipKeys
    Right (PayPKHash a)    -> 
        return $ maybeToList $ find ((== a) . pubKeyAddr . snd) zipKeys
    Right (PayMulSig ps r) -> 
        return $ take r $ filter ((`elem` ps) . snd) zipKeys
    _ -> Left "sigKeys: Invalid output script" 
  where
    zipKeys = zip keys (map derivePubKey keys)

{- Tx verification -}

-- | Validate a list of signatures against a list of public keys. Returns True
-- if all signatures are valid.
checkSigs :: Tx             -- ^ Transaction
          -> Script         -- ^ PkScript
          -> Int            -- ^ Input index
          -> [TxSignature]  -- ^ Signatures
          -> [PubKey]       -- ^ Corresponding public keys
          -> Bool           -- ^ Result from verification
checkSigs tx scr ini sigs pubs = length sigs == length (catMaybes vs)
  where
    vs = matchTemplate sigs pubs f
    f (TxSignature sig sh) pub =
        let msg = txSigHash tx scr ini sh
        in  verifySig msg sig pub

-- | Validate signatures in transaction input.
verifyInput :: Tx            -- ^ Transaction
           -> Int           -- ^ Input index
           -> Script        -- ^ PkScript
           -> Bool          -- ^ Result from verification
verifyInput tx ini out = if bad then False else chk
  where
    (TxIn _ scr _) = txIn tx !! ini
    pksE = decodeOutput out
    pks = fromRight pksE
    dinE = decodeInputBS pks scr
    Right (sgs, pks', _) = dinE
    chk = case sgs of
        SpendPK (TxSignature sig sh) ->
            let (PayPK pub) = pks'
                msg = txSigHash tx (encodeOutput pks') ini sh
            in  verifySig msg sig pub
        SpendPKHash (TxSignature sig sh) pub ->
            let msg = txSigHash tx (encodeOutput pks') ini sh
            in  verifySig msg sig pub
        SpendMulSig txsigs _ ->
            let (PayMulSig pubs r) = pks'
            in  if length txsigs < r
                then False
                else checkSigs tx (encodeOutput pks') ini txsigs pubs
    bad = isLeft dinE || isLeft pksE

-- | Limited transaction signature validation for standard transactions only.
verifyTx :: Tx                     -- ^ Transaction
         -> [(Script, OutPoint)]   -- ^ (PkScript, OutPoint)
         -> Bool
         -- ^ Result of validation, may return False for non-standard scripts
verifyTx tx xs = if bad then False else and (map v ip)
  where
    ip = [ (ini, out)
         | (ini, soM) <- zip [0..] $ matchTemplate xs (txIn tx) f
         , isJust soM
         , let (out, _) = fromJust soM
         ]
    bad = length ip < length xs
    v (ini, out) = verifyInput tx ini out
    f (_, op) ti = op == prevOutput ti

-- | Verify transaction to obtain status. Used by signature function.
sigStatusTx :: Tx -> [SigInput] -> Bool
sigStatusTx tx sigis = valid
  where
    valid = either error (verifyTx tx) $ mapM mf sigis
    mf (SigInput s o _ p) = if p
      then do
        rdm <- decodeOutput s
        let out = encodeOutput . PayScriptHash $ scriptAddr rdm
        return (out, o)
      else
        return (s, o)

