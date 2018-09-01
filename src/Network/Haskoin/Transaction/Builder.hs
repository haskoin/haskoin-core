{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Transaction.Builder
    ( -- * Transaction Creation & Signing
      buildAddrTx
    , buildTx
    , buildInput
    , SigInput(..)
    , signTx
    , signInput
    , verifyStdTx
    , mergeTxs
    , sigKeys
    , mergeTxInput
    , findSigInput
    , verifyStdInput
      -- * Coin Selection
    , Coin(..)
    , chooseCoins
    , chooseCoinsSink
    , chooseMSCoins
    , chooseMSCoinsSink
    , countMulSig
    , greedyAddSink
    , guessTxFee
    , guessMSTxFee
    , guessTxSize
    , guessMSSize
    ) where

import           Control.Arrow                      (first)
import           Control.DeepSeq                    (NFData, rnf)
import           Control.Monad                      (foldM, mzero, unless, when)
import           Control.Monad.Identity             (runIdentity)
import           Data.Aeson                         (FromJSON, ToJSON,
                                                     Value (Object), object,
                                                     parseJSON, toJSON, (.:),
                                                     (.:?), (.=))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import           Data.Conduit                       (ConduitT, Void, await,
                                                     runConduit, (.|))
import           Data.Conduit.List                  (sourceList)
import           Data.List                          (find, nub)
import           Data.Maybe                         (catMaybes, fromJust,
                                                     fromMaybe, isJust,
                                                     mapMaybe, maybeToList)
import           Data.Serialize                     (encode)
import           Data.String.Conversions            (cs)
import           Data.Word                          (Word64)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Signature
import           Network.Haskoin.Keys.Common
import           Network.Haskoin.Network.Common
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction.Common
import           Network.Haskoin.Util

-- | Any type can be used as a Coin if it can provide a value in Satoshi.
-- The value is used in coin selection algorithms.
class Coin c where
    coinValue :: c -> Word64

-- | Coin selection algorithm for normal (non-multisig) transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account.
chooseCoins :: Coin c
            => Word64 -- ^ Value to send.
            -> Word64 -- ^ Fee per byte.
            -> Int    -- ^ Number of outputs (including change)
            -> Bool   -- ^ Try to find better solution when one is found
            -> [c]    -- ^ List of ordered coins to choose from.
            -> Either String ([c], Word64)
               -- ^ Coin selection result and change amount.
chooseCoins target fee nOut continue coins =
    runIdentity . runConduit $
    sourceList coins .| chooseCoinsSink target fee nOut continue

-- | Coin selection algorithm for normal (non-multisig) transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account. This version uses a
-- Sink if you need conduit-based coin selection.
chooseCoinsSink :: (Monad m, Coin c)
                => Word64 -- ^ Target price to pay.
                -> Word64 -- ^ Fee per byte.
                -> Int    -- ^ Number of outputs (including change)
                -> Bool   -- ^ Try to find better solution when one is found
                -> ConduitT c Void m (Either String ([c], Word64))
                   -- ^ Coin selection result and change amount.
chooseCoinsSink target fee nOut continue
    | target > 0 =
        maybeToEither err <$>
            greedyAddSink target (guessTxFee fee nOut) continue
    | otherwise = return $ Left "chooseCoins: Target must be > 0"
  where
    err = "chooseCoins: No solution found"

-- | Coin selection algorithm for multisignature transactions. This function
-- returns the selected coins together with the amount of change to send back
-- to yourself, taking the fee into account. This function assumes all the
-- coins are script hash outputs that send funds to a multisignature address.
chooseMSCoins :: Coin c
              => Word64     -- ^ Target price to pay.
              -> Word64     -- ^ Fee per byte.
              -> (Int, Int) -- ^ Multisig parameters m of n (m,n).
              -> Int        -- ^ Number of outputs (including change)
              -> Bool -- ^ Try to find better solution when one is found
              -> [c]
              -> Either String ([c], Word64)
                 -- ^ Coin selection result and change amount.
chooseMSCoins target fee ms nOut continue coins =
    runIdentity . runConduit $
        sourceList coins .| chooseMSCoinsSink target fee ms nOut continue

-- | Coin selection algorithm for multisignature transactions. This function
-- returns the selected coins together with the amount of change to send back
-- to yourself, taking the fee into account. This function assumes all the
-- coins are script hash outputs that send funds to a multisignature address.
-- This version uses a Sink if you need conduit-based coin selection.
chooseMSCoinsSink :: (Monad m, Coin c)
                  => Word64     -- ^ Target price to pay.
                  -> Word64     -- ^ Fee per byte.
                  -> (Int, Int) -- ^ Multisig parameters m of n (m,n).
                  -> Int  -- ^ Number of outputs (including change)
                  -> Bool -- ^ Try to find better solution when one is found
                  -> ConduitT c Void m (Either String ([c], Word64))
                     -- ^ Coin selection result and change amount.
chooseMSCoinsSink target fee ms nOut continue
    | target > 0 =
        maybeToEither err <$>
            greedyAddSink target (guessMSTxFee fee ms nOut) continue
    | otherwise = return $ Left "chooseMSCoins: Target must be > 0"
  where
    err = "chooseMSCoins: No solution found"

-- Select coins greedily by starting from an empty solution. If the continue
-- value is set to True, the algorithm will try to find a better solution in
-- the stream once a solution is found. If the next solution found is not
-- strictly better than the previously found solution, the algorithm stops and
-- returns the previous solution. If the continue value is set to False, the
-- algorithm will return the first solution it finds in the stream.
greedyAddSink :: (Monad m, Coin c)
              => Word64          -- ^ Target to reach
              -> (Int -> Word64) -- ^ Coin count to fee function
              -> Bool            -- ^ Try to find better solutions
              -> ConduitT c Void m (Maybe ([c], Word64)) -- (Selected coins, change)
greedyAddSink target guessFee continue =
    go [] 0 [] 0
  where
    -- The goal is the value we must reach (including the fee) for a certain
    -- amount of selected coins.
    goal c = target + guessFee c
    go acc aTot ps pTot = await >>= \case
        -- A coin is available in the stream
        Just coin -> do
            let val = coinValue coin
            -- We have reached the goal using this coin
            if val + aTot >= goal (length acc + 1)
                -- If we want to continue searching for better solutions
                then if continue
                    -- This solution is the first one or
                    -- This solution is better than the previous one
                    then if pTot == 0 || val + aTot < pTot
                        -- Continue searching for better solutions in the stream
                        then go [] 0 (coin:acc) (val + aTot)
                        -- Otherwise, we stop here and return the previous
                        -- solution
                        else return $ Just (ps, pTot - goal (length ps))
                    -- Otherwise, return this solution
                    else return $
                        Just (coin : acc, val + aTot - goal (length acc + 1))
                -- We have not yet reached the goal. Add the coin to the
                -- accumulator
                else go (coin:acc) (val + aTot) ps pTot
        -- We reached the end of the stream
        Nothing ->
            return $ if null ps
                -- If no solution was found, return Nothing
                then Nothing
                -- If we have a solution, return it
                else Just (ps, pTot - goal (length ps))

guessTxFee :: Word64 -> Int -> Int -> Word64
guessTxFee byteFee nOut nIn =
    byteFee * fromIntegral (guessTxSize nIn [] nOut 0)

guessMSTxFee :: Word64 -> (Int, Int) -> Int -> Int -> Word64
guessMSTxFee byteFee ms nOut nIn =
    byteFee * fromIntegral (guessTxSize 0 (replicate nIn ms) nOut 0)

-- | Computes an upper bound on the size of a transaction based on some known
-- properties of the transaction.
guessTxSize :: Int         -- ^ Number of regular transaction inputs.
            -> [(Int,Int)]
               -- ^ For every multisig input in the transaction, provide
               -- the multisig parameters m of n (m,n) for that input.
            -> Int         -- ^ Number of pay to public key hash outputs.
            -> Int         -- ^ Number of pay to script hash outputs.
            -> Int         -- ^ Upper bound on the transaction size.
guessTxSize pki msi pkout msout =
    8 + inpLen + inp + outLen + out
  where
    inpLen = BS.length $ encode $ VarInt $ fromIntegral $ length msi + pki
    outLen = BS.length $ encode $ VarInt $ fromIntegral $ pkout + msout
    inp = pki * 148 + sum (map guessMSSize msi)
             -- (20: hash160) + (5: opcodes) +
             -- (1: script len) + (8: Word64)
    out =
        pkout * 34 +
             -- (20: hash160) + (3: opcodes) +
             -- (1: script len) + (8: Word64)
        msout * 32

-- Size of a multisig pay2sh input
guessMSSize :: (Int,Int) -> Int
guessMSSize (m, n)
    -- OutPoint (36) + Sequence (4) + Script
 = 40 + fromIntegral (BS.length $ encode $ VarInt $ fromIntegral scp) + scp
    -- OP_M + n*PubKey + OP_N + OP_CHECKMULTISIG
  where
    rdm =
        fromIntegral $
        BS.length $ encode $ opPushData $ BS.replicate (n * 34 + 3) 0
    -- Redeem + m*sig + OP_0
    scp = rdm + m * 73 + 1

{- Build a new Tx -}

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of recipients addresses and amounts as outputs.
buildAddrTx :: Network -> [OutPoint] -> [(ByteString, Word64)] -> Either String Tx
buildAddrTx net xs ys =
    buildTx xs =<< mapM f ys
  where
    f (s, v) = case stringToAddr net s of
        Just a -> return (addressToOutput a, v)
        _      -> Left $ "buildAddrTx: Invalid address " ++ cs s

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of 'ScriptOutput' and amounts as outputs.
buildTx :: [OutPoint] -> [(ScriptOutput, Word64)] -> Either String Tx
buildTx xs ys =
    mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os [] 0
  where
    fi outPoint = TxIn outPoint BS.empty maxBound
    fo (o, v)
        | v <= 2100000000000000 = return $ TxOut v $ encodeOutputBS o
        | otherwise =
            Left $ "buildTx: Invalid amount " ++ show v

-- | Data type used to specify the signing parameters of a transaction input.
-- To sign an input, the previous output script, outpoint and sighash are
-- required. When signing a pay to script hash output, an additional redeem
-- script is required.
data SigInput = SigInput
    { sigInputScript :: !ScriptOutput -- ^ Output script to spend
    , sigInputValue  :: !Word64       -- ^ Output script value
    , sigInputOP     :: !OutPoint     -- ^ Spending tranasction OutPoint
    , sigInputSH     :: !SigHash      -- ^ Signature type
    , sigInputRedeem :: !(Maybe RedeemScript) -- ^ Redeem script
    } deriving (Eq, Show)

instance NFData SigInput where
    rnf (SigInput o v p h b) =
        rnf o `seq` rnf v `seq` rnf p `seq` rnf h `seq` rnf b

instance ToJSON SigInput where
    toJSON (SigInput so val op sh rdm) = object $
        [ "pkscript" .= so
        , "value"    .= val
        , "outpoint" .= op
        , "sighash"  .= sh
        ] ++ [ "redeem" .= r | r <- maybeToList rdm ]

instance FromJSON SigInput where
    parseJSON (Object o) = do
        so  <- o .: "pkscript"
        val <- o .: "value"
        op  <- o .: "outpoint"
        sh  <- o .: "sighash"
        rdm <- o .:? "redeem"
        return $ SigInput so val op sh rdm
    parseJSON _ = mzero

-- | Sign a transaction by providing the 'SigInput' signing paramters and
-- a list of private keys. The signature is computed deterministically as
-- defined in RFC-6979.
signTx :: Network
       -> Tx               -- ^ Transaction to sign
       -> [SigInput]       -- ^ SigInput signing parameters
       -> [SecKeyI]        -- ^ List of private keys to use for signing
       -> Either String Tx -- ^ Signed transaction
signTx net otx sigis allKeys
    | null ti   = Left "signTx: Transaction has no inputs"
    | otherwise = foldM go otx $ findSigInput sigis ti
  where
    ti = txIn otx
    go tx (sigi@(SigInput so _ _ _ rdmM), i) = do
        keys <- sigKeys net so rdmM allKeys
        foldM (\t k -> signInput net t i sigi k) tx keys

-- | Sign a single input in a transaction deterministically (RFC-6979).
signInput :: Network -> Tx -> Int -> SigInput -> SecKeyI -> Either String Tx
signInput net tx i (SigInput so val _ sh rdmM) key = do
    let sig = TxSignature (signHash (secKeyData key) msg) sh
    si <- buildInput net tx i so val rdmM sig $ derivePubKeyI key
    let ins = updateIndex i (txIn tx) (f si)
    return $ Tx (txVersion tx) ins (txOut tx) [] (txLockTime tx)
  where
    f si x = x {scriptInput = encodeInputBS si}
    msg = txSigHash net tx (encodeOutput $ fromMaybe so rdmM) val i sh

-- | Order the 'SigInput' with respect to the transaction inputs. This allows
-- the user to provide the SigInput in any order. Users can also provide only a
-- partial set of 'SigInput' entries.
findSigInput :: [SigInput] -> [TxIn] -> [(SigInput, Int)]
findSigInput si ti =
    mapMaybe g $ zip (matchTemplate si ti f) [0..]
  where
    f s txin = sigInputOP s == prevOutput txin
    g (Just s, i)  = Just (s,i)
    g (Nothing, _) = Nothing

-- | Find from the list of provided private keys which one is required to sign
-- the 'ScriptOutput'.
sigKeys ::
       Network
    -> ScriptOutput
    -> Maybe RedeemScript
    -> [SecKeyI]
    -> Either String [SecKeyI]
sigKeys net so rdmM keys =
    case (so, rdmM) of
        (PayPK p, Nothing) ->
            return . map fst . maybeToList $ find ((== p) . snd) zipKeys
        (PayPKHash h, Nothing) ->
            return . map fst . maybeToList $
            find ((== h) . getAddrHash160 . pubKeyAddr net . snd) zipKeys
        (PayMulSig ps r, Nothing) ->
            return $ map fst $ take r $ filter ((`elem` ps) . snd) zipKeys
        (PayScriptHash _, Just rdm) -> sigKeys net rdm Nothing keys
        _ -> Left "sigKeys: Could not decode output script"
  where
    zipKeys = map (\k -> (k, derivePubKeyI k)) keys

-- | Construct an input for a transaction given a signature, public key and data
-- about the previous output.
buildInput ::
       Network
    -> Tx -- ^ transaction where input will be added
    -> Int -- ^ input index where signature will go
    -> ScriptOutput -- ^ output script being spent
    -> Word64 -- ^ amount of previous output
    -> Maybe RedeemScript -- ^ redeem script if pay-to-script-hash
    -> TxSignature
    -> PubKeyI
    -> Either String ScriptInput
buildInput net tx i so val rdmM sig pub = do
    when (i >= length (txIn tx)) $ Left "buildInput: Invalid input index"
    case (so, rdmM) of
        (PayPK _, Nothing) -> return $ RegularInput $ SpendPK sig
        (PayPKHash _, Nothing) -> return $ RegularInput $ SpendPKHash sig pub
        (PayMulSig msPubs r, Nothing) -> do
            let mSigs = take r $ catMaybes $ matchTemplate allSigs msPubs f
            return $ RegularInput $ SpendMulSig mSigs
        (PayScriptHash _, Just rdm) -> do
            inp <- buildInput net tx i rdm val Nothing sig pub
            return $ ScriptHashInput (getRegularInput inp) rdm
        _ -> Left "buildInput: Invalid output/redeem script combination"
  where
    scp = scriptInput $ txIn tx !! i
    allSigs =
        nub $
        sig :
        case decodeInputBS net scp of
            Right (ScriptHashInput (SpendMulSig xs) _) -> xs
            Right (RegularInput (SpendMulSig xs))      -> xs
            _                                          -> []
    out = encodeOutput so
    f (TxSignature x sh) p =
        verifyHashSig (txSigHash net tx out val i sh) x (pubKeyPoint p)
    f TxSignatureEmpty _ = False

{- Merge multisig transactions -}

-- | Merge partially-signed multisig transactions.
mergeTxs :: Network -> [Tx] -> [(ScriptOutput, Word64, OutPoint)] -> Either String Tx
mergeTxs net txs os
    | null txs = error "Transaction list is empty"
    | length (nub emptyTxs) /= 1 = Left "Transactions do not match"
    | length txs == 1 = return $ head txs
    | otherwise = foldM (mergeTxInput net txs) (head emptyTxs) outs
  where
    zipOp = zip (matchTemplate os (txIn $ head txs) f) [0..]
    outs = map (first $ (\(o,v,_) -> (o,v)) . fromJust) $ filter (isJust . fst) zipOp
    f (_, _, o) txin = o == prevOutput txin
    emptyTxs = map (\tx -> foldl clearInput tx outs) txs
    ins is i = updateIndex i is (\ti -> ti{ scriptInput = BS.empty })
    clearInput tx (_, i) =
        Tx (txVersion tx) (ins (txIn tx) i) (txOut tx) [] (txLockTime tx)

-- | Merge input from partially-signed multisig transactions.
mergeTxInput ::
       Network
    -> [Tx]
    -> Tx
    -> ((ScriptOutput, Word64), Int)
    -> Either String Tx
mergeTxInput net txs tx ((so, val), i)
    -- Ignore transactions with empty inputs
 = do
    let ins = map (scriptInput . (!! i) . txIn) txs
    sigRes <- mapM extractSigs $ filter (not . BS.null) ins
    let rdm = snd $ head sigRes
    unless (all (== rdm) $ map snd sigRes) $ Left "Redeem scripts do not match"
    si <- encodeInputBS <$> go (nub $ concatMap fst sigRes) so rdm
    let ins' = updateIndex i (txIn tx) (\ti -> ti {scriptInput = si})
    return $ Tx (txVersion tx) ins' (txOut tx) [] (txLockTime tx)
  where
    go allSigs out rdmM =
        case out of
            PayMulSig msPubs r ->
                let sigs =
                        take r $
                        catMaybes $ matchTemplate allSigs msPubs $ f out
                 in return $ RegularInput $ SpendMulSig sigs
            PayScriptHash _ ->
                case rdmM of
                    Just rdm -> do
                        si <- go allSigs rdm Nothing
                        return $ ScriptHashInput (getRegularInput si) rdm
                    _ -> Left "Invalid output script type"
            _ -> Left "Invalid output script type"
    extractSigs si =
        case decodeInputBS net si of
            Right (RegularInput (SpendMulSig sigs)) -> Right (sigs, Nothing)
            Right (ScriptHashInput (SpendMulSig sigs) rdm) ->
                Right (sigs, Just rdm)
            _ -> Left "Invalid script input type"
    f out (TxSignature x sh) p =
        verifyHashSig
            (txSigHash net tx (encodeOutput out) val i sh)
            x
            (pubKeyPoint p)
    f _ TxSignatureEmpty _ = False

{- Tx verification -}

-- | Verify if a transaction is valid and all of its inputs are standard.
verifyStdTx :: Network -> Tx -> [(ScriptOutput, Word64, OutPoint)] -> Bool
verifyStdTx net tx xs =
    not (null (txIn tx)) && all go (zip (matchTemplate xs (txIn tx) f) [0 ..])
  where
    f (_, _, o) txin = o == prevOutput txin
    go (Just (so, val, _), i) = verifyStdInput net tx i so val
    go _                      = False

-- | Verify if a transaction input is valid and standard.
verifyStdInput :: Network -> Tx -> Int -> ScriptOutput -> Word64 -> Bool
verifyStdInput net tx i = go (scriptInput $ txIn tx !! i)
  where
    dec = decodeInputBS net
    go inp so val =
        case dec inp of
            Right (RegularInput (SpendPK (TxSignature sig sh))) ->
                case so of
                    PayPK pub ->
                        verifyHashSig
                            (txSigHash net tx out val i sh)
                            sig
                            (pubKeyPoint pub)
                    _ -> False
            Right (RegularInput (SpendPKHash (TxSignature sig sh) pub)) ->
                case so of
                    PayPKHash h ->
                        pubKeyAddr net pub == PubKeyAddress h net &&
                        verifyHashSig
                            (txSigHash net tx out val i sh)
                            sig
                            (pubKeyPoint pub)
                    _ -> False
            Right (RegularInput (SpendMulSig sigs)) ->
                case so of
                    PayMulSig pubs r ->
                        countMulSig net tx out val i (map pubKeyPoint pubs) sigs ==
                        r
                    _ -> False
            Right (ScriptHashInput si rdm) ->
                case so of
                    PayScriptHash h ->
                        p2shAddr net rdm == ScriptAddress h net &&
                        go (encodeInputBS $ RegularInput si) rdm val
                    _ -> False
            _ -> False
      where
        out = encodeOutput so

-- | Count the number of valid signatures for a multi-signature transaction.
countMulSig :: Network -> Tx -> Script -> Word64 -> Int -> [PubKey] -> [TxSignature] -> Int
countMulSig _ _ _ _ _ [] _  = 0
countMulSig _ _ _ _ _ _  [] = 0
countMulSig net tx out val i (_:pubs) (TxSignatureEmpty:rest) =
    countMulSig net tx out val i pubs rest
countMulSig net tx out val i (pub:pubs) sigs@(TxSignature sig sh:rest)
    | verifyHashSig (txSigHash net tx out val i sh) sig pub =
        1 + countMulSig net tx out val i pubs rest
    | otherwise = countMulSig net tx out val i pubs sigs
