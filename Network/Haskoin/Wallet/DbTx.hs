{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.DbTx
( dbImportTx
, dbRemoveTx
, dbSendTx
, dbSendSolution
, dbSendCoins
, dbSignTx
, yamlTx
) where

import Control.Applicative ((<$>))
import Control.Monad (forM, unless, when, liftM)
import Control.Monad.Trans (liftIO)
import Control.Exception (throwIO)

import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word64)
import Data.List ((\\), nub)
import Data.Maybe (catMaybes, isNothing, isJust, fromJust)
import Data.Either (rights)
import Data.Yaml (Value, object, (.=))
import qualified Data.Map.Strict as M 

import Database.Persist 
    ( PersistStore
    , PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , Entity(..)
    , entityVal
    , entityKey
    , get
    , getBy
    , selectList
    , deleteWhere
    , updateWhere
    , update
    , insert_
    , insertUnique
    , replace
    , (=.), (==.), (<-.)
    )

import Network.Haskoin.Wallet.DbAccount
import Network.Haskoin.Wallet.DbAddress
import Network.Haskoin.Wallet.DbCoin
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Util

import Network.Haskoin.Transaction
    ( SigInput (SigInput, SigInputSH)
    , Coin
    , buildAddrTx
    , chooseCoins
    , chooseMSCoins
    , coinOutPoint
    , coinTxOut
    , coinRedeem
    , detSignTx
    , isTxComplete
    )
import Network.Haskoin.Script
    ( SigHash (SigAll)
    , ScriptOutput (PayMulSig)
    , encodeOutput
    , scriptRecipient
    , scriptSender
    , sortMulSig
    )
import Network.Haskoin.Protocol
    ( Tx
    , TxIn (TxIn)
    , TxOut (TxOut)
    , OutPoint (OutPoint)
    , Script (Script)
    , prevOutput
    , scriptOutput
    , txIn
    , txOut
    , txid
    )
import Network.Haskoin.Crypto
    ( Hash256
    , PrvKey
    , addrToBase58
    , accPrvKey
    , extMulSigKey
    , intMulSigKey
    , getAddrPrvKey
    , getAddrPubKey
    , intPrvKey
    , extPrvKey
    , xPrvKey
    , xPubKey
    )
import Network.Haskoin.Util
    ( fromRight
    , fromLeft
    , isLeft
    )
import Network.Haskoin.Util.BuildMonad
    ( isBroken
    , isComplete
    , runBroken
    , runBuild
    )

yamlTx :: DbTxGeneric b -> Value
yamlTx tx = object $ concat
    [ [ "Recipients" .= dbTxRecipients tx
      , "Value" .= dbTxValue tx
      ]
    , if dbTxOrphan tx then ["Orphan" .= True] else []
    , if dbTxPartial tx then ["Partial" .= True] else []
    ]

-- |Remove a transaction from the database and any parent transaction
dbRemoveTx :: PersistQuery m => Hash256 -> m [Hash256]
dbRemoveTx tid = do
    -- Find all parents of this transaction
    -- Partial transactions should not have any coins. Won't check for it
    coins <- selectList [ DbCoinTxid ==. tid ] []
    let parents = nub $ catStatus $ map (dbCoinStatus . entityVal) coins
    -- Recursively remove parents
    pids <- forM parents dbRemoveTx
    -- Delete output coins generated from this transaction
    deleteWhere [ DbCoinTxid ==. tid ]
    -- Delete account transactions
    deleteWhere [ DbTxTxid ==. tid ]
    -- Delete transaction blob
    deleteWhere [ DbTxBlobTxid ==. tid ]
    -- Delete orphaned input coins spent by this transaction
    deleteWhere [ DbCoinOrphan ==. True
                , DbCoinStatus <-. [Spent tid, Reserved tid]
                ]
    -- Unspend input coins that were previously spent by this transaction
    updateWhere [ DbCoinStatus <-. [Spent tid, Reserved tid] ]
                [ DbCoinStatus =. Unspent ]
    return $ tid:(concat pids)
          
-- |Import a transaction into the database
dbImportTx :: ( PersistQuery m
              , PersistUnique m
              , PersistMonadBackend m ~ b
              ) 
           => Tx -> m [DbTxGeneric b]
dbImportTx tx = do
    coinsM <- mapM (getBy . f) $ map prevOutput $ txIn tx
    let inCoins = catMaybes coinsM
        -- Unspent coins in the wallet related to this transaction
        unspent = filter ((== Unspent) . dbCoinStatus . entityVal) inCoins
        -- OutPoints from this transaction with no associated coins
        unknown = map snd $ filter (isNothing . fst) $ zip coinsM $ txIn tx
    -- Fail if an input is spent by a transaction which is not this one
    unless (isImportValid tid $ map entityVal inCoins) $ liftIO $ throwIO $
        DoubleSpendException "Transaction import failed due to a double spend"
    let toRemove = txToRemove $ map entityVal inCoins
    if length toRemove > 0
        then do
            -- Partial transactions need to be removed 
            -- before trying to re-import
            _ <- mapM dbRemoveTx toRemove
            dbImportTx tx
        else do
            time <- liftIO getCurrentTime
            -- Change status of all the unspent coins
            _ <- forM unspent $ \(Entity ci _) -> 
                update ci [DbCoinStatus =. status]
            -- Insert orphaned coins
            orphans <- liftM catMaybes $ mapM (dbImportOrphan status) unknown
            -- Import new coins and update existing coins
            outCoins <- liftM catMaybes $ 
                (mapM (dbImportCoin tid complete) $ zip (txOut tx) [0..])
            let accTxs = buildAccTx tx ((map entityVal inCoins) ++ orphans)
                            outCoins (not complete) time
            -- Insert transaction blob. Ignore if already exists
            _ <- insertUnique $ DbTxBlob tid tx time
            -- insert account transactions into database, 
            -- rewriting if they exist
            _ <- forM accTxs $ \accTx -> do
                prev <- getBy $ UniqueTx (dbTxTxid accTx) (dbTxAccount accTx)
                if isNothing prev 
                    then insert_ accTx  
                    else replace (entityKey $ fromJust prev) accTx
            -- Re-import parent transactions (transactions spending this one)
            if complete
                then do
                    let ids = nub $ catStatus $ map dbCoinStatus outCoins
                    blobs <- mapM dbGetTxBlob ids
                    reImported <- forM blobs $ dbImportTx . dec
                    return $ accTxs ++ (concat reImported)
                else return accTxs
  where
    tid              = txid tx
    f (OutPoint h i) = CoinOutPoint h (fromIntegral i)
    complete         = isTxComplete tx
    status           = if complete then Spent tid else Reserved tid
    dec              = dbTxBlobValue . entityVal

-- |A transaction can not be imported if it double spends coins in the wallet.
-- Upstream code needs to remove the conflicting transaction first using
-- dbTxRemove function
isImportValid :: Hash256 -> [DbCoinGeneric b] -> Bool
isImportValid tid coins = all (f . dbCoinStatus) coins
  where
    f (Spent parent) = parent == tid
    f _              = True

-- When a transaction spends coins previously spent by a partial transaction,
-- we need to remove the partial transactions from the database and try to
-- re-import the transaction. Coins with Reserved status are spent by a partial
-- transaction.
txToRemove :: [DbCoinGeneric b] -> [Hash256]
txToRemove coins = catMaybes $ map (f . dbCoinStatus) coins
  where
    f (Reserved parent) = Just parent
    f _                 = Nothing

-- |Group input and output coins by accounts and create 
-- account-level transaction
buildAccTx :: Tx -> [DbCoinGeneric b] -> [DbCoinGeneric b]
           -> Bool -> UTCTime -> [DbTxGeneric b]
buildAccTx tx inCoins outCoins partial time = map build $ M.toList oMap
  where
    iMap = foldr (f (\(i,o) x -> (x:i,o))) M.empty inCoins
    oMap = foldr (f (\(i,o) x -> (i,x:o))) iMap outCoins
    f g coin accMap = case M.lookup (dbCoinAccount coin) accMap of
        Just tuple -> M.insert (dbCoinAccount coin) (g tuple coin) accMap
        Nothing    -> M.insert (dbCoinAccount coin) (g ([],[]) coin) accMap
    allRecip = rights $ map toAddr $ txOut tx
    toAddr   = (addrToBase58 <$>) . scriptRecipient . scriptOutput
    sumVal   = sum . (map dbCoinValue)
    build (ai,(i,o)) = 
        DbTx (txid tx) recips total ai orphan partial time
      where
        orphan = or $ map dbCoinOrphan i
        total | orphan    = 0
              | otherwise = (fromIntegral $ sumVal o)
                          - (fromIntegral $ sumVal i)
        addrs = map dbCoinAddress o
        recips | null addrs = allRecip
               | orphan     = allRecip \\ addrs -- assume this is an outgoing tx
               | total < 0  = allRecip \\ addrs -- remove the change
               | otherwise  = addrs

-- |Create an orphaned coin if the input spends from an address in the wallet
-- but the coin doesn't exist in the wallet. This allows out-of-order tx import
dbImportOrphan :: (PersistUnique m, PersistMonadBackend m ~ b) 
               => CoinStatus -> TxIn 
               -> m (Maybe (DbCoinGeneric b))
dbImportOrphan status (TxIn (OutPoint h i) s _)
    | isLeft a  = return Nothing
    | otherwise = getBy (UniqueAddress b58) >>= \addrM -> case addrM of
        Nothing              -> return Nothing
        Just (Entity _ add) -> do
            time <- liftIO $ getCurrentTime
            rdm  <- dbGetRedeem add
            let coin = build rdm add time
            insert_ coin >> return (Just coin)
  where
    a   = scriptSender s
    b58 = addrToBase58 $ fromRight a
    build rdm add time = 
        DbCoin h (fromIntegral i) 0 (Script []) rdm b58 status
               (dbAddressAccount add) True time

-- |Create a new coin for an output if it sends coins to an 
-- address in the wallet. Does not actually write anything to the database
-- if commit is False. This is for correctly reporting on partial transactions
-- without creating coins in the database from a partial transaction
dbImportCoin :: ( PersistQuery m
                , PersistUnique m
                , PersistMonadBackend m ~ b
                )
             => Hash256 -> Bool -> (TxOut,Int) 
             -> m (Maybe (DbCoinGeneric b))
dbImportCoin tid commit ((TxOut v s), index) 
    | isLeft a  = return Nothing
    | otherwise = getBy (UniqueAddress b58) >>= \addrM -> case addrM of
        Nothing              -> return Nothing
        Just (Entity _ add) -> do
            time  <- liftIO getCurrentTime
            coinM <- getBy $ CoinOutPoint tid index
            case coinM of
                Just (Entity ci coin) -> do
                    -- Update existing coin with up to date information
                    let newCoin = coin{ dbCoinOrphan  = False
                                      , dbCoinValue   = (fromIntegral v)
                                      , dbCoinScript  = s
                                      , dbCoinCreated = time
                                      }
                    when commit $ replace ci newCoin
                    return $ Just newCoin
                Nothing -> do
                    rdm <- dbGetRedeem add
                    let coin = build rdm add time
                    when commit $ insert_ coin
                    dbAdjustGap add -- Adjust address gap
                    return $ Just coin
  where
    a   = scriptRecipient s
    b58 = addrToBase58 $ fromRight a
    build rdm add time = 
        DbCoin tid index (fromIntegral v) s rdm b58 Unspent
               (dbAddressAccount add) False time
    
-- |Builds a redeem script given an address. Only relevant for addresses
-- linked to multisig accounts. Otherwise it returns Nothing
dbGetRedeem :: (PersistStore m, PersistMonadBackend m ~ b) 
            => DbAddressGeneric b -> m (Maybe Script)
dbGetRedeem add = do
    acc <- liftM fromJust (get $ dbAddressAccount add)
    rdm <- if isMSAcc acc 
        then do
            let key      = dbAccountKey acc
                msKeys   = dbAccountMsKeys acc
                deriv    = fromIntegral $ dbAddressIndex add
                addrKeys = fromJust $ f key msKeys deriv
                pks      = map (xPubKey . getAddrPubKey) addrKeys
                req      = fromJust $ dbAccountMsRequired acc
            return $ Just $ sortMulSig $ PayMulSig pks req
        else return Nothing
    return $ encodeOutput <$> rdm
  where
    f = if dbAddressInternal add then intMulSigKey else extMulSigKey

-- |Build and sign a transactoin given a list of recipients
dbSendTx :: ( PersistUnique m
            , PersistQuery m
            )
         => AccountName -> [(String,Word64)] -> Word64
         -> m (Tx, Bool)
dbSendTx name dests fee = do
    (coins,recips) <- dbSendSolution name dests fee
    dbSendCoins coins recips (SigAll False)

-- |Given a list of recipients and a fee, finds a valid combination of coins
dbSendSolution :: ( PersistUnique m
                  , PersistQuery m
                  )
               => AccountName -> [(String,Word64)] -> Word64
               -> m ([Coin],[(String,Word64)])
dbSendSolution name dests fee = do
    (Entity ai acc) <- dbGetAccount name
    unspent <- liftM (map toCoin) $ dbCoins ai
    let msParam = ( fromJust $ dbAccountMsRequired acc
                  , fromJust $ dbAccountMsTotal acc
                  )
        resE | isMSAcc acc = chooseMSCoins tot fee msParam unspent
             | otherwise   = chooseCoins tot fee unspent
        (coins, change)    = fromRight resE
    when (isLeft resE) $ liftIO $ throwIO $
        CoinSelectionException $ fromLeft resE
    recips <- if change < 5000 then return dests else do
        cAddr <- dbGenIntAddrs name 1
        return $ dests ++ [(dbAddressBase58 $ head cAddr,change)]
    return (coins,recips)
  where
    tot = sum $ map snd dests
    
-- | Build and sign a transaction by providing coins and recipients
dbSendCoins :: PersistUnique m
            => [Coin] -> [(String,Word64)] -> SigHash
            -> m (Tx, Bool)
dbSendCoins coins recipients sh = do
    let txE = buildAddrTx (map coinOutPoint coins) recipients
        tx  = fromRight txE
    when (isLeft txE) $ liftIO $ throwIO $
        TransactionBuildingException $ fromLeft txE
    ys <- mapM (dbGetSigData sh) coins
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
    when (isBroken sigTx) $ liftIO $ throwIO $
        TransactionSigningException $ runBroken sigTx
    return (runBuild sigTx, isComplete sigTx)

dbSignTx :: PersistUnique m
         => AccountName -> Tx -> SigHash -> m (Tx, Bool)
dbSignTx name tx sh = do
    (Entity ai _) <- dbGetAccount name
    coins <- liftM catMaybes (mapM (getBy . f) $ map prevOutput $ txIn tx)
    -- Filter coins for this account only
    let accCoinsDB = filter ((== ai) . dbCoinAccount . entityVal) coins
        accCoins   = map (toCoin . entityVal) accCoinsDB
    ys <- forM accCoins (dbGetSigData sh)
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
    when (isBroken sigTx) $ liftIO $ throwIO $
        TransactionSigningException $ runBroken sigTx
    return (runBuild sigTx, isComplete sigTx)
  where
    f (OutPoint h i) = CoinOutPoint h (fromIntegral i)

-- |Given a coin, retrieves the necessary data to sign a transaction
dbGetSigData :: PersistUnique m
             => SigHash -> Coin -> m (SigInput,PrvKey)
dbGetSigData sh coin = do
    (Entity _ w) <- dbGetWallet "main"
    let a = fromRight $ scriptRecipient out
    (Entity _ add) <- dbGetAddr $ addrToBase58 a
    acc  <- liftM fromJust (get $ dbAddressAccount add)
    let master = dbWalletMaster w
        deriv  = fromIntegral $ dbAccountIndex acc
        accKey = fromJust $ accPrvKey master deriv
        g      = if dbAddressInternal add then intPrvKey else extPrvKey
        sigKey = fromJust $ g accKey $ fromIntegral $ dbAddressIndex add
    return (sigi, xPrvKey $ getAddrPrvKey sigKey)
  where
    out    = scriptOutput $ coinTxOut coin
    rdm    = coinRedeem coin
    sigi | isJust rdm = SigInputSH out (coinOutPoint coin) (fromJust rdm) sh
         | otherwise  = SigInput out (coinOutPoint coin) sh
