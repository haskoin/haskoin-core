{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Tx
( AccTx(..)
, getTx
, getTxEntity
, getAccTx
, getConfirmations
, toAccTx
, txList
, txPage
, importTx
, removeTx
, sendTx
, signWalletTx
, getSigBlob
, signSigBlob
, walletBloomFilter
, getProposition
, isTxInWallet
, firstKeyTime
, importBlock
, getBestHeight
, setBestHash
, accountBalance
, addressBalance
, spendableAccountBalance
, unspentCoins
, spendableCoins
) where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, when, liftM, filterM)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Exception (throwIO)

import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word32, Word64)
import Data.Maybe (catMaybes, isNothing, isJust, fromJust)
import Data.Either (rights)
import qualified Data.List as L ((\\), nub)
import qualified Data.Map.Strict as M (toList, empty, lookup, insert)
import qualified Data.ByteString as BS (empty)

import Database.Persist 
    ( PersistStore
    , PersistUnique
    , PersistQuery
    , Entity(..)
    , entityVal
    , entityKey
    , get
    , getBy
    , selectList
    , selectFirst
    , deleteWhere
    , updateWhere
    , update
    , insert_
    , insert
    , replace
    , count
    , delete
    , (=.), (==.), (<-.)
    , SelectOpt( Asc, LimitTo, OffsetBy )
    , Key
    )

import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Address
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Node
import Network.Haskoin.Crypto
import Network.Haskoin.Util

toAccTx :: (MonadIO m, PersistUnique b, PersistQuery b)
        => DbAccTxGeneric b -> ReaderT b m AccTx
toAccTx aTx = do
    Entity _ tx <- getTxEntity $ dbAccTxHash aTx
    conf <- getConfirmations $ dbAccTxHash aTx
    confDate <- getConfirmationDate $ dbAccTxHash aTx
    recipAddrs <- forM (dbAccTxRecipients aTx) $ \a -> do
        -- Only get the address if it matches the account id
        addrM <- getBy $ UniqueAddressAccount a (dbAccTxAccount aTx)
        let label = dbAddressLabel $ entityVal $ fromJust addrM
        return $ if isJust addrM
            then RecipientAddress a label True
            else RecipientAddress a "" False
    return $ AccTx { accTxHash           = dbAccTxHash aTx
                   , accTxRecipients     = recipAddrs
                   , accTxValue          = dbAccTxValue aTx
                   , accTxConfidence     = dbTxConfidence tx
                   , accIsCoinbase       = dbTxIsCoinbase tx
                   , accTxConfirmations  = fromIntegral conf
                   , accTx               = dbTxValue tx
                   , accReceivedDate     = dbTxCreated tx
                   , accConfirmationDate = confDate
                   }

getConfirmations :: (MonadIO m, PersistUnique b, PersistQuery b)
                 => TxHash -> ReaderT b m Int
getConfirmations h = do
    Entity _ tx <- getTxEntity h
    height <- getBestHeight
    return $ if isNothing $ dbTxConfirmedBy tx then 0 else 
        fromIntegral $ height - (fromJust $ dbTxConfirmedHeight tx) + 1

getConfirmationDate :: (MonadIO m, PersistUnique b, PersistQuery b)
                    => TxHash -> ReaderT b m (Maybe Timestamp)
getConfirmationDate h = do
    Entity _ tx <- getTxEntity h
    if isNothing $ dbTxConfirmedBy tx then return Nothing else do
        let bid = fromJust $ dbTxConfirmedBy tx
        confM <- getBy $ UniqueConfirmation h bid
        return $ dbConfirmationBlockTimestamp . entityVal <$> confM

-- | Fetch a full transaction by transaction id.
getTx :: (MonadIO m, PersistQuery b, PersistUnique b)
      => TxHash         -- ^ Transaction ID
      -> ReaderT b m Tx -- ^ Transaction
getTx tid = liftM (dbTxValue . entityVal) $ getTxEntity tid

getTxEntity :: (MonadIO m, PersistUnique b, PersistQuery b)
            => TxHash
            -> ReaderT b m (Entity (DbTxGeneric b))
getTxEntity tid = do
    entM <- getBy $ UniqueTx tid
    when (isNothing entM) $ liftIO $ throwIO $ WalletException $
        unwords ["Transaction", encodeTxHashLE tid, "not in database"]
    return $ fromJust entM

-- | Fetch an account transaction by account name and transaction id.
getAccTx :: (MonadIO m, PersistUnique b, PersistQuery b)
         => WalletName        -- ^ Wallet name
         -> AccountName       -- ^ Account name
         -> TxHash            -- ^ Transaction id
         -> ReaderT b m AccTx -- ^ Account transaction
getAccTx wallet name tid = do
    Entity wk _ <- getWalletEntity wallet
    Entity ai _ <- getAccountEntity wk name
    entM <- getBy $ UniqueAccTx tid ai
    when (isNothing entM) $ liftIO $ throwIO $ WalletException $
        unwords ["Transaction", encodeTxHashLE tid, "not in database"]
    toAccTx $ entityVal $ fromJust entM

-- | List all the transaction entries for an account. Transaction entries
-- summarize information for a transaction in a specific account only (such as
-- the total movement of for this account).
txList :: (MonadIO m, PersistQuery b, PersistUnique b)
       => WalletName          -- ^ Wallet name
       -> AccountName         -- ^ Account name
       -> ReaderT b m [AccTx] -- ^ List of transaction entries
txList wallet name = do
    Entity wk _ <- getWalletEntity wallet
    Entity ai _ <- getAccountEntity wk name
    e <- selectList [ DbAccTxAccount ==. ai ] [ Asc DbAccTxCreated ]
    mapM (toAccTx . entityVal) e

-- | Returns a page of transactions for an account. Pages are numbered starting
-- from page 1. Requesting page 0 will return the last page.
txPage :: (MonadIO m, PersistQuery b, PersistUnique b)
       => WalletName   -- ^ Wallet name
       -> AccountName  -- ^ Account name
       -> Int          -- ^ Requested page number
       -> Int          -- ^ Number of addresses per page
       -> ReaderT b m ([AccTx], Int) -- ^ (AccTx page, Max page)
txPage wallet name pageNum resPerPage
    | pageNum < 0 = liftIO $ throwIO $ WalletException $ 
        unwords ["Invalid page number:", show pageNum]
    | resPerPage < 1 = liftIO $ throwIO $ WalletException $
        unwords ["Invalid results per page:",show resPerPage]
    | otherwise = do
        Entity wk _ <- getWalletEntity wallet
        Entity ai _ <- getAccountEntity wk name
        txCount <- count [ DbAccTxAccount ==. ai ]
        let maxPage = max 1 $ (txCount + resPerPage - 1) `div` resPerPage
            page | pageNum == 0 = maxPage
                 | otherwise    = pageNum
        when (page > maxPage) $ liftIO $ throwIO $ WalletException $ 
            unwords ["The page number", show pageNum, "is too high"]
        dbTxs <- selectList 
            [ DbAccTxAccount ==. ai ] 
            [ Asc DbAccTxId
            , LimitTo resPerPage
            , OffsetBy $ (page - 1) * resPerPage
            ]
        liftM (flip (,) maxPage) $ mapM (toAccTx . entityVal) dbTxs

-- | Import a transaction into the wallet
importTx :: (MonadIO m, PersistQuery b, PersistUnique b) 
         => Tx       -- ^ Transaction to import
         -> TxSource -- ^ Where does the transaction come from
         -> Maybe (String, String) -- ^ (Wallet, Account)
         -> ReaderT b m (Maybe (TxHash, TxConfidence, Bool))
         -- ^ (Imported Tx, Confidence, New addresses generated)
importTx tx source nameM = getBy (UniqueTx tid) >>= \txM -> case txM of
    Just (Entity tkey dbtx) -> do
        -- Change the confidence from offline to pending if we get a
        -- transaction from the network
        let isOffline = dbTxConfidence dbtx == TxOffline
        if isOffline && source == NetworkSource
            then do
                replace tkey $ dbtx{ dbTxConfidence = TxPending }
                return $ Just (tid, TxPending, False)
            else do
                when (source == UnknownSource) $ checkUnknownTx tx nameM
                return $ Just (tid, dbTxConfidence dbtx, False)
    Nothing -> do
        when (source == UnknownSource) $ checkUnknownTx tx nameM
        isOrphan <- isOrphanTx tx
        if not isOrphan then addTx tx source else do
            -- Can not store UnknownSource transactions as orphans
            when (source == UnknownSource) $ liftIO $ throwIO $ WalletException
                "Trying to import an untrusted orphan transaction"
            time <- liftIO getCurrentTime
            insert_ $ DbOrphan tid tx source time
            return Nothing
  where
    tid = txHash tx

-- Only allow importing unknown transaction if one of the inputs is in the
-- given account.
checkUnknownTx :: (MonadIO m, PersistQuery b, PersistUnique b) 
               => Tx -> Maybe (String, String) -> ReaderT b m ()
checkUnknownTx tx nameM = do
    -- Was a wallet/account combination provided?
    when (isNothing nameM) $ liftIO $ throwIO exc
    let (wallet, name) = fromJust nameM

    -- Is one of the transaction inputs ours ?
    Entity wk _ <- getWalletEntity wallet
    Entity ai _ <- getAccountEntity wk name
    coinsE <- liftM catMaybes (mapM (getBy . f) $ map prevOutput $ txIn tx)
    ours   <- filterM (belongs ai) coinsE
    when (null ours) $ liftIO $ throwIO exc
  where
    exc = WalletException "Trying to import an invalid untrusted transaction"
    belongs ai (Entity ci _ ) = liftM isJust $ getBy (UniqueCoinAccount ci ai)
    f (OutPoint h i) = CoinOutPoint h (fromIntegral i)

addTx :: (MonadIO m, PersistQuery b, PersistUnique b) 
      => Tx -> TxSource -> ReaderT b m (Maybe (TxHash, TxConfidence, Bool))
addTx initTx source = do
    -- A non-network transaction can not double spend coins
    checkDoubleSpend initTx source
    -- Retrieve the coins we have from the transaction inputs
    coinsE <- liftM catMaybes (mapM (getBy . f) $ map prevOutput $ txIn initTx)
    let coins = map entityVal coinsE
    -- Merge the transaction with existing ones if possible
    -- The new transaction could have the same txid as an existing offline
    -- tx but we remove them anyway in the next step.
    tx <- mergeOfflineTxs source initTx $ map dbCoinValue coins
    -- We must remove offline transactions which spend the same coins as us
    removeOfflineTxs $ map prevOutput $ txIn tx
    -- Import new coins 
    let tid = txHash tx
    outRes <- liftM catMaybes $ mapM (importCoin tid) $ zip (txOut tx) [0..]
    let outCoins = map fst outRes
        genA     = or $ map snd outRes
    -- Ignore this transaction if it is not ours
    if null coins && null outCoins then return Nothing else do
        time <- liftIO getCurrentTime
        -- Mark all inputs of this transaction as spent
        forM_ (txIn tx) $ \(TxIn op _ _) -> insert_ $ DbSpentCoin op tid time
        -- Update conflicts with other transactions
        conf <- updateConflicts tx (map dbCoinValue coins) source 
        -- Save the transaction 
        let isCB = isCoinbaseTx tx
        insert_ $ DbTx tid tx conf Nothing Nothing isCB (nosigTxHash tx) time 
        -- Build transactions that report on individual accounts
        dbAccTxs <- buildAccTx tx coinsE outCoins time
        -- insert account transactions into database
        forM_ dbAccTxs insert_
        -- Re-import orphans
        genO <- tryImportOrphans
        return $ Just (tid, conf, genA || genO)
  where
    f (OutPoint h i) = CoinOutPoint h (fromIntegral i)

mergeOfflineTxs :: (MonadIO m, PersistUnique b, PersistQuery b)
                => TxSource -> Tx -> [Coin] -> ReaderT b m Tx
mergeOfflineTxs NetworkSource tx _ = return tx
mergeOfflineTxs _ tx [] = return tx
mergeOfflineTxs _ tx coins = do
    -- Find offline transactions with the same nosigTxHash
    res <- selectList [ DbTxConfidence ==. TxOffline
                      , DbTxNosigHash ==. (nosigTxHash tx)
                      ] []
    return $ foldl f tx $ map (dbTxValue . entityVal) res
  where
    ops = map (\c -> (coinScript c, coinOutPoint c)) coins
    f t1 t2 = case mergeTxs [t1, t2] ops of
        Right (mTx, _) -> mTx
        _              -> t1

checkDoubleSpend :: (MonadIO m, PersistUnique b, PersistQuery b)
                 => Tx -> TxSource -> ReaderT b m ()
checkDoubleSpend tx source
    -- We only allow double spends that come from the network
    | source == NetworkSource = return ()
    | otherwise = do
        -- We can not spend the same coins as someone else
        spendM <- mapM (getBy . UniqueTx) =<< findSpendingTxs outpoints
        let spendConfs = map (dbTxConfidence . entityVal) $ catMaybes spendM
            isDoubleSpend = any (`elem` [TxBuilding, TxPending]) spendConfs

        -- We can not build on top of a chain with conflicts
        txsM <- mapM (getBy . UniqueTx) $ L.nub $ map outPointHash outpoints
        xs   <- mapM (getConflicts . dbTxHash . entityVal) $ catMaybes txsM
        conflM <- mapM (getBy . UniqueTx) $ L.nub $ concat xs
        let confidences = map (dbTxConfidence . entityVal) $ catMaybes conflM
            hasConflict = any (`elem` [TxBuilding, TxPending]) confidences

        when (isDoubleSpend || hasConflict) $ liftIO $ throwIO $
            WalletException "Can not import double-spending transaction"
  where
    outpoints = map prevOutput $ txIn tx

updateConflicts :: (MonadIO m, PersistUnique b, PersistQuery b)
                => Tx -> [Coin] -> TxSource -> ReaderT b m TxConfidence
updateConflicts tx coins source 
    | source == NetworkSource = do
        -- Find conflicts with transactions that spend the same coins
        confls <- liftM (filter (/= tid)) $ findSpendingTxs outpoints

        -- We are also in conflict with any children of a conflicted tx
        childConfls <- forM confls findChildTxs
        let allConfls = L.nub $ concat $ confls : childConfls
        isDead <- liftM or $ mapM isTxBuilding allConfls 

        -- A transaction iherits the conflicts of its parent transaction
        txsM <- mapM (getBy . UniqueTx) $ L.nub $ map outPointHash outpoints
        let txs     = catMaybes txsM
            parDead = any ((== TxDead) . dbTxConfidence . entityVal) txs
        cs <- mapM (getConflicts . dbTxHash . entityVal) txs

        -- Insert unique conflict links
        time  <- liftIO getCurrentTime
        forM_ (L.nub $ concat $ allConfls : cs) $ \h -> 
            insert_ $ DbTxConflict tid h time

        -- Compute the confidence
        return $ if isDead || parDead then TxDead else TxPending

    | verifyStdTx tx vs = return TxPending
    | otherwise         = return TxOffline
  where
    outpoints        = map prevOutput $ txIn tx
    vs               = map f coins
    f (Coin _ s o _) = (s,o)
    tid              = txHash tx

-- Finds all the children of a given transaction recursively
findChildTxs :: (MonadIO m, PersistUnique b, PersistQuery b)
             => TxHash -> ReaderT b m [TxHash]
findChildTxs tid = do
    txM <- getBy $ UniqueTx tid
    let tx        = dbTxValue $ entityVal $ fromJust txM
        len       = fromIntegral $ length (txOut tx)
        outpoints = map (OutPoint tid) [0 .. (len - 1)]
    if isNothing txM then return [] else do
        hs   <- findSpendingTxs outpoints
        rest <- forM hs findChildTxs
        return $ L.nub $ concat $ hs : rest

-- Returns the transactions that spend the given outpoints
findSpendingTxs :: (MonadIO m, PersistQuery b)
                => [OutPoint] -> ReaderT b m [TxHash]
findSpendingTxs outpoints = do
    res <- selectList [DbSpentCoinKey <-. outpoints] []
    return $ L.nub $ map (dbSpentCoinTx . entityVal) res

-- Try to re-import all orphan transactions. Returns True if new addresses
-- were generated.
tryImportOrphans :: (MonadIO m, PersistQuery b, PersistUnique b)
                 => ReaderT b m Bool
tryImportOrphans = do
    orphans <- selectList [] []
    -- TODO: Can we use deleteWhere [] ?
    forM_ orphans $ delete . entityKey
    resM <- forM orphans $ \(Entity _ otx) -> 
        importTx (dbOrphanValue otx) (dbOrphanSource otx) Nothing
    return $ or $ map lst3 $ catMaybes resM

removeOfflineTxs :: (MonadIO m, PersistUnique b, PersistQuery b)
                 => [OutPoint] -> ReaderT b m ()
removeOfflineTxs outpoints = do
    hs  <- findSpendingTxs outpoints
    txs <- liftM catMaybes $ mapM (getBy . UniqueTx) hs
    let offline = filter ((== TxOffline) . dbTxConfidence . entityVal) txs
    forM_ offline $ removeTx . dbTxHash . entityVal

isCoinbaseTx :: Tx -> Bool
isCoinbaseTx (Tx _ tin _ _) =
    length tin == 1 && (outPointHash $ prevOutput $ head tin) == 0x00

-- Create a new coin for an output if it is ours. Return the coin entity and
-- a bool indicating if new addresses have been generated.
importCoin :: (MonadIO m, PersistQuery b, PersistUnique b)
           => TxHash 
           -> (TxOut, Int) 
           -> ReaderT b m (Maybe ((Entity (DbCoinGeneric b), Bool)))
importCoin tid (tout, i) = do
    dbAddrs <- isMyOutput tout
    let soE    = decodeOutputBS $ scriptOutput tout
        so     = fromRight soE
    if null dbAddrs || isLeft soE then return Nothing else do
        time <- liftIO getCurrentTime
        -- All redeem scripts should be the same for the same address
        rdm <- getRedeem $ head dbAddrs
        let coin   = Coin (outValue tout) so (OutPoint tid $ fromIntegral i) rdm
            add    = dbAddressValue $ head dbAddrs
            dbcoin = DbCoin tid i coin add time
        ci <- insert dbcoin
        -- Insert coin / account links
        forM_ (map dbAddressAccount dbAddrs) $ \ai ->
            insert_ $ DbCoinAccount ci ai time
        cnt <- adjustLookAhead $ head dbAddrs
        return $ Just (Entity ci dbcoin, cnt > 0)

-- Builds a redeem script given an address. Only relevant for addresses
-- linked to multisig accounts. Otherwise it returns Nothing
getRedeem :: (MonadIO m, PersistUnique b, PersistQuery b)
          => DbAddressGeneric b -> ReaderT b m (Maybe RedeemScript)
getRedeem add = do
    acc <- liftM fromJust (get $ dbAddressAccount add)
    let deriv    = dbAddressIndex add
        internal = dbAddressInternal add
    return $ getRedeemIndex (dbAccountValue acc) deriv internal

getRedeemIndex :: Account -> KeyIndex -> Bool -> Maybe RedeemScript
getRedeemIndex acc deriv internal 
    | isMSAccount acc = Just $ sortMulSig $ PayMulSig pks req
    | otherwise       = Nothing
  where
    key      = head $ accountKeys acc 
    msKeys   = tail $ accountKeys acc
    addrKeys = fromJust $ f (AccPubKey key) msKeys deriv
    pks      = map (xPubKey . getAddrPubKey) addrKeys
    req      = accountRequired acc
    f        = if internal then intMulSigKey else extMulSigKey

-- Returns True if the transaction has an input that belongs to the wallet
-- but we don't have a coin for it yet. We are missing a parent transaction.
isOrphanTx :: (MonadIO m, PersistUnique b, PersistQuery b)
           => Tx -> ReaderT b m Bool
isOrphanTx tx = do
    myInputFlags <- mapM isMyInput $ txIn tx
    coinsM       <- mapM (getBy . f) $ map prevOutput $ txIn tx
    missing      <- filterM g $ zip myInputFlags coinsM
    return $ length missing > 0
  where
    f (OutPoint h i)  = CoinOutPoint h (fromIntegral i)
    g (True, Just c)  = isTxOffline $ dbCoinHash $ entityVal c
    g (True, Nothing) = return True
    g _               = return False

-- Returns True if the input address is part of the wallet
isMyInput :: (MonadIO m, PersistQuery b)
          => TxIn -> ReaderT b m Bool
isMyInput input
    | isLeft senderE = return False
    | otherwise      = liftM (> 0) $ count [ DbAddressValue ==. sender ]
  where
    senderE = scriptSender =<< (decodeToEither $ scriptInput input)
    sender  = fromRight senderE

-- Returns True if the output address is part of the wallet
isMyOutput :: (MonadIO m, PersistQuery b)
           => TxOut -> ReaderT b m [DbAddressGeneric b]
isMyOutput out
    | isLeft recipientE = return []
    | otherwise         = do
        res <- selectList [ DbAddressValue ==. recipient ] []
        return $ map entityVal res
  where
    recipientE = scriptRecipient =<< (decodeToEither $ scriptOutput out)
    recipient  = fromRight recipientE

-- | Group input and output coins by accounts and create 
-- account-level transaction
buildAccTx :: (MonadIO m, PersistQuery b)
           => Tx 
           -> [Entity (DbCoinGeneric b)] 
           -> [Entity (DbCoinGeneric b)] 
           -> UTCTime 
           -> ReaderT b m [DbAccTxGeneric b]
buildAccTx tx inCoins outCoins time = do
    inAccs <- forM inCoins $ \(Entity ci _) -> 
        liftM (map (dbCoinAccountAccount . entityVal)) $ 
            selectList [ DbCoinAccountCoin ==. ci ] []
    outAccs <- forM outCoins $ \(Entity ci _) ->
        liftM (map (dbCoinAccountAccount . entityVal)) $ 
            selectList [ DbCoinAccountCoin ==. ci ] []
    let is = concat $ map (\(Entity _ c, ais) -> 
                map (\ai -> (c, ai)) ais) $ zip inCoins inAccs
        os = concat $ map (\(Entity _ c, ais) -> 
                map (\ai -> (c, ai)) ais) $ zip outCoins outAccs
    return $ map build $ M.toList $ oMap is os
  where
    -- We build a map of accounts to ([input coins], [output coins])
    iMap is    = foldr (f (\(i,o) x -> (x:i,o))) M.empty is
    oMap is os = foldr (f (\(i,o) x -> (i,x:o))) (iMap is) os
    f g (coin, ai) accMap = case M.lookup ai accMap of
        Just tuple -> M.insert ai (g tuple coin) accMap
        Nothing    -> M.insert ai (g ([],[]) coin) accMap
    allRecip = rights $ map toRecip $ txOut tx
    toRecip  = (scriptRecipient =<<) . decodeToEither . scriptOutput
    sumVal   = sum . (map (coinValue . dbCoinValue))
    build (ai,(i,o)) = DbAccTx (txHash tx) recips total ai time
      where
        total = (fromIntegral $ sumVal o) - (fromIntegral $ sumVal i)
        addrs = map dbCoinAddress o
        recips | null addrs = allRecip
               | total < 0 = 
                   let xs = allRecip L.\\ addrs -- Remove the change
                   in if null xs then allRecip else xs
               | otherwise = addrs

-- | Remove a transaction from the database. This will remove all transaction
-- entries for this transaction as well as any child transactions and coins
-- deriving from it.
removeTx :: (MonadIO m, PersistUnique b, PersistQuery b)
         => TxHash      -- ^ Transaction hash to remove
         -> ReaderT b m [TxHash]  -- ^ List of removed transaction hashes
removeTx tid = do
    txM <- getBy $ UniqueTx tid
    let tx        = dbTxValue $ entityVal $ fromJust txM
        len       = fromIntegral $ length (txOut tx)
        outpoints = map (OutPoint tid) [0 .. (len - 1)]
    if isNothing txM then return [] else do 
        childs <- findSpendingTxs outpoints
        -- Recursively remove children
        cids <- forM childs removeTx
        -- Find the coins we are going to delete
        cis <- liftM (map entityKey) $ selectList [ DbCoinHash ==. tid ] []
        -- Delete Coin/Account links
        forM_ cis $ \ci -> deleteWhere [ DbCoinAccountCoin ==. ci ]
        -- Delete output coins generated from this transaction
        deleteWhere [ DbCoinHash ==. tid ]
        -- Delete account transactions
        deleteWhere [ DbAccTxHash ==. tid ]
        -- Delete conflicts
        deleteWhere [ DbTxConflictFst ==. tid ]
        deleteWhere [ DbTxConflictSnd ==. tid ]
        -- Delete transaction
        deleteWhere [ DbTxHash ==. tid ]
        -- Delete from orphan pool
        deleteWhere [ DbOrphanHash ==. tid ]
        -- Delete from confirmation table
        deleteWhere [ DbConfirmationTx ==. tid ]
        -- Unspend input coins that were previously spent by this transaction
        deleteWhere [ DbSpentCoinTx ==. tid ]
        return $ tid:(concat cids)

-- | Create a transaction sending some coins to a list of recipient addresses.
sendTx :: (MonadIO m, PersistUnique b, PersistQuery b)
       => WalletName          -- ^ Wallet name
       -> AccountName         -- ^ Account name
       -> Word32              -- ^ Minimum confirmations
       -> [(Address,Word64)]  -- ^ List of recipient addresses and amounts
       -> Word64              -- ^ Fee per 1000 bytes 
       -> ReaderT b m (TxHash, Bool, Bool) 
            -- ^ (Payment transaction, Completed flag, New addresses generated)
sendTx wallet name minConf dests fee = do
    Entity wk _ <- getWalletEntity wallet
    accE@(Entity ai acc) <- getAccountEntity wk name
    tx <- buildUnsignedTx accE minConf dests fee
    (signedTx, _) <- if isReadAccount $ dbAccountValue acc 
        then return (tx, False)
        else signSigBlob wallet name =<< buildSigBlob ai tx
    confM <- importTx signedTx WalletSource Nothing
    let (tid, conf, genA) = fromJust confM
    when (isNothing confM) $ liftIO $ throwIO $
        WalletException "Transaction could not be imported in the wallet"
    return (tid, conf `elem` [ TxPending, TxBuilding ], genA)

-- | Try to sign the inputs of an existing transaction using the private keys
-- of an account. This command will return an indication if the transaction is
-- fully signed or if additional signatures are required. This command will
-- work for both normal inputs and multisignature inputs. Signing is limited to
-- the keys of one account only to allow for more control when the wallet is
-- used as the backend of a web service.
signWalletTx :: (MonadIO m, PersistUnique b, PersistQuery b)
             => WalletName  -- ^ Wallet name
             -> AccountName -- ^ Account name
             -> Tx          -- ^ Transaction to sign 
             -> ReaderT b m (TxHash, Bool, Bool)
             -- ^ (Signed transaction, Completed flag, New addresses generated)
signWalletTx wallet name tx = do
    Entity wk _ <- getWalletEntity wallet
    Entity ai acc <- getAccountEntity wk name
    (signedTx, _) <- if isReadAccount $ dbAccountValue acc 
        then return (tx, False)
        else signSigBlob wallet name =<< buildSigBlob ai tx
    confM <- importTx signedTx UnknownSource (Just (wallet, name))
    let (tid, conf, genA) = fromJust confM
    when (isNothing confM) $ liftIO $ throwIO $
        WalletException "Transaction is not relevant to this wallet"
    return (tid, conf `elem` [ TxPending, TxBuilding ], genA)

-- | Retrieve the 'OfflineSignData' that can be used to sign a transaction from
-- an offline wallet
getSigBlob :: (MonadIO m, PersistUnique b, PersistQuery b)
           => WalletName
           -> AccountName
           -> TxHash
           -> ReaderT b m SigBlob
getSigBlob wallet name tid = do
    Entity wk _ <- getWalletEntity wallet
    Entity ai _ <- getAccountEntity wk name
    tx <- getTx tid
    buildSigBlob ai tx 

-- Build an unsigned transaction given a list of recipients and a fee
buildUnsignedTx :: (MonadIO m, PersistUnique b, PersistQuery b)
                => Entity (DbAccountGeneric b)
                -> Word32
                -> [(Address,Word64)] 
                -> Word64
                -> ReaderT b m Tx
buildUnsignedTx accE@(Entity ai acc) minConf dests fee = do
    spendable <- spendableCoins ai minConf
    let msParam = ( accountRequired $ dbAccountValue acc
                  , accountTotal $ dbAccountValue acc
                  )
        resE | isMSAccount $ dbAccountValue acc = 
                chooseMSCoins tot fee msParam spendable
             | otherwise = chooseCoins tot fee spendable
        (coins, change)  = fromRight resE
    when (isLeft resE) $ liftIO $ throwIO $ WalletException $ fromLeft resE
    -- TODO: Put this value in a constant file somewhere
    -- TODO: We need a better way to identify dust transactions
    recips <- if change < 5430 then return dests else do
        cAddr <- internalAddr accE -- internal address
        -- TODO: Change must be randomly placed
        return $ dests ++ [(dbAddressValue $ cAddr,change)]
    let txE = buildAddrTx (map coinOutPoint coins) $ map f recips
    when (isLeft txE) $ liftIO $ throwIO $ WalletException $ fromLeft txE
    return $ fromRight txE
  where
    tot     = sum $ map snd dests
    f (a,v) = (addrToBase58 a,v)
    
buildSigBlob :: (MonadIO m, PersistUnique b, PersistQuery b)
             => Key (DbAccountGeneric b)
             -> Tx
             -> ReaderT b m SigBlob
buildSigBlob ai tx = do
    coins <- liftM catMaybes (mapM (getBy . f) $ map prevOutput $ txIn tx)
    -- Filter coins for this account only
    accCoinsDB <- filterM belongs coins
    dat <- mapM toDat $ map entityVal accCoinsDB 
    return $ SigBlob dat tx
  where
    f (OutPoint h i) = CoinOutPoint h (fromIntegral i)
    belongs (Entity ci _) = liftM isJust $ getBy (UniqueCoinAccount ci ai)
    toDat c = do
        a <- liftM fromJust $ getBy $ UniqueAddressAccount (dbCoinAddress c) ai
        return ( coinOutPoint $ dbCoinValue c
               , coinScript $ dbCoinValue c
               , dbAddressInternal $ entityVal a
               , dbAddressIndex $ entityVal a
               )

signSigBlob :: (MonadIO m, PersistUnique b, PersistQuery b)
            => WalletName
            -> AccountName
            -> SigBlob
            -> ReaderT b m (Tx, Bool)
signSigBlob wallet name (SigBlob dat tx) = do
    Entity wk wE  <- getWalletEntity wallet
    Entity _ accE <- getAccountEntity wk name
    let acc = dbAccountValue accE
        w   = dbWalletValue wE
    when (isReadAccount acc) $ liftIO $ throwIO $
        WalletException "This operation is not supported on read-only accounts"
    let master  = walletMasterKey w
        accKey  = fromJust $ accPrvKey master $ accountIndex acc
        f (_,_,internal,k) | internal  = intPrvKey accKey k
                           | otherwise = extPrvKey accKey k
        prvKeys = map (xPrvKey . getAddrPrvKey . fromJust . f) dat
    let sigi = map (toSigi acc) dat
        resE = detSignTx tx sigi prvKeys
        (signedTx, complete) = fromRight resE
    when (isLeft resE) $ liftIO $ throwIO $ WalletException $ fromLeft resE
    return (signedTx, complete)
  where
    toSigi acc (op, so, i, k) = 
        -- TODO: Here we override the SigHash to be SigAll False all the time.
        -- Should we be more flexible?
        SigInput so op (SigAll False) $ getRedeemIndex acc k i 

getProposition :: (MonadIO m, PersistUnique b, PersistQuery b)
               => WalletName -> AccountName -> TxHash -> ReaderT b m Tx
getProposition wallet name tid = do
    tx <- liftM accTx $ getAccTx wallet name tid
    return tx{ txIn = map f $ txIn tx }
  where
    f ti = ti{ scriptInput = BS.empty }

-- | Produces a bloom filter containing all the addresses in this wallet. This
-- includes internal and external addresses. The bloom filter can be set on a
-- peer connection to filter the transactions received by that peer.
walletBloomFilter :: (MonadIO m, PersistUnique b, PersistQuery b) 
                  => Double
                  -> ReaderT b m BloomFilter
walletBloomFilter fpRate = do
    addrs <- selectList [] []
    rdms  <- liftM catMaybes $ forM addrs (getRedeem . entityVal)
    pks   <- liftM catMaybes $ forM addrs (addrPubKey . entityVal)
    -- TODO: Choose a random nonce for the bloom filter
    let len     = length addrs + length rdms + length pks
        -- Create bloom filter
        bloom   = bloomCreate len fpRate 0 BloomUpdateNone
        -- Add the Hash160 of the addresses
        f b a   = bloomInsert b $ encode' $ getAddrHash a
        bloom1  = foldl f bloom $ map (dbAddressValue . entityVal) addrs
        -- Add the redeem scripts
        g b r   = bloomInsert b $ encodeOutputBS r
        bloom2  = foldl g bloom1 rdms
        -- Add the public keys
        h b p   = bloomInsert b $ encode' p
        bloom3  = foldl h bloom2 pks
    return bloom3

-- | Return the creation time (POSIX seconds) of the first key in the wallet.
-- This is used to ignore full/filtered blocks prior to this time.
firstKeyTime :: (MonadIO m, PersistQuery b) => ReaderT b m (Maybe Word32)
firstKeyTime = do
    res <- selectFirst [] [Asc DbAddressCreated] 
    return $ (fromInteger . round . toPOSIX) <$> res
  where
    toPOSIX = utcTimeToPOSIXSeconds . dbAddressCreated . entityVal

-- | Returns true if the transaction is in the wallet
isTxInWallet :: (MonadIO m, PersistUnique b) => TxHash -> ReaderT b m Bool
isTxInWallet tid = liftM isJust $ getBy $ UniqueTx tid

-- TODO: The transactions *need* to be in the wallet already to get their first
-- confirmation mark. Otherwise they will stay unconfirmed forever. Look into
-- this.
-- | Import filtered blocks into the wallet. This will update the confirmations
-- of the relevant transactions.
importBlock :: (MonadIO m, PersistQuery b, PersistUnique b)
            => BlockChainAction -> [TxHash] -> ReaderT b m ()
importBlock action expTxs = do

    -- Insert transaction/block confirmation links. We have to keep this
    -- information even for side blocks as we need it when a reorg occurs.
    time <- liftIO getCurrentTime
    let bid = nodeBlockHash $ getActionNode action
        ts  = blockTimestamp $ nodeHeader $ getActionNode action
    myTxs <- filterM ((liftM isJust) . getBy . UniqueTx) expTxs
    forM_ myTxs $ \h -> insert_ $ DbConfirmation h bid ts time

    case action of
        BestBlock node -> do
            forM_ myTxs $ \h -> do
                -- Mark all conflicts as dead
                conflicts <- getConflicts h
                forM_ conflicts $ \c -> 
                    updateWhere [ DbTxHash ==. c ]
                                [ DbTxConfidence =. TxDead ]
                -- Confidence in this transaction is now building up
                updateWhere 
                    [ DbTxHash ==. h ]
                    [ DbTxConfirmedBy     =. Just (nodeBlockHash node)
                    , DbTxConfirmedHeight =. Just (nodeHeaderHeight node)
                    , DbTxConfidence      =. TxBuilding
                    ]
            setBestHash (nodeHeaderHeight node) (nodeBlockHash node)
        BlockReorg _ o n -> do
            -- Unconfirm transactions from the old chain
            forM_ (reverse o) $ \b -> do
                otxs <- selectList 
                    [ DbTxConfirmedBy ==. Just (nodeBlockHash b) ] []
                forM_ otxs $ \(Entity oKey _) -> do
                    update oKey [ DbTxConfirmedBy     =. Nothing
                                , DbTxConfirmedHeight =. Nothing
                                , DbTxConfidence      =. TxPending
                                ]
                -- Update conflicted transactions
                forM_ otxs $ \(Entity _ otx) -> do
                    conflicts <- getConflicts $ dbTxHash otx
                    forM_ conflicts reviveTransaction

            -- Confirm transactions in the new chain. This will also confirm
            -- the transactions in the best block of the new chain as we
            -- inserted it in DbConfirmations at the start of the function.
            forM_ n $ \b -> do
                cnfs <- selectList 
                            [ DbConfirmationBlock ==. nodeBlockHash b ] []
                let ntxs = map (dbConfirmationTx . entityVal) cnfs 
                forM_ ntxs $ \h -> do
                    -- Mark all conflicts as dead
                    conflicts <- getConflicts h
                    forM_ conflicts $ \c -> 
                        updateWhere [ DbTxHash ==. c ]
                                    [ DbTxConfidence =. TxDead ]
                    updateWhere 
                        [ DbTxHash ==. h ] 
                        [ DbTxConfirmedBy     =. Just (nodeBlockHash b)
                        , DbTxConfirmedHeight =. Just (nodeHeaderHeight b)
                        , DbTxConfidence      =. TxBuilding
                        ]

            setBestHash (nodeHeaderHeight $ last n) (nodeBlockHash $ last n)

        SideBlock _ -> return ()

-- If a transaction is Dead but has no more conflicting Building transactions,
-- we update the status to Pending
reviveTransaction :: (MonadIO m, PersistUnique b, PersistQuery b)
                  => TxHash -> ReaderT b m ()
reviveTransaction tid = do
    Entity tKey tx <- getTxEntity tid
    when (dbTxConfidence tx == TxDead) $ do
        conflicts <- getConflicts tid
        res <- filterM isTxBuilding conflicts
        when (null res) $ update tKey [ DbTxConfidence =. TxPending ]

getBestHeight :: (MonadIO m, PersistQuery b) => ReaderT b m Word32
getBestHeight = do
    cnf <- selectFirst [] []
    when (isNothing cnf) $ liftIO $ throwIO $
        WalletException "getBestHeight: Database not initialized"
    return $ dbConfigBestHeight $ entityVal $ fromJust cnf

setBestHash :: (MonadIO m, PersistQuery b) 
            => Word32 -> BlockHash -> ReaderT b m ()
setBestHash i h = updateWhere [] [ DbConfigBestHeight    =. i
                                 , DbConfigBestBlockHash =. h
                                 ]

getConflicts :: (MonadIO m, PersistQuery b) => TxHash -> ReaderT b m [TxHash]
getConflicts h = do
    asE <- selectList [DbTxConflictFst ==. h] []
    bsE <- selectList [DbTxConflictSnd ==. h] []
    let as = map (dbTxConflictSnd . entityVal) asE
        bs = map (dbTxConflictFst . entityVal) bsE
    return $ L.nub $ as ++ bs

isTxDead :: (MonadIO m, PersistUnique b, PersistQuery b)
         => TxHash -> ReaderT b m Bool
isTxDead h = do
    Entity _ tx <- getTxEntity h
    return $ dbTxConfidence tx == TxDead

isTxBuilding :: (MonadIO m, PersistUnique b, PersistQuery b)
             => TxHash -> ReaderT b m Bool
isTxBuilding h = do
    Entity _ tx <- getTxEntity h
    return $ dbTxConfidence tx == TxBuilding

isTxOffline :: (MonadIO m, PersistUnique b, PersistQuery b)
            => TxHash -> ReaderT b m Bool
isTxOffline h = do
    Entity _ tx <- getTxEntity h
    return $ dbTxConfidence tx == TxOffline

-- Coin functions

-- | Returns the true balances of an address. If a balance can not be computed
-- deterministically (due to conflicting transactions), this function will
-- return BalanceConflict where appropriate together with a list of conflicting
-- transactions. This functions returns a final balance, a total received
-- balance and a list of funding, spending and conflicting transactions.
-- Funding transactions are transactions funding this address and spending
-- transactions are transactions spending from this address.
addressBalance :: (PersistQuery b, PersistUnique b, MonadIO m) 
               => PaymentAddress -- ^ Payment Address
               -> Word32         -- ^ Minimum confirmations
               -> ReaderT b m BalanceAddress -- ^ Address balance
addressBalance pa minConf = do
    -- Find coins related to the address
    allCoins <- liftM (map entityVal) $ selectList [DbCoinAddress ==. a] [] 
    (fb, tr, ft, st, ct) <- getBalanceData allCoins minConf
    return $ BalanceAddress
        { balanceAddress = pa
        , finalBalance   = fb
        , totalReceived  = tr
        , fundingTxs     = ft
        , spendingTxs    = st
        , conflictTxs    = ct
        }
  where
    a = paymentAddress pa

-- | Returns the true balance of a wallet. If the balance can not be
-- computed deterministically (due to conflicting transactions), this
-- function will return BalanceConflict and a list of conflicting transactions.
accountBalance :: (MonadIO m, PersistUnique b, PersistQuery b)
               => WalletName  -- ^ Wallet name
               -> AccountName -- ^ Account name
               -> Word32      -- ^ Minimum confirmations
               -> ReaderT b m (Balance, [TxHash]) 
                    -- ^ (Account balance, conflicts)
accountBalance wallet name minConf = do
    Entity wk _ <- getWalletEntity wallet
    Entity ai _ <- getAccountEntity wk name
    -- Find coins related to the account
    ais <- selectList [ DbCoinAccountAccount ==. ai ] []
    allCoins <- liftM catMaybes $ forM ais $ get . dbCoinAccountCoin . entityVal
    (fb, _, _, _, ct) <- getBalanceData allCoins minConf
    return (fb, ct)

-- | Return the balance that can be spent by the wallet. We ignore conflicts
-- and coinbase transactions with less than 100 confirmations. This value
-- may not represent the true balance on the wallet. Use 'accountBalance'
-- for a true representation of the balance.
spendableAccountBalance :: (MonadIO m, PersistUnique b, PersistQuery b)
                        => WalletName
                        -> AccountName
                        -> Word32
                        -> ReaderT b m Word64
spendableAccountBalance wallet name minConf = do
    Entity wk _ <- getWalletEntity wallet
    Entity ai _ <- getAccountEntity wk name
    liftM (sum . (map coinValue)) $ spendableCoins ai minConf

-- Compute the balance data form a list of coins
getBalanceData :: (MonadIO m, PersistQuery b, PersistUnique b) 
               => [DbCoinGeneric b]
               -> Word32
               -> ReaderT b m (Balance, Balance, [TxHash], [TxHash], [TxHash])
getBalanceData allCoins minConf = do
    -- Current best height
    bestH <- getBestHeight

    -- filter coins that have the minimum confirmation requirement
    coins <- filterM ((minConfNotDead bestH minConf) . dbCoinHash) allCoins

    -- Find conflicts on transactions creating the coins
    fundRes <- liftM (zip coins) $ mapM (notDeadConfls . dbCoinHash) coins
    let fundConf = concat $ map snd fundRes
        noConf   = map fst $ filter (null . snd) fundRes

    -- Find spending transactions
    spendRes <- liftM (zip coins) $ mapM getSpendTxs coins
    let notSpend = map fst $ filter (null . snd) spendRes
        spend    = filter (not . null . snd) spendRes

    -- Find conflicts on the spending transactions
    sConf <- mapM (\(c,s:_) -> notDeadConfls s >>= \x -> return (c,s,x)) spend
    let spendConf = concat $ map lst3 sConf
        allConf   = L.nub $ fundConf ++ spendConf

    let tr | null fundConf = Balance $ 
                sum $ map (coinValue . dbCoinValue) coins
           | otherwise     = BalanceConflict
        fb | null allConf  = Balance $ 
                sum $ map (coinValue . dbCoinValue) notSpend
           | otherwise     = BalanceConflict

    return ( fb, tr
           , map dbCoinHash noConf
           , map snd3 $ filter (null . lst3) sConf
           , allConf
           )
  where
    -- Find non-dead conflicts
    notDeadConfls h = do
        confls <- getConflicts h
        res <- filterM ((liftM not) . isTxDead) confls
        return $ if null res then [] else h:res
    -- Find non-dead spending transactions
    getSpendTxs c = do
        xs <- findSpendingTxs [coinOutPoint $ dbCoinValue c]
        filterM ((liftM not) . isTxDead) xs

-- Returns coins that have not been spent. A coin spent by a dead transaction
-- is considered unspent. Also doesn't return coins spent by conflicting
-- transactions.
unspentCoins :: (MonadIO m, PersistUnique b, PersistQuery b) 
             => Key (DbAccountGeneric b) -- ^ Account key
             -> Word32                   -- ^ Minimum confirmations
             -> ReaderT b m [Coin]       -- ^ List of unspent coins
unspentCoins ai minConf = do
    -- Current best height
    bestH <- getBestHeight

    -- Find coins for this account
    ais <- selectList [ DbCoinAccountAccount ==. ai ] []
    allCoins <- liftM catMaybes $ forM ais $ get . dbCoinAccountCoin . entityVal

    -- filter coins that have the minimum confirmation requirement
    coins <- filterM ((minConfNotDead bestH minConf) . dbCoinHash) allCoins
    filterM notSpent $ map dbCoinValue coins
  where
    notSpent c = do
        spendTxs <- selectList [DbSpentCoinKey ==. coinOutPoint c] []
        let spendingHs = map (dbSpentCoinTx . entityVal) spendTxs
        liftM null $ filterM ((liftM not) . isTxDead) spendingHs

-- Returns unspent coins that can be spent. For example, coins from 
-- conflicting transactions cannot be spent, even if they are unspent.
spendableCoins :: (MonadIO m, PersistUnique b, PersistQuery b) 
               => Key (DbAccountGeneric b) -- ^ Account key
               -> Word32                   -- ^ Minimum confirmations
               -> ReaderT b m [Coin]       -- ^ List of coins that can be spent
spendableCoins ai minConf = 
    filterM isSpendable =<< unspentCoins ai minConf
  where
    isSpendable c = do
        let tid = outPointHash $ coinOutPoint c
        Entity _ tx <- getTxEntity tid
        confirmations <- getConfirmations tid
        if isCoinbaseTx (dbTxValue tx) && confirmations < 100 
            then return False
            else case dbTxConfidence tx of
                -- Building txs will only have conflicts with Dead txs
                -- so we can spend their coins
                TxBuilding -> return True
                -- Check that pending txs have no conflicts
                TxPending  -> do
                    -- Only allow conflicts with dead transactions
                    conflicts <- getConflicts tid
                    liftM null $ filterM ((liftM not) . isTxDead) conflicts
                _ -> return False

-- Tx is not dead and matches minimum confirmations
minConfNotDead :: (MonadIO m, PersistUnique b, PersistQuery b)
               => BlockHeight -> Word32 -> TxHash -> ReaderT b m Bool
minConfNotDead bestHeight minConf h = do
    Entity _ tx <- getTxEntity h
    let heightM = dbTxConfirmedHeight tx
        conf | isNothing heightM = 0
             | otherwise = bestHeight - (fromJust heightM) + 1 
    return $ dbTxConfidence tx /= TxDead && conf >= minConf

