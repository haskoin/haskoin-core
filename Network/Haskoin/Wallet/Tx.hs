{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Tx
( AccTx(..)
, getTx
, getTxEntity
, toAccTx
, txList
, txPage
, importTx
, removeTx
, sendTx
, signWalletTx
, walletBloomFilter
, isTxInWallet
, firstKeyTime
, importBlock
, getBestHeight
, setBestHeight
) where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, unless, when, liftM, void)
import Control.Monad.Trans (liftIO)
import Control.Exception (throwIO)

import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word32, Word64)
import Data.List ((\\), nub)
import Data.Maybe (catMaybes, isNothing, isJust, fromJust)
import Data.Either (rights)
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
    , deleteBy
    , selectList
    , selectFirst
    , deleteWhere
    , updateWhere
    , update
    , insert_
    , insertUnique
    , replace
    , count
    , (=.), (==.), (<-.)
    , SelectOpt( Asc, Desc, LimitTo, OffsetBy )
    )

import Network.Haskoin.Node.HeaderChain

import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Address
import Network.Haskoin.Wallet.Coin
import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util

toAccTx :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b) 
        => DbAccTxGeneric b -> m AccTx
toAccTx accTx = do
    (Entity _ tx) <- getTxEntity $ dbAccTxHash accTx
    height <- getBestHeight
    let conf | isNothing $ dbTxConfirmedBy tx = 0
             | otherwise = height - (fromJust $ dbTxConfirmedHeight tx) + 1
    return $ AccTx { accTxHash          = dbAccTxHash accTx
                   , accTxRecipients    = dbAccTxRecipients accTx
                   , accTxValue         = dbAccTxValue accTx
                   , accTxOffline       = dbTxOffline tx
                   , accTxConfirmations = fromIntegral conf
                   }

getTxEntity :: (PersistUnique m, PersistMonadBackend m ~ b)
            => TxHash -> m (Entity (DbTxGeneric b))
getTxEntity tid = do
    entM <- getBy $ UniqueTx tid
    when (isNothing entM) $ liftIO $ throwIO $ WalletException $
        unwords ["Transaction", encodeTxHashLE tid, "not in database"]
    return $ fromJust entM

getTx :: PersistUnique m => TxHash -> m Tx
getTx tid = liftM (dbTxValue . entityVal) $ getTxEntity tid

-- | List all the transaction entries for an account. Transaction entries
-- summarize information for a transaction in a specific account only (such as
-- the total movement of for this account).
--
-- Transaction entries can also be tagged as /Partial/. Partial transactions
-- are transactions that are not fully signed yet, such as a partially signed
-- multisignature transaction. Partial transactions are visible in the wallet
-- mostly for informational purposes. They can not generate any coins as the
-- txid or partial transactions will change once they are fully signed.
-- However, importing a partial transaction will /lock/ the coins that it
-- spends so that you don't mistakenly spend them. Partial transactions are
-- replaced once the fully signed transaction is imported.
txList :: (PersistQuery m, PersistUnique m)
       => AccountName  -- ^ Account name
       -> m [AccTx]    -- ^ List of transaction entries
txList name = do
    (Entity ai _) <- getAccountEntity name
    e <- selectList [ DbAccTxAccount ==. ai ] [ Asc DbAccTxCreated ]
    mapM (toAccTx . entityVal) e

-- | Returns a page of transactions for an account. Pages are numbered starting
-- from page 1. Requesting page 0 will return the last page.
txPage :: (PersistQuery m, PersistUnique m)
       => AccountName   -- ^ Account name
       -> Int           -- ^ Requested page number
       -> Int           -- ^ Number of addresses per page
       -> m ([AccTx], Int)
txPage name pageNum resPerPage
    | pageNum < 0 = liftIO $ throwIO $ WalletException $ 
        unwords ["Invalid page number:", show pageNum]
    | resPerPage < 1 = liftIO $ throwIO $ WalletException $
        unwords ["Invalid results per page:",show resPerPage]
    | otherwise = do
        (Entity ai acc) <- getAccountEntity name
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

-- | Import a transaction into the wallet. If the offline transaction flag is
-- set to true, we consider that the transaction does not originate from the
-- bitcoin network. Therefore, it will lock the coins it spends so that you
-- don't mistakely spend them, but it will not generate new coins. A new
-- transaction entry will be created for every account affected by this
-- transaction. Every transaction entry will summarize the information related
-- to its account only (such as total movement for this account).
importTx :: (PersistQuery m, PersistUnique m) 
         => Tx        -- ^ Transaction to import
         -> Bool      -- ^ Is this an offline transaction
         -> m [AccTx] -- ^ New transaction entries created
importTx tx offline = do
    txM <- getBy $ UniqueTx tid
    -- Allow the same transaction to replace itself it the offline flag
    -- was False and now is True
    let updatedFlag = dbTxOffline (entityVal $ fromJust txM) && not offline
    if isJust txM && not updatedFlag then return [] else do
        when (isJust txM) $ removeTx tid >> return ()
        isOrphan <- isOrphanTx tx
        if not isOrphan then addTx tx offline else do
            time <- liftIO getCurrentTime
            insert_ $ DbTx tid tx True offline Nothing Nothing time
            return []
  where
    tid = txHash tx

addTx :: (PersistQuery m, PersistUnique m) 
      => Tx -> Bool -> m [AccTx] 
addTx tx offline = do
    -- Retrieve the coins we have from the transaction inputs
    eCoins <- liftM catMaybes (mapM (getBy . f) $ map prevOutput $ txIn tx)
    let coins = map entityVal eCoins
    when (isDoubleSpend tid coins) $ liftIO $ throwIO $
        WalletException "Transaction is double spending coins"
    -- We must remove offline transactions which spend the same coins as us
    forM_ (txToRemove coins) removeTx 
    -- Change status of the coins
    forM_ eCoins $ \(Entity ci _) -> update ci [DbCoinStatus =. status]
    -- Import new coins 
    outCoins <- liftM catMaybes $ 
        (mapM (importCoin tid (not offline)) $ zip (txOut tx) [0..])
    -- Ignore this transaction if it is not ours
    if null $ coins ++ outCoins then return [] else do
        time <- liftIO getCurrentTime
        -- Save the whole transaction 
        insert_ $ DbTx tid tx False offline Nothing Nothing time 
        -- Build transactions that report on individual accounts
        let dbAccTxs = buildAccTx tx coins outCoins time
        accTxs <- forM dbAccTxs toAccTx
        -- insert account transactions into database
        forM_ dbAccTxs insert_
        -- Re-import orphans
        liftM (accTxs ++) tryImportOrphans
  where
    tid              = txHash tx
    f (OutPoint h i) = CoinOutPoint h (fromIntegral i)
    status | offline   = Reserved tid
           | otherwise = Spent tid

-- Try to re-import all orphan transactions
tryImportOrphans :: (PersistQuery m, PersistUnique m) => m [AccTx]
tryImportOrphans = do
    orphans <- selectList [DbTxOrphan ==. True] []
    res <- forM orphans $ \(Entity _ otx) -> do
        deleteBy $ UniqueTx $ dbTxHash otx
        importTx (dbTxValue otx) $ dbTxOffline otx
    return $ concat res

-- Create a new coin for an output if it is ours. If commit is False, it will
-- not write the coin to the database, it will only return it. We need the coin
-- data for partial transactions (for reporting) but we don't want to store
-- them as they can not be spent.
importCoin :: ( PersistQuery m
              , PersistUnique m
              , PersistMonadBackend m ~ b
              )
           => TxHash -> Bool -> (TxOut, Int)
           -> m (Maybe (DbCoinGeneric b))
importCoin tid commit (tout, i) = do
    dbAddrM <- isMyOutput tout
    let dbAddr = fromJust dbAddrM
        soE    = decodeOutputBS $ scriptOutput tout
        so     = fromRight soE
    if isNothing dbAddrM || isLeft soE then return Nothing else do
        rdm   <- getRedeem dbAddr
        time  <- liftIO getCurrentTime
        let coin   = Coin (outValue tout) so (OutPoint tid $ fromIntegral i) rdm
            add    = dbAddressValue dbAddr
            acc    = dbAddressAccount dbAddr
            dbcoin = DbCoin tid i coin add Unspent acc time
        when commit $ insert_ dbcoin
        adjustLookAhead dbAddr
        return $ Just dbcoin

-- Builds a redeem script given an address. Only relevant for addresses
-- linked to multisig accounts. Otherwise it returns Nothing
getRedeem :: (PersistStore m, PersistMonadBackend m ~ b) 
          => DbAddressGeneric b -> m (Maybe RedeemScript)
getRedeem add = do
    acc <- liftM fromJust (get $ dbAddressAccount add)
    if isMSAccount acc 
        then do
            let key      = accountKey $ dbAccountValue acc
                msKeys   = accountKeys $ dbAccountValue acc
                deriv    = dbAddressIndex add
                addrKeys = fromJust $ f key msKeys deriv
                pks      = map (xPubKey . getAddrPubKey) addrKeys
                req      = accountRequired $ dbAccountValue acc
            return $ Just $ sortMulSig $ PayMulSig pks req
        else return Nothing
  where
    f = if dbAddressInternal add then intMulSigKey else extMulSigKey

-- Returns True if the transaction has an input that belongs to the wallet
-- but we don't have a coin for it yet. We are missing a parent transaction.
-- This function will also add the transaction to the orphan pool if it is
-- orphaned and commit is True.
isOrphanTx :: PersistUnique m => Tx -> m Bool
isOrphanTx tx = do
    myInputFlags <- mapM isMyInput $ txIn tx
    coinsM       <- mapM (getBy . f) $ map prevOutput $ txIn tx
    let missing = filter g $ zip myInputFlags coinsM
    return $ length missing > 0
  where
    f (OutPoint h i)  = CoinOutPoint h (fromIntegral i)
    g (isMine, coinM) = isJust isMine && isNothing coinM

-- Returns True if the input address is part of the wallet
isMyInput :: (PersistUnique m, PersistMonadBackend m ~ b) 
          => TxIn -> m (Maybe (DbAddressGeneric b))
isMyInput input = do
    let senderE = scriptSender =<< (decodeToEither $ scriptInput input)
        sender  = fromRight senderE
    if isLeft senderE 
        then return Nothing
        else do
            res <- getBy $ UniqueAddress sender
            return $ entityVal <$> res

-- Returns True if the output address is part of the wallet
isMyOutput :: (PersistUnique m, PersistMonadBackend m ~ b) 
           => TxOut -> m (Maybe (DbAddressGeneric b))
isMyOutput out = do
    let recipientE = scriptRecipient =<< (decodeToEither $ scriptOutput out)
        recipient  = fromRight recipientE
    if isLeft recipientE
        then return Nothing 
        else do
            res <- getBy $ UniqueAddress recipient
            return $ entityVal <$> res

-- |A transaction can not be imported if it double spends coins in the wallet.
-- Upstream code needs to remove the conflicting transaction first using
-- dbTxRemove function
-- TODO: We need to consider malleability here
isDoubleSpend :: TxHash -> [DbCoinGeneric b] -> Bool
isDoubleSpend tid coins = any (f . dbCoinStatus) coins
  where
    f (Spent parent) = parent /= tid
    f _              = False

-- When a transaction spends coins previously spent by an offline transaction,
-- we need to remove the offline transactions from the database and try to
-- re-import the transaction. Coins with Reserved status are spent by an
-- offline transaction.
txToRemove :: [DbCoinGeneric b] -> [TxHash]
txToRemove coins = catMaybes $ map (f . dbCoinStatus) coins
  where
    f (Reserved tid) = Just tid
    f _              = Nothing

-- |Group input and output coins by accounts and create 
-- account-level transaction
buildAccTx :: Tx -> [DbCoinGeneric b] -> [DbCoinGeneric b] -> UTCTime 
           -> [DbAccTxGeneric b]
buildAccTx tx inCoins outCoins time = map build $ M.toList oMap
  where
    -- We build a map of accounts to ([input coins], [output coins])
    iMap = foldr (f (\(i,o) x -> (x:i,o))) M.empty inCoins
    oMap = foldr (f (\(i,o) x -> (i,x:o))) iMap outCoins
    f g coin accMap = case M.lookup (dbCoinAccount coin) accMap of
        Just tuple -> M.insert (dbCoinAccount coin) (g tuple coin) accMap
        Nothing    -> M.insert (dbCoinAccount coin) (g ([],[]) coin) accMap
    allRecip = rights $ map toAddr $ txOut tx
    toAddr   = (scriptRecipient =<<) . decodeToEither . scriptOutput
    sumVal   = sum . (map (coinValue . dbCoinValue))
    build (ai,(i,o)) = DbAccTx (txHash tx) recips total ai time
      where
        total = (fromIntegral $ sumVal o) - (fromIntegral $ sumVal i)
        addrs = map dbCoinAddress o
        recips | null addrs = allRecip
               | total < 0  = allRecip \\ addrs -- remove the change
               | otherwise  = addrs

-- | Remove a transaction from the database. This will remove all transaction
-- entries for this transaction as well as any parent transactions and coins
-- deriving from it.
removeTx :: (PersistUnique m, PersistQuery m)
         => TxHash      -- ^ Transaction hash to remove
         -> m [TxHash]  -- ^ List of removed transaction hashes
removeTx tid = do
    -- Find all parents of this transaction
    -- Offline transactions should not have any coins. Won't check for it
    coins <- selectList [ DbCoinHash ==. tid ] []
    let parents = nub $ catStatus $ map (dbCoinStatus . entityVal) coins
    -- Recursively remove parents
    pids <- forM parents removeTx
    -- Delete output coins generated from this transaction
    deleteWhere [ DbCoinHash ==. tid ]
    -- Delete account transactions
    deleteWhere [ DbAccTxHash ==. tid ]
    -- Delete transaction
    deleteWhere [ DbTxHash ==. tid ]
    -- Unspend input coins that were previously spent by this transaction
    updateWhere [ DbCoinStatus <-. [Spent tid, Reserved tid] ]
                [ DbCoinStatus =. Unspent ]
    return $ tid:(concat pids)

-- | Create a transaction sending some coins to a list of recipient addresses.
sendTx :: (PersistUnique m, PersistQuery m)
       => AccountName         -- ^ Account name
       -> [(Address,Word64)]  -- ^ List of recipient addresses and amounts
       -> Word64              -- ^ Fee per 1000 bytes 
       -> m (TxHash, Bool)    -- ^ (Payment transaction, status flag)
sendTx name strDests fee = do
    (coins,recips) <- sendSolution name strDests fee
    resE <- sendCoins coins recips (SigAll False)
    case resE of
        Left err    -> liftIO $ throwIO $ WalletException err
        Right (tx, complete) -> do
            importTx tx $ not complete
            return (txHash tx, complete)

-- Given a list of recipients and a fee, finds a valid combination of coins
sendSolution :: (PersistUnique m, PersistQuery m)
             => AccountName -> [(Address,Word64)] -> Word64
             -> m ([Coin],[(Address,Word64)])
sendSolution name dests fee = do
    (Entity ai acc) <- getAccountEntity name
    unspent <- unspentCoins name
    let msParam = ( accountRequired $ dbAccountValue acc
                  , accountTotal $ dbAccountValue acc
                  )
        resE | isMSAccount acc = chooseMSCoins tot fee msParam unspent
             | otherwise       = chooseCoins tot fee unspent
        (coins, change)        = fromRight resE
    when (isLeft resE) $ liftIO $ throwIO $
        WalletException $ fromLeft resE
    recips <- if change < 5000 then return dests else do
        cAddr <- newAddrsGeneric name 1 True -- internal addresses
        -- TODO: Change must be randomly placed
        return $ dests ++ [(dbAddressValue $ head cAddr,change)]
    return (coins,recips)
  where
    tot        = sum $ map snd dests
    
-- Build and sign a transaction by providing coins and recipients
sendCoins :: PersistUnique m
          => [Coin] -> [(Address,Word64)] -> SigHash
          -> m (Either String (Tx, Bool))
sendCoins coins recipients sh = do
    let txE = buildAddrTx (map coinOutPoint coins) $ map f recipients
        tx  = fromRight txE
    when (isLeft txE) $ liftIO $ throwIO $
        WalletException $ fromLeft txE
    ys <- mapM (getSigData sh) coins
    return $ detSignTx tx (map fst ys) (map snd ys)
  where
    f (a,v) = (addrToBase58 a, v)

-- | Try to sign the inputs of an existing transaction using the private keys
-- of an account. This command will return an indication if the transaction is
-- fully signed or if additional signatures are required. This command will
-- work for both normal inputs and multisignature inputs. Signing is limited to
-- the keys of one account only to allow for more control when the wallet is
-- used as the backend of a web service.
signWalletTx :: (PersistUnique m, PersistQuery m)
             => AccountName      -- ^ Account name
             -> Tx               -- ^ Transaction to sign 
             -> SigHash          -- ^ Signature type to create 
             -> m (TxHash, Bool) -- ^ (Signed transaction, completed flag)
signWalletTx name tx sh = do
    (Entity ai _) <- getAccountEntity name
    coins <- liftM catMaybes (mapM (getBy . f) $ map prevOutput $ txIn tx)
    -- Filter coins for this account only
    let accCoinsDB = filter ((== ai) . dbCoinAccount . entityVal) coins
        accCoins   = map (dbCoinValue . entityVal) accCoinsDB
    ys <- forM accCoins (getSigData sh)
    let resE = detSignTx tx (map fst ys) (map snd ys)
    case resE of
        Left err    -> liftIO $ throwIO $ WalletException err
        Right (t, complete) -> do
            -- TODO: If a complete transaction is imported as offline, it will
            -- not be broadcast to the network. This happens if we do not know
            -- about some of the inputs in the transaction.
            let knowAllInputs = length coins == length (txIn tx)
                online        = complete && knowAllInputs
            importTx t $ not online
            return (txHash t, online)
  where
    f (OutPoint h i) = CoinOutPoint h (fromIntegral i)

-- Given a coin, retrieves the necessary data to sign a transaction
getSigData :: PersistUnique m
           => SigHash -> Coin -> m (SigInput,PrvKey)
getSigData sh coin = do
    (Entity _ w) <- getWalletEntity "main"
    unless (isFullWallet $ dbWalletValue w) $ liftIO $ throwIO $ 
        WalletException "This operation is not supported on read-only wallets"
    (Entity _ add) <- liftM fromJust $ getBy $ UniqueAddress a
    acc <- liftM fromJust (get $ dbAddressAccount add)
    let master = walletMasterKey $ dbWalletValue w
        deriv  = accountIndex $ dbAccountValue acc
        accKey = fromJust $ accPrvKey master deriv
        g      = if dbAddressInternal add then intPrvKey else extPrvKey
        sigKey = fromJust $ g accKey $ dbAddressIndex add
    return (sigi, xPrvKey $ getAddrPrvKey sigKey)
  where
    so   = coinScript coin
    a    = fromRight $ scriptRecipient $ encodeOutput so
    sigi = SigInput so (coinOutPoint coin) sh (coinRedeem coin)

-- | Produces a bloom filter containing all the addresses in this wallet. This
-- includes internal, external and look-ahead addresses. The bloom filter can
-- be set on a peer connection to filter the transactions received by that
-- peer.
walletBloomFilter :: (PersistUnique m, PersistQuery m) => m BloomFilter
walletBloomFilter = do
    addrs <- selectList [] []
    -- TODO: Choose a random nonce for the bloom filter
    let bloom  = bloomCreate (length addrs * 2) 0.001 0 BloomUpdateP2PubKeyOnly
        bloom' = foldl f bloom $ map (dbAddressValue . entityVal) addrs
        f b a  = bloomInsert b $ encode' $ getAddrHash a
    return bloom'

-- | Return the creation time (POSIX seconds) of the first key in the wallet.
-- This is used to ignore full/filtered blocks prior to this time.
firstKeyTime :: PersistQuery m => m (Maybe Word32)
firstKeyTime = do
    res <- selectFirst [] [Asc DbAddressCreated] 
    return $ (fromIntegral . round . toPOSIX) <$> res
  where
    toPOSIX = utcTimeToPOSIXSeconds . dbAddressCreated . entityVal

-- | Returns true if the transaction is in the wallet
isTxInWallet :: PersistUnique m => TxHash -> m Bool
isTxInWallet tid = liftM isJust $ getBy $ UniqueTx tid

-- TODO: The transactions *need* to be in the wallet already to get their first
-- confirmation mark. Otherwise they will stay unconfirmed forever. Look into
-- this.
-- | Import filtered blocks into the wallet. This will update the confirmations
-- of the relevant transactions.
importBlock :: (PersistQuery m, PersistUnique m)
            => BlockChainAction -> [TxHash] -> m ()
importBlock a txs = do
    -- Insert transaction/block confirmation links. We have to keep this
    -- information even for side blocks as we need it when a reorg occurs.
    myTxs <- liftM (map (dbTxHash . entityVal)) $ 
                        selectList [DbTxHash <-. txs] []
    forM_ myTxs $ \x -> insert_ $ DbConfirmation x (nodeBlockHash $ newNode a)
    case a of
        SideBlock _ -> return () -- Do nothing
        BestBlock node -> do
            updateWhere 
                [ DbTxHash <-. myTxs]
                [ DbTxConfirmedBy     =. Just (nodeBlockHash node)
                , DbTxConfirmedHeight =. Just (nodeHeaderHeight node)
                ]
            setBestHeight $ nodeHeaderHeight node
        BlockReorg _ o n -> do
            -- Unconfirm transactions from the old chain
            forM_ (reverse o) $ \x -> do
                updateWhere [ DbTxConfirmedBy ==. Just (nodeBlockHash x) ]
                            [ DbTxConfirmedBy     =. Nothing
                            , DbTxConfirmedHeight =. Nothing
                            ]
            -- Confirm transactions in the new chain This will also confirm
            -- the transactions in the best block of the new chain as we
            -- inserted it in DbConfirmations at the start of the function.
            forM_ n $ \x -> do
                cnfs <- selectList 
                            [DbConfirmationBlock ==. nodeBlockHash x] []
                let ntxs = map (dbConfirmationTx . entityVal) cnfs 
                updateWhere 
                    [ DbTxHash <-. ntxs ] 
                    [ DbTxConfirmedBy     =. Just (nodeBlockHash x)
                    , DbTxConfirmedHeight =. Just (nodeHeaderHeight x)
                    ]
            setBestHeight $ nodeHeaderHeight $ last n
  where
    newNode (BestBlock node) = node
    newNode (SideBlock node) = node
    newNode (BlockReorg _ _ n) = last n

getBestHeight :: PersistQuery m => m Word32
getBestHeight = do
    cnf <- selectFirst [] []
    when (isNothing cnf) $ liftIO $ throwIO $
        WalletException "getBestHeight: Database not initialized"
    return $ dbConfigBestHeight $ entityVal $ fromJust cnf

setBestHeight :: PersistQuery m => Word32 -> m ()
setBestHeight h = updateWhere [] [DbConfigBestHeight =. h]

