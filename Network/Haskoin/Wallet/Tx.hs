{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Tx
( AccTx(..)
, getTx
, getTxEntity
, getConfirmations
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
, importBlocks
, getBestHeight
, setBestHeight
, balance
, unspentCoins
, spendableCoins
) where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, unless, when, liftM, void, filterM)
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
    , Key(..)
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
    , delete
    , (=.), (==.), (<-.)
    , SelectOpt( Asc, Desc, LimitTo, OffsetBy )
    )

import Network.Haskoin.Node.HeaderChain
import Network.Haskoin.Node.Types

import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Address
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
    conf <- getConfirmations $ dbAccTxHash accTx
    return $ AccTx { accTxHash          = dbAccTxHash accTx
                   , accTxRecipients    = dbAccTxRecipients accTx
                   , accTxValue         = dbAccTxValue accTx
                   , accTxConfidence    = dbTxConfidence tx
                   , accIsCoinbase      = dbTxIsCoinbase tx
                   , accTxConfirmations = fromIntegral conf
                   }

getConfirmations :: (PersistUnique m, PersistQuery m) => TxHash -> m Int
getConfirmations h = do
    (Entity _ tx) <- getTxEntity h
    height <- getBestHeight
    return $ if isNothing $ dbTxConfirmedBy tx then 0 else 
        fromIntegral $ height - (fromJust $ dbTxConfirmedHeight tx) + 1

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

-- | Import a transaction into the wallet
importTx :: (PersistQuery m, PersistUnique m) 
         => Tx        -- ^ Transaction to import
         -> TxSource  -- ^ Where does the transaction come from
         -> m (Maybe TxConfidence) -- ^ New transaction entries created
importTx tx source = do
    txM <- getBy $ UniqueTx tid
    let (Entity tkey dbtx) = fromJust txM
    if isJust txM
        then do
            -- Change the confidence from offline to pending if we get a
            -- transaction from the network
            let isOffline = dbTxConfidence dbtx == TxOffline
            if isOffline && source == NetworkSource
                then do
                    replace tkey $ dbtx{ dbTxConfidence = TxPending }
                    return $ Just TxPending
                else return $ Just TxOffline
        else do
            isOrphan <- isOrphanTx tx
            if not isOrphan then addTx tx source else do
                time <- liftIO getCurrentTime
                insert_ $ DbOrphan tid tx source time
                return Nothing
  where
    tid = txHash tx

addTx :: (PersistQuery m, PersistUnique m) 
      => Tx -> TxSource -> m (Maybe TxConfidence)
addTx tx source = do
    -- A non-network transaction can not double spend coins
    checkDoubleSpend tx source
    -- Retrieve the coins we have from the transaction inputs
    coinsE <- liftM catMaybes (mapM (getBy . f) $ map prevOutput $ txIn tx)
    let coins = map entityVal coinsE
    -- Import new coins 
    outCoins <- liftM catMaybes $ mapM (importCoin tid) $ zip (txOut tx) [0..]
    -- Ignore this transaction if it is not ours
    if null coins && null outCoins then return Nothing else do
        time <- liftIO getCurrentTime
        -- We must remove offline transactions which spend the same coins as us
        removeOfflineTxs $ map prevOutput $ txIn tx
        -- Mark all inputs of this transaction as spent
        forM_ (txIn tx) $ \(TxIn op _ _) -> insert_ $ DbSpentCoin op tid time
        -- Update conflicts with other transactions
        conf <- updateConflicts tx (map dbCoinValue coins) source 
        -- Save the transaction 
        let isCB = isCoinbaseTx tx
        insert_ $ DbTx tid tx conf Nothing Nothing isCB time 
        -- Build transactions that report on individual accounts
        let dbAccTxs = buildAccTx tx coins outCoins time
        accTxs <- forM dbAccTxs toAccTx
        -- insert account transactions into database
        forM_ dbAccTxs insert_
        -- Re-import orphans
        tryImportOrphans
        return $ Just conf
  where
    tid              = txHash tx
    f (OutPoint h i) = CoinOutPoint h (fromIntegral i)

checkDoubleSpend :: (PersistUnique m, PersistQuery m)
                 => Tx -> TxSource -> m ()
checkDoubleSpend tx source
    -- We only allow double spends that come from the network
    | source == NetworkSource = return ()
    | otherwise = do
        spent <- findSpendingTxs outpoints
        xs <- forM spent $ \h -> do
            txM <- getBy $ UniqueTx h
            let tx = entityVal $ fromJust $ txM
            -- A coin is spent if the transaction is not offline or dead
            return $ isJust txM && 
                     dbTxConfidence tx `elem` [TxBuilding, TxPending]
        when (or xs) $ liftIO $ throwIO $
            WalletException "Can not import double-spending transaction"
  where
    outpoints = map prevOutput $ txIn tx

updateConflicts :: (PersistUnique m, PersistQuery m)
                => Tx -> [Coin] -> TxSource -> m TxConfidence
updateConflicts tx coins source 
    | source == NetworkSource = do
        -- Find conflicts with transactions that spend the same coins
        confls <- liftM (filter (/= tid)) $ findSpendingTxs outpoints

        -- We are also in conflict with any children of a conflicted tx
        childConfls <- forM confls findChildTxs
        let allConfls = nub $ concat $ confls : childConfls
        isDead <- liftM or $ mapM isTxBuilding allConfls 

        -- A transaction iherits the conflicts of its parent transaction
        txsM <- mapM (getBy . UniqueTx) $ nub $ map outPointHash outpoints
        let txs     = catMaybes txsM
            parDead = any ((== TxDead) . dbTxConfidence . entityVal) txs
        cs <- mapM (getConflicts . dbTxHash . entityVal) txs

        -- Insert unique conflict links
        time  <- liftIO getCurrentTime
        forM (nub $ concat $ allConfls : cs) $ \h -> 
            insert_ $ DbTxConflict tid h time

        -- Compute the confidence
        return $ if isDead || parDead then TxDead else TxPending

    | verifyTx tx vs = return TxPending
    | otherwise      = return TxOffline
  where
    outpoints        = map prevOutput $ txIn tx
    vs               = map f coins
    f (Coin _ s o _) = (encodeOutput s, o)
    tid              = txHash tx

-- Finds all the children of a given transaction recursively
findChildTxs :: (PersistUnique m, PersistQuery m) => TxHash -> m [TxHash]
findChildTxs tid = do
    txM <- getBy $ UniqueTx tid
    let tx        = dbTxValue $ entityVal $ fromJust txM
        len       = fromIntegral $ length (txOut tx)
        outpoints = map (OutPoint tid) [0 .. (len - 1)]
    if isNothing txM then return [] else do
        hs   <- findSpendingTxs outpoints
        rest <- forM hs findChildTxs
        return $ nub $ concat $ hs : rest

-- Returns the transactions that spend the given outpoints
findSpendingTxs :: PersistQuery m => [OutPoint] -> m [TxHash]
findSpendingTxs outpoints = do
    res <- selectList [DbSpentCoinKey <-. outpoints] []
    return $ nub $ map (dbSpentCoinTx . entityVal) res

-- Try to re-import all orphan transactions
tryImportOrphans :: (PersistQuery m, PersistUnique m) => m ()
tryImportOrphans = do
    orphans <- selectList [] []
    -- TODO: Can we use deleteWhere [] ?
    forM_ orphans $ delete . entityKey
    forM_ orphans $ \(Entity _ otx) -> do
        importTx (dbOrphanValue otx) $ dbOrphanSource otx

removeOfflineTxs :: (PersistUnique m, PersistQuery m) => [OutPoint] -> m ()
removeOfflineTxs outpoints = do
    hs  <- findSpendingTxs outpoints
    txs <- liftM catMaybes $ mapM (getBy . UniqueTx) hs
    let offline = filter ((== TxOffline) . dbTxConfidence . entityVal) txs
    forM_ offline $ removeTx . dbTxHash . entityVal

isCoinbaseTx :: Tx -> Bool
isCoinbaseTx (Tx _ tin _ _) =
    length tin == 1 && (outPointHash $ prevOutput $ head tin) == 0x00

-- Create a new coin for an output if it is ours
importCoin :: ( PersistQuery m
              , PersistUnique m
              , PersistMonadBackend m ~ b
              )
           => TxHash -> (TxOut, Int) -> m (Maybe (DbCoinGeneric b))
importCoin tid (tout, i) = do
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
            dbcoin = DbCoin tid i coin add acc time
        insert_ dbcoin
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
isOrphanTx :: PersistUnique m => Tx -> m Bool
isOrphanTx tx = do
    myInputFlags <- mapM isMyInput $ txIn tx
    coinsM       <- mapM (getBy . f) $ map prevOutput $ txIn tx
    missing      <- filterM g $ zip myInputFlags coinsM
    return $ length missing > 0
  where
    f (OutPoint h i)   = CoinOutPoint h (fromIntegral i)
    g (Just _, Just c) = isTxOffline $ dbCoinHash $ entityVal c
    g (Just _, Nothing) = return True
    g _ = return False

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
               | total < 0 = 
                   let xs = allRecip \\ addrs -- Remove the change
                   in if null xs then allRecip else xs
               | otherwise = addrs

-- | Remove a transaction from the database. This will remove all transaction
-- entries for this transaction as well as any child transactions and coins
-- deriving from it.
removeTx :: (PersistUnique m, PersistQuery m)
         => TxHash      -- ^ Transaction hash to remove
         -> m [TxHash]  -- ^ List of removed transaction hashes
removeTx tid = do
    txM <- getBy $ UniqueTx tid
    let tx        = dbTxValue $ entityVal $ fromJust txM
        len       = fromIntegral $ length (txOut tx)
        outpoints = map (OutPoint tid) [0 .. (len - 1)]
    if isNothing txM then return [] else do 
        childs <- findSpendingTxs outpoints
        -- Recursively remove children
        cids <- forM childs removeTx
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
sendTx :: (PersistUnique m, PersistQuery m)
       => AccountName         -- ^ Account name
       -> [(Address,Word64)]  -- ^ List of recipient addresses and amounts
       -> Word64              -- ^ Fee per 1000 bytes 
       -> m (TxHash, Bool)    -- ^ (Payment transaction, Completed flag)
sendTx name strDests fee = do
    (coins,recips) <- sendSolution name strDests fee
    resE <- sendCoins coins recips (SigAll False)
    case resE of
        Left err -> liftIO $ throwIO $ WalletException err
        Right (tx, complete) -> do
            confM <- importTx tx WalletSource
            let conf = fromJust confM
            return (txHash tx, isJust confM && conf == TxPending)

-- Given a list of recipients and a fee, finds a valid combination of coins
sendSolution :: (PersistUnique m, PersistQuery m)
             => AccountName -> [(Address,Word64)] -> Word64
             -> m ([Coin],[(Address,Word64)])
sendSolution name dests fee = do
    (Entity ai acc) <- getAccountEntity name
    spendable <- spendableCoins name
    let msParam = ( accountRequired $ dbAccountValue acc
                  , accountTotal $ dbAccountValue acc
                  )
        resE | isMSAccount acc = chooseMSCoins tot fee msParam spendable
             | otherwise       = chooseCoins tot fee spendable
        (coins, change)        = fromRight resE
    when (isLeft resE) $ liftIO $ throwIO $
        WalletException $ fromLeft resE
    -- TODO: Put this value in a constant file somewhere
    -- TODO: We need a better way to identify dust transactions
    recips <- if change < 5430 then return dests else do
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
             -> m (TxHash, Bool) -- ^ (Signed transaction, Completed flag)
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
        Right (t,_) -> do
            confM <- importTx t UnknownSource
            let conf = fromJust confM
            return (txHash t, isJust confM && conf == TxPending)
  where
    f (OutPoint h i) = CoinOutPoint h (fromIntegral i)

-- Given a coin, retrieves the necessary data to sign a transaction
getSigData :: PersistUnique m
           => SigHash -> Coin -> m (SigInput,PrvKey)
getSigData sh coin = do
    (Entity _ add) <- liftM fromJust $ getBy $ UniqueAddress a
    acc <- liftM fromJust (get $ dbAddressAccount add)
    w   <- liftM fromJust $ get (dbAccountWallet acc)
    unless (isFullWallet $ dbWalletValue w) $ liftIO $ throwIO $ 
        WalletException "This operation is not supported on read-only wallets"
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
walletBloomFilter :: (PersistUnique m, PersistQuery m) 
                  => Double
                  -> m BloomFilter
walletBloomFilter fpRate = do
    addrs <- selectList [] []
    rdms  <- liftM catMaybes $ forM addrs (getRedeem . entityVal)
    -- TODO: Choose a random nonce for the bloom filter
    let len     = length addrs + length rdms
        bloom   = bloomCreate len fpRate 0 BloomUpdateNone
        bloom'  = foldl f bloom $ map (dbAddressValue . entityVal) addrs
        f b a   = bloomInsert b $ encode' $ getAddrHash a
        bloom'' = foldl g bloom' rdms
        g b r   = bloomInsert b $ encodeOutputBS r
    return bloom''

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
importBlocks :: (PersistQuery m, PersistUnique m)
             => [(BlockChainAction, [TxHash])] -> m ()
importBlocks xs = do
    hsM <- forM xs $ \(a, txs) -> do
        -- Insert transaction/block confirmation links. We have to keep this
        -- information even for side blocks as we need it when a reorg occurs.
        myTxsEnt <- liftM catMaybes $ forM txs $ \tx -> getBy $ UniqueTx tx
        let myTxs = map (dbTxHash . entityVal) myTxsEnt
        forM_ myTxs $ \h -> insert_ $ 
            DbConfirmation h (nodeBlockHash $ newNode a)
        case a of
            SideBlock _ -> return Nothing
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
                return $ Just $ nodeHeaderHeight node
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
                return $ Just $ nodeHeaderHeight $ last n
    -- Update best height
    let hs = catMaybes hsM
        m  = foldl max 0 hs
    unless (null hs) $ setBestHeight m
  where
    newNode (BestBlock node) = node
    newNode (SideBlock node) = node
    newNode (BlockReorg _ _ n) = last n

-- If a transaction is Dead but has no more conflicting Building transactions,
-- we update the status to Pending
reviveTransaction :: (PersistUnique m, PersistQuery m) => TxHash -> m ()
reviveTransaction tid = do
    (Entity tKey tx) <- getTxEntity tid
    when (dbTxConfidence tx == TxDead) $ do
        conflicts <- getConflicts tid
        res <- filterM isTxBuilding conflicts
        when (null res) $ update tKey [ DbTxConfidence =. TxPending ]

getBestHeight :: PersistQuery m => m Word32
getBestHeight = do
    cnf <- selectFirst [] []
    when (isNothing cnf) $ liftIO $ throwIO $
        WalletException "getBestHeight: Database not initialized"
    return $ dbConfigBestHeight $ entityVal $ fromJust cnf

setBestHeight :: PersistQuery m => Word32 -> m ()
setBestHeight h = updateWhere [] [DbConfigBestHeight =. h]

getConflicts :: PersistQuery m => TxHash -> m [TxHash]
getConflicts h = do
    asE <- selectList [DbTxConflictFst ==. h] []
    bsE <- selectList [DbTxConflictSnd ==. h] []
    let as = map (dbTxConflictSnd . entityVal) asE
        bs = map (dbTxConflictFst . entityVal) bsE
    return $ nub $ as ++ bs

isTxDead :: PersistUnique m => TxHash -> m Bool
isTxDead h = do
    (Entity _ tx) <- getTxEntity h
    return $ dbTxConfidence tx == TxDead

isTxBuilding :: PersistUnique m => TxHash -> m Bool
isTxBuilding h = do
    (Entity _ tx) <- getTxEntity h
    return $ dbTxConfidence tx == TxBuilding

isTxOffline :: PersistUnique m => TxHash -> m Bool
isTxOffline h = do
    (Entity _ tx) <- getTxEntity h
    return $ dbTxConfidence tx == TxOffline

-- Coin functions

-- | Returns the balance of an account.
balance :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
        => AccountName -- ^ Account name
        -> m Word64    -- ^ Account balance
balance name = do
    (Entity ai _) <- getAccountEntity name
    coins <- spendableCoins name
    return $ sum $ map coinValue coins

-- Returns coins that have not been spent. A coin spent by a dead transaction
-- is considered unspent.
unspentCoins :: (PersistUnique m, PersistQuery m) 
             => AccountName   -- ^ Account name
             -> m [Coin]      -- ^ List of unspent coins
unspentCoins name = do
    (Entity ai _) <- getAccountEntity name
    coinsE <- selectList [ DbCoinAccount ==. ai ] [Asc DbCoinCreated]
    let coins = map (dbCoinValue . entityVal) coinsE
    resM <- forM coins $ \c -> do
        spent <- selectList [DbSpentCoinKey ==. coinOutPoint c] []
        let hs = nub $ map (dbSpentCoinTx . entityVal) spent
        txsM <- mapM (getBy . UniqueTx) hs
        let confidences = map (dbTxConfidence . entityVal) $ catMaybes txsM
        return $ if all (== TxDead) confidences then Just c else Nothing
    return $ catMaybes resM

-- Returns unspent coins that can be spent. For example, coins from 
-- conflicting transactions cannot be spent, even if they are unspent.
spendableCoins :: (PersistUnique m, PersistQuery m) 
               => AccountName   -- ^ Account name
               -> m [Coin]      -- ^ List of coins that can be spent
spendableCoins name = do
    coins <- unspentCoins name
    res <- forM coins $ \c -> do
        let tid = outPointHash $ coinOutPoint c
        (Entity _ tx) <- getTxEntity tid
        confirmations <- getConfirmations tid
        if isCoinbaseTx (dbTxValue tx) && confirmations < 100 
            then return Nothing 
            else case dbTxConfidence tx of
                -- Building txs will only have conflicts with Dead txs
                -- so we can spend their coins
                TxBuilding -> return $ Just c
                -- Check that pending txs have no conflicts
                TxPending  -> do
                    conflicts <- getConflicts tid
                    -- Only allow conflicts with dead transactions
                    res <- filterM ((liftM not) . isTxDead) conflicts
                    return $ if null res then Just c else Nothing
                _ -> return Nothing
    return $ catMaybes res

