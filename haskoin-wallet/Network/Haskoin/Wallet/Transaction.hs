module Network.Haskoin.Wallet.Transaction
(
-- *Database transactions
  txs
, addrTxs
, getTx
, getAccountTx
, importTx
, importNetTx
, signAccountTx
, createTx
, signOfflineTx
, getOfflineTxData
, killTxs
, reviveTx
, getPendingTxs
, deleteTx

-- *Database blocks
, importMerkles
, getBestBlock

-- *Database coins and balances
, spendableCoins
, accountBalance
, addressBalances

-- *Rescan
, resetRescan

-- *Helpers
, InCoinData(..)
) where

import Control.Arrow (second)
import Control.Monad (forM, forM_, when, unless)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (TBMChan, writeTBMChan)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Exception (throwIO, throw)

import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word32, Word64)
import Data.Either (rights)
import Data.List ((\\), nub, nubBy, find)
import Data.Maybe (isNothing, isJust, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as M
    ( Map, toList, map, unionWith, fromListWith )
import Data.String.Conversions (cs)

import qualified Database.Persist as P
    ( Filter, selectFirst, deleteWhere, insertBy)
import Database.Esqueleto
    ( Value(..), SqlQuery, SqlExpr
    , InnerJoin(..), LeftOuterJoin(..), OrderBy, update, sum_, groupBy
    , select, from, where_, val, valList, sub_select, countRows, count
    , orderBy, limit, asc, desc, set, offset, delete, countDistinct
    , in_, unValue, not_, coalesceDefault, just, on
    , case_, when_, then_, else_, distinct
    , (^.), (=.), (==.), (&&.), (||.), (<.)
    , (<=.), (>=.), (-.), (?.), (!=.)
    -- Reexports from Database.Persist
    , SqlPersistT, Entity(..)
    , getBy, replace
    )
import qualified Database.Esqueleto as E (isNothing)

import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Node.STM
import Network.Haskoin.Node.HeaderTree

import Network.Haskoin.Wallet.Accounts
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

-- Input coin type with transaction and address information
data InCoinData = InCoinData
    { inCoinDataCoin :: !(Entity WalletCoin)
    , inCoinDataTx   :: !WalletTx
    , inCoinDataAddr :: !WalletAddr
    }

instance Coin InCoinData where
    coinValue (InCoinData (Entity _ c) _ _) = walletCoinValue c

-- Output coin type with address information
data OutCoinData = OutCoinData
    { outCoinDataAddr   :: !(Entity WalletAddr)
    , outCoinDataPos    :: !KeyIndex
    , outCoinDataValue  :: !Word64
    , outCoinDataScript :: !ScriptOutput
    }

{- List transactions -}

-- | Get transactions.
txs :: MonadIO m
    => AccountId        -- ^ Account ID
    -> ListRequest      -- ^ List request
    -> SqlPersistT m ([WalletTx], Word32)
    -- ^ List result
txs ai ListRequest{..} = do
    cntRes <- select $ from $ \t -> do
        where_ $ t ^. WalletTxAccount ==. val ai
        return countRows

    let cnt = maybe 0 unValue $ listToMaybe cntRes

    when (listOffset > 0 && listOffset >= cnt) $ throw $ WalletException
        "Offset beyond end of data set"

    res <- fmap (map entityVal) $ select $ from $ \t -> do
        where_ $ t ^. WalletTxAccount ==. val ai
        let order = if listReverse then asc else desc
        orderBy [ order (t ^. WalletTxId) ]
        limit $ fromIntegral listLimit
        offset $ fromIntegral listOffset
        return t

    return (res, cnt)

{- List transactions for an account and address -}

addrTxs :: MonadIO m
        => Entity Account        -- ^ Account entity
        -> Entity WalletAddr     -- ^ Address entity
        -> ListRequest           -- ^ List request
        -> SqlPersistT m ([WalletTx], Word32)
addrTxs (Entity ai _) (Entity addrI WalletAddr{..}) ListRequest{..} = do
    let joinSpentCoin c2 s =
                c2 ?. WalletCoinAccount ==. s ?. SpentCoinAccount
            &&. c2 ?. WalletCoinHash    ==. s ?. SpentCoinHash
            &&. c2 ?. WalletCoinPos     ==. s ?. SpentCoinPos
            &&. c2 ?. WalletCoinAddr    ==. just (val addrI)
        joinSpent s t =
            s ?. SpentCoinSpendingTx ==. just (t ^. WalletTxId)
        joinCoin c t =
                c ?. WalletCoinTx   ==. just (t ^. WalletTxId)
            &&. c ?. WalletCoinAddr ==. just (val addrI)
        joinAll t c c2 s = do
            on $ joinSpentCoin c2 s
            on $ joinSpent s t
            on $ joinCoin c t
        tables f = from $ \(t `LeftOuterJoin` c `LeftOuterJoin`
                            s `LeftOuterJoin` c2) -> f t c s c2
        query t c s c2 = do
            joinAll t c c2 s
            where_ (   t ^. WalletTxAccount ==. val ai
                    &&. (   not_ (E.isNothing (c  ?. WalletCoinId))
                        ||. not_ (E.isNothing (c2 ?. WalletCoinId))
                        )
                    )
            orderBy [(if listReverse then desc else asc) (t ^. WalletTxId)]


    cntRes <- select $ tables $ \t c s c2 -> do
        query t c s c2
        return $ countDistinct $ t ^. WalletTxId

    let cnt = maybe 0 unValue $ listToMaybe cntRes

    when (listOffset > 0 && listOffset >= cnt) $ throw $ WalletException
        "Offset beyond end of data set"

    -- Find all the tids
    res <- select $ distinct $ tables $ \t c s c2 -> do
         query t c s c2
         offset $ fromIntegral listOffset
         limit $ fromIntegral listLimit
         return t

    return (map (updBals . entityVal) res, cnt)

  where
    agg = sum . mapMaybe addressInfoValue .
        filter ((== walletAddrAddress) . addressInfoAddress)
    updBals t =
      let
        input  = agg $ walletTxInputs  t
        output = agg $ walletTxOutputs t
        change = agg $ walletTxChange  t
      in
        t { walletTxInValue  = output + change
          , walletTxOutValue = input
          }


-- Helper function to get a transaction from the wallet database. The function
-- will look across all accounts and return the first available transaction. If
-- the transaction does not exist, this function will throw a wallet exception.
getTx :: MonadIO m => TxHash -> SqlPersistT m (Maybe Tx)
getTx txid =
    fmap (listToMaybe . map unValue) $ select $ from $ \t -> do
        where_ $ t ^. WalletTxHash ==. val txid
        limit 1
        return $ t ^. WalletTxTx

getAccountTx :: MonadIO m
             => AccountId -> TxHash -> SqlPersistT m WalletTx
getAccountTx ai txid = do
    res <- select $ from $ \t -> do
        where_ (   t ^. WalletTxAccount ==. val ai
               &&. t ^. WalletTxHash    ==. val txid
               )
        return t
    case res of
        (Entity _ tx:_) -> return tx
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Transaction does not exist:", cs $ txHashToHex txid ]

-- Helper function to get all the pending transactions from the database. It is
-- used to re-broadcast pending transactions in the wallet that have not been
-- included into blocks yet.
getPendingTxs :: MonadIO m => Int -> SqlPersistT m [TxHash]
getPendingTxs i =
    fmap (map unValue) $ select $ from $ \t -> do
        where_ $ t ^. WalletTxConfidence ==. val TxPending
        when (i > 0) $ limit $ fromIntegral i
        return $ t ^. WalletTxHash

{- Transaction Import -}

-- | Import a transaction into the wallet from an unknown source. If the
-- transaction is standard, valid, all inputs are known and all inputs can be
-- spent, then the transaction will be imported as a network transaction.
-- Otherwise, the transaction will be imported into the local account as an
-- offline transaction.
importTx :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
         => Tx               -- ^ Transaction to import
         -> AccountId        -- ^ Account ID
         -> SqlPersistT m ([WalletTx], [WalletAddr])
         -- ^ New transactions and addresses created
importTx tx ai = importTx' tx ai =<< getInCoins tx (Just ai)

importTx' :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
          => Tx                   -- ^ Transaction to import
          -> AccountId            -- ^ Account ID
          -> [InCoinData]         -- ^ Input coins
          -> SqlPersistT m ([WalletTx], [WalletAddr])
             -- ^ Transaction hash (after possible merges)
importTx' origTx ai origInCoins = do
    -- Merge the transaction with any previously existing transactions
    mergeResM <- mergeNoSigHashTxs ai origTx origInCoins
    let tx       = fromMaybe origTx mergeResM
        origTxid = txHash origTx
        txid     = txHash tx

    -- If the transaction was merged into a new transaction,
    -- update the old hashes to the new ones. This allows us to
    -- keep the spending information of our coins. It is thus possible
    -- to spend partially signed multisignature transactions (as offline
    -- transactions) even before all signatures have arrived.
    inCoins <- if origTxid == txid then return origInCoins else do
        -- Update transactions
        update $ \t -> do
            set t [ WalletTxHash =. val txid
                  , WalletTxTx   =. val tx
                  ]
            where_ (   t ^. WalletTxAccount ==. val ai
                   &&. t ^. WalletTxHash    ==. val origTxid
                   )
        -- Update coins
        update $ \t -> do
            set t [ WalletCoinHash =. val txid ]
            where_ (   t ^. WalletCoinAccount ==. val ai
                   &&. t ^. WalletCoinHash    ==. val origTxid
                   )
        let f (InCoinData c t x) = if walletTxHash t == origTxid
                then InCoinData c
                     t{ walletTxHash = txid, walletTxTx = tx } x
                else InCoinData c t x
        return $ map f origInCoins

    spendingTxs <- getSpendingTxs tx (Just ai)

    let validTx = verifyStdTx tx $ map toVerDat inCoins
        validIn = length inCoins == length (txIn tx)
               && canSpendCoins inCoins spendingTxs False
    if validIn && validTx
        then importNetTx tx Nothing
        else importOfflineTx tx ai inCoins spendingTxs
  where
    toVerDat (InCoinData (Entity _ c) t _) =
        (walletCoinScript c, OutPoint (walletTxHash t) (walletCoinPos c))

-- Offline transactions are usually multisignature transactions requiring
-- additional signatures. This function will merge the signatures of
-- the same offline transactions together into one single transaction.
mergeNoSigHashTxs :: MonadIO m
                  => AccountId
                  -> Tx
                  -> [InCoinData]
                  -> SqlPersistT m (Maybe Tx)
mergeNoSigHashTxs ai tx inCoins = do
    prevM <- getBy $ UniqueAccNoSig ai $ nosigTxHash tx
    return $ case prevM of
        Just (Entity _ prev) -> case walletTxConfidence prev of
            TxOffline -> eitherToMaybe $
                mergeTxs [tx, walletTxTx prev] outPoints
            _ -> Nothing
        -- Nothing to merge. Return the original transaction.
        _ -> Nothing
  where
    buildOutpoint c t = OutPoint (walletTxHash t) (walletCoinPos c)
    f (InCoinData (Entity _ c) t _) = (walletCoinScript c, buildOutpoint c t)
    outPoints = map f inCoins

-- | Import an offline transaction into a specific account. Offline transactions
-- are imported either manually or from the wallet when building a partially
-- signed multisignature transaction. Offline transactions are only imported
-- into one specific account. They will not affect the input or output coins
-- of other accounts, including read-only accounts that may watch the same
-- addresses as this account.
--
-- We allow transactions to be imported manually by this function (unlike
-- `importNetTx` which imports only transactions coming from the network). This
-- means that it is possible to import completely crafted and invalid
-- transactions into the wallet. It is thus important to limit the scope of
-- those transactions to only the specific account in which it was imported.
--
-- This function will not broadcast these transactions to the network as we
-- have no idea if they are valid or not. Transactions are broadcast from the
-- transaction creation function and only if the transaction is complete.
importOfflineTx
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
    => Tx
    -> AccountId
    -> [InCoinData]
    -> [Entity WalletTx]
    -> SqlPersistT m ([WalletTx], [WalletAddr])
importOfflineTx tx ai inCoins spendingTxs = do
    -- Get all the new coins to be created by this transaction
    outCoins <- getNewCoins tx $ Just ai
    -- Only continue if the transaction is relevant to the account
    when (null inCoins && null outCoins) err
    -- Find the details of an existing transaction if it exists.
    prevM <- fmap (fmap entityVal) $ getBy $ UniqueAccTx ai txid
    -- Check if we can import the transaction
    unless (canImport $ walletTxConfidence <$> prevM) err
    -- Kill transactions that are spending our coins
    killTxIds $ map entityKey spendingTxs
    -- Create all the transaction records for this account.
    -- This will spend the input coins and create the output coins
    txsRes <- buildAccTxs tx TxOffline inCoins outCoins
    -- use the addresses (refill the gap addresses)
    newAddrs <- forM (nubBy sameKey $ map outCoinDataAddr outCoins) $
        useAddress . entityVal
    return (txsRes, concat newAddrs)
  where
    txid = txHash tx
    canImport prevConfM =
        -- We can only re-import offline txs through this function.
        (isNothing prevConfM || prevConfM == Just TxOffline) &&
        -- Check that all coins can be spent. We allow offline
        -- coins to be spent by this function unlike importNetTx.
        canSpendCoins inCoins spendingTxs True
    sameKey e1 e2 = entityKey e1 == entityKey e2
    err  = liftIO . throwIO $ WalletException
        "Could not import offline transaction"

-- | Import a transaction from the network into the wallet. This function
-- assumes transactions are imported in-order (parents first). It also assumes
-- that the confirmations always arrive after the transaction imports. This
-- function is idempotent.
--
-- When re-importing an existing transaction, this function will recompute
-- the inputs, outputs and transaction details for each account. A non-dead
-- transaction could be set to dead due to new inputs being double spent.
-- However, we do not allow dead transactions to be revived by reimporting them.
-- Transactions can only be revived if they make it into the main chain.
--
-- This function returns the network confidence of the imported transaction.
importNetTx
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
    => Tx -- Network transaction to import
    -> Maybe (TBMChan Notif)
    -> SqlPersistT m ([WalletTx], [WalletAddr])
       -- ^ Returns the new transactions and addresses created
importNetTx tx notifChanM = do
    -- Find all the coins spent by this transaction
    inCoins <- getInCoins tx Nothing
    -- Get all the new coins to be created by this transaction
    outCoins <- getNewCoins tx Nothing
    -- Only continue if the transaction is relevant to the wallet
    if null inCoins && null outCoins then return ([],[]) else do
        -- Update incomplete offline transactions when the completed
        -- transaction comes in from the network.
        updateNosigHash tx (nosigTxHash tx) txid
        -- Get the transaction spending our coins
        spendingTxs <- getSpendingTxs tx Nothing
        -- Compute the confidence
        let confidence | canSpendCoins inCoins spendingTxs False = TxPending
                       | otherwise = TxDead
        -- Kill transactions that are spending our coins if we are not dead
        when (confidence /= TxDead) $ killTxIds $ map entityKey spendingTxs
        -- Create all the transaction records for this account.
        -- This will spend the input coins and create the output coins
        txRes <- buildAccTxs tx confidence inCoins outCoins
        -- Use up the addresses of our new coins (replenish gap addresses)
        newAddrs <- forM (nubBy sameKey $ map outCoinDataAddr outCoins) $
            useAddress . entityVal
        forM_ notifChanM $ \notifChan -> liftIO $ atomically $ forM_ txRes $
            writeTBMChan notifChan . NotifTx . flip toJsonTx Nothing
        return (txRes, concat newAddrs)
  where
    sameKey e1 e2 = entityKey e1 == entityKey e2
    txid = txHash tx

updateNosigHash :: MonadIO m => Tx -> TxHash -> TxHash -> SqlPersistT m ()
updateNosigHash tx nosig txid = do
    res <- select $ from $ \t -> do
        where_ (   t ^. WalletTxNosigHash ==. val nosig
               &&. t ^. WalletTxHash      !=. val txid
               )
        return $ t ^. WalletTxHash
    let toUpdate = map unValue res
    unless (null toUpdate) $ do
        splitUpdate toUpdate $ \hs t -> do
            set t [ WalletTxHash =. val txid
                  , WalletTxTx   =. val tx
                  ]
            where_ $ t ^. WalletTxHash `in_` valList hs
        splitUpdate toUpdate $ \hs c -> do
            set c [ WalletCoinHash =. val txid ]
            where_ $ c ^. WalletCoinHash `in_` valList hs

-- Check if the given coins can be spent.
canSpendCoins :: [InCoinData]
              -> [Entity WalletTx]
              -> Bool -- True for offline transactions
              -> Bool
canSpendCoins inCoins spendingTxs offline =
    all validCoin inCoins &&
    all validSpend spendingTxs
  where
    -- We can only spend pending and building coins
    validCoin (InCoinData _ t _)
        | offline   = walletTxConfidence t /= TxDead
        | otherwise = walletTxConfidence t `elem` [TxPending, TxBuilding]
    -- All transactions spending the same coins as us should be offline
    validSpend = (== TxOffline) . walletTxConfidence . entityVal

-- Get the coins in the wallet related to the inputs of a transaction. You
-- can optionally provide an account to limit the returned coins to that
-- account only.
getInCoins :: MonadIO m
           => Tx
           -> Maybe AccountId
           -> SqlPersistT m [InCoinData]
getInCoins tx aiM = do
    res <- splitSelect ops $ \os -> from $ \(c `InnerJoin` t `InnerJoin` x) -> do
        on $ x ^. WalletAddrId  ==. c ^. WalletCoinAddr
        on $ t ^. WalletTxId ==. c ^. WalletCoinTx
        where_ $ case aiM of
            Just ai ->
                c ^. WalletCoinAccount ==. val ai &&. limitOutPoints c os
            _ -> limitOutPoints c os
        return (c, t, x)
    return $ map (\(c, t, x) -> InCoinData c (entityVal t) (entityVal x)) res
  where
    ops = map prevOutput $ txIn tx
    limitOutPoints c os = join2 $ map (f c) os
    f c (OutPoint h i) =
        c ^. WalletCoinHash ==. val h &&.
        c ^. WalletCoinPos  ==. val i

-- Find all the transactions that are spending the same coins as the given
-- transaction. You can optionally provide an account to limit the returned
-- transactions to that account only.
getSpendingTxs :: MonadIO m
               => Tx
               -> Maybe AccountId
               -> SqlPersistT m [Entity WalletTx]
getSpendingTxs tx aiM
    | null txInputs = return []
    | otherwise =
        splitSelect txInputs $ \ins -> from $ \(s `InnerJoin` t) -> do
            on $ s ^. SpentCoinSpendingTx ==. t ^. WalletTxId
                        -- Filter out the given transaction
            let cond = t ^. WalletTxHash !=. val txid
                        -- Limit to only the input coins of the given tx
                       &&. limitSpent s ins
            where_ $ case aiM of
                Just ai -> cond &&. s ^. SpentCoinAccount ==. val ai
                _       -> cond
            return t
  where
    txid = txHash tx
    txInputs = map prevOutput $ txIn tx
    limitSpent s ins = join2 $ map (f s) ins
    f s (OutPoint h i) =
        s ^. SpentCoinHash ==. val h &&.
        s ^. SpentCoinPos  ==. val i

-- Returns all the new coins that need to be created from a transaction.
-- Also returns the addresses associted with those coins.
getNewCoins :: MonadIO m
            => Tx
            -> Maybe AccountId
            -> SqlPersistT m [OutCoinData]
getNewCoins tx aiM = do
    -- Find all the addresses which are in the transaction outputs
    addrs <- splitSelect uniqueAddrs $ \as -> from $ \x -> do
        let cond = x ^. WalletAddrAddress `in_` valList as
        where_ $ case aiM of
            Just ai -> cond &&. x ^. WalletAddrAccount ==. val ai
            _       -> cond
        return x
    return $ concatMap toCoins addrs
  where
    uniqueAddrs      = nub $ map (\(addr,_,_,_) -> addr) outList
    outList          = rights $ map toDat txOutputs
    txOutputs        = zip (txOut tx) [0..]
    toDat (out, pos) = getDataFromOutput out >>= \(addr, so) ->
        return (addr, out, pos, so)
    toCoins addrEnt@(Entity _ addr) =
        let f (a,_,_,_) = a == walletAddrAddress addr
        in  map (toCoin addrEnt) $ filter f outList
    toCoin addrEnt (_, out, pos, so) = OutCoinData
        { outCoinDataAddr   = addrEnt
        , outCoinDataPos    = pos
        , outCoinDataValue  = outValue out
        , outCoinDataScript = so
        }

-- Decode an output and extract an output script and a recipient address
getDataFromOutput :: TxOut -> Either String (Address, ScriptOutput)
getDataFromOutput out = do
    so   <- decodeOutputBS $ scriptOutput out
    addr <- scriptRecipient $ encodeOutput so
    return (addr, so)

isCoinbaseTx :: Tx -> Bool
isCoinbaseTx (Tx _ tin _ _) =
    length tin == 1 && outPointHash (prevOutput $ head tin) ==
        "0000000000000000000000000000000000000000000000000000000000000000"

-- | Spend the given input coins. We also create dummy coins for the inputs
-- in a transaction that do not belong to us. This is to be able to detect
-- double spends when reorgs occur.
spendInputs :: MonadIO m
            => AccountId
            -> WalletTxId
            -> Tx
            -> SqlPersistT m ()
spendInputs ai ti tx = do
    now <- liftIO getCurrentTime
    -- Spend the coins by inserting values in SpentCoin
    splitInsertMany_ $ map (buildSpentCoin now) txInputs
  where
    txInputs = map prevOutput $ txIn tx
    buildSpentCoin now (OutPoint h p) =
        SpentCoin{ spentCoinAccount    = ai
                 , spentCoinHash       = h
                 , spentCoinPos        = p
                 , spentCoinSpendingTx = ti
                 , spentCoinCreated    = now
                 }

-- Build account transaction for the given input and output coins
buildAccTxs :: MonadIO m
            => Tx
            -> TxConfidence
            -> [InCoinData]
            -> [OutCoinData]
            -> SqlPersistT m [WalletTx]
buildAccTxs tx confidence inCoins outCoins = do
    now <- liftIO getCurrentTime
    -- Group the coins by account
    let grouped = groupCoinsByAccount inCoins outCoins
    forM (M.toList grouped) $ \(ai, (is, os)) -> do
        let atx = buildAccTx tx confidence ai is os now
        -- Insert the new transaction. If it already exists, update the
        -- information with the newly computed values. Also make sure that the
        -- confidence is set to the new value (it could have changed to TxDead).
        Entity ti newAtx <- P.insertBy atx >>= \resE -> case resE of
            Left (Entity ti prev) -> do
                let prevConf = walletTxConfidence prev
                    newConf | confidence == TxDead     = TxDead
                            | prevConf   == TxBuilding = TxBuilding
                            | otherwise                = confidence
                -- If the transaction already exists, preserve confirmation data
                let newAtx = atx
                        { walletTxConfidence = newConf
                        , walletTxConfirmedBy = walletTxConfirmedBy prev
                        , walletTxConfirmedHeight = walletTxConfirmedHeight prev
                        , walletTxConfirmedDate = walletTxConfirmedDate prev
                        }
                replace ti newAtx
                -- Spend inputs only if the previous transaction was dead
                when (newConf /= TxDead && prevConf == TxDead) $
                    spendInputs ai ti tx
                -- If the transaction changed from non-dead to dead, kill it.
                -- This will remove spent coins and child transactions.
                when (prevConf /= TxDead && newConf == TxDead) $ killTxIds [ti]
                return (Entity ti newAtx)
            Right ti -> do
                when (confidence /= TxDead) $ spendInputs ai ti tx
                return (Entity ti atx)

        -- Insert the output coins with updated accTx key
        let newOs = map (toCoin ai ti now) os
        forM_ newOs $ \c -> P.insertBy c >>= \resE -> case resE of
            Left (Entity ci _) -> replace ci c
            _ -> return ()

        -- Return the new transaction record
        return newAtx
  where
    toCoin ai accTxId now (OutCoinData addrEnt pos vl so) = WalletCoin
        { walletCoinAccount = ai
        , walletCoinHash    = txHash tx
        , walletCoinPos     = pos
        , walletCoinTx      = accTxId
        , walletCoinValue   = vl
        , walletCoinScript  = so
        , walletCoinAddr    = entityKey addrEnt
        , walletCoinCreated = now
        }

-- | Build an account transaction given the input and output coins relevant to
-- this specific account. An account transaction contains the details of how a
-- transaction affects one particular account (value sent to and from the
-- account). The first value is Maybe an existing transaction in the database
-- which is used to get the existing confirmation values.
buildAccTx :: Tx
           -> TxConfidence
           -> AccountId
           -> [InCoinData]
           -> [OutCoinData]
           -> UTCTime
           -> WalletTx
buildAccTx tx confidence ai inCoins outCoins now = WalletTx
    { walletTxAccount = ai
    , walletTxHash    = txHash tx
    -- This is a hash of the transaction excluding signatures. This allows us
    -- to track the evolution of offline transactions as we add more signatures
    -- to them.
    , walletTxNosigHash = nosigTxHash tx
    , walletTxType      = txType
    , walletTxInValue   = inVal
    , walletTxOutValue  = outVal
    , walletTxInputs =
        let f h i (InCoinData (Entity _ c) t _) =
                walletTxHash t == h && walletCoinPos c == i
            toInfo (a, OutPoint h i) = case find (f h i) inCoins of
                Just (InCoinData (Entity _ c) _ _) ->
                    AddressInfo a (Just $ walletCoinValue c) True
                _ -> AddressInfo a Nothing False
        in  map toInfo allInAddrs
    , walletTxOutputs =
        let toInfo (a,i,v) = AddressInfo a (Just v) $ ours i
            ours i = isJust $ find ((== i) . outCoinDataPos) outCoins
        in  map toInfo allOutAddrs \\ changeAddrs
    , walletTxChange     = changeAddrs
    , walletTxTx         = tx
    , walletTxIsCoinbase = isCoinbaseTx tx
    , walletTxConfidence = confidence
        -- Reuse the confirmation information of the existing transaction if
        -- we have it.
    , walletTxConfirmedBy     = Nothing
    , walletTxConfirmedHeight = Nothing
    , walletTxConfirmedDate   = Nothing
    , walletTxCreated         = now
    }
  where
    -- The value going into the account is the sum of the output coins
    inVal  = sum $ map outCoinDataValue outCoins
    -- The value going out of the account is the sum on the input coins
    outVal = sum $ map coinValue inCoins
    allMyCoins = length inCoins  == length (txIn tx) &&
                 length outCoins == length (txOut tx)
    txType
        -- If all the coins belong to the same account, it is a self
        -- transaction (even if a fee was payed).
        | allMyCoins = TxSelf
        -- This case can happen in complex transactions where the total
        -- input/output sum for a given account is 0. In this case, we count
        -- that transaction as a TxSelf. This should not happen with simple
        -- transactions.
        | inVal == outVal = TxSelf
        | inVal > outVal  = TxIncoming
        | otherwise       = TxOutgoing
    -- List of all the decodable input addresses in the transaction
    allInAddrs =
        let f inp = do
                addr <- scriptSender =<< decodeToEither (scriptInput inp)
                return (addr, prevOutput inp)
        in  rights $ map f $ txIn tx
    -- List of all the decodable output addresses in the transaction
    allOutAddrs =
        let f op i = do
                addr <- scriptRecipient =<< decodeToEither (scriptOutput op)
                return (addr, i, outValue op)
        in  rights $ zipWith f (txOut tx) [0..]
    changeAddrs
        | txType == TxIncoming = []
        | otherwise =
            let isInternal = (== AddressInternal) . walletAddrType
                                . entityVal . outCoinDataAddr
                f = walletAddrAddress . entityVal . outCoinDataAddr
                toInfo c = AddressInfo (f c) (Just $ outCoinDataValue c) True
            in  map toInfo $ filter isInternal outCoins

-- Group all the input and outputs coins from the same account together.
groupCoinsByAccount
    :: [InCoinData]
    -> [OutCoinData]
    -> M.Map AccountId ([InCoinData], [OutCoinData])
groupCoinsByAccount inCoins outCoins =
    M.unionWith merge inMap outMap
  where
    -- Build a map from accounts -> (inCoins, outCoins)
    f coin@(InCoinData _ t _) = (walletTxAccount t, [coin])
    g coin = (walletAddrAccount $ entityVal $  outCoinDataAddr coin, [coin])
    merge (is, _) (_, os) = (is, os)
    inMap  = M.map (\is -> (is, [])) $ M.fromListWith (++) $ map f inCoins
    outMap = M.map (\os -> ([], os)) $ M.fromListWith (++) $ map g outCoins

deleteTx :: (MonadIO m, MonadThrow m) => TxHash -> SqlPersistT m ()
deleteTx txid = do
    ts <- select $ from $ \t -> do
        where_ $ t ^. WalletTxHash ==. val txid
        return t
    case ts of
        [] -> throwM $ WalletException $ unwords
            [ "Cannot delete inexistent transaction"
            , cs (txHashToHex txid)
            ]
        Entity{entityVal = WalletTx{walletTxConfidence = TxBuilding}} : _ ->
            throwM $ WalletException $ unwords
                [ "Cannot delete confirmed transaction"
                , cs (txHashToHex txid)
                ]
        _ -> return ()
    children <- fmap (map unValue) $ select $ from $
        \(t `InnerJoin` c `InnerJoin` s `InnerJoin` t2) -> do
            on $   s ^. SpentCoinSpendingTx ==. t2 ^. WalletTxId
            on (   c ^. WalletCoinAccount   ==. t  ^. WalletTxAccount
               &&. c ^. WalletCoinHash      ==. s  ^. SpentCoinHash
               &&. c ^. WalletCoinPos       ==. s  ^. SpentCoinPos
               )
            on $   c ^. WalletCoinTx        ==. t  ^. WalletTxId
            where_ $ t ^. WalletTxHash ==. val txid
            return $ t2 ^. WalletTxHash
    forM_ children deleteTx
    forM_ ts $ \Entity{entityKey = ti} ->
        delete $ from $ \s -> where_ $ s ^. SpentCoinSpendingTx ==. val ti
    delete $ from $ \s -> where_ $ s ^. SpentCoinHash ==. val txid
    forM_ ts $ \Entity{entityKey = ti} -> do
        delete $ from $ \c -> where_ $ c ^. WalletCoinTx ==. val ti
        delete $ from $ \t -> where_ $ t ^. WalletTxId   ==. val ti

-- Kill transactions and their children by ids.
killTxIds :: MonadIO m => [WalletTxId] -> SqlPersistT m ()
killTxIds txIds = do
    -- Find all the transactions spending the coins of these transactions
    -- (Find all the child transactions)
    children <- splitSelect txIds $ \ts -> from $ \(t `InnerJoin` s) -> do
        on (   s ^. SpentCoinAccount ==. t ^. WalletTxAccount
           &&. s ^. SpentCoinHash    ==. t ^. WalletTxHash
           )
        where_ $ t ^. WalletTxId `in_` valList ts
        return $ s ^. SpentCoinSpendingTx

    -- Kill these transactions
    splitUpdate txIds $ \ts t -> do
        set t [ WalletTxConfidence =. val TxDead ]
        where_ $ t ^. WalletTxId `in_` valList ts

    -- This transaction doesn't spend any coins
    splitDelete txIds $ \ts -> from $ \s ->
        where_ $ s ^. SpentCoinSpendingTx `in_` valList ts

    -- Recursively kill all the child transactions.
    -- (Recurse at the end in case there are closed loops)
    unless (null children) $ killTxIds $ nub $ map unValue children

-- Kill transactions and their child transactions by hashes.
killTxs :: MonadIO m => [TxHash] -> SqlPersistT m ()
killTxs txHashes = do
    res <- splitSelect txHashes $ \hs -> from $ \t -> do
        where_ $ t ^. WalletTxHash `in_` valList hs
        return $ t ^. WalletTxId
    killTxIds $ map unValue res

{- Confirmations -}

importMerkles :: MonadIO m
              => BlockChainAction
              -> [MerkleTxs]
              -> Maybe (TBMChan Notif)
              -> SqlPersistT m ()
importMerkles action expTxsLs notifChanM =
    when (isBestChain action || isChainReorg action) $ do
        case action of
            ChainReorg _ os _ ->
                -- Unconfirm transactions from the old chain.
                let hs = map (Just . nodeBlockHash) os
                in  splitUpdate hs $ \h t -> do
                        set t [ WalletTxConfidence      =. val TxPending
                              , WalletTxConfirmedBy     =. val Nothing
                              , WalletTxConfirmedHeight =. val Nothing
                              , WalletTxConfirmedDate   =. val Nothing
                              ]
                        where_ $ t ^. WalletTxConfirmedBy `in_` valList h
            _ -> return ()

        -- Find all the dead transactions which need to be revived
        deadTxs <- splitSelect (concat expTxsLs) $ \ts -> from $ \t -> do
            where_ (   t ^. WalletTxHash `in_` valList ts
                &&. t ^. WalletTxConfidence ==. val TxDead
                )
            return $ t ^. WalletTxTx

        -- Revive dead transactions (in no particular order)
        forM_ deadTxs $ reviveTx . unValue

        -- Confirm the transactions
        forM_ (zip (actionNodes action) expTxsLs) $ \(node, hs) -> do
            let hash   = nodeBlockHash    node
                height = nodeHeaderHeight node

            splitUpdate hs $ \h t -> do
                set t [ WalletTxConfidence =. val TxBuilding
                      , WalletTxConfirmedBy =. val (Just hash)
                      , WalletTxConfirmedHeight =.
                          val (Just height)
                      , WalletTxConfirmedDate =.
                          val (Just (blockTimestamp $ nodeHeader node))
                      ]
                where_ $ t ^. WalletTxHash `in_` valList h

            ts <- fmap (map entityVal) $ splitSelect hs $ \h -> from $ \t -> do
                    where_ $ t ^. WalletTxHash `in_` valList h
                    return t

            let jsonTxs = map (`toJsonTx` Just height) ts

            -- Update the best height in the wallet (used to compute the number
            -- of confirmations of transactions)
            setBestBlock hash height

            -- Send notification for block
            forM_ notifChanM $ \notifChan -> liftIO $ atomically $
                writeTBMChan notifChan $ NotifBlock JsonBlock
                    { jsonBlockHash    = hash
                    , jsonBlockHeight  = height
                    , jsonBlockPrev    = prevBlock $ nodeHeader node
                    , jsonBlockTxs     = jsonTxs
                    }

-- Helper function to set the best block and best block height in the DB.
setBestBlock :: MonadIO m => BlockHash -> Word32 -> SqlPersistT m ()
setBestBlock bid i = update $ \t -> set t [ WalletStateBlock  =. val bid
                                          , WalletStateHeight =. val i
                                          ]

-- Helper function to get the best block and best block height from the DB
getBestBlock :: MonadIO m => SqlPersistT m (BlockHash, Word32)
getBestBlock = do
    cfgM <- fmap entityVal <$> P.selectFirst [] []
    return $ case cfgM of
        Just WalletState{..} -> (walletStateBlock, walletStateHeight)
        Nothing -> throw $ WalletException $ unwords
            [ "Could not get the best block."
            , "Wallet database is probably not initialized"
            ]

-- Revive a dead transaction. All transactions that are in conflict with this
-- one will be killed.
reviveTx :: MonadIO m => Tx -> SqlPersistT m ()
reviveTx tx = do
    -- Kill all transactions spending our coins
    spendingTxs <- getSpendingTxs tx Nothing
    killTxIds $ map entityKey spendingTxs

    -- Find all the WalletTxId that have to be revived
    ids <- select $ from $ \t -> do
        where_ (   t ^. WalletTxHash       ==. val (txHash tx)
               &&. t ^. WalletTxConfidence ==. val TxDead
               )
        return (t ^. WalletTxAccount, t ^. WalletTxId)

    -- Spend the inputs for all our transactions
    forM_ ids $ \(Value ai, Value ti) -> spendInputs ai ti tx

    -- Update the transactions
    splitUpdate (map (unValue . snd) ids) $ \is t -> do
        set t [ WalletTxConfidence      =. val TxPending
              , WalletTxConfirmedBy     =. val Nothing
              , WalletTxConfirmedHeight =. val Nothing
              , WalletTxConfirmedDate   =. val Nothing
              ]
        where_ $ t ^. WalletTxId `in_` valList is

{- Transaction creation and signing (local wallet functions) -}

-- | Create a transaction sending some coins to a list of recipient addresses.
createTx :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
         => Entity Account        -- ^ Account Entity
         -> Maybe XPrvKey         -- ^ Key if not provided by account
         -> [(Address,Word64)]    -- ^ List of recipient addresses and amounts
         -> Word64                -- ^ Fee per 1000 bytes
         -> Word32                -- ^ Minimum confirmations
         -> Bool                  -- ^ Should fee be paid by recipient
         -> Bool                  -- ^ Should the transaction be signed
         -> SqlPersistT m (WalletTx, [WalletAddr])
            -- ^ (New transaction hash, Completed flag)
createTx accE@(Entity ai acc) masterM dests fee minConf rcptFee sign = do
    -- Build an unsigned transaction from the given recipient values and fee
    (unsignedTx, inCoins, newChangeAddrs) <-
        buildUnsignedTx accE dests fee minConf rcptFee
    -- Sign our new transaction if signing was requested
    let dat = map toCoinSignData inCoins
        tx | sign      = signOfflineTx acc masterM unsignedTx dat
           | otherwise = unsignedTx
    -- Import the transaction in the wallet either as a network transaction if
    -- it is complete, or as an offline transaction otherwise.
    (res, newAddrs) <- importTx' tx ai inCoins
    case res of
        (txRes:_) -> return (txRes, newAddrs ++ newChangeAddrs)
        _ -> liftIO . throwIO $ WalletException
            "Error while importing the new transaction"

toCoinSignData :: InCoinData -> CoinSignData
toCoinSignData (InCoinData (Entity _ c) t x) =
    CoinSignData (OutPoint (walletTxHash t) (walletCoinPos c))
                 (walletCoinScript c)
                 deriv
  where
    deriv = Deriv :/ addrTypeIndex (walletAddrType x) :/ walletAddrIndex x

-- Build an unsigned transaction given a list of recipients and a fee. Returns
-- the unsigned transaction together with the input coins that have been
-- selected or spending.
buildUnsignedTx
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
    => Entity Account
    -> [(Address, Word64)]
    -> Word64
    -> Word32
    -> Bool
    -> SqlPersistT m (Tx, [InCoinData], [WalletAddr])
    -- ^ Generated change addresses
buildUnsignedTx _ [] _ _ _ = liftIO . throwIO $ WalletException
    "buildUnsignedTx: No transaction recipients have been provided"
buildUnsignedTx accE@(Entity ai acc) origDests origFee minConf rcptFee = do
    let p = case accountType acc of
                AccountMultisig m n -> (m, n)
                _ -> throw . WalletException $ "Invalid account type"
        fee = if rcptFee then 0 else origFee
        coins | isMultisigAccount acc = chooseMSCoins tot fee p True
              | otherwise             = chooseCoins   tot fee   True
        -- TODO: Add more policies like confirmations or coin age
        -- Sort coins by their values in descending order
        orderPolicy c _ = [desc $ c ^. WalletCoinValue]
        -- Find the spendable coins in the given account with the required number
        -- of minimum confirmations.
    selectRes <- spendableCoins ai minConf orderPolicy
    -- Find a selection of spendable coins that matches our target value
    let (selected, change) = either (throw . WalletException) id $ coins selectRes
        totFee | isMultisigAccount acc = getMSFee origFee p (length selected)
               | otherwise             = getFee   origFee   (length selected)
        -- Subtract fees from first destination if rcptFee
        value = snd $ head origDests

    -- First output must not be dust after deducting fees
    when (rcptFee && value < totFee + 5430) $ throw $ WalletException
        "First recipient cannot cover transaction fees"

        -- Subtract fees from first destination if rcptFee
    let dests | rcptFee =
                    second (const $ value - totFee) (head origDests) :
                    tail origDests
              | otherwise = origDests

    -- Make sure the first recipient has enough funds to cover the fee
    when (snd (head dests) <= 0) $ throw $
        WalletException "Transaction fees too high"

    -- If the change amount is not dust, we need to add a change address to
    -- our list of recipients.
    -- TODO: Put the dust value in a constant somewhere. We also need a more
    -- general way of detecting dust such as our transactions are not
    -- rejected by full nodes.
    (allDests, addrs) <- if change < 5430
        then return (dests, [])
        else do
             (addr, chng) <- newChangeAddr change
             return ((walletAddrAddress addr, chng) : dests, [addr])

    case buildAddrTx (map toOutPoint selected) $ map toBase58 allDests of
        Right tx -> return (tx, selected, addrs)
        Left err -> liftIO . throwIO $ WalletException err
  where
    tot = sum $ map snd origDests
    toBase58 (a, v) = (addrToBase58 a, v)
    toOutPoint (InCoinData (Entity _ c) t _) =
        OutPoint (walletTxHash t) (walletCoinPos c)
    newChangeAddr change = do
        as <- unusedAddresses accE AddressInternal
        case as of
            (a:_) -> do
                -- Use the address to prevent reusing it again
                _ <- useAddress a
                -- TODO: Randomize the change position
                return (a, change)
            _ -> liftIO . throwIO $ WalletException
                "No unused addresses available"

signAccountTx :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
              => Entity Account -> Maybe XPrvKey -> TxHash
              -> SqlPersistT m ([WalletTx], [WalletAddr])
signAccountTx (Entity ai acc) masterM txid = do
    (OfflineTxData tx dat, inCoins) <- getOfflineTxData ai txid
    let signedTx = signOfflineTx acc masterM tx dat
    importTx' signedTx ai inCoins

getOfflineTxData
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
    => AccountId
    -> TxHash
    -> SqlPersistT m (OfflineTxData, [InCoinData])
getOfflineTxData ai txid = do
    txM <- getBy $ UniqueAccTx ai txid
    case txM of
        Just (Entity _ tx) -> do
            unless (walletTxConfidence tx == TxOffline) $ liftIO . throwIO $
                WalletException "Can only sign offline transactions."
            inCoins <- getInCoins (walletTxTx tx) $ Just ai
            return
                ( OfflineTxData (walletTxTx tx) $ map toCoinSignData inCoins
                , inCoins
                )
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Invalid txid", cs $ txHashToHex txid ]

-- Sign a transaction using a list of CoinSignData. This allows an offline
-- signer without access to the coins to sign a given transaction.
signOfflineTx :: Account        -- ^ Account used for signing
              -> Maybe XPrvKey  -- ^ Key if not provided in account
              -> Tx             -- ^ Transaction to sign
              -> [CoinSignData] -- ^ Input signing data
              -> Tx
signOfflineTx acc masterM tx coinSignData
    | not validMaster = throw $ WalletException
        "Master key not valid"
    -- Fail for read-only accounts
    | isReadAccount acc = throw $ WalletException
        "signOfflineTx is not supported on read-only accounts"
    -- Sign the transaction deterministically
    | otherwise = either (throw . WalletException) id $
        signTx tx sigData $ map (toPrvKeyG . xPrvKey) prvKeys
  where
    -- Compute all the SigInputs
    sigData = map (toSigData acc) coinSignData
    -- Compute all the private keys
    prvKeys = map toPrvKey coinSignData
    -- Build a SigInput from a CoinSignData
    toSigData acc' (CoinSignData op so deriv) =
        -- TODO: Here we override the SigHash to be SigAll False all the time.
        -- Should we be more flexible?
        SigInput so op (SigAll False) $
            if isMultisigAccount acc
                then Just $ getPathRedeem acc' deriv
                else Nothing
    toPrvKey (CoinSignData _ _ deriv) = derivePath deriv master
    master = case masterM of
        Just m -> case accountDerivation acc of
            Just d -> derivePath d m
            Nothing -> m
        Nothing -> fromMaybe
            (throw $ WalletException "No extended private key available")
            (accountMaster acc)
    validMaster = deriveXPubKey master `elem` accountKeys acc

-- Returns unspent coins that can be spent in an account that have a minimum
-- number of confirmations. Coinbase coins can only be spent after 100
-- confirmations.
spendableCoins
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
    => AccountId                   -- ^ Account key
    -> Word32                      -- ^ Minimum confirmations
    -> (    SqlExpr (Entity WalletCoin)
         -> SqlExpr (Entity WalletTx)
         -> [SqlExpr OrderBy]
       )
    -- ^ Coin ordering policy
    -> SqlPersistT m [InCoinData]       -- ^ Spendable coins
spendableCoins ai minConf orderPolicy =
    fmap (map f) $ select $ spendableCoinsFrom ai minConf orderPolicy
  where
    f (c, t, x) = InCoinData c (entityVal t) (entityVal x)

spendableCoinsFrom
    :: AccountId                   -- ^ Account key
    -> Word32                      -- ^ Minimum confirmations
    -> (    SqlExpr (Entity WalletCoin)
         -> SqlExpr (Entity WalletTx)
         -> [SqlExpr OrderBy]
       )
    -- ^ Coin ordering policy
    -> SqlQuery ( SqlExpr (Entity WalletCoin)
                , SqlExpr (Entity WalletTx)
                , SqlExpr (Entity WalletAddr)
                )
spendableCoinsFrom ai minConf orderPolicy =
    from $ \(c `InnerJoin` t `InnerJoin` x `LeftOuterJoin` s) -> do
        -- Joins have to be set in reverse order !
        -- Left outer join on spent coins
        on (   s ?. SpentCoinAccount ==. just (c ^. WalletCoinAccount)
           &&. s ?. SpentCoinHash    ==. just (c ^. WalletCoinHash)
           &&. s ?. SpentCoinPos     ==. just (c ^. WalletCoinPos)
           )
        on $ x ^. WalletAddrId ==. c ^. WalletCoinAddr
        -- Inner join on coins and transactions
        on $  t ^. WalletTxId ==. c ^. WalletCoinTx
        where_ (   c ^. WalletCoinAccount ==. val ai
               &&. t ^. WalletTxConfidence
                   `in_` valList [ TxPending, TxBuilding ]
                -- We only want unspent coins
               &&. E.isNothing (s ?. SpentCoinId)
               &&. limitConfirmations (Right t) minConf
               )
        orderBy (orderPolicy c t)
        return (c, t, x)

-- If the current height is 200 and a coin was confirmed at height 198, then it
-- has 3 confirmations. So, if we require 3 confirmations, we want coins with a
-- confirmed height of 198 or less (200 - 3 + 1).
limitConfirmations :: Either (SqlExpr (Maybe (Entity WalletTx)))
                             (SqlExpr (Entity WalletTx))
                   -> Word32
                   -> SqlExpr (Value Bool)
limitConfirmations txE minconf
    | minconf == 0  = limitCoinbase
    | minconf < 100 = limitConfs minconf &&. limitCoinbase
    | otherwise     = limitConfs minconf
  where
    limitConfs i = case txE of
        Left t -> t ?. WalletTxConfirmedHeight
            <=. just (just (selectHeight -. val (i - 1)))
        Right t -> t ^. WalletTxConfirmedHeight
            <=. just (selectHeight -. val (i - 1))
    -- Coinbase transactions require 100 confirmations
    limitCoinbase = case txE of
        Left t ->
            not_ (coalesceDefault [t ?. WalletTxIsCoinbase] (val False)) ||.
            limitConfs 100
        Right t ->
            not_ (t ^. WalletTxIsCoinbase) ||. limitConfs 100
    selectHeight :: SqlExpr (Value Word32)
    selectHeight = sub_select $ from $ \co -> do
        limit 1
        return $ co ^. WalletStateHeight

{- Balances -}

accountBalance :: MonadIO m
               => AccountId
               -> Word32
               -> Bool
               -> SqlPersistT m Word64
accountBalance ai minconf offline = do
    res <- select $ from $ \(c `InnerJoin`
                             t `LeftOuterJoin` s `LeftOuterJoin` st) -> do
        on $ st ?. WalletTxId ==. s ?. SpentCoinSpendingTx
        on (   s ?. SpentCoinAccount ==. just (c ^. WalletCoinAccount)
           &&. s ?. SpentCoinHash    ==. just (c ^. WalletCoinHash)
           &&. s ?. SpentCoinPos     ==. just (c ^. WalletCoinPos)
           )
        on $ t ^. WalletTxId ==. c ^. WalletCoinTx
        let unspent = E.isNothing ( s ?. SpentCoinId )
            spentOffline = st ?. WalletTxConfidence ==. just (val TxOffline)
            cond =     c ^. WalletCoinAccount ==. val ai
                   &&. t ^. WalletTxConfidence `in_` valList validConfidence
                   -- For non-offline balances, we have to take into account
                   -- the coins which are spent by offline transactions.
                   &&. if offline then unspent else unspent ||. spentOffline
        where_ $ if minconf == 0
                    then cond
                    else cond &&. limitConfirmations (Right t) minconf
        return $ sum_ (c ^. WalletCoinValue)
    case res of
        (Value (Just s):_) -> return $ floor (s :: Double)
        _ -> return 0
  where
    validConfidence = TxPending : TxBuilding : [ TxOffline | offline ]

addressBalances :: MonadIO m
                => Entity Account
                -> KeyIndex
                -> KeyIndex
                -> AddressType
                -> Word32
                -> Bool
                -> SqlPersistT m [(KeyIndex, BalanceInfo)]
addressBalances accE@(Entity ai _) iMin iMax addrType minconf offline = do
        -- We keep our joins flat to improve performance in SQLite.
    res <- select $ from $ \(x `LeftOuterJoin` c `LeftOuterJoin`
                             t `LeftOuterJoin` s `LeftOuterJoin` st) -> do
        let joinCond = st ?. WalletTxId ==. s ?. SpentCoinSpendingTx
        -- Do not join the spending information for offline transactions if we
        -- request the online balances. This will count the coin as unspent.
        on $ if offline
            then joinCond
            else joinCond &&.
                 st ?. WalletTxConfidence !=. just (val TxOffline)
        on $   s ?. SpentCoinAccount ==. c ?. WalletCoinAccount
           &&. s ?. SpentCoinHash    ==. c ?. WalletCoinHash
           &&. s ?. SpentCoinPos     ==. c ?. WalletCoinPos
        let txJoin =     t ?. WalletTxId          ==.  c ?. WalletCoinTx
                     &&. t ?. WalletTxConfidence `in_` valList validConfidence
        on $ if minconf == 0
                then txJoin
                else txJoin &&. limitConfirmations (Left t) minconf
        on $ c ?. WalletCoinAddr ==. just (x ^. WalletAddrId)
        let limitIndex
                | iMin == iMax = x ^. WalletAddrIndex ==. val iMin
                | otherwise = x ^. WalletAddrIndex >=. val iMin
                          &&. x ^. WalletAddrIndex <=. val iMax
        where_ (   x ^. WalletAddrAccount ==. val ai
               &&. limitIndex
               &&. x ^. WalletAddrIndex <.  subSelectAddrCount accE addrType
               &&. x ^. WalletAddrType  ==. val addrType
               )
        groupBy $ x ^. WalletAddrIndex
        let unspent   = E.isNothing $ st ?. WalletTxId
            invalidTx = E.isNothing $ t ?. WalletTxId
        return ( x ^. WalletAddrIndex -- Address index
               , sum_ $ case_
                   [ when_ invalidTx
                     then_ (val (Just 0))
                   ] (else_ $ c ?. WalletCoinValue) -- Out value
               , sum_ $ case_
                   [ when_ (unspent ||. invalidTx)
                     then_ (val (Just 0))
                   ] (else_ $ c ?. WalletCoinValue) -- Out value
               , count $ t ?. WalletTxId -- New coins
               , count $ case_
                   [ when_ invalidTx
                     then_ (val Nothing)
                   ] (else_ $ st ?. WalletTxId) -- Spent coins
               )
    return $ map f res
  where
    validConfidence = Just TxPending : Just TxBuilding :
                        [ Just TxOffline | offline ]
    f (Value i, Value inM, Value outM, Value newC, Value spentC) =
        let b = BalanceInfo
                    { balanceInfoInBalance  =
                        floor $ fromMaybe (0 :: Double) inM
                    , balanceInfoOutBalance =
                        floor $ fromMaybe (0 :: Double) outM
                    , balanceInfoCoins      = newC
                    , balanceInfoSpentCoins = spentC
                    }
        in (i, b)

{- Rescans -}

resetRescan :: MonadIO m => SqlPersistT m ()
resetRescan = do
    P.deleteWhere ([] :: [P.Filter WalletCoin])
    P.deleteWhere ([] :: [P.Filter SpentCoin])
    P.deleteWhere ([] :: [P.Filter WalletTx])
    setBestBlock (headerHash genesisHeader) 0

