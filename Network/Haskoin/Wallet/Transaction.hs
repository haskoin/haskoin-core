module Network.Haskoin.Wallet.Transaction 
( 
-- *Database transactions
  txPage
, addrTxPage
, getTx 
, importTx
, importNetTx
, signKeyRingTx
, createTx
, signOfflineTx
, getOfflineTxData
, killTxs
, reviveTx
, getPendingTxs

-- *Database blocks
, importMerkles
, getBestBlock

-- *Database coins and balances
, spendableCoins
, spendableCoinsSource
, accountBalance
, addressBalances

-- *Rescan
, resetRescan
) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad (forM, forM_, when, liftM, unless)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Exception (throwIO, throw)

import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word32, Word64)
import Data.Either (rights)
import Data.List ((\\), nub, nubBy, delete, find)
import Data.List.Split (splitEvery)
import Data.Text (unpack)
import Data.Conduit (Source, mapOutput, ($$))
import Data.Maybe 
    (catMaybes, mapMaybe, isNothing, isJust, fromMaybe, listToMaybe)
import qualified Data.Conduit.List as CL (consume)
import qualified Data.Map.Strict as M 
    ( Map, toList, map, lookup
    , unionWith, fromListWith, filter
    )

import qualified Database.Persist as P
    ( Filter, SelectOpt( Asc, Desc, OffsetBy, LimitTo )
    , selectFirst, updateWhere, selectSource, count, update
    , deleteWhere, insertBy, insertMany_, selectList, PersistEntity
    , PersistEntityBackend
    , (=.), (==.), (<.), (>.), (<-.)
    )
import Database.Esqueleto 
    ( Value(..), Esqueleto, SqlQuery, SqlExpr, SqlBackend, SqlEntity
    , InnerJoin(..), LeftOuterJoin(..), OrderBy, update, sum_, groupBy
    , select, from, where_, val, valList, sub_select, countRows, count
    , orderBy, limit, asc, desc, set, offset, selectSource, updateCount
    , subList_select, in_, unValue, max_, not_, coalesceDefault, just, on
    , case_, when_, then_, else_
    , (^.), (=.), (==.), (&&.), (||.), (<.)
    , (<=.), (>.), (>=.), (-.), (*.), (?.), (!=.)
    -- Reexports from Database.Persist
    , SqlPersistT, Entity(..)
    , getBy, insertUnique, updateGet, replace, get, insertMany_, insert_
    )
import qualified Database.Esqueleto as E (isNothing, delete)
import Database.Esqueleto.Internal.Sql (SqlSelect)

import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Constants

import Network.Haskoin.Wallet.KeyRing
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Database

-- Input coin type with transaction and address information
type InCoinData = (Entity KeyRingCoin, KeyRingTx, KeyRingAddr)

instance Coin InCoinData where
    coinValue (Entity _ c, _, _) = keyRingCoinValue c

-- Output coin type with address information
data OutCoinData = OutCoinData
    { outCoinDataAddr   :: !(Entity KeyRingAddr)
    , outCoinDataPos    :: !KeyIndex
    , outCoinDataValue  :: !Word64
    , outCoinDataScript :: !ScriptOutput
    }

{- List transaction -}

-- | Get transactions by page
txPage :: MonadIO m
       => KeyRingName -- ^ KeyRing name
       -> AccountName -- ^ Account name
       -> PageRequest -- ^ Page request
       -> SqlPersistT m ([(KeyRing, KeyRingAccount, KeyRingTx)], Word32) 
          -- ^ Page result
txPage keyRingName accountName page@PageRequest{..}
    | validPageRequest page = do
        cntRes <- select $ from $ \(k `InnerJoin` a `InnerJoin` t) -> do
            on $ t ^. KeyRingTxAccount      ==. a ^. KeyRingAccountId
            on $ a ^. KeyRingAccountKeyRing ==. k ^. KeyRingId
            where_ (   k ^. KeyRingName        ==. val keyRingName
                   &&. a ^. KeyRingAccountName ==. val accountName
                   )
            return countRows

        let cnt     = maybe 0 unValue $ listToMaybe cntRes
            (d, m)  = cnt `divMod` pageLen
            maxPage = max 1 $ d + min 1 m

        when (pageNum > maxPage) $ liftIO . throwIO $ WalletException $
            unwords [ "Invalid page number", show pageNum ]

        res <- select $ from $ \(k `InnerJoin` a `InnerJoin` t) -> do
            on $ t ^. KeyRingTxAccount      ==. a ^. KeyRingAccountId
            on $ a ^. KeyRingAccountKeyRing ==. k ^. KeyRingId
            where_ (   k ^. KeyRingName        ==. val keyRingName
                   &&. a ^. KeyRingAccountName ==. val accountName
                   )
            let order = if pageReverse then asc else desc
            orderBy [ order (t ^. KeyRingTxId) ]
            limit $ fromIntegral pageLen
            offset $ fromIntegral $ (pageNum - 1) * pageLen

            return (k, a, t)

        let f | pageReverse = id
              | otherwise   = reverse
            g (Entity _ k, Entity _ a, Entity _ t) = (k, a, t)
        return (f $ map g res, maxPage)
    | otherwise = liftIO . throwIO $ WalletException $
        concat [ "Invalid page request"
               , " (Page: ", show pageNum, ", Page size: ", show pageLen, ")"
               ]

-- | Get address transactions by page
addrTxPage :: MonadIO m
           => KeyRingName -- ^ KeyRing name
           -> AccountName -- ^ Account name
           -> AddressType -- ^ Address type
           -> KeyIndex    -- ^ Address index
           -> PageRequest -- ^ Page request
           -> SqlPersistT m ( [ ( KeyRing
                                , KeyRingAccount
                                , KeyRingAddr
                                , KeyRingAddrTx
                                , KeyRingTx
                                )
                              ]
                            , Word32
                            ) 
              -- ^ Page result
addrTxPage keyRingName accountName addrType index page@PageRequest{..}
    | validPageRequest page = do
        cntRes <- select $ from $ 
            \(k `InnerJoin` a `InnerJoin` x `InnerJoin` xt) -> do
            on $ xt ^. KeyRingAddrTxAddr    ==. x ^. KeyRingAddrId
            on $ x ^. KeyRingAddrAccount    ==. a ^. KeyRingAccountId
            on $ a ^. KeyRingAccountKeyRing ==. k ^. KeyRingId
            where_ (   k ^. KeyRingName        ==. val keyRingName
                   &&. a ^. KeyRingAccountName ==. val accountName
                   &&. x ^. KeyRingAddrType    ==. val addrType
                   &&. x ^. KeyRingAddrIndex   ==. val index
                   &&. x ^. KeyRingAddrIndex   <.  subSelectAddrCount a addrType
                   )
            return countRows

        let cnt     = maybe 0 unValue $ listToMaybe cntRes
            (d, m)  = cnt `divMod` pageLen
            maxPage = max 1 $ d + min 1 m

        when (pageNum > maxPage) $ liftIO . throwIO $ WalletException $
            unwords [ "Invalid page number", show pageNum ]

        res <- select $ from $ 
            \(k `InnerJoin` a `InnerJoin` x `InnerJoin` xt `InnerJoin` t) -> do
            on $ t ^. KeyRingTxId           ==. xt ^. KeyRingAddrTxAccTx
            on $ xt ^. KeyRingAddrTxAddr    ==. x ^. KeyRingAddrId
            on $ x ^. KeyRingAddrAccount    ==. a ^. KeyRingAccountId
            on $ a ^. KeyRingAccountKeyRing ==. k ^. KeyRingId
            where_ (   k ^. KeyRingName        ==. val keyRingName
                   &&. a ^. KeyRingAccountName ==. val accountName
                   &&. x ^. KeyRingAddrType    ==. val addrType
                   &&. x ^. KeyRingAddrIndex   ==. val index
                   &&. x ^. KeyRingAddrIndex   <.  subSelectAddrCount a addrType
                   )
            let order = if pageReverse then asc else desc
            orderBy [ order (xt ^. KeyRingAddrTxId) ]
            limit $ fromIntegral pageLen
            offset $ fromIntegral $ (pageNum - 1) * pageLen

            return (k, a, x, xt, t)


        let f | pageReverse = id
              | otherwise   = reverse
            g (Entity _ k, Entity _ a, Entity _ x, Entity _ xt, Entity _ t) = 
                (k, a, x, xt, t)
        return (f $ map g res, maxPage)
    | otherwise = liftIO . throwIO $ WalletException $
        concat [ "Invalid page request"
               , " (Page: ", show pageNum, ", Page size: ", show pageLen, ")"
               ]

-- Helper function to get a transaction from the wallet database. The function
-- will look across all accounts and return the first available transaction. If
-- the transaction does not exist, this function will throw a wallet exception.
getTx :: MonadIO m => TxHash -> SqlPersistT m Tx
getTx txid = do
    txM <- P.selectFirst [ KeyRingTxHash P.==. txid ] []
    case txM of
        Just (Entity _ KeyRingTx{..}) -> return keyRingTxTx
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "getTx: Transaction does not exist:", encodeTxHashLE txid ]

getPendingTxs :: MonadIO m => Int -> SqlPersistT m [Tx]
getPendingTxs i = do
    txEs <- P.selectList
         [ KeyRingTxConfidence P.==. TxPending ]
         [ P.LimitTo i ]
    return $ map (keyRingTxTx . entityVal) txEs

{- Transaction Import -}

-- | Import a transaction into the wallet from an unknown source. If the
-- transaction is standard, valid, all inputs are known and all inputs can be
-- spent, then the transaction will be imported as a network transaction.
-- Otherwise, the transaction will be imported into the local account as an
-- offline transaction.
importTx :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
         => Tx               -- ^ Transaction to import
         -> KeyRingAccountId -- ^ Account ID
         -> SqlPersistT m (TxHash, TxConfidence) 
            -- ^ Transaction hash (after possible merges)
importTx tx ai = importTx' tx ai =<< getInCoins tx (Just ai)

importTx' :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
          => Tx                   -- ^ Transaction to import
          -> KeyRingAccountId     -- ^ Account ID
          -> [InCoinData]         -- ^ Input coins
          -> SqlPersistT m (TxHash, TxConfidence) 
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
            set t [ KeyRingTxHash =. val txid 
                  , KeyRingTxTx   =. val tx
                  ]
            where_ (   t ^. KeyRingTxAccount ==. val ai 
                   &&. t ^. KeyRingTxHash    ==. val origTxid
                   )
        -- Update coins
        update $ \t -> do
            set t [ KeyRingCoinHash =. val txid ]
            where_ (   t ^. KeyRingCoinAccount ==. val ai 
                   &&. t ^. KeyRingCoinHash    ==. val origTxid
                   )
        let f (c, t, x) = if keyRingTxHash t == origTxid
                then (c, t{ keyRingTxHash = txid, keyRingTxTx = tx }, x)
                else (c, t, x)
        return $ map f origInCoins

    spendingTxs <- getSpendingTxs tx (Just ai)

    let validTx = verifyStdTx tx $ map toVerDat inCoins
        validIn = length inCoins == length (txIn tx)
               && canSpendCoins inCoins spendingTxs False
    if validIn && validTx 
        then importNetTx tx >>= \resM -> case resM of
                Just (confidence, _) -> return (txid, confidence)
                _ -> liftIO . throwIO $ WalletException 
                    "Error while importing the transaction"
        else importOfflineTx tx ai inCoins spendingTxs
  where
    toVerDat (Entity _ c, t, _) = 
        (keyRingCoinScript c, OutPoint (keyRingTxHash t) (keyRingCoinPos c))

-- Offline transactions are usually multisignature transactions requiring
-- additional signatures. This function will merge the signatures of
-- the same offline transactions together into one single transaction.
mergeNoSigHashTxs :: MonadIO m 
                  => KeyRingAccountId 
                  -> Tx 
                  -> [InCoinData]
                  -> SqlPersistT m (Maybe Tx)
mergeNoSigHashTxs ai tx inCoins = do
    prevM <- getBy $ UniqueAccNoSig ai $ nosigTxHash tx
    return $ case prevM of
        Just (Entity _ prev) -> case keyRingTxConfidence prev of
            TxOffline -> eitherToMaybe $ 
                mergeTxs [tx, keyRingTxTx prev] outPoints
            _ -> Nothing
        -- Nothing to merge. Return the original transaction.
        _ -> Nothing
  where
    buildOutpoint c t = OutPoint (keyRingTxHash t) (keyRingCoinPos c)
    f (Entity _ c, t, _) = (keyRingCoinScript c, buildOutpoint c t)
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
    -> KeyRingAccountId
    -> [InCoinData] 
    -> [Entity KeyRingTx]
    -> SqlPersistT m (TxHash, TxConfidence)
importOfflineTx tx ai inCoins spendingTxs = do
    -- Get all the new coins to be created by this transaction
    outCoins <- getNewCoins tx $ Just ai
    -- Only continue if the transaction is relevant to the account
    when (null inCoins && null outCoins) err
    -- Find the details of an existing transaction if it exists.
    prevM <- liftM (fmap entityVal) $ getBy $ UniqueAccTx ai txid
    -- Check if we can import the transaction
    unless (canImport $ keyRingTxConfidence <$> prevM) err
    -- Kill transactions that are spending our coins
    killTxIds $ map entityKey spendingTxs
    -- Create all the transaction records for this account.
    -- This will spend the input coins and create the output coins
    buildAccTxs tx TxOffline inCoins outCoins
    -- use the addresses (refill the gap addresses)
    forM_ (nubBy sameKey $ map outCoinDataAddr outCoins) $ 
        useAddress . entityVal
    return (txid, TxOffline)
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
        "importLocalTx: Could not import local transaction"

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
    -> SqlPersistT m (Maybe (TxConfidence, M.Map KeyRingAccountId Word32))
       -- ^ For each account implicated, the tx confidence and the number of
       -- new addresses generated.
importNetTx tx = do
    -- Find all the coins spent by this transaction
    inCoins <- getInCoins tx Nothing
    -- Get all the new coins to be created by this transaction
    outCoins <- getNewCoins tx Nothing
    -- Only continue if the transaction is relevant to the wallet
    if null inCoins && null outCoins then return Nothing else do
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
        buildRes <- buildAccTxs tx confidence inCoins outCoins
        -- Use up the addresses of our new coins (replenish gap addresses)
        newAddrCnt <- forM (nubBy sameKey $ map outCoinDataAddr outCoins) $
            useAddress . entityVal
        -- Return the confidence and number of new addresses generated in each
        -- account
        let newConf | any (\(_,c) -> c == TxBuilding) buildRes = TxBuilding
                    | otherwise = confidence
        return $ Just ( newConf
                      , M.filter (> 0) $ M.fromListWith (+) newAddrCnt
                      )
  where
    sameKey e1 e2 = entityKey e1 == entityKey e2
    txid = txHash tx

updateNosigHash :: MonadIO m => Tx -> TxHash -> TxHash -> SqlPersistT m ()
updateNosigHash tx nosig txid = do
    res <- select $ from $ \t -> do
        where_ (   t ^. KeyRingTxNosigHash ==. val nosig
               &&. t ^. KeyRingTxHash      !=. val txid
               )
        return $ t ^. KeyRingTxHash
    let toUpdate = map unValue res
    unless (null toUpdate) $ do
        splitUpdate toUpdate $ \hs -> \t -> do
            set t [ KeyRingTxHash =. val txid
                  , KeyRingTxTx   =. val tx 
                  ]
            where_ $ t ^. KeyRingTxHash `in_` valList hs
        splitUpdate toUpdate $ \hs -> \c -> do
            set c [ KeyRingCoinHash =. val txid ]
            where_ $ c ^. KeyRingCoinHash `in_` valList hs

-- Check if the given coins can be spent.
canSpendCoins :: [InCoinData]
              -> [Entity KeyRingTx]
              -> Bool -- True for offline transactions
              -> Bool
canSpendCoins inCoins spendingTxs offline =
    all validCoin inCoins && 
    all validSpend spendingTxs
  where
    -- We can only spend pending and building coins
    validCoin (_,t,_) 
        | offline   = keyRingTxConfidence t /= TxDead
        | otherwise = keyRingTxConfidence t `elem` [TxPending, TxBuilding]
    -- All transactions spending the same coins as us should be offline
    validSpend = (== TxOffline) . keyRingTxConfidence . entityVal

-- Get the coins in the wallet related to the inputs of a transaction. You
-- can optionally provide an account to limit the returned coins to that
-- account only.
getInCoins :: MonadIO m 
           => Tx 
           -> Maybe KeyRingAccountId
           -> SqlPersistT m [InCoinData]
getInCoins tx aiM = do
    res <- splitSelect ops $ \os -> from $ \(c `InnerJoin` t `InnerJoin` x) -> do
        on $ x ^. KeyRingAddrId ==. c ^. KeyRingCoinAddr
        on $ t ^. KeyRingTxId   ==. c ^. KeyRingCoinTx
        where_ $ case aiM of
            Just ai -> 
                c ^. KeyRingCoinAccount ==. val ai &&. limitOutPoints c os
            _ -> limitOutPoints c os
        return $ (c, t, x)
    return $ map (\(c, t, x) -> (c, entityVal t, entityVal x)) res
  where
    ops = map prevOutput $ txIn tx
    limitOutPoints c os = fromMaybe (val False) $ foldl (f c) Nothing os
    f c accM (OutPoint h i) = 
        let b =   c ^. KeyRingCoinHash ==. val h
              &&. c ^. KeyRingCoinPos  ==. val i
        in  Just $ case accM of
                Nothing  -> b
                Just acc -> acc ||. b

-- Find all the transactions that are spending the same coins as the given
-- transaction. You can optionally provide an account to limit the returned
-- transactions to that account only.
getSpendingTxs :: MonadIO m
               => Tx
               -> Maybe KeyRingAccountId
               -> SqlPersistT m [Entity KeyRingTx]
getSpendingTxs tx aiM 
    | null txInputs = return []
    | otherwise =  do
        splitSelect txInputs $ \ins -> from $ \(s `InnerJoin` t) -> do
            on $ s ^. KeyRingSpentCoinSpendingTx ==. t ^. KeyRingTxId
                        -- Filter out the given transaction
            let cond = (   t ^. KeyRingTxHash !=. val txid
                        -- Limit to only the input coins of the given tx
                       &&. limitSpent s ins
                       )
            where_ $ case aiM of
                Just ai -> cond &&. s ^. KeyRingSpentCoinAccount ==. val ai
                _       -> cond
            return t
  where
    txid = txHash tx
    limitSpent s ins = fromMaybe (val False) $ foldl (f s) Nothing ins
    txInputs = map prevOutput $ txIn tx
    f s accM (OutPoint h i) = 
        let b =   s ^. KeyRingSpentCoinHash ==. val h 
              &&. s ^. KeyRingSpentCoinPos  ==. val i
        in  Just $ case accM of
                Nothing  -> b
                Just acc -> acc ||. b

-- Returns all the new coins that need to be created from a transaction.
-- Also returns the addresses associted with those coins.
getNewCoins :: MonadIO m 
            => Tx 
            -> Maybe KeyRingAccountId
            -> SqlPersistT m [OutCoinData]
getNewCoins tx aiM = do
    -- Find all the addresses which are in the transaction outputs
    addrs <- splitSelect uniqueAddrs $ \as -> from $ \x -> do
        let cond = x ^. KeyRingAddrAddress `in_` valList as
        where_ $ case aiM of
            Just ai -> cond &&. x ^. KeyRingAddrAccount ==. val ai
            _       -> cond
        return x
    return $ concat $ map toCoins addrs
  where
    uniqueAddrs      = nub $ map (\(addr,_,_,_) -> addr) outList
    outList          = rights $ map toDat txOutputs
    txOutputs        = zip (txOut tx) [0..]
    toDat (out, pos) = getDataFromOutput out >>= \(addr, so) -> 
        return (addr, out, pos, so)
    toCoins addrEnt@(Entity _ addr) = 
        let f (a,_,_,_) = a == keyRingAddrAddress addr
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
    length tin == 1 && outPointHash (prevOutput $ head tin) == 0x00

-- | Spend the given input coins. We also create dummy coins for the inputs
-- in a transaction that do not belong to us. This is to be able to detect
-- double spends when reorgs occur. 
spendInputs :: MonadIO m 
            => KeyRingAccountId
            -> KeyRingTxId
            -> Tx
            -> SqlPersistT m ()
spendInputs ai ti tx = do
    now <- liftIO getCurrentTime
    -- Spend the coins by inserting values in KeyRingSpentCoin
    P.insertMany_ $ map (buildSpentCoin now) txInputs
  where
    txInputs = map prevOutput $ txIn tx
    buildSpentCoin now (OutPoint h p) =
        KeyRingSpentCoin{ keyRingSpentCoinAccount    = ai
                        , keyRingSpentCoinHash       = h
                        , keyRingSpentCoinPos        = p
                        , keyRingSpentCoinSpendingTx = ti
                        , keyRingSpentCoinCreated    = now
                        }

-- Build account transaction for the given input and output coins
buildAccTxs :: MonadIO m 
            => Tx 
            -> TxConfidence 
            -> [InCoinData] 
            -> [OutCoinData] 
            -> SqlPersistT m [(KeyRingTxId, TxConfidence)]
buildAccTxs tx confidence inCoins outCoins = do
    now <- liftIO getCurrentTime
    -- Group the coins by account
    let grouped = groupCoinsByAccount inCoins outCoins
    forM (M.toList grouped) $ \(ai, (is, os)) -> do
        let atx = buildAccTx tx confidence ai is os now
        -- Insert the new transaction. If it already exists, update the
        -- information with the newly computed values. Also make sure that the
        -- confidence is set to the new value (it could have changed to TxDead).
        (ti, newConf) <- P.insertBy atx >>= \resE -> case resE of
            Left (Entity ti prev) -> do
                let prevConf = keyRingTxConfidence prev
                    newConf | confidence == TxDead     = TxDead
                            | prevConf   == TxBuilding = TxBuilding
                            | otherwise                = confidence
                -- If the transaction already exists, preserve confirmation data
                replace ti atx
                    { keyRingTxConfidence      = newConf
                    , keyRingTxConfirmedBy     = keyRingTxConfirmedBy prev
                    , keyRingTxConfirmedHeight = keyRingTxConfirmedHeight prev
                    , keyRingTxConfirmedDate   = keyRingTxConfirmedDate prev
                    }
                -- Spend inputs only if the previous transaction was dead
                when (newConf /= TxDead && prevConf == TxDead) $
                    spendInputs ai ti tx
                -- If the transaction changed from non-dead to dead, kill it.
                -- This will remove spent coins and child transactions.
                when (prevConf /= TxDead && newConf == TxDead) $ killTxIds [ti]
                return (ti, newConf)
            Right ti -> do
                when (confidence /= TxDead) $ spendInputs ai ti tx
                return (ti, confidence)

        -- Insert the output coins with updated accTx key
        let newOs = map (toCoin ai ti now) os
        forM_ newOs $ \c -> P.insertBy c >>= \resE -> case resE of
            Left (Entity ci _) -> replace ci c
            _ -> return ()

        -- Insert address transactions
        let addrGrouped = groupCoinsByAddress is os
        forM_ (M.toList addrGrouped) $ \(addrId, (addrIs, addrOs)) -> do
            let addrTx = buildKeyRingAddrTx 
                            addrId ti (keyRingTxType atx) addrIs addrOs now
            P.insertBy addrTx >>= \resE -> case resE of
                Left (Entity ati _) -> replace ati addrTx
                _ -> return ()

        -- Return the new database id
        return (ti, newConf)
  where
    toCoin ai accTxId now (OutCoinData addrEnt pos val so) = KeyRingCoin
        { keyRingCoinAccount = ai
        , keyRingCoinHash    = txHash tx
        , keyRingCoinPos     = pos
        , keyRingCoinTx      = accTxId
        , keyRingCoinValue   = val
        , keyRingCoinScript  = so
        , keyRingCoinAddr    = entityKey addrEnt
        , keyRingCoinCreated = now
        }

-- | Build an account transaction given the input and output coins relevant to
-- this specific account. An account transaction contains the details of how a
-- transaction affects one particular account (value sent to and from the
-- account). The first value is Maybe an existing transaction in the database
-- which is used to get the existing confirmation values.
buildAccTx :: Tx 
           -> TxConfidence
           -> KeyRingAccountId 
           -> [InCoinData] 
           -> [OutCoinData] 
           -> UTCTime 
           -> KeyRingTx
buildAccTx tx confidence ai inCoins outCoins now = KeyRingTx
    { keyRingTxAccount = ai
    , keyRingTxHash    = txHash tx
    -- This is a hash of the transaction excluding signatures. This allows us
    -- to track the evolution of offline transactions as we add more signatures
    -- to them.
    , keyRingTxNosigHash = nosigTxHash tx
    , keyRingTxType      = txType 
    , keyRingTxInValue   = inVal
    , keyRingTxOutValue  = outVal
    , keyRingTxInputs = 
        let f h i (Entity _ c, t, _) = 
                keyRingTxHash t == h && keyRingCoinPos c == i
            toInfo (a, OutPoint h i) = case find (f h i) inCoins of
                Just (Entity _ c,_,_) -> 
                    AddressInfo a (Just $ keyRingCoinValue c) True
                _ -> AddressInfo a Nothing False
        in  map toInfo allInAddrs
    , keyRingTxOutputs = 
        let toInfo (a,i,v) = AddressInfo a (Just v) $ ours i
            ours i = isJust $ find ((== i) . outCoinDataPos) outCoins
        in  map toInfo allOutAddrs \\ changeAddrs
    , keyRingTxChange     = changeAddrs
    , keyRingTxTx         = tx
    , keyRingTxIsCoinbase = isCoinbaseTx tx
    , keyRingTxConfidence = confidence
        -- Reuse the confirmation information of the existing transaction if
        -- we have it.
    , keyRingTxConfirmedBy     = Nothing
    , keyRingTxConfirmedHeight = Nothing
    , keyRingTxConfirmedDate   = Nothing
    , keyRingTxCreated         = now
    }
  where
    -- The value going into the account is the sum of the output coins
    inVal  = sum $ map outCoinDataValue outCoins
    -- The value going out of the account is the sum on the input coins
    outVal = sum $ map (keyRingCoinValue . entityVal . fst3) inCoins
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
        let f (op, i) = do
                addr <- scriptRecipient =<< decodeToEither (scriptOutput op)
                return (addr, i, outValue op)
        in  rights $ map f $ zip (txOut tx) [0..]
    changeAddrs 
        | txType == TxIncoming = []
        | otherwise = 
            let isInternal = (== AddressInternal) . keyRingAddrType 
                                . entityVal . outCoinDataAddr
                f = keyRingAddrAddress . entityVal . outCoinDataAddr 
                toInfo c = AddressInfo (f c) (Just $ outCoinDataValue c) True
            in  map toInfo $ filter isInternal outCoins

buildKeyRingAddrTx :: KeyRingAddrId
                   -> KeyRingTxId
                   -> TxType
                   -> [InCoinData] 
                   -> [OutCoinData] 
                   -> UTCTime
                   -> KeyRingAddrTx
buildKeyRingAddrTx aid tid txType inCoins outCoins now = KeyRingAddrTx
    { keyRingAddrTxAddr     = aid
    , keyRingAddrTxAccTx    = tid
    , keyRingAddrTxType     = addrTxType
    , keyRingAddrTxInValue  = inVal
    , keyRingAddrTxOutValue = outVal
    , keyRingAddrTxCreated  = now
    }
  where
    inVal  = sum $ map outCoinDataValue outCoins
    outVal = sum $ map (keyRingCoinValue . entityVal . fst3) inCoins
    change =
        let cs = map lst3 inCoins ++ map (entityVal . outCoinDataAddr) outCoins
            isInternal = any ((AddressInternal ==) . keyRingAddrType) cs
        in  isInternal && txType /= TxIncoming
    addrTxType | outVal > inVal = AddrTxOutgoing
               | change         = AddrTxChange
               | otherwise      = AddrTxIncoming

-- Group all the input and outputs coins from the same account together.
groupCoinsByAccount 
    :: [InCoinData] 
    -> [OutCoinData] 
    -> M.Map KeyRingAccountId ([InCoinData], [OutCoinData])
groupCoinsByAccount inCoins outCoins =
    M.unionWith merge inMap outMap
  where
    -- Build a map from accounts -> (inCoins, outCoins)
    f coin@(_,t,_) = (keyRingTxAccount t  , [coin])
    g coin = (keyRingAddrAccount $ entityVal $  outCoinDataAddr coin, [coin])
    merge (is, _) (_, os) = (is, os)
    inMap  = M.map (\is -> (is, [])) $ M.fromListWith (++) $ map f inCoins
    outMap = M.map (\os -> ([], os)) $ M.fromListWith (++) $ map g outCoins

-- Group all the input and outputs coins from the same address together.
groupCoinsByAddress 
    :: [InCoinData] 
    -> [OutCoinData] 
    -> M.Map KeyRingAddrId ([InCoinData], [OutCoinData])
groupCoinsByAddress inCoins outCoins =
    M.unionWith merge inMap outMap
  where
    -- Build a map from address -> (inCoins, outCoins)
    f coin@(Entity _ c,_,_) = (keyRingCoinAddr c, [coin])
    g coin = (entityKey $ outCoinDataAddr coin, [coin])
    merge (is, _) (_, os) = (is, os)
    inMap  = M.map (\is -> (is, [])) $ M.fromListWith (++) $ map f inCoins
    outMap = M.map (\os -> ([], os)) $ M.fromListWith (++) $ map g outCoins

-- Kill transactions and their child transactions by ids.
killTxIds :: MonadIO m => [KeyRingTxId] -> SqlPersistT m ()
killTxIds txIds = do
    -- Find all the transactions spending the coins of these transactions
    -- (Find all the child transactions)
    childs <- splitSelect txIds $ \ts -> from $ \(t `InnerJoin` s) -> do
        on (   s ^. KeyRingSpentCoinAccount ==. t ^. KeyRingTxAccount
           &&. s ^. KeyRingSpentCoinHash    ==. t ^. KeyRingTxHash
           )
        where_ $ t ^. KeyRingTxId `in_` valList ts
        return $ s ^. KeyRingSpentCoinSpendingTx

    -- Recursively kill all the child transactions.
    unless (null childs) $ killTxIds $ nub $ map unValue childs

    -- Kill these transactions
    splitUpdate txIds $ \ts -> \t -> do
        set t [ KeyRingTxConfidence =. val TxDead ]
        where_ $ t ^. KeyRingTxId `in_` valList ts

    -- This transaction doesn't spend any coins
    splitDelete txIds $ \ts -> from $ \s -> 
        where_ $ s ^. KeyRingSpentCoinSpendingTx `in_` valList ts

-- Kill transactions and their child transactions by hashes.
killTxs :: MonadIO m => [TxHash] -> SqlPersistT m ()
killTxs txHashes = do
    res <- splitSelect txHashes $ \hs -> from $ \t -> do
        where_ $ t ^. KeyRingTxHash `in_` valList hs
        return $ t ^. KeyRingTxId
    killTxIds $ map unValue res

{- Confirmations -}

importMerkles :: MonadIO m
              => BlockChainAction -> [[TxHash]] -> SqlPersistT m ()
importMerkles action expTxsLs = unless (isSideChain action) $ do
    case action of
        ChainReorg _ os _ -> 
            -- Unconfirm transactions from the old chain.
            let hs = map (\node -> Just $ nodeBlockHash node) os
            in  splitUpdate hs $ \h -> \t -> do
                    set t [ KeyRingTxConfidence      =. val TxPending
                          , KeyRingTxConfirmedBy     =. val Nothing
                          , KeyRingTxConfirmedHeight =. val Nothing
                          , KeyRingTxConfirmedDate   =. val Nothing
                          ]
                    where_ $ t ^. KeyRingTxConfirmedBy `in_` valList h
        _ -> return ()

    -- Find all the dead transactions which need to be revived
    deadTxs <- splitSelect (concat expTxsLs) $ \ts -> from $ \t -> do
        where_ (   t ^. KeyRingTxHash `in_` valList ts
               &&. t ^. KeyRingTxConfidence ==. val TxDead
               )
        return $ t ^. KeyRingTxTx

    -- Revive dead transactions (in no particular order)
    forM_ deadTxs $ reviveTx . unValue

    -- Confirm the transactions
    forM_ (zip (actionNewNodes action) expTxsLs) $ \(node, hs) -> 
        splitUpdate hs $ \h -> \t -> do
            set t [ KeyRingTxConfidence =. 
                        val TxBuilding
                  , KeyRingTxConfirmedBy =. 
                        val (Just (nodeBlockHash node))
                  , KeyRingTxConfirmedHeight =. 
                        val (Just (nodeHeaderHeight node))
                  , KeyRingTxConfirmedDate =. 
                        val (Just (blockTimestamp $ nodeHeader node))
                  ]
            where_ $ t ^. KeyRingTxHash `in_` valList h

    -- Update the best height in the wallet (used to compute the number
    -- of confirmations of transactions)
    case reverse $ actionNewNodes action of
        (best:_) -> setBestBlock (nodeBlockHash best) (nodeHeaderHeight best)
        _ -> return ()

-- Helper function to set the best block and best block height in the DB.
setBestBlock :: MonadIO m => BlockHash -> Word32 -> SqlPersistT m ()
setBestBlock bid i = update $ \t -> set t [ KeyRingConfigBlock  =. val bid
                                          , KeyRingConfigHeight =. val i
                                          ]

-- Helper function to get the best block and best block height from the DB
getBestBlock :: MonadIO m => SqlPersistT m (BlockHash, Word32)
getBestBlock = do
    cfgM <- liftM (fmap entityVal) $ P.selectFirst [] []
    return $ case cfgM of
        Just KeyRingConfig{..} -> (keyRingConfigBlock, keyRingConfigHeight)
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

    -- Find all the KeyRingTxId that have to be revived
    ids <- select $ from $ \t -> do
        where_ (   t ^. KeyRingTxHash       ==. val (txHash tx)
               &&. t ^. KeyRingTxConfidence ==. val TxDead
               )
        return (t ^. KeyRingTxAccount, t ^. KeyRingTxId)

    -- Spend the inputs for all our transactions
    forM_ ids $ \(Value ai, Value ti) -> spendInputs ai ti tx

    -- Update the transactions
    splitUpdate (map (unValue . snd) ids) $ \is -> \t -> do
        set t [ KeyRingTxConfidence      =. val TxPending 
              , KeyRingTxConfirmedBy     =. val Nothing
              , KeyRingTxConfirmedHeight =. val Nothing
              , KeyRingTxConfirmedDate   =. val Nothing
              ]
        where_ $ t ^. KeyRingTxId `in_` valList is

{- Transaction creation and signing (local wallet functions) -}

-- | Create a transaction sending some coins to a list of recipient addresses.
createTx :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
         => KeyRingName         -- ^ KeyRing name
         -> AccountName         -- ^ Account name
         -> Word32              -- ^ Minimum confirmations
         -> [(Address,Word64)]  -- ^ List of recipient addresses and amounts
         -> Word64              -- ^ Fee per 1000 bytes 
         -> Bool                -- ^ Should fee be paid by recipient
         -> Bool                -- ^ Should the transaction be signed
         -> SqlPersistT m (TxHash, TxConfidence) 
            -- ^ (New transaction hash, Completed flag)
createTx keyRingName accountName minConf dests fee rcptFee sign = do
    Entity _ keyRing <- getKeyRing keyRingName
    (_, accE@(Entity ai acc)) <- getAccount keyRingName accountName
    -- Build an unsigned transaction from the given recipient values and fee
    (unsignedTx, inCoins) <- buildUnsignedTx accE minConf dests fee rcptFee
    -- Sign our new transaction if signing was requested
    let tx | sign = signOfflineTx keyRing acc unsignedTx $ 
                        map toCoinSignData inCoins
           | otherwise = unsignedTx
    -- Import the transaction in the wallet either as a network transaction if
    -- it is complete, or as an offline transaction otherwise.
    importTx' tx ai inCoins

toCoinSignData :: InCoinData -> CoinSignData
toCoinSignData (Entity _ c, t, x) =
    CoinSignData (OutPoint (keyRingTxHash t) (keyRingCoinPos c))
                 (keyRingCoinScript c)
                 (keyRingAddrDerivation x)

-- Build an unsigned transaction given a list of recipients and a fee. Returns
-- the unsigned transaction together with the input coins that have been
-- selected or spending.
buildUnsignedTx 
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
    => Entity KeyRingAccount
    -> Word32
    -> [(Address, Word64)] 
    -> Word64
    -> Bool
    -> SqlPersistT m (Tx, [InCoinData])
buildUnsignedTx accE@(Entity ai acc) minConf origDests origFee rcptFee = do
    let p = case keyRingAccountType acc of
                AccountMultisig _ m n -> (m, n)
                _ -> throw . WalletException $ "Invalid account type"
        fee = if rcptFee then 0 else origFee
        sink | isMultisigAccount acc = chooseMSCoinsSink tot fee p True
             | otherwise             = chooseCoinsSink   tot fee   True
        -- TODO: Add more policies like confirmations or coin age
        -- Sort coins by their values in descending order
        orderPolicy c _ = [desc $ c ^. KeyRingCoinValue]

    -- Find the spendable coins in the given account with the required number
    -- of minimum confirmations.
    selectRes <- spendableCoinsSource ai minConf orderPolicy $$ sink

    -- Find a selection of spendable coins that matches our target value
    let (selected, change) = either (throw . WalletException) id selectRes
        totFee | isMultisigAccount acc = getMSFee origFee p (length selected)
               | otherwise             = getFee   origFee   (length selected)
        -- Subtract fees from first destination if rcptFee
        value = snd $ head origDests

    -- First output must not be dust after deducting fees
    when (rcptFee && value < totFee + 5430) $ throw $ WalletException
        "First recipient cannot cover transaction fees"

        -- Subtract fees from first destination if rcptFee
    let dests = if rcptFee
            then second (const $ value - totFee) (head origDests) : 
                 tail origDests
            else origDests

    -- Make sure the first recipient has enough funds to cover the fee
    when (snd (head dests) <= 0) $ throw $
        WalletException "Transaction fees too high" 

    -- If the change amount is not dust, we need to add a change address to
    -- our list of recipients.
    -- TODO: Put the dust value in a constant somewhere. We also need a more
    -- general way of detecting dust such as our transactions are not
    -- rejected by full nodes.
    allDests <- if change < 5430 
        then return dests 
        else addChangeAddr change dests

    case buildAddrTx (map toOutPoint selected) $ map toBase58 allDests of
        Right tx -> return (tx, selected)
        Left err -> liftIO . throwIO $ WalletException err
  where
    tot = sum $ map snd origDests
    toBase58 (a, v) = (addrToBase58 a, v)
    toOutPoint ((Entity _ c), t, _) = 
        OutPoint (keyRingTxHash t) (keyRingCoinPos c)
    addChangeAddr change dests = do
        a <- firstUnusedAddress accE AddressInternal
        -- Use the address to prevent reusing it again
        useAddress a
        -- TODO: Randomize the change position
        return $ (keyRingAddrAddress a, change) : dests

signKeyRingTx :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => KeyRing -> Entity KeyRingAccount -> TxHash
              -> SqlPersistT m (TxHash, TxConfidence)
signKeyRingTx keyRing (Entity ai acc) txid = do
    (OfflineTxData tx dat, inCoins) <- getOfflineTxData ai txid
    let signedTx = signOfflineTx keyRing acc tx dat
    importTx' signedTx ai inCoins

getOfflineTxData 
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
    => KeyRingAccountId 
    -> TxHash 
    -> SqlPersistT m (OfflineTxData, [InCoinData])
getOfflineTxData ai txid = do
    txM <- getBy $ UniqueAccTx ai txid
    case txM of
        Just (Entity _ tx) -> do
            unless (keyRingTxConfidence tx == TxOffline) $ liftIO . throwIO $ 
                WalletException "Can only sign offline transactions."
            inCoins <- getInCoins (keyRingTxTx tx) $ Just ai
            return 
                ( OfflineTxData (keyRingTxTx tx) $ map toCoinSignData inCoins
                , inCoins
                )
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Invalid txid", encodeTxHashLE txid ]

-- Sign a transaction using a list of CoinSignData. This allows an offline
-- signer without access to the coins to sign a given transaction.
signOfflineTx :: KeyRing        -- ^ KeyRing
              -> KeyRingAccount -- ^ Account used for signing
              -> Tx             -- ^ Transaction to sign
              -> [CoinSignData] -- ^ Input signing data
              -> Tx
signOfflineTx keyRing acc tx coinSignData 
    -- Fail for read-only accounts
    | isReadAccount acc = throw $ WalletException
        "signOfflineTx is not supported on read-only accounts"
    -- Sign the transaction deterministically
    | otherwise = either (throw . WalletException) id $
        detSignTx tx sigData $ map (toPrvKeyG . xPrvKey) prvKeys
  where
    -- Compute all the SigInputs
    sigData = map (toSigData acc) coinSignData
    -- Compute all the private keys
    prvKeys = map (toPrvKey (keyRingMaster keyRing) acc) coinSignData
    -- Build a SigInput from a CoinSignData
    toSigData acc' (CoinSignData op so deriv) =
        -- TODO: Here we override the SigHash to be SigAll False all the time.
        -- Should we be more flexible?
        SigInput so op (SigAll False) $ 
            if isMultisigAccount acc 
                then Just $ getPathRedeem acc' deriv
                else Nothing
    toPrvKey master acc' (CoinSignData _ _ deriv) =
        case keyRingAccountDerivation acc' of
            Just root -> derivePath (root ++| deriv) master
            _ -> throw $ WalletException $ unwords
                [ "No derivation available in account"
                , unpack $ keyRingAccountName acc'
                ]

-- Returns unspent coins that can be spent in an account that have a minimum
-- number of confirmations. Coinbase coins can only be spent after 100
-- confirmations.
spendableCoins 
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
    => KeyRingAccountId                   -- ^ Account key
    -> Word32                             -- ^ Minimum confirmations
    -> (SqlExpr (Entity KeyRingCoin) -> SqlExpr (Entity KeyRingTx) -> [SqlExpr OrderBy]) 
        -- ^ Coin ordering policy
    -> SqlPersistT m [InCoinData]       -- ^ Spendable coins
spendableCoins ai minConf orderPolicy = 
    spendableCoinsSource ai minConf orderPolicy $$ CL.consume

spendableCoinsSource
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
    => KeyRingAccountId -- ^ Account key
    -> Word32           -- ^ Minimum confirmations
    -> (SqlExpr (Entity KeyRingCoin) -> SqlExpr (Entity KeyRingTx) -> [SqlExpr OrderBy]) 
        -- ^ Coin ordering policy
    -> Source (SqlPersistT m) InCoinData
        -- ^ Spendable coins
spendableCoinsSource ai minConf orderPolicy = mapOutput f $ selectSource $ 
    from $ \(c `InnerJoin` t `InnerJoin` x `LeftOuterJoin` s) -> do
        -- Joins have to be set in reverse order !
        -- Left outer join on spent coins
        on (   s ?. KeyRingSpentCoinAccount ==. just (c ^. KeyRingCoinAccount)
           &&. s ?. KeyRingSpentCoinHash    ==. just (c ^. KeyRingCoinHash) 
           &&. s ?. KeyRingSpentCoinPos     ==. just (c ^. KeyRingCoinPos)
           )
        on $ x ^. KeyRingAddrId ==. c ^. KeyRingCoinAddr
        -- Inner join on coins and transactions
        on $  t ^. KeyRingTxId ==. c ^. KeyRingCoinTx
        where_ (   c ^. KeyRingCoinAccount ==. val ai
               &&. t ^. KeyRingTxConfidence 
                   `in_` valList [ TxPending, TxBuilding ]
                -- We only want unspent coins
               &&. E.isNothing (s ?. KeyRingSpentCoinId)
               &&. limitConfirmations (Right t) minConf
               )
        orderBy (orderPolicy c t)
        return (c, t, x)
  where
    f (c, t, x) = (c, entityVal t, entityVal x)

-- If the current height is 200 and a coin was confirmed at height 198, then it
-- has 3 confirmations. So, if we require 3 confirmations, we want coins with a
-- confirmed height of 198 or less (200 - 3 + 1).
limitConfirmations :: Either (SqlExpr (Maybe (Entity KeyRingTx))) 
                             (SqlExpr (Entity KeyRingTx))
                   -> Word32
                   -> SqlExpr (Value Bool)
limitConfirmations txE minconf
    | minconf == 0  = limitCoinbase
    | minconf < 100 = limitConfs minconf &&. limitCoinbase
    | otherwise     = limitConfs minconf
  where
    limitConfs i = case txE of
        Left t -> t ?. KeyRingTxConfirmedHeight 
            <=. just (just (selectHeight -. (val $ i - 1)))
        Right t -> t ^. KeyRingTxConfirmedHeight 
            <=. just (selectHeight -. (val $ i - 1))
    -- Coinbase transactions require 100 confirmations
    limitCoinbase = case txE of
        Left t ->
            (not_ $ coalesceDefault [t ?. KeyRingTxIsCoinbase] (val False)) ||. 
            limitConfs 100
        Right t ->
            (not_ $ t ^. KeyRingTxIsCoinbase) ||. limitConfs 100
    selectHeight :: SqlExpr (Value Word32)
    selectHeight = sub_select $ from $ \co -> do
        limit 1
        return $ co ^. KeyRingConfigHeight

{- Balances -}

accountBalance :: MonadIO m 
               => KeyRingName
               -> AccountName
               -> Word32
               -> Bool 
               -> SqlPersistT m Word64
accountBalance keyRingName accountName minconf offline = do
    res <- select $ from $ \(k `InnerJoin` a `InnerJoin` c `InnerJoin` t
                               `LeftOuterJoin` s `LeftOuterJoin` st) -> do
        on $ st ?. KeyRingTxId ==. s ?. KeyRingSpentCoinSpendingTx
        on (   s ?. KeyRingSpentCoinAccount ==. just (c ^. KeyRingCoinAccount)
           &&. s ?. KeyRingSpentCoinHash    ==. just (c ^. KeyRingCoinHash) 
           &&. s ?. KeyRingSpentCoinPos     ==. just (c ^. KeyRingCoinPos)
           )
        on $ t ^. KeyRingTxId           ==. c ^. KeyRingCoinTx
        on $ c ^. KeyRingCoinAccount    ==. a ^. KeyRingAccountId
        on $ a ^. KeyRingAccountKeyRing ==. k ^. KeyRingId
        let unspent = E.isNothing ( s ?. KeyRingSpentCoinId )
            spentOffline = st ?. KeyRingTxConfidence ==. just (val TxOffline)
            cond = (   k ^. KeyRingName        ==. val keyRingName
                   &&. a ^. KeyRingAccountName ==. val accountName
                   &&. t ^. KeyRingTxConfidence `in_` valList validConfidence
                   -- For non-offline balances, we have to take into account
                   -- the coins which are spent by offline transactions.
                   &&. if offline then unspent else (unspent ||. spentOffline)
                   )
        where_ $ if minconf == 0 
                    then cond 
                    else cond &&. limitConfirmations (Right t) minconf
        return $ sum_ (c ^. KeyRingCoinValue)
    case res of
        ((Value (Just s)):_) -> return $ floor (s :: Double)
        _ -> return 0
  where
    validConfidence = TxPending : TxBuilding : [ TxOffline | offline ]

addressBalances :: MonadIO m
                => KeyRingName
                -> AccountName
                -> KeyIndex
                -> KeyIndex
                -> AddressType
                -> Word32
                -> Bool
                -> SqlPersistT m [(KeyIndex, AddressBalance)]
addressBalances keyRingName accountName iMin iMax addrType minconf offline = do
    res <- select $ from $ 
        \(k `InnerJoin` a `InnerJoin` x `LeftOuterJoin` 
          c `LeftOuterJoin` t `LeftOuterJoin` s `LeftOuterJoin` st) -> do
        let joinCond = st ?. KeyRingTxId ==. s ?. KeyRingSpentCoinSpendingTx
        -- Do not join the spending information for offline transactions if we
        -- request the online balances. This will count the coin as unspent.
        on $ if offline 
            then joinCond
            else joinCond &&. st ?. KeyRingTxConfidence !=. just (val TxOffline)
        on (   s ?. KeyRingSpentCoinAccount ==. c ?. KeyRingCoinAccount
           &&. s ?. KeyRingSpentCoinHash    ==. c ?. KeyRingCoinHash
           &&. s ?. KeyRingSpentCoinPos     ==. c ?. KeyRingCoinPos
           )
        on $ t ?. KeyRingTxId           ==. c ?. KeyRingCoinTx
        on $ c ?. KeyRingCoinAddr       ==. just (x ^. KeyRingAddrId)
        on $ x ^. KeyRingAddrAccount    ==. a ^. KeyRingAccountId
        on $ a ^. KeyRingAccountKeyRing ==. k ^. KeyRingId
        let limitIndex 
                | iMin == iMax = x ^. KeyRingAddrIndex ==. val iMin
                | otherwise = (   x ^. KeyRingAddrIndex >=. val iMin
                              &&. x ^. KeyRingAddrIndex <=. val iMax
                              )
            cond = (   k ^. KeyRingName        ==. val keyRingName
                   &&. a ^. KeyRingAccountName ==. val accountName
                   &&. limitIndex
                   &&. x ^. KeyRingAddrIndex   <.  subSelectAddrCount a addrType
                   &&. x ^. KeyRingAddrType    ==. val addrType
                   )
        where_ $ if minconf == 0 
                    then cond 
                    else cond &&. limitConfirmations (Left t) minconf
        groupBy $ x ^. KeyRingAddrIndex
        let unspent = E.isNothing ( st ?. KeyRingTxId )
        return ( x ^. KeyRingAddrIndex -- Address index
               , sum_ $ c ?. KeyRingCoinValue -- In value
               , sum_ $ case_ 
                   [ when_ unspent
                     then_ (val (Just 0)) 
                   ] (else_ $ c ?. KeyRingCoinValue) -- Out value
               , count $ c ?. KeyRingCoinId -- New coins
               , count $ st ?. KeyRingTxId  -- Spent coins
               )
    return $ map f res
  where
    validConfidence = TxPending : TxBuilding : [ TxOffline | offline ]
    f (Value i, Value inM, Value outM, Value newC, Value spentC) = 
        let b = AddressBalance 
                    { addrBalanceInBalance  = 
                        floor $ fromMaybe (0 :: Double) inM
                    , addrBalanceOutBalance = 
                        floor $ fromMaybe (0 :: Double) outM
                    , addrBalanceCoins      = newC
                    , addrBalanceSpentCoins = spentC
                    }
        in (i, b)
    f _ = throw $ WalletException "Could not compute the balance"

{- Rescans -}

resetRescan :: MonadIO m => SqlPersistT m ()
resetRescan = do
    P.deleteWhere ([] :: [P.Filter KeyRingCoin])
    P.deleteWhere ([] :: [P.Filter KeyRingAddrTx])
    P.deleteWhere ([] :: [P.Filter KeyRingSpentCoin])
    P.deleteWhere ([] :: [P.Filter KeyRingTx])
    setBestBlock (headerHash genesisHeader) 0

{- Helpers -}

splitSelect :: (SqlSelect a r, MonadIO m) 
            => [t]
            -> ([t] -> SqlQuery a) 
            -> SqlPersistT m [r] 
splitSelect ts queryF = 
    liftM concat $ forM vals $ select . queryF
  where
    vals = splitEvery paramLimit ts

splitUpdate :: ( MonadIO m
               , P.PersistEntity val
               , P.PersistEntityBackend val ~ SqlBackend
               )
            => [t]
            -> ([t] -> SqlExpr (Entity val) -> SqlQuery ())
            -> SqlPersistT m ()
splitUpdate ts updateF =
    forM_ vals $ update . updateF
  where
    vals = splitEvery paramLimit ts

splitDelete :: MonadIO m => [t] -> ([t] -> SqlQuery ()) -> SqlPersistT m ()
splitDelete ts deleteF =
    forM_ vals $ E.delete . deleteF
  where
    vals = splitEvery paramLimit ts

