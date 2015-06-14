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
, killTx
, killAccTx
, reviveTx
, getPendingTxs

-- *Database blocks
, importMerkles
, getBestBlock

-- *Database coins and balances
, spendableCoins
, spendableCoinsSource
, accountBalance
, offlineBalance

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
import Data.Maybe (catMaybes, mapMaybe, isNothing)
import Data.Either (rights)
import Data.List ((\\), nub, delete)
import Data.Text (unpack)
import Data.Conduit (Source, mapOutputMaybe, ($$))
import qualified Data.Conduit.List as CL (consume)
import qualified Data.Map.Strict as M 
    ( Map, toList, map
    , unionWith, fromListWith, filter
    )

import Database.Persist.Sql (SqlPersistT)
import Database.Persist 
    ( Entity(..), Filter
    , entityVal, entityKey, getBy, replace, deleteWhere, selectSource
    , selectList, selectFirst, updateWhere, insertBy
    , update , count, (=.), (==.), (<-.), (<=.), (!=.), (-=.), (+=.)
    , SelectOpt( Asc, Desc, LimitTo, OffsetBy ), Key
    )

import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Constants

import Network.Haskoin.Wallet.KeyRing
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

{- List transaction -}

-- | Get transactions by page
txPage :: MonadIO m
       => KeyRingName -- ^ KeyRing name
       -> AccountName -- ^ Account name
       -> PageRequest -- ^ Page request
       -> SqlPersistT m ([KeyRingTx], Int) -- ^ Page result
txPage keyRingName accountName page@PageRequest{..}
    | validPageRequest page = do
        Entity ai _ <- getAccount keyRingName accountName
        cnt <- count [ KeyRingTxAccount ==. ai ]
        if cnt == 0 then return ([], 1) else do
            let (d, m)  = cnt `divMod` pageLen
                maxPage = d + min 1 m
            when (pageNum > maxPage) $ liftIO . throwIO $ WalletException $
                unwords [ "Invalid page number", show pageNum ]
            res <- selectList [ KeyRingTxAccount ==. ai ]
                              [ if pageReverse 
                                      then Asc KeyRingTxId 
                                      else Desc KeyRingTxId
                              , LimitTo pageLen
                              , OffsetBy $ (pageNum - 1) * pageLen
                              ]
            let f | pageReverse = id
                  | otherwise   = reverse
            return (f $ map entityVal res, maxPage)
    | otherwise = liftIO . throwIO $ WalletException $
        unwords [ "Invalid page request:", show page ]

-- | Get address transactions by page
addrTxPage :: MonadIO m
           => KeyRingName -- ^ KeyRing name
           -> AccountName -- ^ Account name
           -> KeyIndex    -- ^ Address index
           -> AddressType -- ^ Address type
           -> PageRequest -- ^ Page request
           -> SqlPersistT m ([KeyRingAddrTx], Int) -- ^ Page result
addrTxPage keyRingName accountName index addrType page@PageRequest{..}
    | validPageRequest page = do
        Entity ai _ <- getAccount keyRingName accountName
        cnt <- count [ KeyRingAddrTxAccount      ==. ai 
                     , KeyRingAddrTxAddressIndex ==. index
                     , KeyRingAddrTxAddressType  ==. addrType
                     ]
        if cnt == 0 then return ([], 1) else do
            let (d, m)  = cnt `divMod` pageLen
                maxPage = d + min 1 m
            when (pageNum > maxPage) $ liftIO . throwIO $ WalletException $
                unwords [ "Invalid page number", show pageNum ]
            res <- selectList [ KeyRingAddrTxAccount      ==. ai 
                              , KeyRingAddrTxAddressIndex ==. index
                              , KeyRingAddrTxAddressType  ==. addrType
                              ]
                              [ if pageReverse 
                                      then Asc KeyRingAddrTxId 
                                      else Desc KeyRingAddrTxId
                              , LimitTo pageLen
                              , OffsetBy $ (pageNum - 1) * pageLen
                              ]
            let f | pageReverse = id
                  | otherwise   = reverse
            return (f $ map entityVal res, maxPage)
    | otherwise = liftIO . throwIO $ WalletException $
        unwords [ "Invalid page request:", show page ]

-- Helper function to get a transaction from the wallet database. The function
-- will look across all accounts and return the first available transaction. If
-- the transaction does not exist, this function will throw a wallet exception.
getTx :: MonadIO m => TxHash -> SqlPersistT m Tx
getTx txid = do
    txM <- selectFirst [ KeyRingTxHash ==. txid ] []
    case txM of
        Just (Entity _ KeyRingTx{..}) -> return keyRingTxTx
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "getTx: Transaction does not exist:", encodeTxHashLE txid ]

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
importTx tx ai = importTx' tx ai =<< getAccInCoins ai tx

importTx' :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
          => Tx                   -- ^ Transaction to import
          -> KeyRingAccountId     -- ^ Account ID
          -> [Entity KeyRingCoin] -- ^ Input coins
          -> SqlPersistT m (TxHash, TxConfidence) 
             -- ^ Transaction hash (after possible merges)
importTx' origTx ai origInCoins = do
    -- Merge the transaction with any previously existing transactions
    mergeResM <- mergeNoSigHashTxs ai origTx $ map entityVal origInCoins
    let (tx, oldTxid) = case mergeResM of
                            Just mergeRes -> mergeRes
                            _ -> (origTx, txHash origTx)
        txid = txHash tx

    -- If the transaction was merged into a new transaction,
    -- update the old hashes to the new ones. This allows us to
    -- keep the spending information of our coins. It is thus possible
    -- to spend partially signed multisignature transactions (as offline
    -- transactions) even before all signatures have arrived. 
    when (oldTxid /= txid) $ do
        -- Update the output coins
        updateWhere [ KeyRingCoinAccount ==. ai 
                    , KeyRingCoinHash    ==. oldTxid
                    ]
                    [ KeyRingCoinHash =. txid ]
        -- Update the account transactions
        updateWhere [ KeyRingTxAccount ==. ai 
                    , KeyRingTxHash    ==. oldTxid
                    ]
                    [ KeyRingTxHash  =. txid 
                    , KeyRingTxTx    =. tx
                    ]
        -- Update the address transactions
        updateWhere [ KeyRingAddrTxAccount ==. ai 
                    , KeyRingAddrTxHash    ==. oldTxid
                    ]
                    [ KeyRingAddrTxHash  =. txid 
                    , KeyRingAddrTxTx    =. tx
                    ]
        -- Update any coins that this transaction spent
        updateWhere [ KeyRingCoinAccount ==. ai
                    , KeyRingCoinSpentBy ==. Just oldTxid
                    ]
                    [ KeyRingCoinSpentBy =. Just txid ]

    -- Find the updated input coins spent by this transaction
    entInCoins <- if oldTxid == txid 
                    -- Save ourselves some work if the txid did not change
                    then return origInCoins 
                    -- Otherwise, fetch the new inputs coins again
                    else getAccInCoins ai tx
    let inCoins = map entityVal entInCoins
        dat     = mapMaybe toDat inCoins
        validTx = verifyStdTx tx dat
        validIn = length inCoins == length (txIn tx)
               && all (canSpendCoin txid) inCoins
    if validIn && validTx 
        then importNetTx tx >>= \resM -> case resM of
                Just (confidence, _) -> return (txid, confidence)
                _ -> liftIO . throwIO $ WalletException 
                    "Error while importing the transaction"
        else importLocalTx tx ai entInCoins
  where
    toDat KeyRingCoin{..} = case keyRingCoinScript of
        Just s -> Just (s, OutPoint keyRingCoinHash keyRingCoinPos)
        _ -> Nothing

-- Offline transactions are usually multisignature transactions requiring
-- additional signatures. This function will merge the signatures of
-- the same offline transactions together into one single transaction.
mergeNoSigHashTxs :: MonadIO m 
                  => KeyRingAccountId -> Tx -> [KeyRingCoin] 
                  -> SqlPersistT m (Maybe (Tx, TxHash))
mergeNoSigHashTxs ai tx inCoins = do
    prevM <- getBy $ UniqueAccNoSig ai $ nosigTxHash tx
    return $ case prevM of
        Just (Entity _ prev) -> case keyRingTxConfidence prev of
            TxOffline -> case mergeTxs [tx, keyRingTxTx prev] outpoints of
                Right merged -> Just (merged, keyRingTxHash prev)
                _            -> Nothing
            _ -> Nothing
        -- Nothing to merge. Return the original transaction.
        _ -> Nothing
  where
    buildOutpoint coin = OutPoint (keyRingCoinHash coin) (keyRingCoinPos coin)
    f c = case keyRingCoinScript c of
        Just s -> Just (s, buildOutpoint c)
        _ -> Nothing
    outpoints = mapMaybe f inCoins

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
importLocalTx :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => Tx 
              -> KeyRingAccountId
              -> [Entity KeyRingCoin] 
              -> SqlPersistT m (TxHash, TxConfidence)
importLocalTx tx ai entInCoins = do
    -- Get all the new coins to be created by this transaction
    (outCoins, outAddrs) <- getAccOutCoins ai tx
    let inCoins = map entityVal entInCoins
        txid    = txHash tx
    -- Only continue if the transaction is relevant to the account
    if null inCoins && null outCoins 
        then liftIO . throwIO $ WalletException $ unwords
            [ "importLocalTx: The transaction", encodeTxHashLE txid
            , "has no input or output coins which are relevant to this account."
            ]
        else do
            -- Check if the transaction already exists and find the confidence.
            prevM <- liftM (fmap entityVal) $ getBy $ UniqueAccTx ai txid
            let prevConfM = keyRingTxConfidence <$> prevM

            -- We can only re-import offline transaction through this function.
            if isNothing prevConfM || prevConfM == Just TxOffline
                -- Check that all coins can be spent. We allow offline coins
                -- to be spent by this function unlike importNetTx.
                then if all canSpendOfflineCoin inCoins 
                    then do
                        -- Lock the input coins
                        spendInputs txid CoinLocked entInCoins
                        -- Lock the inputs which are external to the wallet
                        spendExternalInputs tx CoinLocked inCoins outCoins
                        -- Insert new coins into the database. 
                        newOutCoins <- liftM catMaybes $ forM outCoins $ \coin -> 
                            insertBy coin >>= \resE -> case resE of
                                Left (Entity ci prev) -> do
                                    -- The existing coin could be an external
                                    -- coin. Replace everything except the
                                    -- status and the spentBy tx.
                                    replace ci coin
                                        { keyRingCoinStatus  = 
                                            keyRingCoinStatus prev
                                        , keyRingCoinSpentBy = 
                                            keyRingCoinSpentBy prev
                                        }
                                    -- If the previous coin was an external
                                    -- coin, we want to use the new coin in the
                                    -- balance computations.
                                    if keyRingCoinConfidence prev == TxExternal
                                        then return $ Just coin
                                        else return Nothing
                                Right _ -> return $ Just coin
                        -- Create all the transaction records for this account.
                        buildAccTxs tx prevM TxOffline inCoins outCoins
                        -- use the addresses (refill the gap addresses)
                        forM_ outAddrs useAddress
                        -- Update Account balances
                        let isUnspent c = 
                                ( keyRingCoinStatus c == CoinUnspent ) || 
                                ( keyRingCoinStatus c == CoinLocked && 
                                  keyRingCoinSpentBy c /= Just txid
                                )
                            unspent = filter isUnspent inCoins
                        updateBalancesWith 
                            (+) unspent newOutCoins BalanceOffline
                        -- Update address balances
                        updateAddrBalances 
                            unspent newOutCoins BalanceOffline False
                        -- Return the txid which might be different from the 
                        -- original one.
                        return (txid, TxOffline)
                    else liftIO . throwIO $ WalletException 
                        "importLocalTx: can not double spend coins"
                else liftIO . throwIO $ WalletException $ unwords
                    [ "importLocalTx: The transaction already exists"
                    , "and is not offline"
                    ]

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
    -> SqlPersistT m (Maybe (TxConfidence, M.Map KeyRingAccountId Int))
       -- ^ For each account implicated, the tx confidence and the number of
       -- new addresses generated.
importNetTx tx = do
    -- Find all the coins spent by this transaction
    entInCoins <- getInCoins tx
    let inCoins = map entityVal entInCoins
    -- Get all the new coins to be created by this transaction
    (initOutCoins, outAddrs) <- getOutCoins tx
    -- Only continue if the transaction is relevant to the wallet
    if null entInCoins && null initOutCoins then return Nothing else do
        -- Check if we have transactions that need to be merged. This can
        -- happen if someone has an offline transaction and the full
        -- transaction comes in from the network. We need to update the old
        -- offline transaction hash to the new hash.
        toMerge <- selectList [ KeyRingTxNosigHash ==. nosigTxHash tx ] []
        forM_ (nub $ map (keyRingTxHash . entityVal) toMerge) $ \oldTxid -> do
            -- Update the output coins
            updateWhere [ KeyRingCoinHash ==. oldTxid ]
                        [ KeyRingCoinHash =. txid ]
            -- Update the account transactions
            updateWhere [ KeyRingTxHash ==. oldTxid ]
                        [ KeyRingTxHash =. txid 
                        , KeyRingTxTx   =. tx
                        ]
            -- Update the address transactions
            updateWhere [ KeyRingAddrTxHash ==. oldTxid ]
                        [ KeyRingAddrTxHash =. txid 
                        , KeyRingAddrTxTx   =. tx
                        ]
            -- Update any coins that this transaction spent
            updateWhere [ KeyRingCoinSpentBy ==. Just oldTxid ]
                        [ KeyRingCoinSpentBy =. Just txid ]
        -- Check if the transaction already exists
        prevM <- liftM (fmap entityVal) $ 
            selectFirst [ KeyRingTxHash ==. txid ] []
            -- Compute the previous confidence
        let prevConfM = keyRingTxConfidence <$> prevM
            -- Compute the new confidence
            confidence 
                -- If all the coins can be spent
                | all (canSpendCoin txid) inCoins =
                    -- Use the previous confidence if it is building.
                    if prevConfM == Just TxBuilding 
                        then TxBuilding 
                        else TxPending
                | otherwise = TxDead
            -- Update the confidence values of our coins
            outCoins = map (f prevM confidence) initOutCoins
        -- Spend the input coins. We have to do this before creating new
        -- coins / txs in the database to make sure to kill old txs first.
        when (confidence /= TxDead) $ do
            -- Spend our inputs
            spendInputs txid CoinSpent entInCoins 
            -- Create coins for non-wallet inputs to detect double spends
            spendExternalInputs tx CoinSpent inCoins outCoins
        -- Insert new coins into the database. 
        newOutCoins <- liftM catMaybes $ forM outCoins $ \coin -> 
            insertBy coin >>= \resE -> case resE of
                Left (Entity ci prev) -> do
                    -- The existing coin could be an external coin. Replace 
                    -- everything except the status and the spentBy tx.
                    replace ci coin
                        { keyRingCoinStatus  = keyRingCoinStatus prev
                        , keyRingCoinSpentBy = keyRingCoinSpentBy prev
                        }
                    -- If the previous coin was an external coin, we want to
                    -- use the new coin in the balance computations.
                    if keyRingCoinConfidence prev == TxExternal
                        then return $ Just coin
                        else return Nothing
                Right _ -> return $ Just coin
        -- Use up the addresses of our new coins (replenish gap addresses)
        newAddrCnt <- forM outAddrs useAddress
        -- Create the transaction record for this account
        buildAccTxs tx prevM confidence inCoins outCoins
        -- Update balances if the transaction is not dead
        when (confidence /= TxDead) $ do
            -- Update account balances
            let isUnspent c = keyRingCoinStatus c /= CoinSpent
                unspent     = filter isUnspent inCoins
            if prevConfM == Just TxOffline
                then updateBalancesWith (+) unspent outCoins BalancePending
                else do 
                    updateBalancesWith (+) unspent newOutCoins BalancePending
                    updateBalancesWith (+) unspent newOutCoins BalanceOffline
            -- Update address balances
            if prevConfM == Just TxOffline
                then updateAddrBalances unspent outCoins BalancePending False
                else do
                    updateAddrBalances unspent newOutCoins BalancePending False
                    updateAddrBalances unspent newOutCoins BalanceOffline False

        -- The transaction changed status from non-dead to dead. Kill it.
        when (prevConfM /= Just TxDead && confidence == TxDead) $ killTx txid

        -- Return the confidence and number of new addresses generated in each
        -- account
        return $ Just ( confidence
                      , M.filter (> 0) $ M.fromListWith (+) newAddrCnt
                      )
  where
    txid = txHash tx
    -- Prepare the initial state of the new coins. Use the confirmation
    -- information of the existing transaction if we have it.
    f prevM confidence coin = coin
        { keyRingCoinConfidence      = confidence 
        , keyRingCoinConfirmedBy     = keyRingTxConfirmedBy =<< prevM
        , keyRingCoinConfirmedHeight = keyRingTxConfirmedHeight =<< prevM
        , keyRingCoinConfirmedDate   = keyRingTxConfirmedDate =<< prevM
        }

signKeyRingTx :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => KeyRing -> Entity KeyRingAccount -> TxHash
              -> SqlPersistT m (TxHash, TxConfidence)
signKeyRingTx keyRing (Entity ai acc) txid = do
    (OfflineTxData tx dat, entInCoins) <- getOfflineTxData ai txid
    let signedTx = signOfflineTx keyRing acc tx dat
    importTx' signedTx ai entInCoins

getOfflineTxData :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
                 => KeyRingAccountId -> TxHash 
                 -> SqlPersistT m (OfflineTxData, [Entity KeyRingCoin])
getOfflineTxData ai txid = do
    txM <- getBy $ UniqueAccTx ai txid
    case txM of
        Just (Entity _ KeyRingTx{..}) -> do
            unless (keyRingTxConfidence == TxOffline) $ liftIO . throwIO $ 
                WalletException "Can only sign offline transactions."
            entInCoins <- getAccInCoins ai keyRingTxTx
            return 
                ( OfflineTxData keyRingTxTx $ mapMaybe toDat entInCoins
                , entInCoins
                )
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Invalid txid", encodeTxHashLE txid ]
  where
    toDat (Entity _ KeyRingCoin{..}) = 
        case (keyRingCoinScript, keyRingCoinDerivation) of
            (Just s, Just d) -> Just $
                CoinSignData (OutPoint keyRingCoinHash keyRingCoinPos) s d
            _ -> Nothing

getPendingTxs :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
              => SqlPersistT m [Tx]
getPendingTxs = do
    txEs <- selectList
         [ KeyRingTxConfirmedBy ==. Nothing
         , KeyRingTxConfidence ==. TxPending
         ]
         []
    return $ map (keyRingTxTx . entityVal) txEs

-- | Returns true if the given coin can be spent. A coin can be spent if it is
-- not dead and it isn't already spent by a transaction other than the given
-- transaction (a transaction can always re-spend an input that it already
-- spends for idempotent transaction import).
canSpendCoin :: TxHash -> KeyRingCoin -> Bool
canSpendCoin hash coin =
    -- The coin is not offline. Offline coins are usually partially signed
    -- multisig transaction and their hash will change, so spending them is
    -- not a good idea.
    keyRingCoinConfidence coin /= TxOffline &&
    -- The coin is not dead and
    keyRingCoinConfidence coin /= TxDead &&
    -- The coin is not already spent (or already spent by us)
    ( keyRingCoinStatus coin /= CoinSpent || 
      keyRingCoinSpentBy coin == Just hash )

-- | Returns true if the given coin can be spent by an offline transaction.
-- Offline transactions can spend other offline coins unlike online
-- transactions.
canSpendOfflineCoin :: KeyRingCoin -> Bool
canSpendOfflineCoin coin =
    -- The coin is not dead and
    keyRingCoinConfidence coin /= TxDead &&
    -- The coin is not already spent (offline transactions only lock coins)
    keyRingCoinStatus coin /= CoinSpent

-- Find all input coins of a transaction that belong to any account.
getInCoins :: MonadIO m => Tx -> SqlPersistT m [Entity KeyRingCoin]
getInCoins tx = 
    liftM concat $ forM (map prevOutput $ txIn tx) $ \(OutPoint h i) -> 
        selectList [ KeyRingCoinHash ==. h 
                   , KeyRingCoinPos  ==. i
                   ] []

-- Find all input coins of a transactions that belong to a specific account.
-- This version is more efficient than the generic `getInCoins` version.
getAccInCoins :: MonadIO m 
              => KeyRingAccountId -> Tx -> SqlPersistT m [Entity KeyRingCoin]
getAccInCoins ai tx = 
    liftM catMaybes $ forM (map prevOutput $ txIn tx) $ \(OutPoint h i) -> 
        getBy $ UniqueCoin ai h i

-- Returns all the new coins that need to be created from a transaction.
-- Also returns the addresses associted with those coins.
getOutCoins :: MonadIO m => Tx -> SqlPersistT m ([KeyRingCoin], [KeyRingAddr])
getOutCoins tx = do
    -- res :: [([KeyRingCoin], [KeyRingCoin])]
    res <- forM (zip (txOut tx) [0..]) buildKeyRingCoin 
    return (concatMap fst res, concatMap snd res)
  where
    buildKeyRingCoin (out, pos) = case getDataFromOutput out of
        -- Try to decode the output script
        Right (so, addr) -> do
            now <- liftIO getCurrentTime
            -- Find all the address objects in the database
            addrs <- liftM (map entityVal) $ 
                selectList [ KeyRingAddrAddress ==. addr ] []
            let f = buildCoin (txHash tx) (out, pos) so (isCoinbaseTx tx) now
            return (map f addrs, addrs)
        _ -> return ([], [])

-- Returns all the new coins that need to be created for one account only. Also
-- returns the addresses associated with those coins. This version is more
-- efficient than the generic `getOutCoins` function.
getAccOutCoins :: MonadIO m 
               => KeyRingAccountId -> Tx 
               -> SqlPersistT m ([KeyRingCoin], [KeyRingAddr])
getAccOutCoins ai tx = do
    -- res :: [(KeyRingCoin, KeyRingAddr)]
    res <- liftM catMaybes $ forM (zip (txOut tx) [0..]) buildKeyRingCoin 
    return (map fst res, map snd res)
  where
    buildKeyRingCoin (out, pos) = case getDataFromOutput out of
        -- Try to decode the output script
        Right (so, addr) -> do
            now <- liftIO getCurrentTime
            let f = buildCoin (txHash tx) (out, pos) so (isCoinbaseTx tx) now
            -- Find the address in the database if it exists
            addrM <- getBy $ UniqueAddr ai addr
            -- Return a pair of Maybe (coin, addr)
            return $ (\(Entity _ a) -> (f a, a)) <$> addrM
        _ -> return Nothing

-- Decode an output and extract an output script and a recipient address
getDataFromOutput :: TxOut -> Either String (ScriptOutput, Address)
getDataFromOutput out = do
    so   <- decodeOutputBS $ scriptOutput out
    addr <- scriptRecipient $ encodeOutput so
    return (so, addr)

isCoinbaseTx :: Tx -> Bool
isCoinbaseTx (Tx _ tin _ _) =
    length tin == 1 && outPointHash (prevOutput $ head tin) == 0x00

buildCoin :: TxHash -> (TxOut, Word32) -> ScriptOutput -> Bool
          -> UTCTime -> KeyRingAddr -> KeyRingCoin
buildCoin txid (out, pos) so isCoinbase keyRingCoinCreated KeyRingAddr{..} =
    KeyRingCoin{..}
  where
    keyRingCoinKeyRingName     = keyRingAddrKeyRingName
    keyRingCoinAccountName     = keyRingAddrAccountName
    keyRingCoinAccount         = keyRingAddrAccount
    keyRingCoinHash            = txid
    keyRingCoinPos             = pos
    keyRingCoinValue           = outValue out
    keyRingCoinScript          = Just so
    keyRingCoinRedeem          = keyRingAddrRedeem
    keyRingCoinRootDerivation  = keyRingAddrRootDerivation
    keyRingCoinDerivation      = Just keyRingAddrDerivation
    keyRingCoinKey             = keyRingAddrKey
    keyRingCoinAddress         = Just keyRingAddrAddress
    keyRingCoinAddressType     = Just keyRingAddrType
    keyRingCoinAddressIndex    = Just keyRingAddrIndex
    keyRingCoinIsCoinbase      = isCoinbase
    keyRingCoinConfidence      = TxOffline
    keyRingCoinConfirmedBy     = Nothing
    keyRingCoinConfirmedHeight = Nothing
    keyRingCoinConfirmedDate   = Nothing
    keyRingCoinStatus          = CoinUnspent
    keyRingCoinSpentBy         = Nothing

-- | Spend the given input coins. We also create dummy coins for the inputs
-- in a transaction that do not belong to us. This is to be able to detect
-- double spends when reorgs occur. 
spendInputs :: MonadIO m 
            => TxHash -> CoinStatus -> [Entity KeyRingCoin]
            -> SqlPersistT m ()
spendInputs txid status entInCoins = do
    -- Loop over each account involved and kill offline transactions that are
    -- locking the coins.
    forM_ (M.toList $ groupAccountCoins (map entityVal entInCoins) []) $ 
        \(ai, (inCoins, _)) -> do
            let lockingTxs = nub $ mapMaybe keyRingCoinSpentBy $
                    filter isLocked inCoins
            -- Only kill offline transactions locally in individual accounts
            forM_ lockingTxs $ killAccTx ai

    -- Change the status of the input coins
    forM_ (map entityKey entInCoins) $ \ci -> 
        update ci [ KeyRingCoinStatus  =. status
                  , KeyRingCoinSpentBy =. Just txid
                  ]
  where
    isLocked coin =
        keyRingCoinStatus coin == CoinLocked && 
        keyRingCoinSpentBy coin /= Just txid

spendExternalInputs :: MonadIO m 
                    => Tx -> CoinStatus -> [KeyRingCoin] -> [KeyRingCoin]
                    -> SqlPersistT m ()
spendExternalInputs tx status allInCoins allOutCoins = do
    now <- liftIO getCurrentTime
    forM_ grouped $ \(ai, (inCoins, outCoins)) -> do
        let 
            coinOps = map (\KeyRingCoin{..} -> 
                            OutPoint keyRingCoinHash keyRingCoinPos) inCoins
            -- Filter inputs that are not in this account (unknown inputs)
            ops = filter (not . (`elem` coinOps)) $ map prevOutput $ txIn tx
        unless (null ops) $
            -- Insert all unknown transaction inputs to detect double
            -- spends on reorgs.
            forM_ ops $ \(OutPoint hash pos) -> do
                let KeyRingCoin _ k a _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 
                        case (inCoins, outCoins) of
                            (c:_, _) -> c
                            (_, c:_) -> c
                            -- Should not happen in theory
                            _ -> throw $ WalletException
                                "spendExternalInputs: No coins available"
                    keyRingCoinKeyRingName     = k
                    keyRingCoinAccountName     = a
                    keyRingCoinAccount         = ai
                    keyRingCoinHash            = hash
                    keyRingCoinPos             = pos
                    keyRingCoinValue           = 0
                    keyRingCoinScript          = Nothing
                    keyRingCoinRedeem          = Nothing
                    keyRingCoinRootDerivation  = Nothing
                    keyRingCoinDerivation      = Nothing
                    keyRingCoinKey             = Nothing
                    keyRingCoinAddress         = Nothing
                    keyRingCoinAddressType     = Nothing
                    keyRingCoinAddressIndex    = Nothing
                    keyRingCoinIsCoinbase      = False
                    -- This coin comes from an external (non-wallet)
                    -- transaction
                    keyRingCoinConfidence      = TxExternal 
                    keyRingCoinConfirmedBy     = Nothing
                    keyRingCoinConfirmedHeight = Nothing
                    keyRingCoinConfirmedDate   = Nothing
                    keyRingCoinStatus          = status
                    keyRingCoinSpentBy         = Just txid
                    keyRingCoinCreated         = now

                insertBy KeyRingCoin{..} >>= \resE -> case resE of
                    Left (Entity ci _) -> 
                        update ci [ KeyRingCoinStatus  =. status
                                    , KeyRingCoinSpentBy =. Just txid 
                                    ]
                    Right _ -> return ()
  where
    txid    = txHash tx
    grouped = M.toList $ groupAccountCoins allInCoins allOutCoins

buildAccTxs :: MonadIO m 
            => Tx -> Maybe KeyRingTx -> TxConfidence 
            -> [KeyRingCoin] -> [KeyRingCoin] 
            -> SqlPersistT m ()
buildAccTxs tx prevM confidence inCoins outCoins = do
    now <- liftIO getCurrentTime
    -- Group the coins by account
    forM_ grouped $ \(ai, (is, os)) -> do
        let atx = buildAccTx tx prevM confidence ai is os now
        -- Insert the new transaction. If it already exists, update the
        -- information with the newly computed values. Also make sure that the
        -- confidence is set to the new value (it could have changed to TxDead).
        insertBy atx >>= \resE -> case resE of
            -- Update the existing record
            Left (Entity ti _) -> update ti
                [ KeyRingTxType       =. keyRingTxType atx
                , KeyRingTxInValue    =. keyRingTxInValue atx
                , KeyRingTxOutValue   =. keyRingTxOutValue atx
                , KeyRingTxValue      =. keyRingTxValue atx
                , KeyRingTxFrom       =. keyRingTxFrom atx
                , KeyRingTxTo         =. keyRingTxTo atx
                , KeyRingTxChange     =. keyRingTxChange atx
                , KeyRingTxConfidence =. confidence
                ]
            Right _ -> return () -- Nothing to do
        -- Group the coins by address
        let addrGrouped = M.toList $ groupAddressCoins is os
        forM_ addrGrouped $ \(addr, (addrIs, addrOs)) -> do
            let addrTx = buildKeyRingAddrTx atx addr addrIs addrOs
            insertBy addrTx >>= \resE -> case resE of
                -- Update the existing record
                Left (Entity ti _) -> update ti
                    [ KeyRingAddrTxTxType     =. keyRingAddrTxTxType addrTx
                    , KeyRingAddrTxInValue    =. keyRingAddrTxInValue addrTx
                    , KeyRingAddrTxOutValue   =. keyRingAddrTxOutValue addrTx
                    , KeyRingAddrTxValue      =. keyRingAddrTxValue addrTx
                    , KeyRingAddrTxFrom       =. keyRingAddrTxFrom addrTx
                    , KeyRingAddrTxTo         =. keyRingAddrTxTo addrTx
                    , KeyRingAddrTxChange     =. keyRingAddrTxChange addrTx
                    , KeyRingAddrTxConfidence =. confidence
                    ]
                Right _ -> return () -- Nothing to do
  where
    grouped = M.toList $ groupAccountCoins inCoins outCoins

-- | Build an account transaction given the input and output coins relevant to
-- this specific account. An account transaction contains the details of how a
-- transaction affects one particular account (value sent to and from the
-- account). The first value is Maybe an existing transaction in the database
-- which is used to get the existing confirmation values.
buildAccTx :: Tx -> Maybe KeyRingTx -> TxConfidence
           -> KeyRingAccountId -> [KeyRingCoin] -> [KeyRingCoin] 
           -> UTCTime -> KeyRingTx
buildAccTx tx prevM confidence keyRingTxAccount inCoins outCoins now =
    KeyRingTx{..}
  where
    keyRingTxKeyRingName = case inCoins ++ outCoins of
        c:_ -> keyRingCoinKeyRingName c
        _   -> undefined -- Should not happen
    keyRingTxAccountName = case inCoins ++ outCoins of
        c:_ -> keyRingCoinAccountName c
        _   -> undefined -- Should not happen
    keyRingTxHash = txHash tx
    -- This is a hash of the transaction excluding signatures. This allows us
    -- to track the evolution of offline transactions as we add more signatures
    -- to them.
    keyRingTxNosigHash = nosigTxHash tx
    allMyCoins = length inCoins  == length (txIn tx) && 
                 length outCoins == length (txOut tx)
                  -- If all the coins belong to the same account, it is a self
                  -- transaction (even if a fee was payed).
    keyRingTxType | allMyCoins                            = TxSelf
                  -- This case can happen in complex transactions where the
                  -- total input/output sum for a given account is 0. In this
                  -- case, we count that transaction as a TxSelf. This should
                  -- not happen with simple transactions.
                  | keyRingTxInValue == keyRingTxOutValue = TxSelf
                  | keyRingTxInValue > keyRingTxOutValue  = TxIncoming
                  | otherwise                             = TxOutgoing
    keyRingTxInValue  = sum $ map keyRingCoinValue outCoins
    keyRingTxOutValue = sum $ map keyRingCoinValue inCoins
    keyRingTxValue = 
        fromIntegral keyRingTxInValue - fromIntegral keyRingTxOutValue
    allInAddrs = rights $ 
        map ((scriptSender =<<) . decodeToEither . scriptInput) $ txIn tx
    allOutAddrs = rights $ 
        map ((scriptRecipient =<<) . decodeToEither . scriptOutput) $ txOut tx
    keyRingTxFrom = case keyRingTxType of
        TxSelf     -> mapMaybe keyRingCoinAddress inCoins
        TxIncoming -> allInAddrs
        TxOutgoing -> mapMaybe keyRingCoinAddress inCoins
    keyRingTxTo = case keyRingTxType of
        TxSelf     -> mapMaybe keyRingCoinAddress outCoins
        TxIncoming -> mapMaybe keyRingCoinAddress outCoins
        -- For outgoing transactions, we want to display all the outgoing
        -- addresses except for change addresses (which are displayed in their
        -- own section).
        TxOutgoing -> allOutAddrs \\ keyRingTxChange
    keyRingTxChange = case keyRingTxType of
        TxOutgoing -> mapMaybe keyRingCoinAddress outCoins
        _ -> []
    keyRingTxTx         = tx
    keyRingTxIsCoinbase = isCoinbaseTx tx
    keyRingTxConfidence = confidence
    -- Reuse the confirmation information of the existing transaction if
    -- we have it.
    keyRingTxConfirmedBy = case prevM of
        Just (KeyRingTx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ cb _ _ _) -> cb
        _ -> Nothing
    keyRingTxConfirmedHeight = case prevM of
        Just (KeyRingTx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ch _ _) -> ch
        _ -> Nothing
    keyRingTxConfirmedDate = case prevM of
        Just (KeyRingTx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ cd _) -> cd
        _ -> Nothing
    keyRingTxCreated = now

buildKeyRingAddrTx :: KeyRingTx -> Address -> [KeyRingCoin] -> [KeyRingCoin] 
                  -> KeyRingAddrTx
buildKeyRingAddrTx KeyRingTx{..} addr inCoins outCoins =
    KeyRingAddrTx{..}
  where
    keyRingAddrTxAccount         = keyRingTxAccount
    keyRingAddrTxAddressIndex = 
        case mapMaybe keyRingCoinAddressIndex $ inCoins ++ outCoins of
            (i:_) -> i
            _     -> undefined -- Should not happen
    keyRingAddrTxAddressType = 
        case mapMaybe keyRingCoinAddressType $ inCoins ++ outCoins of
            (t:_) -> t
            _     -> undefined -- Should not happen
    keyRingAddrTxAddress         = addr
    keyRingAddrTxKeyRingName     = keyRingTxKeyRingName
    keyRingAddrTxAccountName     = keyRingTxAccountName
    keyRingAddrTxHash            = keyRingTxHash
    keyRingAddrTxNosigHash       = keyRingTxNosigHash
    keyRingAddrTxTxType          = keyRingTxType
    keyRingAddrTxInValue         = sum $ map keyRingCoinValue outCoins
    keyRingAddrTxOutValue        = sum $ map keyRingCoinValue inCoins
    keyRingAddrTxValue           = fromIntegral keyRingAddrTxInValue - 
                                   fromIntegral keyRingAddrTxOutValue
    keyRingAddrTxFrom            = keyRingTxFrom
    keyRingAddrTxTo              = keyRingTxTo
    keyRingAddrTxChange          = keyRingTxChange
    keyRingAddrTxTx              = keyRingTxTx
    keyRingAddrTxIsCoinbase      = keyRingTxIsCoinbase
    keyRingAddrTxConfidence      = keyRingTxConfidence
    keyRingAddrTxConfirmedBy     = keyRingTxConfirmedBy
    keyRingAddrTxConfirmedHeight = keyRingTxConfirmedHeight
    keyRingAddrTxConfirmedDate   = keyRingTxConfirmedDate
    keyRingAddrTxCreated         = keyRingTxCreated

-- Group all the input and outputs coins from the same account together.
groupAccountCoins :: [KeyRingCoin] -> [KeyRingCoin] 
                  -> M.Map KeyRingAccountId ([KeyRingCoin], [KeyRingCoin])
groupAccountCoins inCoins outCoins =
    M.unionWith g inMap outMap
  where
    -- Build a map from accounts -> (inCoins, outCoins)
    f coin  = (keyRingCoinAccount coin, [coin])
    g (is, _) (_, os) = (is, os)
    inMap  = M.map (\is -> (is, [])) $ M.fromListWith (++) $ map f inCoins
    outMap = M.map (\os -> ([], os)) $ M.fromListWith (++) $ map f outCoins

-- Group all the input and outputs coins from the same account together.
groupAddressCoins :: [KeyRingCoin] -> [KeyRingCoin] 
                  -> M.Map Address ([KeyRingCoin], [KeyRingCoin])
groupAddressCoins inCoins outCoins =
    M.unionWith g inMap outMap
  where
    -- Build a map from address -> (inCoins, outCoins)
    f coin = case keyRingCoinAddress coin of
        Just addr -> Just (addr, [coin])
        _ -> Nothing
    g (is, _) (_, os) = (is, os)
    inMap  = M.map (\is -> (is, [])) $ 
                M.fromListWith (++) $ mapMaybe f inCoins
    outMap = M.map (\os -> ([], os)) $ 
                M.fromListWith (++) $ mapMaybe f outCoins

-- | Set a transaction to TxDead status. That transaction will not spend any
-- coins, its output coins will be marked as dead and any child transactions
-- will also be set to TxDead status.
killTx :: MonadIO m => TxHash -> SqlPersistT m ()
killTx txid = do
    -- Find all input coins spent by this transaction
    inCoins <- liftM (map entityVal) $
        selectList [ KeyRingCoinSpentBy ==. Just txid ] []
    -- Find all output coins
    outCoins <- liftM (map entityVal) $ 
        selectList [ KeyRingCoinHash ==. txid ] []
    -- This transaction doesn't spend any coins anymore.
    updateWhere [ KeyRingCoinSpentBy ==. Just txid ] 
                [ KeyRingCoinSpentBy =. Nothing
                , KeyRingCoinStatus  =. CoinUnspent
                ]
    -- Update the output coin statuses to TxDead
    updateWhere [ KeyRingCoinHash ==. txid ]
                [ KeyRingCoinConfidence =. TxDead ]
    -- Update all the transaction statuses to TxDead
    updateWhere [ KeyRingTxHash ==. txid ] 
                [ KeyRingTxConfidence =. TxDead ]
    -- Recursively kill child transactions. We must update balance in the
    -- reverse order in which the transactions have been created. So we must
    -- recurse here before updating the balances.
    let childs = nub $ mapMaybe keyRingCoinSpentBy outCoins
    forM_ childs killTx
    -- Update account balances
    let liveOutCoins = filter (\c -> keyRingCoinConfidence c /= TxDead) outCoins
        inSpent      = filter (\c -> keyRingCoinStatus c == CoinSpent) inCoins
        outNotOffline = 
            filter (\c -> keyRingCoinConfidence c /= TxOffline) liveOutCoins
    updateBalancesWith (-) inCoins liveOutCoins BalanceOffline
    updateBalancesWith (-) inSpent outNotOffline BalancePending
    -- Update address balances
    updateAddrBalances inCoins liveOutCoins BalanceOffline True
    updateAddrBalances inSpent outNotOffline BalancePending True

-- | Similar to killTx but limits the scope to one account only.
killAccTx :: MonadIO m => KeyRingAccountId -> TxHash -> SqlPersistT m ()
killAccTx ai txid = do
    -- Find all input coins spent by this transaction
    entInCoins <- selectList [ KeyRingCoinSpentBy ==. Just txid 
                             , KeyRingCoinAccount ==. ai
                             ] []
    -- Find all output coins
    entOutCoins <- selectList [ KeyRingCoinHash    ==. txid 
                              , KeyRingCoinAccount ==. ai
                              ] []
    let outCoins = map entityVal entOutCoins
        inCoins  = map entityVal entInCoins
    -- This transaction doesn't spend any coins anymore.
    updateWhere [ KeyRingCoinSpentBy ==. Just txid 
                , KeyRingCoinAccount ==. ai
                ] 
                [ KeyRingCoinSpentBy =. Nothing
                , KeyRingCoinStatus  =. CoinUnspent
                ]
    -- Update the output coin statuses to TxDead
    updateWhere [ KeyRingCoinHash    ==. txid 
                , KeyRingCoinAccount ==. ai
                ]
                [ KeyRingCoinConfidence =. TxDead ]
    -- Update all the transaction statuses to TxDead
    updateWhere [ KeyRingTxHash    ==. txid 
                , KeyRingTxAccount ==. ai
                ] 
                [ KeyRingTxConfidence =. TxDead ]

    -- Recursively kill child transactions. We must update balance in the
    -- reverse order in which the transactions have been created. So we must
    -- recurse here before updating the balances.
    let childs = nub $ mapMaybe keyRingCoinSpentBy outCoins
    forM_ childs $ killAccTx ai

    -- Update account balances
    let liveOutCoins = filter (\c -> keyRingCoinConfidence c /= TxDead) outCoins
        inSpent      = filter (\c -> keyRingCoinStatus c == CoinSpent) inCoins
        outNotOffline = 
            filter (\c -> keyRingCoinConfidence c /= TxOffline) liveOutCoins
    updateBalancesWith (-) inCoins liveOutCoins BalanceOffline
    updateBalancesWith (-) inSpent outNotOffline BalancePending
    -- Update address balances
    updateAddrBalances inCoins liveOutCoins BalanceOffline True
    updateAddrBalances inSpent outNotOffline BalancePending True
    
{- Confirmations -}

importMerkles :: MonadIO m
              => BlockChainAction -> [[TxHash]] -> SqlPersistT m ()
importMerkles action expTxsLs = unless (isSideChain action) $ do
    case action of
        -- Unconfirm coins and transactions from the old chain if we have a
        -- reorg. We reverse the list to unconfirm newest transactions first.
        ChainReorg _ os _ -> forM_ (reverse os) $ \node -> do
            -- Unconfirm coins
            updateWhere [ KeyRingCoinConfirmedBy ==. Just (nodeBlockHash node) ]
                        [ KeyRingCoinConfidence      =. TxPending
                        , KeyRingCoinConfirmedBy     =. Nothing
                        , KeyRingCoinConfirmedHeight =. Nothing
                        , KeyRingCoinConfirmedDate   =. Nothing
                        ]
            -- Unconfirm transactions
            updateWhere [ KeyRingTxConfirmedBy ==. Just (nodeBlockHash node) ]
                        [ KeyRingTxConfidence      =. TxPending
                        , KeyRingTxConfirmedBy     =. Nothing
                        , KeyRingTxConfirmedHeight =. Nothing
                        , KeyRingTxConfirmedDate   =. Nothing
                        ]
            -- Unconfirm address transactions
            updateWhere [ KeyRingAddrTxConfirmedBy ==. Just (nodeBlockHash node) ]
                        [ KeyRingAddrTxConfidence      =. TxPending
                        , KeyRingAddrTxConfirmedBy     =. Nothing
                        , KeyRingAddrTxConfirmedHeight =. Nothing
                        , KeyRingAddrTxConfirmedDate   =. Nothing
                        ]
            -- Delete balances
            let balType = BalanceHeight $ nodeHeaderHeight node
            deleteWhere [ KeyRingBalanceType ==. balType ]
        _ -> return ()

    -- Confirm the coins and transactions in the new chain
    forM_ (zip (actionNewNodes action) expTxsLs) $ \(node, expTxs) -> do
        -- Loop over all the transactions in the merkle block. Some might not
        -- be our transactions (false positive in the bloom filter).
        _ <- liftM catMaybes $ forM expTxs $ \hash -> do
            -- Find the transaction if we have it
            prevM <- selectFirst [ KeyRingTxHash ==. hash ] []
            case prevM of
                -- Only process the transaction if it is relevant to us
                Just (Entity _ tx) -> do

                    -- Revive the transaction if it is dead.
                    when (keyRingTxConfidence tx == TxDead) $ 
                        reviveTx $ keyRingTxTx tx

                    let bid = nodeBlockHash node
                        h   = nodeHeaderHeight node
                        ts  = blockTimestamp $ nodeHeader node

                    -- Update the coins
                    updateWhere [ KeyRingCoinHash ==. hash ]
                                [ KeyRingCoinConfidence      =. TxBuilding
                                , KeyRingCoinConfirmedBy     =. Just bid
                                , KeyRingCoinConfirmedHeight =. Just h
                                , KeyRingCoinConfirmedDate   =. Just ts
                                ]

                    -- Update the transactions
                    updateWhere [ KeyRingTxHash ==. hash ]
                                [ KeyRingTxConfidence      =. TxBuilding
                                , KeyRingTxConfirmedBy     =. Just bid
                                , KeyRingTxConfirmedHeight =. Just h
                                , KeyRingTxConfirmedDate   =. Just ts
                                ]

                    -- Update the address transactions
                    updateWhere [ KeyRingAddrTxHash ==. hash ]
                                [ KeyRingAddrTxConfidence      =. TxBuilding
                                , KeyRingAddrTxConfirmedBy     =. Just bid
                                , KeyRingAddrTxConfirmedHeight =. Just h
                                , KeyRingAddrTxConfirmedDate   =. Just ts
                                ]
                _ -> return ()
            return $ (keyRingTxTx . entityVal) <$> prevM

        -- Update balances
        addHeightBalance $ nodeHeaderHeight node

    -- Update the best height in the wallet (used to compute the number
    -- of confirmations of transactions)
    case reverse $ actionNewNodes action of
        (best:_) -> setBestBlock (nodeBlockHash best) (nodeHeaderHeight best)
        _ -> return ()

-- Helper function to set the best block and best block height in the DB.
setBestBlock :: MonadIO m => BlockHash -> Word32 -> SqlPersistT m ()
setBestBlock bid i = updateWhere [] [ KeyRingConfigBlock  =. bid
                                    , KeyRingConfigHeight =. i 
                                    ]

-- Helper function to get the best block and best block height from the DB
getBestBlock :: MonadIO m => SqlPersistT m (BlockHash, Word32)
getBestBlock = do
    cfgM <- liftM (fmap entityVal) $ selectFirst [] []
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
    -- Find all the coins spent by this transaction
    entInCoins <- getInCoins tx
    let inCoins = map entityVal entInCoins
        -- Find all the transactions spending these coins
    let hs = mapMaybe keyRingCoinSpentBy inCoins
        -- Find all the transactions to kill
        toKill = delete txid $ nub hs
    -- Kill the conflicting transactions
    forM_ toKill killTx
    -- Spend the inputs of this transaction
    spendInputs txid CoinSpent entInCoins
    outCoins <- liftM (map entityVal) $ 
        selectList [ KeyRingCoinHash       ==. txid 
                   , KeyRingCoinConfidence ==. TxDead
                   ] []
    -- Update the output coins of this transaction
    unless (null outCoins) $ do
        updateWhere [ KeyRingCoinHash       ==. txid 
                    , KeyRingCoinConfidence ==. TxDead
                    ]
                    [ KeyRingCoinConfidence =. TxPending ]
        -- Update the transactions
        updateWhere [ KeyRingTxHash       ==. txid 
                    , KeyRingTxConfidence ==. TxDead
                    ]
                    [ KeyRingTxConfidence =. TxPending ]
    -- Update account balances
    let inNotMine = filter (\c -> keyRingCoinSpentBy c /= Just txid) inCoins
    updateBalancesWith (+) inNotMine outCoins BalanceOffline
    updateBalancesWith (+) inNotMine outCoins BalancePending
    -- Update address balances
    updateAddrBalances inNotMine outCoins BalanceOffline False 
    updateAddrBalances inNotMine outCoins BalancePending False
  where
    txid = txHash tx

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
    accE@(Entity ai acc) <- getAccount keyRingName accountName
    -- Build an unsigned transaction from the given recipient values and fee
    (unsignedTx, entInCoins) <- buildUnsignedTx accE minConf dests fee rcptFee
    -- Sign our new transaction if signing was requested
    let tx | sign = signOfflineTx keyRing acc unsignedTx $ 
                        mapMaybe toDat entInCoins
           | otherwise = unsignedTx
    -- Import the transaction in the wallet either as a network transaction if
    -- it is complete, or as an offline transaction otherwise.
    importTx' tx ai entInCoins
  where
    -- Map KeyRingCoin to CoinSignData which is required for signing
    toDat (Entity _ KeyRingCoin{..}) = 
        case (keyRingCoinScript, keyRingCoinDerivation) of
            (Just s, Just d) -> Just $ 
                CoinSignData (OutPoint keyRingCoinHash keyRingCoinPos) s d
            _ -> Nothing

-- Build an unsigned transaction given a list of recipients and a fee. Returns
-- the unsigned transaction together with the input coins that have been
-- selected or spending.
buildUnsignedTx :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
                => Entity KeyRingAccount
                -> Word32
                -> [(Address,Word64)] 
                -> Word64
                -> Bool
                -> SqlPersistT m (Tx, [Entity KeyRingCoin])
buildUnsignedTx
    (Entity ai acc@KeyRingAccount{..}) minConf dests fee rcptFee = do
    let p = case (keyRingAccountRequiredSigs, keyRingAccountTotalKeys) of
                (Just m, Just n) -> (m, n)
                _ -> throw . WalletException $ "Invalid multisig parameters"
        fee' = if rcptFee then 0 else fee
        sink | isMultisigAccount acc = chooseMSCoinsSink tot fee' p True
             | otherwise             = chooseCoinsSink   tot fee'   True
        -- TODO: Add more policies like confirmations or coin age
        -- Sort coins by their values in descending order
        policy = [Desc KeyRingCoinValue]
    -- Find the spendable coins in the given account with the required number
    -- of minimum confirmations.
    selectRes <- spendableCoinsSource ai minConf policy $$ sink
    -- Find a selection of spendable coins that matches our target value
    let (selected, change) =
            either (throw . WalletException) id selectRes
        totFee | isMultisigAccount acc = getMSFee fee p (length selected)
               | otherwise             = getFee   fee   (length selected)

    -- Subtract fees from first destination if rcptFee
        value = snd $ head dests
    -- First output must not be dust after deducting fees
    when (rcptFee && value < totFee + 5430) $ throw $ WalletException
        "First recipient cannot cover transaction fees"
    let dests' = if rcptFee
            then second (const $ value - totFee) (head dests) : tail dests
            else dests

    -- If the change amount is not dust, we need to add a change address to
    -- our list of recipients.
    -- TODO: Put the dust value in a constant somewhere. We also need a more
    -- general way of detecting dust such as our transactions are not
    -- rejected by full nodes.
    allDests <- if change < 5430 then return dests'
                                 else addChangeAddr change dests'
    case buildAddrTx (map toOutPoint selected) $ map toBase58 allDests of
        Right tx -> return (tx, selected)
        Left err -> liftIO . throwIO $ WalletException err
  where
    tot = sum $ map snd dests
    toBase58 (a, v) = (addrToBase58 a, v)
    toOutPoint (Entity _ c) = OutPoint (keyRingCoinHash c) (keyRingCoinPos c)
    addChangeAddr change dests' = do
        unused <- addressUnused keyRingAccountKeyRingName 
                                keyRingAccountName 
                                AddressInternal
        case unused of
            (a:_) -> do
                -- Use the address to prevent reusing it again
                _ <- useAddress a 
                -- TODO: Randomize the change position
                return $ (keyRingAddrAddress a, change) : dests'
            _ -> liftIO . throwIO $ 
                WalletException "No change addresses available"

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
spendableCoins :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
               => Key KeyRingAccount                 -- ^ Account key
               -> Word32                             -- ^ Minimum confirmations
               -> [SelectOpt KeyRingCoin]            -- ^ Special orderings
               -> SqlPersistT m [Entity KeyRingCoin] -- ^ Spendable coins
spendableCoins ai minConf order = 
    spendableCoinsSource ai minConf order $$ CL.consume

spendableCoinsSource
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
    => Key KeyRingAccount      -- ^ Account key
    -> Word32                  -- ^ Minimum confirmations
    -> [SelectOpt KeyRingCoin] -- ^ Special orderings
    -> Source (SqlPersistT m) (Entity KeyRingCoin) -- ^ Spendable coins
spendableCoinsSource ai minConf order = do
    (_, height) <- lift getBestBlock 
    let filterConfirmations
            -- No minimum confirmations. No confirmation filtering.
            | minConf == 0 = []
            -- If the current height is 200 and a coin was confirmed at height
            -- 198, then it has 3 confirmations. So, if we require 3
            -- confirmations, we want coins with a confirmed height of 198 or
            -- less (200 - 3 + 1).
            | otherwise = 
                [ KeyRingCoinConfirmedHeight !=. Nothing 
                , KeyRingCoinConfirmedHeight <=. Just (height - minConf + 1)
                ]
    mapOutputMaybe (checkCoinbase height) $ flip selectSource order $
            [ KeyRingCoinAccount ==. ai
            , KeyRingCoinStatus ==. CoinUnspent
            -- We can not spent offline or dead coins
            , KeyRingCoinConfidence <-. [ TxPending, TxBuilding ]
            ] ++ filterConfirmations
  where
    checkCoinbase height coin@(Entity _ KeyRingCoin{..})
        -- Coinbase transactions require 100 confirmations, so they have
        -- to be confirmed at block "height - 100 + 1" or less.
        | keyRingCoinIsCoinbase &&
          ( isNothing keyRingCoinConfirmedHeight ||
            keyRingCoinConfirmedHeight > Just (height - 99)
          ) = Nothing
        | otherwise = Just coin

{- Balances -}

-- | Update the account balances from a list of spent input coins and a list
-- of new output coins. This function is meant for updating offline balances and
-- pending (0-conf) balances.
updateBalancesWith :: MonadIO m 
                   => (Integer -> Integer -> Integer)
                   -> [KeyRingCoin] -> [KeyRingCoin] -> BalanceType 
                   -> SqlPersistT m ()
updateBalancesWith op allInCoins allOutCoins balType =
    forM_ grouped $ \(ai, (inCoins, outCoins)) -> do
        -- Find the previous balance. We compute the new balance as a delta
        -- from the previous balance.
        prevBalM <- getBy $ UniqueBalance ai balType
        let prevBal = maybe 0 (keyRingBalanceValue . entityVal) prevBalM
            inVal  = toInteger $ sum $ map keyRingCoinValue outCoins
            outVal = toInteger $ sum $ map keyRingCoinValue inCoins
            val    = op (toInteger prevBal) (inVal - outVal)
            bal    = KeyRingBalance ai balType $ fromInteger val
        when (val < 0) $ liftIO . throwIO $ WalletException
            "Account balance can not be negative"
        insertBy bal >>= \resE -> case resE of
            Left (Entity bi _) -> replace bi bal
            _ -> return ()
  where
    grouped = M.toList $ groupAccountCoins allInCoins allOutCoins

-- Update the offline or pending balances of addresses given new input and
-- output coins. To remove the effects of coins on addresses (for example when
-- a transaction is killed), call this function with rem = True. Only offline
-- and pending balances are available for addresses. No n-conf addresses are
-- stored. This function should be called for 1 transaction only.
updateAddrBalances :: MonadIO m 
                   => [KeyRingCoin] -> [KeyRingCoin] -> BalanceType -> Bool
                   -> SqlPersistT m ()
updateAddrBalances inCoins outCoins balType r = do
    -- Update the inflow balances of addresses
    forM_ (M.toList inVals) $ \(addr, val) -> updateWhere 
        [ KeyRingAddrAddress ==. addr ]
        [ (if r then (-=.) else (+=.)) inBal val
        , (if r then (-=.) else (+=.)) fundTxs 1 
        ]
    -- Update the outflow balances of addresses
    forM_ (M.toList outVals) $ \(addr, val) -> updateWhere
        [ KeyRingAddrAddress ==. addr ]
        [ (if r then (-=.) else (+=.)) outBal val
        , (if r then (-=.) else (+=.)) spendTxs 1 
        ]
  where
    (inBal, fundTxs) = case balType of
        BalanceOffline -> ( KeyRingAddrInOfflineBalance
                          , KeyRingAddrFundingOfflineTxs
                          )
        _              -> ( KeyRingAddrInBalance
                          , KeyRingAddrFundingTxs
                          )
    (outBal, spendTxs) = case balType of
        BalanceOffline -> ( KeyRingAddrOutOfflineBalance
                          , KeyRingAddrSpendingOfflineTxs
                          )
        _              -> ( KeyRingAddrOutBalance
                          , KeyRingAddrSpendingTxs
                          )
    f KeyRingCoin{..} = case keyRingCoinAddress of
        Just a -> Just (a, keyRingCoinValue)
        _ -> Nothing
    -- Group addresses to reduce the number of database updates
    outVals = M.fromListWith (+) $ mapMaybe f inCoins
    inVals  = M.fromListWith (+) $ mapMaybe f outCoins

-- Compute the non-offline balance at a given height from the pending balance.
-- All current pending transactions are removed from the pending balance to
-- compute the balance at the given height.
addHeightBalance :: MonadIO m => BlockHeight -> SqlPersistT m ()
addHeightBalance height = selectList [] [] >>= \accs -> 
    forM_ accs $ \(Entity ai _) -> do
        pendingBalM <- getBy $ UniqueBalance ai BalancePending
        val <- case pendingBalM of
            -- If a previous balance exists for this account, compute only the
            -- delta between the previous balance and the new height.
            Just (Entity _ KeyRingBalance{..}) -> do
                -- Find all pending transactions. Most of the time, this result
                -- will be empty and thus fast to process.
                pendingTxs <- liftM (map entityVal) $ selectList 
                    [ KeyRingTxAccount    ==. ai 
                    , KeyRingTxConfidence ==. TxPending 
                    ] []
                if null pendingTxs then return keyRingBalanceValue else do
                    -- Find the coins spent by the pending transactions
                    inCoins <- liftM (map entityVal . concat) $ 
                        forM pendingTxs $ getAccInCoins ai . keyRingTxTx
                    -- Find the coins created by pending transactions
                    outCoins <- liftM (map entityVal) $
                        selectList [ KeyRingCoinAccount    ==. ai 
                                   , KeyRingCoinConfidence ==. TxPending
                                   ] []
                    let outVal = toInteger $ sum $ map keyRingCoinValue inCoins
                        inVal  = toInteger $ sum $ map keyRingCoinValue outCoins
                        -- Remove the effect of the pending transactions on
                        -- the pending balance
                        bal = toInteger keyRingBalanceValue - (inVal - outVal)
                    return $ fromInteger bal
            -- If we have no pending balance for this account, the balance
            -- at the given height is 0.
            Nothing -> return 0 
        -- Save the account balance at this height
        let bal = KeyRingBalance ai (BalanceHeight height) val
        insertBy bal >>= \resE -> case resE of
            Left (Entity bi _) -> replace bi bal
            _ -> return ()

-- | Get the balance of an account with the given number of confirmations
accountBalance :: MonadIO m 
               => KeyRingAccountId -> Word32 -> SqlPersistT m Word64
accountBalance ai minconf = do
    (_, currHeight) <- getBestBlock
       -- The 0-confirmation balance is requested
    if minconf == 0
        then do
            -- Find the 0-confirmation balance if it is available
            balM <- getBy $ UniqueBalance ai BalancePending
            return $ maybe 0 (keyRingBalanceValue . entityVal) balM
        else if minconf > currHeight
            -- More confirmations are required than the height of the best chain
            then return 0
            else do
                -- Find the pre-computed balance for the given confirmations
                let height = currHeight - minconf + 1
                balM <- getBy $ UniqueBalance ai $ BalanceHeight height
                -- If no pre-computed balance is available, the balance is 0
                return $ maybe 0 (keyRingBalanceValue . entityVal) balM

offlineBalance :: MonadIO m => KeyRingAccountId -> SqlPersistT m Word64
offlineBalance ai = do
    balM <- getBy $ UniqueBalance ai BalanceOffline
    return $ maybe 0 (keyRingBalanceValue . entityVal) balM

resetRescan :: MonadIO m => SqlPersistT m ()
resetRescan = do
    deleteWhere ([] :: [Filter KeyRingCoin])
    deleteWhere ([] :: [Filter KeyRingTx])
    deleteWhere ([] :: [Filter KeyRingAddrTx])
    deleteWhere ([] :: [Filter KeyRingBalance])
    updateWhere [] [ KeyRingAddrInBalance          =. 0
                   , KeyRingAddrOutBalance         =. 0
                   , KeyRingAddrInOfflineBalance   =. 0
                   , KeyRingAddrOutOfflineBalance  =. 0
                   , KeyRingAddrFundingTxs         =. 0
                   , KeyRingAddrSpendingTxs        =. 0
                   , KeyRingAddrFundingOfflineTxs  =. 0
                   , KeyRingAddrSpendingOfflineTxs =. 0
                   ]
    setBestBlock (headerHash genesisHeader) 0

