{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  This module provides an API to the Haskoin wallet.
  The wallet commands run within the Persistent framework for database support:

  <http://hackage.haskell.org/package/persistent>
-}
module Network.Haskoin.Wallet.Commands
( cmdInitMnemo
, cmdInit
, cmdNewAcc
, cmdNewMS
, cmdAddKeys
, cmdAccInfo
, cmdListAcc
, cmdDumpKeys
, cmdList
, cmdGenAddrs
, cmdGenWithLabel
, cmdLabel
, cmdPrvKey
, cmdBalance
, cmdBalances
, cmdCoins
, cmdAllCoins
, cmdImportTx 
, cmdRemoveTx
, cmdListTx
, cmdSend
, cmdSendMany
, cmdSignTx
, cmdDecodeTx
, cmdBuildRawTx
, cmdSignRawTx
) where

import Control.Monad (when, unless, liftM)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Exception (throwIO)

import qualified Data.ByteString as BS
import Data.Time (getCurrentTime)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List (nub)
import qualified Data.Aeson as JSON (decode)
import qualified Data.Text as T (pack)
import Data.Word (Word64)

import Database.Persist
    ( PersistStore
    , PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , Entity(..)
    , entityVal
    , entityKey
    , get
    , selectList
    , insert_
    , replace
    , update
    , count
    , (=.), (<=.), (==.)
    , SelectOpt( Asc, OffsetBy, LimitTo )
    )
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Util.BuildMonad

import Network.Haskoin.Wallet.DbAccount
import Network.Haskoin.Wallet.DbAddress
import Network.Haskoin.Wallet.DbCoin
import Network.Haskoin.Wallet.DbTx
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Util


-- | Initialize a wallet from an optional mnemonic seed and a passphrase,
-- which could be blank.
cmdInitMnemo :: PersistUnique m
             => String
             -- ^ Passphrase to protect mnemonic.
             -> Maybe String
             -- ^ Mnemonic sentence to initialize wallet with.
             -- Use entropy from /dev/random otherwise.
             -> m Mnemonic
             -- ^ Mnemonic sentence used to initialize wallet.
             -- Impossible to retrieve in the future.

cmdInitMnemo p (Just s) = do
    let ms = T.pack s
        pass = T.pack p
        seedE = mnemonicToSeed english pass ms
        seed  = fromRight seedE
    when (isLeft seedE) $ liftIO $ throwIO $ 
        InitializationException $ fromLeft seedE
    _ <- cmdInit seed
    return ms

cmdInitMnemo p Nothing = do
    ent  <- liftIO $ devRandom 16
    let pass = T.pack p
        msSeedE = do
            m <- toMnemonic english ent
            s <- mnemonicToSeed english pass m
            return (m, s)
    when (isLeft msSeedE) $ liftIO $ throwIO $
        InitializationException $ fromLeft msSeedE
    let (ms, seed) = fromRight msSeedE
    _ <- cmdInit seed
    return ms

-- | Initialize a wallet from a secret seed. This function will fail if the
-- wallet is already initialized.
cmdInit :: PersistUnique m
        => BS.ByteString    -- ^ Secret seed.
        -> m ()             -- ^ Returns Null.
cmdInit seed 
    | BS.null seed = liftIO $ throwIO $ 
        InitializationException "The seed is empty"
    | otherwise = do
        isInit <- isWalletInit "main"
        when isInit $ liftIO $ throwIO $ 
            InitializationException "The wallet is already initialized"
        time <- liftIO getCurrentTime
        let master = makeMasterKey seed
        when (isNothing master) $ liftIO $ throwIO $ InitializationException
            "The seed derivation produced an invalid key. Use another seed"
        insert_ $ DbWallet "main" "full" (fromJust master) (-1) time

{- Account Commands -}

-- | Create a new account from an account name. Accounts are identified by
-- their name and they must be unique.
cmdNewAcc :: ( PersistUnique m
             , PersistQuery m
             , PersistMonadBackend m ~ b
             ) 
          => String  -- ^ Account name.
          -> m (DbAccountGeneric b) -- ^ Returns the new account information.
cmdNewAcc name = checkInit >> do
    time <- liftIO getCurrentTime
    (Entity wk w) <- dbGetWallet "main"
    let deriv = fromIntegral $ dbWalletAccIndex w + 1
        (k,i) = head $ accPubKeys (dbWalletMaster w) deriv
        acc   = DbAccount name 
                          (fromIntegral i) 
                          (concat ["m/",show i,"'/"])
                          k
                          (-1) (-1) (-1) (-1)
                          Nothing Nothing [] wk time
    insert_ acc
    update wk [DbWalletAccIndex =. fromIntegral i]
    dbSetGap name 30 False
    dbSetGap name 30 True
    return acc

-- | Create a new multisignature account. The thirdparty keys can be provided
-- now or later using the 'cmdAddKeys' command. The number of thirdparty keys
-- can not exceed n-1 as your own account key will be used as well in the
-- multisignature scheme. If less than n-1 keys are provided, the account will
-- be in a pending state and no addresses can be generated.
--
-- In order to prevent usage mistakes, you can not create a multisignature 
-- account with other keys from your own wallet.
cmdNewMS :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
         => String    -- ^ Account name.
         -> Int       -- ^ Required number of keys (m in m of n).
         -> Int       -- ^ Total number of keys (n in m of n).
         -> [XPubKey] -- ^ Thirdparty public keys.
         -> m (DbAccountGeneric b)   -- ^ Returns the new account information.
cmdNewMS name m n mskeys = checkInit >> do
    let keys = nub mskeys
    time <- liftIO getCurrentTime
    unless (n >= 1 && n <= 16 && m >= 1 && m <= n) $ liftIO $ throwIO $ 
        AccountSetupException "Invalid multisig parameters"
    unless (length keys < n) $ liftIO $ throwIO $
        AccountSetupException "Too many keys"
    checkOwnKeys keys
    (Entity wk w) <- dbGetWallet "main"
    let deriv = fromIntegral $ dbWalletAccIndex w + 1
        (k,i) = head $ accPubKeys (dbWalletMaster w) deriv
        acc   = DbAccount name 
                          (fromIntegral i) 
                          (concat ["m/",show i,"'/"])
                          k
                          (-1) (-1) (-1) (-1) 
                          (Just m) (Just n) 
                          keys
                          wk time
    insert_ acc
    update wk [DbWalletAccIndex =. fromIntegral i]
    when (length (dbAccountMsKeys acc) == n - 1) $ do
        -- Generate gap addresses
        dbSetGap name 30 False
        dbSetGap name 30 True
    return acc

-- | Add new thirdparty keys to a multisignature account. This function can
-- fail if the multisignature account already has all required keys. In order
-- to prevent usage mistakes, adding a key from your own wallet will fail.
cmdAddKeys :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
           => AccountName -- ^ Account name.
           -> [XPubKey]   -- ^ Thirdparty public keys to add.
           -> m (DbAccountGeneric b)     -- ^ Returns the account information.
cmdAddKeys name keys 
    | null keys = liftIO $ throwIO $
         AccountSetupException "Thirdparty key list can not be empty"
    | otherwise = checkInit >> do
        (Entity ai acc) <- dbGetAccount name
        unless (isMSAcc acc) $ liftIO $ throwIO $ AccountSetupException 
            "Can only add keys to a multisig account"
        checkOwnKeys keys
        let prevKeys = dbAccountMsKeys acc
        when (length prevKeys == (fromJust $ dbAccountMsTotal acc) - 1) $ 
            liftIO $ throwIO $ AccountSetupException 
                "The account is complete and no further keys can be added"
        let newKeys = nub $ prevKeys ++ keys
            newAcc  = acc { dbAccountMsKeys = newKeys }
        unless (length newKeys < (fromJust $ dbAccountMsTotal acc)) $
            liftIO $ throwIO $ AccountSetupException
                "Adding too many keys to the account"
        replace ai newAcc
        let n = fromJust $ dbAccountMsTotal newAcc
        when (length (dbAccountMsKeys acc) == n - 1) $ do
            -- Generate gap addresses
            dbSetGap name 30 False
            dbSetGap name 30 True
        return newAcc

checkOwnKeys :: PersistQuery m => [XPubKey] -> m ()
checkOwnKeys keys = do
    -- TODO: Match PubKey instead of XPubKey to avoid playing 
    -- with height or other values
    exists <- mapM (\x -> count [DbAccountKey ==. AccPubKey x]) keys
    unless (sum exists == 0) $ liftIO $ throwIO $ AccountSetupException 
        "Can not add your own keys to a multisig account"

-- | Returns information on an account.
cmdAccInfo :: (PersistUnique m, PersistMonadBackend m ~ b)
           => AccountName   -- ^ Account name.
           -> m (DbAccountGeneric b)       -- ^ Account information.
cmdAccInfo name = checkInit >> do
    acc <- dbGetAccount name
    return $ entityVal acc

-- | Returns a list of all accounts in the wallet.
cmdListAcc :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
           => m [DbAccountGeneric b]       -- ^ List of accounts
cmdListAcc = checkInit >> do
    e <- selectList [] []
    return $ map entityVal e

-- | Returns information on extended public and private keys of an account.
-- For a multisignature account, thirdparty keys are also returned.
cmdDumpKeys :: (PersistUnique m, PersistMonadBackend m ~ b)
            => AccountName  -- ^ Account name.
            -> m (DbAccountGeneric b, XPubKey, XPrvKey, Maybe [XPubKey])
            -- ^ Extended key information.
            -- Account, Account XPubKey, Account XPrvKey, Multisig XPubKey list.
cmdDumpKeys name = checkInit >> do
    (Entity _ acc) <- dbGetAccount name
    w <- liftM fromJust (get $ dbAccountWallet acc)
    let master = dbWalletMaster w
        deriv  = fromIntegral $ dbAccountIndex acc
        accPrv = fromJust $ accPrvKey master deriv
        prvKey = getAccPrvKey accPrv
        pubKey = deriveXPubKey prvKey
        ms | isMSAcc acc = Just $ dbAccountMsKeys acc
           | otherwise   = Nothing
    return $ (acc, pubKey, prvKey, ms)

{- Address Commands -}

-- | Returns a page of addresses for an account. Pages are numbered starting
-- from page 1. Requesting page 0 will return the last page. 
cmdList :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
        => AccountName             -- ^ Account name.
        -> Int                     -- ^ Requested page number.
        -> Int                     -- ^ Number of addresses per page.
        -> m ([DbAddressGeneric b], Int, Int, Int)
        -- ^ The requested page
        -- (addresses, page, results per page, address count)
cmdList name pageNum resPerPage 
    | pageNum < 0 = liftIO $ throwIO $ InvalidPageException $ 
        unwords ["Invalid page number:", show pageNum]
    | resPerPage < 1 = liftIO $ throwIO $ InvalidPageException $
        unwords ["Invalid results per page:",show resPerPage]
    | otherwise = checkInit >> do
        (Entity ai acc) <- dbGetAccount name
        addrCount <- count 
            [ DbAddressAccount ==. ai
            , DbAddressInternal ==. False
            , DbAddressIndex <=. dbAccountExtIndex acc
            ] 
        let maxPage = max 1 $ (addrCount + resPerPage - 1) `div` resPerPage
            page | pageNum == 0 = maxPage
                 | otherwise = pageNum
        when (page > maxPage) $ liftIO $ throwIO $ InvalidPageException $ 
            unwords ["The page number", show pageNum, "is too high"]
        addrs <- selectList [ DbAddressAccount ==. ai
                            , DbAddressInternal ==. False
                            , DbAddressIndex <=. dbAccountExtIndex acc
                            ] 
                            [ Asc DbAddressId
                            , LimitTo resPerPage
                            , OffsetBy $ (page - 1) * resPerPage
                            ]

        return $ ((map entityVal addrs), page, resPerPage, addrCount)

-- | Generate new payment addresses for an account. 
cmdGenAddrs :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
            => AccountName  -- ^ Account name.
            -> Int          -- ^ Number of addresses to generate.
            -> m [DbAddressGeneric b]      -- ^ List of new addresses.
cmdGenAddrs name c 
    | c < 1     = liftIO $ throwIO $
        AddressGenerationException "Can not generate less than 1 address"
    | otherwise = cmdGenWithLabel name (replicate c "")

-- | Generate new payment addresses with labels for an account.
cmdGenWithLabel :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
                => AccountName  -- ^ Account name.
                -> [String]     -- ^ List of address labels. 
                -> m [DbAddressGeneric b]      -- ^ List of new addresses.
cmdGenWithLabel name labels = checkInit >> dbGenAddrs name labels False

-- | Add a label to an address.
cmdLabel :: (PersistUnique m, PersistMonadBackend m ~ b)
         => AccountName   -- ^ Account name.
         -> Int           -- ^ Derivation index of the address. 
         -> String        -- ^ New label.
         -> m (DbAddressGeneric b) -- ^ New address information.
cmdLabel name key label = checkInit >> do
    (Entity ai acc) <- dbGetAccount name
    when (key > dbAccountExtIndex acc) $ liftIO $ throwIO $
        InvalidAddressException "The address key does not exist"
    (Entity i add) <- dbGetAddressByIndex ai key False
    let newAddr = add { dbAddressLabel = label }
    replace i newAddr
    return newAddr

-- | Returns the private key tied to a payment address.
cmdPrvKey :: (PersistUnique m, PersistMonadBackend m ~ b)
          => AccountName      -- ^ Account name.
          -> Int              -- ^ Derivation index of the address. 
          -> m PrvKey         -- ^ Private key.
cmdPrvKey name key = checkInit >> do
    (Entity ai acc) <- dbGetAccount name
    w <- liftM fromJust (get $ dbAccountWallet acc)
    when (key > dbAccountExtIndex acc) $ liftIO $ throwIO $
        InvalidAddressException "The address key does not exist"
    (Entity _ add) <- dbGetAddressByIndex ai key False
    let master     = dbWalletMaster w
        deriv      = fromIntegral $ dbAccountIndex acc
        accKey     = fromJust $ accPrvKey master deriv
        index      = fromIntegral $ dbAddressIndex add
        addrPrvKey = fromJust $ extPrvKey accKey index
        prvKey     = xPrvKey $ getAddrPrvKey addrPrvKey
    return prvKey

{- Coin Commands -}

-- | Returns the balance of an account.
cmdBalance :: (PersistUnique m, PersistQuery m)
           => AccountName   -- ^ Account name.
           -> m Word64      -- ^ Account balance.
cmdBalance name = checkInit >> do
    acc     <- dbGetAccount name
    dbBalance acc

-- | Returns a list of balances for every account in the wallet.
cmdBalances :: (PersistQuery m, PersistUnique m, PersistMonadBackend m ~ b)
            => m [(DbAccountGeneric b, Word64)]
            -- ^ All account balances
cmdBalances = checkInit >> do
    accs <- selectList [] []
    bals <- mapM dbBalance accs
    return $ zip (map entityVal accs) bals

-- | Returns the list of unspent coins for an account.
cmdCoins :: ( PersistQuery m
            , PersistUnique m 
            , PersistMonadBackend m ~ b
            )
         => AccountName         -- ^ Account name.
         -> m [DbCoinGeneric b] -- ^ List of unspent coins.
cmdCoins name = checkInit >> do
    (Entity ai _) <- dbGetAccount name
    dbCoins ai

-- | Returns a list of all the unspent coins for every account in the wallet.
cmdAllCoins :: ( PersistQuery m, PersistUnique m
               , PersistMonadBackend m ~ b
               )
            => m [(DbAccountGeneric b, [DbCoinGeneric b])]
            -- ^ Unspent coins for all accounts.
cmdAllCoins = checkInit >> do
    accs  <- selectList [] []
    coins <- mapM (dbCoins . entityKey) accs
    return $ zip (map entityVal accs) coins

{- Tx Commands -}

-- | Import a transaction into the wallet. If called multiple times, this
-- command will only update the existing transaction in the wallet. A new
-- transaction entry will be created for every account affected by this
-- transaction. Every transaction entry will summarize the information related
-- to its account only (such as total movement for this account).
cmdImportTx :: ( PersistQuery m
               , PersistUnique m
               , PersistMonadBackend m ~ b
               ) 
            => Tx                -- ^ Transaction to import.
            -> m [DbTxGeneric b] -- ^ New transaction entries created.
cmdImportTx tx = checkInit >> dbImportTx tx


-- | Remove a transaction from the database. This will remove all transaction
-- entries for this transaction as well as any child transactions and coins
-- deriving from it.
cmdRemoveTx :: (PersistUnique m, PersistQuery m, PersistMonadBackend m ~ b)
            => Hash256        -- ^ Transaction id (txid)
            -> m [Hash256]    -- ^ List of removed transaction entries
cmdRemoveTx h = checkInit >> dbRemoveTx h

-- | List all the transaction entries for an account. Transaction entries
-- summarize information for a transaction in a specific account only (such as
-- the total movement of for this account).
--
-- Transaction entries can also be tagged as /Orphan/ or /Partial/. Orphaned
-- transactions are transactions with a parent transaction that should be in
-- the wallet but has not been imported yet. Balances for orphaned transactions
-- can not be accurately computed until the parent transaction is imported.
--
-- Partial transactions are transactions that are not fully signed yet, such
-- as a partially signed multisignature transaction. Partial transactions
-- are visible in the wallet mostly for informational purposes. They can not
-- generate any coins as the txid or partial transactions will change once
-- they are fully signed. However, importing a partial transaction will /lock/
-- the coins that it spends so that you don't mistakenly spend them. Partial
-- transactions are replaced once the fully signed transaction is imported.
cmdListTx :: (PersistQuery m, PersistUnique m, PersistMonadBackend m ~ b)
          => AccountName                 -- ^ Account name.
          -> m [DbTxGeneric b]  -- ^ List of transaction entries.
cmdListTx name = checkInit >> do
    (Entity ai _) <- dbGetAccount name
    e <- selectList [ DbTxAccount ==. ai ] [ Asc DbTxCreated ]
    return $ map entityVal e

-- | Create a transaction sending some coins to a single recipient address.
cmdSend :: ( PersistQuery m
           , PersistUnique m
           , PersistMonadBackend m ~ b
           )
        => AccountName      -- ^ Account name.
        -> String           -- ^ Recipient address. 
        -> Int              -- ^ Amount to send.  
        -> Int              -- ^ Fee per 1000 bytes. 
        -> m (Tx, Bool)     -- ^ Payment transaction, complete?
cmdSend name a v fee = checkInit >> do
    dbSendTx name [(a,fromIntegral v)] (fromIntegral fee)

-- | Create a transaction sending some coins to a list of recipient addresses.
cmdSendMany :: ( PersistQuery m
               , PersistUnique m
               , PersistMonadBackend m ~ b
               )
            => AccountName    -- ^ Account name.
            -> [(String,Int)] -- ^ List of recipient addresses and amounts. 
            -> Int            -- ^ Fee per 1000 bytes. 
            -> m (Tx, Bool)   -- ^ Payment transaction.
cmdSendMany name dests fee = checkInit >> do
    dbSendTx name dests' (fromIntegral fee)
  where
    dests' = map (\(a,b) -> (a,fromIntegral b)) dests

-- | Try to sign the inputs of an existing transaction using the private keys
-- of an account. This command will return an indication if the transaction is
-- fully signed or if additional signatures are required. This command will
-- work for both normal inputs and multisignature inputs. Signing is limited to
-- the keys of one account only to allow for more control when the wallet is
-- used as the backend of a web service.
cmdSignTx :: PersistUnique m
          => AccountName  -- ^ Account name.
          -> Tx           -- ^ Transaction to sign. 
          -> SigHash      -- ^ Signature type to create. 
          -> m (Tx, Bool) -- ^ Signed transaction.
cmdSignTx name tx sh = checkInit >> dbSignTx name tx sh

{- Utility Commands -}

-- | Decodes a transaction, providing structural information on the inputs
-- and the outputs of the transaction.
cmdDecodeTx :: MonadIO m
            => String  -- ^ HEX encoded transaction
            -> m Tx    -- ^ Decoded transaction
cmdDecodeTx str 
    | isJust txM = return tx
    | otherwise  = liftIO $ throwIO $
        ParsingException "Could not parse transaction"
  where 
    txM = decodeToMaybe =<< (hexToBS str)
    tx  = fromJust txM

-- | Build a raw transaction from a list of outpoints and recipients encoded
-- in JSON.
--
-- Outpoint format as JSON:
--
-- >   [ 
-- >       { "txid": txid
-- >       , "vout": n
-- >       },...
-- >   ] 
--
--  Recipient list as JSON:
--
-- >   { addr: amnt,... }
--
cmdBuildRawTx :: MonadIO m 
              => String  -- ^ List of JSON encoded Outpoints.
              -> String  -- ^ List of JSON encoded Recipients.
              -> m Tx    -- ^ Transaction result.
cmdBuildRawTx i o 
    | isJust opsM && isJust destsM = do
        when (isLeft txE) $ liftIO $ throwIO $
            TransactionBuildingException $ fromLeft txE
        return tx
    | otherwise = liftIO $ throwIO $
        ParsingException "Could not parse input values"
  where
    opsM   = JSON.decode $ toLazyBS $ stringToBS i
    destsM = JSON.decode $ toLazyBS $ stringToBS o
    (RawTxOutPoints ops) = fromJust opsM
    (RawTxDests dests)   = fromJust destsM
    txE = buildAddrTx ops dests
    tx  = fromRight txE

-- | Sign a raw transaction by providing the signing parameters and private
-- keys manually. None of the keys in the wallet will be used for signing.
--
-- Signing data as JSON (redeemScript is optional):
--
-- >   [ 
-- >       { "txid": txid
-- >       , "vout": n
-- >       , "scriptPubKey": hex
-- >       , "redeemScript": hex
-- >       },...
-- >    ]
--
-- Private keys in JSON foramt:
--
-- >   [ WIF,... ]
cmdSignRawTx :: MonadIO m
             => Tx       -- ^ Transaction to sign.
             -> String   -- ^ List of JSON encoded signing parameters.
             -> String   -- ^ List of JSON encoded WIF private keys.
             -> SigHash  -- ^ Signature type. 
             -> m (Tx, Bool)
cmdSignRawTx tx strSigi strKeys sh 
    | isJust fsM && isJust keysM = do
        let btx    = detSignTx tx (map (\f -> f sh) fs) keys
            sigTx  = runBuild btx
        when (isBroken btx) $ do
            liftIO $ throwIO $ TransactionSigningException $ runBroken btx
        return (sigTx, isComplete btx)
    | otherwise = liftIO $ throwIO $
        ParsingException "Could not parse input values"
  where
    fsM   = JSON.decode $ toLazyBS $ stringToBS strSigi
    keysM = JSON.decode $ toLazyBS $ stringToBS strKeys
    (RawSigInput fs) = fromJust fsM
    (RawPrvKey keys) = fromJust keysM

