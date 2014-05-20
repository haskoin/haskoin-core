{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  This module provides an API to the Haskoin wallet. All commands return a
  'Value' result which can be encoded to JSON or YAML. The wallet commands
  run within the Persistent framework for database support:

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
, cmdWIF
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

import Control.Applicative ((<$>))
import Control.Monad (when, unless, liftM)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Exception (throwIO)
import Control.Monad.Logger 
    ( MonadLogger
    , logDebugN
    , logInfoN
    , logWarnN
    , logErrorN
    )

import qualified Data.ByteString as BS
import Data.Time (getCurrentTime)
import Data.Yaml 
    ( Value (Null)
    , object 
    , (.=)
    , toJSON
    )
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List (sortBy, nub)
import qualified Data.Aeson as Json (decode)
import qualified Data.Text as T (pack)

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
    , insert_
    , replace
    , update
    , count
    , (=.), (<=.), (==.)
    , SelectOpt( Asc, OffsetBy, LimitTo )
    )
import Database.Persist.Sqlite (SqlBackend)

import Network.Haskoin.Wallet.DbAccount
import Network.Haskoin.Wallet.DbAddress
import Network.Haskoin.Wallet.DbCoin
import Network.Haskoin.Wallet.DbTx
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Util.BuildMonad

-- | Initialize a wallet from a mnemonic seed and a passphrase, which
-- could be blank. If mnemonic is Nothing, create new one and print it.
cmdInitMnemo :: (MonadLogger m, PersistUnique m)
             => String           -- ^ Passphrase to protect mnemonic
             -> Maybe String     -- ^ Mnemonic string
             -> m Value          -- ^ String mnemonic or Null

cmdInitMnemo pass (Just ms) = do
    let seedE = mnemonicToSeed english (T.pack pass) (T.pack ms)
        seed  = fromRight seedE
    when (isLeft seedE) $ liftIO $ throwIO $ 
        InitializationException $ fromLeft seedE
    cmdInit seed

cmdInitMnemo pass Nothing = do
    ent  <- liftIO $ devRandom 16
    let msE   = toMnemonic english ent
        ms    = fromRight msE
        seedE = mnemonicToSeed english (T.pack pass) =<< msE
        seed  = fromRight seedE
    when (isLeft seedE) $ liftIO $ throwIO $ 
        InitializationException $ fromLeft seedE
    _ <- cmdInit seed
    return $ object ["Seed" .= ms]

-- | Initialize a wallet from a secret seed. This function will fail if the
-- wallet is already initialized.
cmdInit :: (MonadLogger m, PersistUnique m)
        => BS.ByteString    -- ^ Secret seed.
        -> m Value          -- ^ Returns Null.
cmdInit seed 
    | BS.null seed = liftIO $ throwIO $ 
        InitializationException "The seed i sempty"
    | otherwise = do
        isInit <- isWalletInit "main"
        when isInit $ liftIO $ throwIO $ 
            InitializationException "The wallet is already initialized"
        time <- liftIO getCurrentTime
        let master = makeMasterKey seed
        when (isNothing master) $ liftIO $ throwIO $ InitializationException
            "The seed derivation produced an invalid key. Use another seed"
        insert_ $ DbWallet "main" "full" (fromJust master) (-1) time
        return Null

{- Account Commands -}

-- | Create a new account from an account name. Accounts are identified by
-- their name and they must be unique.
cmdNewAcc :: ( MonadLogger m
             , PersistUnique m
             , PersistQuery m
             , PersistMonadBackend m ~ b
             ) 
          => String  -- ^ Account name.
          -> m Value -- ^ Returns the new account information.
cmdNewAcc name = do
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
    return $ yamlAcc acc

-- | Create a new multisignature account. The thirdparty keys can be provided
-- now or later using the 'cmdAddKeys' command. The number of thirdparty keys
-- can not exceed n-1 as your own account key will be used as well in the
-- multisignature scheme. If less than n-1 keys are provided, the account will
-- be in a pending state and no addresses can be generated.
--
-- In order to prevent usage mistakes, you can not create a multisignature 
-- account with other keys from your own wallet.
cmdNewMS :: (MonadLogger m, PersistUnique m, PersistQuery m)
         => String    -- ^ Account name.
         -> Int       -- ^ Required number of keys (m in m of n).
         -> Int       -- ^ Total number of keys (n in m of n).
         -> [XPubKey] -- ^ Thirdparty public keys.
         -> m Value   -- ^ Returns the new account information.
cmdNewMS name m n mskeys = do
    let keys = nub mskeys
    time <- liftIO getCurrentTime
    unless (n >= 1 && n <= 16 && m >= 1 && m <= n) $ liftIO $ throwIO $ 
        AccountSetupException "Invalid multisig parameters"
    unless (length keys < n) $ liftIO $ throwIO $
        AccountSetupException "Too many keys"
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
    return $ yamlAcc acc

-- | Add new thirdparty keys to a multisignature account. This function can
-- fail if the multisignature account already has all required keys. In order
-- to prevent usage mistakes, adding a key from your own wallet will fail.
cmdAddKeys :: (MonadLogger m, PersistUnique m, PersistQuery m)
           => AccountName -- ^ Account name.
           -> [XPubKey]   -- ^ Thirdparty public keys to add.
           -> m Value     -- ^ Returns the account information.
cmdAddKeys name keys 
    | null keys = liftIO $ throwIO $
         AccountSetupException "Multisig key list can not be empty"
    | otherwise = do
        (Entity ai acc) <- dbGetAccount name
        unless (isMSAcc acc) $ liftIO $ throwIO $ AccountSetupException 
            "Can only add keys to a multisig account"
        -- TODO: Match PubKey instead of XPubKey to avoid playing 
        -- with height or other values
        exists <- mapM (\x -> count [DbAccountKey ==. AccPubKey x]) keys
        unless (sum exists == 0) $ liftIO $ throwIO $ AccountSetupException 
            "Can not add your own keys to a multisig account"
        let prevKeys = dbAccountMsKeys acc
        when (length prevKeys == (fromJust $ dbAccountMsTotal acc) - 1) $ 
            liftIO $ throwIO $ AccountSetupException 
                "The account is complete and no further keys can be added"
        let newKeys = nub $ prevKeys ++ keys
            newAcc  = acc{ dbAccountMsKeys = newKeys }
        unless (length newKeys < (fromJust $ dbAccountMsTotal acc)) $
            liftIO $ throwIO $ AccountSetupException
                "Too many keys were added to this account"
        replace ai newAcc
        let n = fromJust $ dbAccountMsTotal newAcc
        when (length (dbAccountMsKeys acc) == n - 1) $ do
            -- Generate gap addresses
            dbSetGap name 30 False
            dbSetGap name 30 True
        return $ yamlAcc newAcc

-- | Returns information on an account.
cmdAccInfo :: (MonadLogger m, PersistUnique m)
           => AccountName   -- ^ Account name.
           -> m Value       -- ^ Account information.
cmdAccInfo name = do
    acc <- dbGetAccount name
    return $ yamlAcc $ entityVal acc

-- | Returns a list of all accounts in the wallet.
cmdListAcc :: PersistQuery m 
           => m Value       -- ^ List of accounts
cmdListAcc = do
    ls <- selectList [] []
    return $ toJSON $ map (yamlAcc . entityVal) ls

-- | Returns information on extended public and private keys of an account.
-- For a multisignature account, thirdparty keys are also returned.
cmdDumpKeys :: ( MonadLogger m
               , PersistUnique m
               )
            => AccountName  -- ^ Account name.
            -> m Value      -- ^ Extended key information.
cmdDumpKeys name = do
    (Entity _ acc) <- dbGetAccount name
    w <- liftM fromJust (get $ dbAccountWallet acc)
    let master = dbWalletMaster w
        deriv  = fromIntegral $ dbAccountIndex acc
        accPrv = fromJust $ accPrvKey master deriv
        prvKey = getAccPrvKey accPrv
        pubKey = deriveXPubKey prvKey
        ms | isMSAcc acc = 
               ["MSKeys" .= (toJSON $ map xPubExport $ dbAccountMsKeys acc)]
           | otherwise   = []
    return $ object $
        [ "Account" .= yamlAcc acc
        , "PubKey"  .= xPubExport pubKey 
        , "PrvKey"  .= xPrvExport prvKey 
        ] ++ ms

{- Address Commands -}

-- | Returns a page of addresses for an account. Pages are numbered starting
-- from page 1. Requesting page 0 will return the last page. 
cmdList :: (MonadLogger m, PersistUnique m, PersistQuery m) 
        => AccountName   -- ^ Account name.
        -> Int           -- ^ Requested page number.
        -> Int           -- ^ Number of addresses per page.
        -> m Value       -- ^ The requested page.
cmdList name pageNum resPerPage 
    | pageNum < 0 = liftIO $ throwIO $ InvalidPageException $ 
        unwords ["Invalid page number", show pageNum]
    | resPerPage < 1 = liftIO $ throwIO $ InvalidPageException $
        unwords ["Invalid results per page",show resPerPage]
    | otherwise = do
        (Entity ai acc) <- dbGetAccount name
        addrCount <- count 
            [ DbAddressAccount ==. ai
            , DbAddressInternal ==. False
            , DbAddressIndex <=. dbAccountExtIndex acc
            ] 
        let maxPage = max 1 $ (addrCount + resPerPage - 1) `div` resPerPage
            page | pageNum == 0 = maxPage
                 | otherwise = pageNum
        when (page > maxPage) $ liftIO $ throwIO $
            InvalidPageException "The page number is too high"
        addrs <- selectList [ DbAddressAccount ==. ai
                            , DbAddressInternal ==. False
                            , DbAddressIndex <=. dbAccountExtIndex acc
                            ] 
                            [ Asc DbAddressId
                            , LimitTo resPerPage
                            , OffsetBy $ (page - 1) * resPerPage
                            ]
        return $ yamlAddrList (map entityVal addrs) page resPerPage addrCount

-- | Generate new payment addresses for an account. 
cmdGenAddrs :: (MonadLogger m, PersistUnique m, PersistQuery m)
            => AccountName  -- ^ Account name.
            -> Int          -- ^ Number of addresses to generate.
            -> m Value      -- ^ List of new addresses.
cmdGenAddrs name c = cmdGenWithLabel name (replicate c "")

-- | Generate new payment addresses with labels for an account.
cmdGenWithLabel :: (MonadLogger m, PersistUnique m, PersistQuery m)
                => AccountName  -- ^ Account name.
                -> [String]     -- ^ List of address labels. 
                -> m Value      -- ^ List of new addresses.
cmdGenWithLabel name labels = do
    addrs <- dbGenAddrs name labels False
    return $ toJSON $ map yamlAddr addrs

-- | Add a label to an address.
cmdLabel :: (MonadLogger m, PersistUnique m, PersistMonadBackend m ~ SqlBackend)
         => AccountName   -- ^ Account name.
         -> Int           -- ^ Derivation index of the address. 
         -> String        -- ^ New label.
         -> m Value       -- ^ New address information.
cmdLabel name key label = do
    (Entity ai acc) <- dbGetAccount name
    when (key > dbAccountExtIndex acc) $ liftIO $ throwIO $
        InvalidAddressException "This address key does not exist"
    (Entity i add) <- dbGetAddressByIndex ai key False
    let newAddr = add{dbAddressLabel = label}
    replace i newAddr
    return $ yamlAddr newAddr

-- | Returns the private key tied to a payment address in WIF format.
cmdWIF :: (MonadLogger m, PersistUnique m, PersistMonadBackend m ~ SqlBackend)
       => AccountName      -- ^ Account name.
       -> Int              -- ^ Derivation index of the address. 
       -> m Value          -- ^ WIF value.
cmdWIF name key = do
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
    return $ object [ "WIF" .= T.pack (toWIF prvKey) ]

{- Coin Commands -}

-- | Returns the balance of an account.
cmdBalance :: (MonadLogger m, PersistUnique m, PersistQuery m)
           => AccountName   -- ^ Account name.
           -> m Value       -- ^ Account balance.
cmdBalance name = do
    acc     <- dbGetAccount name
    balance <- dbBalance acc
    return $ object [ "Balance" .= toJSON balance ]

-- | Returns a list of balances for every account in the wallet.
cmdBalances :: PersistQuery m
            => m Value         -- ^ All account balances
cmdBalances = do
    accs <- selectList [] []
    bals <- mapM dbBalance accs
    return $ toJSON $ map f $ zip accs bals
  where 
    f (acc,b) = object
        [ "Account" .= (dbAccountName $ entityVal acc)
        , "Balance" .= b
        ]

-- | Returns the list of unspent coins for an account.
cmdCoins :: ( MonadLogger m
            , PersistQuery m
            , PersistUnique m 
            , PersistMonadBackend m ~ SqlBackend
            )
         => AccountName  -- ^ Account name.
         -> m Value      -- ^ List of unspent coins.
cmdCoins name = do
    (Entity ai _) <- dbGetAccount name
    coins         <- dbCoins ai
    return $ toJSON $ map yamlCoin coins

-- | Returns a list of all the unspent coins for every account in the wallet.
cmdAllCoins :: ( PersistQuery m, PersistUnique m
               , PersistMonadBackend m ~ SqlBackend
               )
            => m Value -- ^ Unspent coins for all accounts.
cmdAllCoins = do
    accs  <- selectList [] []
    coins <- mapM (dbCoins . entityKey) accs
    return $ toJSON $ map g $ zip accs coins
  where 
    g (acc,cs) = object
        [ "Account" .= (dbAccountName $ entityVal acc)
        , "Coins" .= (toJSON $ map yamlCoin cs)
        ]

{- Tx Commands -}

-- | Import a transaction into the wallet. If called multiple times, this
-- command will only update the existing transaction in the wallet. A new
-- transaction entry will be created for every account affected by this
-- transaction. Every transaction entry will summarize the information related
-- to its account only (such as total movement for this account).
cmdImportTx :: ( MonadLogger m
               , PersistQuery m
               , PersistUnique m
               , PersistMonadBackend m ~ SqlBackend
               ) 
            => Tx      -- ^ Transaction to import.
            -> m Value -- ^ New transaction entries created.
cmdImportTx tx = do
    accTx <- dbImportTx tx
    return $ toJSON $ map yamlTx $ sortBy f accTx
  where
    f a b = (dbTxCreated a) `compare` (dbTxCreated b)


-- | Remove a transaction from the database. This will remove all transaction
-- entries for this transaction as well as any child transactions and coins
-- deriving from it.
cmdRemoveTx :: (MonadLogger m, PersistQuery m)
            => Hash256    -- ^ Transaction id (txid)
            -> m Value    -- ^ List of removed transaction entries
cmdRemoveTx h = do
    removed <- dbRemoveTx h
    return $ toJSON $ map encodeTxid removed

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
cmdListTx :: (MonadLogger m, PersistQuery m, PersistUnique m)
          => AccountName  -- ^ Account name.
          -> m Value      -- ^ List of transaction entries.
cmdListTx name = do
    (Entity ai _) <- dbGetAccount name
    txs <- selectList [ DbTxAccount ==. ai
                      ] 
                      [ Asc DbTxCreated ]
    return $ toJSON $ map (yamlTx . entityVal) txs

-- | Create a transaction sending some coins to a single recipient address.
cmdSend :: ( MonadLogger m
           , PersistQuery m
           , PersistUnique m
           , PersistMonadBackend m ~ SqlBackend
           )
        => AccountName -- ^ Account name.
        -> String      -- ^ Recipient address. 
        -> Int         -- ^ Amount to send.  
        -> Int         -- ^ Fee per 1000 bytes. 
        -> m Value     -- ^ Payment transaction.
cmdSend name a v fee = do
    (tx,complete) <- dbSendTx name [(a,fromIntegral v)] (fromIntegral fee)
    return $ object [ "Tx" .= (toJSON $ bsToHex $ encode' tx)
                    , "Complete"   .= complete
                    ]

-- | Create a transaction sending some coins to a list of recipient addresses.
cmdSendMany :: ( MonadLogger m
               , PersistQuery m
               , PersistUnique m
               , PersistMonadBackend m ~ SqlBackend
               )
            => AccountName    -- ^ Account name.
            -> [(String,Int)] -- ^ List of recipient addresses and amounts. 
            -> Int            -- ^ Fee per 1000 bytes. 
            -> m Value        -- ^ Payment transaction.
cmdSendMany name dests fee = do
    (tx,complete) <- dbSendTx name dests' (fromIntegral fee)
    return $ object [ "Tx" .= (toJSON $ bsToHex $ encode' tx)
                    , "Complete"   .= complete
                    ]
    where dests' = map (\(a,b) -> (a,fromIntegral b)) dests

-- | Try to sign the inputs of an existing transaction using the private keys
-- of an account. This command will return an indication if the transaction is
-- fully signed or if additional signatures are required. This command will
-- work for both normal inputs and multisignature inputs. Signing is limited to
-- the keys of one account only to allow for more control when the wallet is
-- used as the backend of a web service.
cmdSignTx :: (MonadLogger m, PersistUnique m)
          => AccountName  -- ^ Account name.
          -> Tx           -- ^ Transaction to sign. 
          -> SigHash      -- ^ Signature type to create. 
          -> m Value      -- ^ Signed transaction.
cmdSignTx name tx sh = do
    (newTx, complete) <- dbSignTx name tx sh
    return $ object 
        [ (T.pack "Tx")       .= (toJSON $ bsToHex $ encode' newTx)
        , (T.pack "Complete") .= complete
        ]

{- Utility Commands -}

-- | Decodes a transaction, providing structural information on the inputs
-- and the outputs of the transaction.
cmdDecodeTx :: (MonadIO m, MonadLogger m)
            => String  -- ^ HEX encoded transaction
            -> m Value -- ^ Decoded transaction
cmdDecodeTx str 
    | isJust txM = return $ toJSON (tx :: Tx)
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
cmdBuildRawTx :: (MonadIO m, MonadLogger m) 
              => String  -- ^ List of JSON encoded Outpoints.
              -> String  -- ^ List of JSON encoded Recipients.
              -> m Value -- ^ Transaction result.
cmdBuildRawTx i o 
    | isJust opsM && isJust destsM = do
        when (isLeft txE) $ liftIO $ throwIO $
            TransactionBuildingException $ fromLeft txE
        return $ object [ (T.pack "Tx") .= (bsToHex $ encode' tx) ]
    | otherwise = liftIO $ throwIO $
        ParsingException "Could not parse input values"
  where
    opsM   = Json.decode $ toLazyBS $ stringToBS i
    destsM = Json.decode $ toLazyBS $ stringToBS o
    (RawTxOutPoints ops) = fromJust opsM
    (RawTxDests dests)   = fromJust destsM
    txE = buildAddrTx ops dests
    tx  = fromRight txE

-- | Sign a raw transaction by providing the signing parameters and private
-- keys manually. None of the keys in the wallet will be used for signing.
--
-- Signing data as JSON (scriptRedeem is optional):
--
-- >   [ 
-- >       { "txid": txid
-- >       , "vout": n
-- >       , "scriptPubKey": hex
-- >       , "scriptRedeem": hex
-- >       },...
-- >    ]
--
-- Private keys in JSON foramt:
--
-- >   [ WIF,... ]
cmdSignRawTx :: (MonadIO m, MonadLogger m)
             => Tx                      -- ^ Transaction to sign.
             -> String                  
                -- ^ List of JSON encoded signing parameters.
             -> String                  
                -- ^ List of JSON encoded WIF private keys.
             -> SigHash                 -- ^ Signature type. 
             -> m Value
cmdSignRawTx tx strSigi strKeys sh 
    | isJust fsM && isJust keysM = do
        let buildTx = detSignTx tx (map (\f -> f sh) fs) keys
            tx      = runBuild buildTx
        when (isBroken buildTx) $ liftIO $ throwIO $
            TransactionSigningException $ runBroken buildTx
        return $ object [ (T.pack "Tx") .= (toJSON $ bsToHex $ encode' tx)
                        , (T.pack "Complete") .= isComplete buildTx
                        ]
    | otherwise = liftIO $ throwIO $
        ParsingException "Could not parse input values"
  where
    sigiErr = "cmdSignRawTx: Could not parse parent transaction data"
    keysErr = "cmdSignRawTx: Could not parse private keys (WIF)"
    fsM   = Json.decode $ toLazyBS $ stringToBS strSigi
    keysM = Json.decode $ toLazyBS $ stringToBS strKeys
    (RawSigInput fs) = fromJust fsM
    (RawPrvKey keys) = fromJust keysM

