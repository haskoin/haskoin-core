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
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either (EitherT, left)

import qualified Data.ByteString as BS
import Data.Time (getCurrentTime)
import Data.Yaml 
    ( Value (Null)
    , object 
    , (.=)
    , toJSON
    )
import Data.Maybe (isJust, fromJust)
import Data.List (sortBy)
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
    , count
    , (<=.), (==.)
    , SelectOpt( Asc, OffsetBy, LimitTo )
    )
import Database.Persist.Sqlite (SqlBackend)

import Network.Haskoin.Wallet.DbAccount
import Network.Haskoin.Wallet.DbAddress
import Network.Haskoin.Wallet.DbCoin
import Network.Haskoin.Wallet.DbTx
import Network.Haskoin.Wallet.Util
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Util.BuildMonad

-- | Initialize a wallet from a mnemonic seed and a passphrase, which
-- could be blank. If mnemonic is Nothing, create new one and print it.
cmdInitMnemo :: PersistUnique m
             => String                   -- ^ Passphrase to protect mnemonic
             -> Maybe String             -- ^ Mnemonic string
             -> EitherT String m Value   -- ^ String mnemonic or Null

cmdInitMnemo pass (Just ms) = do
    checkExisting
    seed <- liftEither $ mnemonicToSeed english (T.pack pass) (T.pack ms)
    cmdInit seed

cmdInitMnemo pass Nothing = do
    checkExisting
    ent <- liftIO $ devRandom 16
    ms <- liftEither $ toMnemonic english ent
    seed <- liftEither $ mnemonicToSeed english (T.pack pass) ms
    _ <- cmdInit seed
    return $ object ["Seed" .= ms]


-- | Initialize a wallet from a secret seed. This function will fail if the
-- wallet is already initialized.
cmdInit :: PersistUnique m
        => BS.ByteString          -- ^ Secret seed.
        -> EitherT String m Value -- ^ Returns Null.
cmdInit seed 
    | BS.null seed = left "cmdInit: seed can not be empty"
    | otherwise = do
        checkExisting
        time   <- liftIO getCurrentTime
        master <- liftMaybe err $ makeMasterKey seed
        let str = xPrvExport $ masterKey master
        insert_ $ DbWallet "main" "full" str (-1) time
        return Null
  where 
    err = "cmdInit: Invalid master key generated from seed"

{- Account Commands -}

-- | Create a new account from an account name. Accounts are identified by
-- their name and they must be unique.
cmdNewAcc :: (PersistUnique m, PersistQuery m) 
         => String                 -- ^ Account name.
         -> EitherT String m Value -- ^ Returns the new account information.
cmdNewAcc name = do
    acc <- dbNewAcc name
    -- Generate gap addresses
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
cmdNewMS :: (PersistUnique m, PersistQuery m)
         => String                 -- ^ Account name.
         -> Int                    -- ^ Required number of keys (m in m of n).
         -> Int                    -- ^ Total number of keys (n in m of n).
         -> [XPubKey]              -- ^ Thirdparty public keys.
         -> EitherT String m Value -- ^ Returns the new account information.
cmdNewMS name m n mskeys = do
    acc <- dbNewMS name m n mskeys
    when (length (dbAccountMsKeys acc) == n - 1) $ do
        -- Generate gap addresses
        dbSetGap name 30 False
        dbSetGap name 30 True
    return $ yamlAcc acc

-- | Add new thirdparty keys to a multisignature account. This function can
-- fail if the multisignature account already has all required keys. In order
-- to prevent usage mistakes, adding a key from your own wallet will fail.
cmdAddKeys :: (PersistUnique m, PersistQuery m)
           => AccountName            -- ^ Account name.
           -> [XPubKey]              -- ^ Thirdparty public keys to add.
           -> EitherT String m Value -- ^ Returns the account information.
cmdAddKeys name keys = do
    acc <- dbAddKeys name keys
    let n = fromJust $ dbAccountMsTotal acc
    when (length (dbAccountMsKeys acc) == n - 1) $ do
        -- Generate gap addresses
        dbSetGap name 30 False
        dbSetGap name 30 True
    return $ yamlAcc acc

-- | Returns information on an account.
cmdAccInfo :: PersistUnique m 
           => AccountName            -- ^ Account name.
           -> EitherT String m Value -- ^ Account information.
cmdAccInfo name = yamlAcc . entityVal <$> dbGetAcc name

-- | Returns a list of all accounts in the wallet.
cmdListAcc :: PersistQuery m 
           => EitherT String m Value -- ^ List of accounts
cmdListAcc = toJSON . (map (yamlAcc . entityVal)) <$> selectList [] []

-- | Returns information on extended public and private keys of an account.
-- For a multisignature account, thirdparty keys are also returned.
cmdDumpKeys :: PersistUnique m
            => AccountName            -- ^ Account name.
            -> EitherT String m Value -- ^ Extended key information.
cmdDumpKeys name = do
    (Entity _ acc) <- dbGetAcc name
    w <- liftMaybe walErr =<< (get $ dbAccountWallet acc)
    let keyM = loadMasterKey =<< (xPrvImport $ dbWalletMaster w)
    master <- liftMaybe keyErr keyM
    prv <- liftMaybe prvErr $ 
        accPrvKey master (fromIntegral $ dbAccountIndex acc)
    let prvKey = getAccPrvKey prv
        pubKey = deriveXPubKey prvKey
        ms | isMSAcc acc = ["MSKeys" .= (toJSON $ dbAccountMsKeys acc)]
           | otherwise   = []
    return $ object $
        [ "Account" .= yamlAcc acc
        , "PubKey"  .= xPubExport pubKey 
        , "PrvKey"  .= xPrvExport prvKey 
        ] ++ ms
    where keyErr = "cmdDumpKeys: Could not decode master key"
          prvErr = "cmdDumpKeys: Could not derive account private key"
          walErr = "cmdDumpKeys: Could not find account wallet"

{- Address Commands -}

-- | Returns a page of addresses for an account. Pages are numbered starting
-- from page 1. Requesting page 0 will return the last page. 
cmdList :: (PersistUnique m, PersistQuery m) 
        => AccountName             -- ^ Account name.
        -> Int                     -- ^ Requested page number.
        -> Int                     -- ^ Number of addresses per page.
        -> EitherT String m Value  -- ^ The requested page.
cmdList name pageNum resPerPage 
    | pageNum < 0 = left $ 
        unwords ["cmdList: Invalid page number", show pageNum]
    | resPerPage < 1 = left $ 
        unwords ["cmdList: Invalid results per page",show resPerPage]
    | otherwise = do
        (Entity ai acc) <- dbGetAcc name
        addrCount <- count 
            [ DbAddressAccount ==. ai
            , DbAddressInternal ==. False
            , DbAddressIndex <=. dbAccountExtIndex acc
            ] 
        let maxPage = max 1 $ (addrCount + resPerPage - 1) `div` resPerPage
            page | pageNum == 0 = maxPage
                 | otherwise = pageNum
        when (page > maxPage) $ left "cmdList: Page number too high"
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
cmdGenAddrs :: (PersistUnique m, PersistQuery m)
            => AccountName            -- ^ Account name.
            -> Int                    -- ^ Number of addresses to generate.
            -> EitherT String m Value -- ^ List of new addresses.
cmdGenAddrs name c = cmdGenWithLabel name (replicate c "")

-- | Generate new payment addresses with labels for an account.
cmdGenWithLabel :: (PersistUnique m, PersistQuery m)
                => AccountName            -- ^ Account name.
                -> [String]               -- ^ List of address labels. 
                -> EitherT String m Value -- ^ List of new addresses.
cmdGenWithLabel name labels = do
    addrs <- dbGenAddrs name labels False
    return $ toJSON $ map yamlAddr addrs

-- | Add a label to an address.
cmdLabel :: PersistUnique m
         => AccountName            -- ^ Account name.
         -> Int                    -- ^ Derivation index of the address. 
         -> String                 -- ^ New label.
         -> EitherT String m Value -- ^ New address information.
cmdLabel name key label = do
    (Entity ai acc) <- dbGetAcc name
    (Entity i add) <- liftMaybe keyErr =<< 
        (getBy $ UniqueAddressKey ai key False)
    when (dbAddressIndex add > dbAccountExtIndex acc) $ left keyErr
    let newAddr = add{dbAddressLabel = label}
    replace i newAddr
    return $ yamlAddr newAddr
  where 
    keyErr = unwords ["cmdLabel: Key",show key,"does not exist"]

-- | Returns the private key tied to a payment address in WIF format.
cmdWIF :: PersistUnique m
       => AccountName            -- ^ Account name.
       -> Int                    -- ^ Derivation index of the address. 
       -> EitherT String m Value -- ^ WIF value.
cmdWIF name key = do
    (Entity _ w) <- dbGetWallet "main"
    (Entity ai acc) <- dbGetAcc name
    (Entity _ add) <- liftMaybe keyErr =<< 
        (getBy $ UniqueAddressKey ai key False)
    when (dbAddressIndex add > dbAccountExtIndex acc) $ left keyErr
    mst <- liftMaybe mstErr $ loadMasterKey =<< xPrvImport (dbWalletMaster w)
    aKey <- liftMaybe prvErr $ accPrvKey mst $ fromIntegral $ dbAccountIndex acc
    let index = fromIntegral $ dbAddressIndex add
    addrPrvKey <- liftMaybe addErr $ extPrvKey aKey index
    let prvKey = xPrvKey $ getAddrPrvKey addrPrvKey
    return $ object [ "WIF" .= T.pack (toWIF prvKey) ]
  where 
    keyErr = unwords ["cmdWIF: Key",show key,"does not exist"]
    mstErr = "cmdWIF: Could not load master key"
    prvErr = "cmdWIF: Invalid account derivation index"
    addErr = "cmdWIF: Invalid address derivation index"

{- Coin Commands -}

-- | Returns the balance of an account.
cmdBalance :: (PersistUnique m, PersistQuery m)
           => AccountName            -- ^ Account name.
           -> EitherT String m Value -- ^ Account balance.
cmdBalance name = do
    acc <- dbGetAcc name
    balance <- dbBalance acc
    return $ object [ "Balance" .= toJSON balance ]

-- | Returns a list of balances for every account in the wallet.
cmdBalances :: PersistQuery m
            => EitherT String m Value -- ^ All account balances
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
cmdCoins :: ( PersistQuery m, PersistUnique m 
            , PersistMonadBackend m ~ SqlBackend
            )
         => AccountName            -- ^ Account name.
         -> EitherT String m Value -- ^ List of unspent coins.
cmdCoins name = do
    (Entity ai _) <- dbGetAcc name
    coins <- dbCoins ai
    return $ toJSON $ map yamlCoin coins

-- | Returns a list of all the unspent coins for every account in the wallet.
cmdAllCoins :: ( PersistQuery m, PersistUnique m
               , PersistMonadBackend m ~ SqlBackend
               )
            => EitherT String m Value -- ^ Unspent coins for all accounts.
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
cmdImportTx :: ( PersistQuery m, PersistUnique m
               , PersistMonadBackend m ~ SqlBackend
               ) 
            => Tx                     -- ^ Transaction to import.
            -> EitherT String m Value -- ^ New transaction entries created.
cmdImportTx tx = do
    accTx <- dbImportTx tx
    return $ toJSON $ map yamlTx $ sortBy f accTx
  where
    f a b = (dbTxCreated a) `compare` (dbTxCreated b)


-- | Remove a transaction from the database. This will remove all transaction
-- entries for this transaction as well as any child transactions and coins
-- deriving from it.
cmdRemoveTx :: PersistQuery m
            => String                 -- ^ Transaction id (txid)
            -> EitherT String m Value -- ^ List of removed transaction entries
cmdRemoveTx tid = do
    removed <- dbRemoveTx tid
    return $ toJSON removed

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
cmdListTx :: (PersistQuery m, PersistUnique m)
          => AccountName            -- ^ Account name.
          -> EitherT String m Value -- ^ List of transaction entries.
cmdListTx name = do
    (Entity ai _) <- dbGetAcc name
    txs <- selectList [ DbTxAccount ==. ai
                      ] 
                      [ Asc DbTxCreated ]
    return $ toJSON $ map (yamlTx . entityVal) txs

-- | Create a transaction sending some coins to a single recipient address.
cmdSend :: ( PersistQuery m, PersistUnique m
           , PersistMonadBackend m ~ SqlBackend
           )
        => AccountName            -- ^ Account name.
        -> String                 -- ^ Recipient address. 
        -> Int                    -- ^ Amount to send.  
        -> Int                    -- ^ Fee per 1000 bytes. 
        -> EitherT String m Value -- ^ Payment transaction.
cmdSend name a v fee = do
    (tx,complete) <- dbSendTx name [(a,fromIntegral v)] (fromIntegral fee)
    return $ object [ "Tx" .= (toJSON $ bsToHex $ encode' tx)
                    , "Complete"   .= complete
                    ]

-- | Create a transaction sending some coins to a list of recipient addresses.
cmdSendMany :: ( PersistQuery m, PersistUnique m
               , PersistMonadBackend m ~ SqlBackend
               )
            => AccountName             -- ^ Account name.
            -> [(String,Int)]          
               -- ^ List of recipient addresses and amounts. 
            -> Int                     -- ^ Fee per 1000 bytes. 
            -> EitherT String m Value  -- ^ Payment transaction.
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
cmdSignTx :: PersistUnique m
          => AccountName            -- ^ Account name.
          -> Tx                     -- ^ Transaction to sign. 
          -> SigHash                -- ^ Signature type to create. 
          -> EitherT String m Value -- ^ Signed transaction.
cmdSignTx name tx sh = do
    (newTx,complete) <- dbSignTx name tx sh
    return $ object 
        [ (T.pack "Tx")       .= (toJSON $ bsToHex $ encode' newTx)
        , (T.pack "Complete") .= complete
        ]

{- Utility Commands -}

-- | Decodes a transaction, providing structural information on the inputs
-- and the outputs of the transaction.
cmdDecodeTx :: Monad m 
            => String                 -- ^ HEX encoded transaction
            -> EitherT String m Value -- ^ Decoded transaction
cmdDecodeTx str = do
    tx <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS str)
    return $ toJSON (tx :: Tx)
    where txErr = "cmdDecodeTx: Could not decode transaction"

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
cmdBuildRawTx :: Monad m 
              => String                 -- ^ List of JSON encoded Outpoints.
              -> String                 -- ^ List of JSON encoded Recipients.
              -> EitherT String m Value -- ^ Transaction result.
cmdBuildRawTx i o = do
    (RawTxOutPoints ops) <- liftMaybe opErr $ 
        Json.decode $ toLazyBS $ stringToBS i
    (RawTxDests dests)   <- liftMaybe dsErr $ 
        Json.decode $ toLazyBS $ stringToBS o
    tx  <- liftEither $ buildAddrTx ops dests
    return $ object [ (T.pack "Tx") .= (bsToHex $ encode' tx) ]
  where
    opErr = "cmdBuildRawTx: Could not parse OutPoints"
    dsErr = "cmdBuildRawTx: Could not parse recipients"


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
cmdSignRawTx :: Monad m 
             => Tx                      -- ^ Transaction to sign.
             -> String                  
                -- ^ List of JSON encoded signing parameters.
             -> String                  
                -- ^ List of JSON encoded WIF private keys.
             -> SigHash                 -- ^ Signature type. 
             -> EitherT String m Value
cmdSignRawTx tx strSigi strKeys sh  = do
    (RawSigInput fs) <- liftMaybe sigiErr $ 
        Json.decode $ toLazyBS $ stringToBS strSigi
    (RawPrvKey keys) <- liftMaybe keysErr $
        Json.decode $ toLazyBS $ stringToBS strKeys
    let sigTx = detSignTx tx (map (\f -> f sh) fs) keys
    bsTx <- liftEither $ buildToEither sigTx
    return $ object [ (T.pack "Tx") .= (toJSON $ bsToHex $ encode' bsTx)
                    , (T.pack "Complete") .= isComplete sigTx
                    ]
  where
    sigiErr = "cmdSignRawTx: Could not parse parent transaction data"
    keysErr = "cmdSignRawTx: Could not parse private keys (WIF)"


checkExisting :: PersistUnique m => EitherT String m ()
checkExisting = do
    prev <- getBy $ UniqueWalletName "main"
    when (isJust prev) $ left "checkExisting: Wallet is already initialized"
