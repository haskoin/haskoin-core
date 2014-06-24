{-|
  This package provides a command line application called /hw/ (haskoin
  wallet). It is a lightweight bitcoin wallet featuring BIP32 key management,
  deterministic signatures (RFC-6979) and first order support for
  multisignature transactions. A library API for /hw/ is also exposed.
-}
module Network.Haskoin.Wallet
( 
-- *Initialization Commands
  initMnemo
, initWallet
, checkInit

-- *Account Commands
, AccountName
, getAccount
, newAccount
, newMSAccount
, addAccountKeys
, accountList
, accountPrvKey
, isMSAccount

-- *Address Commands
, getAddress
, addressList
, addressCount
, addressPage
, newAddrs
, addressLabel
, setLookAhead
, addressPrvKey

-- *Coin Commands
, balance
, unspentCoins

-- *Tx Commands
, AccTx(..) 
, txList
, importTx
, removeTx
, sendTx
, signWalletTx
, walletBloomFilter
, isTxInWallet
, firstKeyTime

-- *Block Commands
, importBlocks

-- *Database Types 
, CoinStatus(..)
, WalletException(..)

) where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, unless, when, liftM, void)
import Control.Monad.Trans (liftIO)
import Control.Exception (throwIO)

import Data.Maybe (catMaybes, isNothing, isJust, fromJust)
import Data.Time (getCurrentTime)
import qualified Data.Text as T (pack)
import qualified Data.ByteString as BS (ByteString, null)

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
    , (=.), (==.), (<-.)
    , SelectOpt( Asc )
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Coin
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Model

-- | Initialize a wallet from an optional mnemonic seed and a passphrase,
-- which could be blank.
initMnemo :: PersistUnique m
          => String
          -- ^ Passphrase to protect mnemonic.
          -> Maybe String
          -- ^ Mnemonic sentence to initialize wallet with.
          -- Use entropy from /dev/random otherwise.
          -> m Mnemonic
          -- ^ Mnemonic sentence used to initialize wallet.
          -- Impossible to retrieve in the future.

initMnemo p (Just s) = do
    let seedE = mnemonicToSeed english (T.pack p) $ T.pack s
        seed  = fromRight seedE
    when (isLeft seedE) $ liftIO $ throwIO $ 
        InitializationException $ fromLeft seedE
    _ <- initWallet seed
    return $ T.pack s

initMnemo p Nothing = do
    ent <- liftIO $ devRandom 16
    let msSeedE = do
            m <- toMnemonic english ent
            s <- mnemonicToSeed english (T.pack p) m
            return (m, s)
    when (isLeft msSeedE) $ liftIO $ throwIO $
        InitializationException $ fromLeft msSeedE
    let (ms, seed) = fromRight msSeedE
    _ <- initWallet seed
    return ms

-- | Initialize a wallet from a secret seed. This function will fail if the
-- wallet is already initialized.
initWallet :: PersistUnique m
           => BS.ByteString    -- ^ Secret seed
           -> m ()             
initWallet seed 
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
        insert_ $ DbConfig 0 1 time

isWalletInit :: PersistUnique m => String -> m Bool
isWalletInit name = do
    entM <- getBy $ UniqueWalletName name
    return $ isJust entM

checkInit :: PersistUnique m => m ()
checkInit = do
    isInit <- isWalletInit "main"
    unless isInit $ liftIO $ throwIO $ 
        InitializationException "Wallet main is not initialized"

