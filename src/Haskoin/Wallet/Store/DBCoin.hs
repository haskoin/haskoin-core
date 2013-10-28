module Haskoin.Wallet.Store.DBCoin
( DBCoin(..)
, CoinKey(..)
, getCoin
, putCoin
, importTx
, listCoins
) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Resource

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Haskoin.Wallet.Store.Util
import Haskoin.Wallet.Store.DBAddress
import Haskoin.Wallet.Store.DBAccount
import Haskoin.Script
import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Util

data DBCoin = DBCoin{ coinOutPoint :: OutPoint
                    , coinTxOut    :: TxOut
                    , coinSpent    :: Bool
                    , coinPos      :: Int
                    , coinAccPos   :: Int
                    } deriving (Eq, Show)

data CoinKey = CoinOutPoint OutPoint | CoinPos Int Int
    deriving (Eq, Show)

getCoin :: MonadResource m => CoinKey -> WalletDB m (Maybe DBCoin)
getCoin key = case key of
    CoinOutPoint op -> do
        bsM <- dbGet $ "coinoutpoint_" ++ (bsToString $ encode' op)
        maybe (return Nothing) ((f <$>) . dbGet . bsToString) bsM
    CoinPos ap p -> do
        bsM <- dbGet $ "coin_" ++ (encodeInt ap) ++ "_" ++ (encodeInt p)
        return $ f bsM 
    where f = (decodeToMaybe =<<)

putCoin :: MonadResource m => DBCoin -> WalletDB m ()
putCoin coin = do
    dbPut key $ encode' coin 
    dbPut ("coinoutpoint_" ++ opKey) $ stringToBS key
    where apos  = encodeInt $ coinAccPos coin
          pos   = encodeInt $ coinPos coin
          key   = "coin_" ++ apos ++ "_" ++ pos
          opKey = bsToString $ encode' $ coinOutPoint coin

importTx :: MonadResource m => Tx -> WalletDB m [DBCoin]
importTx tx = do
    coins <- forM (zip (txOut tx) [0..]) $ importCoin id
    return $ catMaybes coins
    where id = txid tx

importCoin :: MonadResource m => Hash256 -> (TxOut,Word32) 
             -> WalletDB m (Maybe DBCoin)
importCoin id (txout,i) = do
    addrM <- case decodeOutput $ scriptOutput txout of
        Right (PayPKHash a)     -> getAddr $ AddrBase58 $ addrToBase58 a
        Right (PayScriptHash a) -> getAddr $ AddrBase58 $ addrToBase58 a
        _                       -> return Nothing
    case addrM of
        Just addr -> getAcc (AccPos $ addrAccPos addr) >>= \accM -> case accM of
            Just acc -> getCoin (CoinOutPoint op) >>= \prevM -> case prevM of
                Just _  -> return Nothing -- Coins already stored in db
                Nothing -> do
                    let aData   = runAccData acc
                        coinPos = accCoinCount aData + 1
                        coin    = DBCoin op txout False coinPos $ accPos aData
                    putCoin coin
                    putAcc acc{ runAccData = aData{accCoinCount = coinPos} }
                    return $ Just coin
            _ -> return Nothing
        _ -> return Nothing
    where op  = OutPoint id i

listCoins :: MonadResource m => Int -> WalletDB m [DBCoin]
listCoins pos = (getAcc $ AccPos pos) >>= \accM -> case accM of
    Nothing -> error $ "Invalid account index: " ++ (show pos)
    Just acc -> do
        vals <- dbIter key prefix $ accCoinCount $ runAccData acc
        let res = catMaybes $ map decodeToMaybe vals
        return $ filter (not . coinSpent) res -- only display unspent coins
    where prefix = "coin_" ++ (encodeInt pos) ++ "_"
          key    = prefix ++ (encodeInt 1)

{- Binary Instance -}

instance Binary DBCoin where
    get = DBCoin <$> get 
                 <*> get 
                 <*> get
                 <*> (fromIntegral . getVarInt <$> get)
                 <*> (fromIntegral . getVarInt <$> get)

    put (DBCoin o t s p ap) = do
        put o >> put t >> put s
        put $ VarInt $ fromIntegral p
        put $ VarInt $ fromIntegral ap

