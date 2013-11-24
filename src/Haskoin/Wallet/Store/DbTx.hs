{-# LANGUAGE OverloadedStrings #-}
module Haskoin.Wallet.Store.DbTx
( 
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Data.Time
import Data.Yaml
import Data.Maybe
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Conduit as C

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Haskoin.Wallet.Keys
import Haskoin.Wallet.Manager
import Haskoin.Wallet.TxBuilder
import Haskoin.Wallet.Store.DbAccount
import Haskoin.Wallet.Store.DbCoin
import Haskoin.Wallet.Store.Util
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

cmdImportTx :: PersistQuery m => Tx -> EitherT String m Value
cmdImportTx tx = do
    inActions  <- mapRights dbImportIn  $ txIn tx
    outActions <- mapRights dbImportOut $ txOut tx

dbImportIn :: PersistQuery m 
           => Hash256 -> TxIn -> EitherT String m (DbAccountId,Int,Bool)
dbImportIn txid (TxIn op@(OutPoint h i) s _) = do
    a     <- liftEither $ scriptSender s   
    addr  <- dbGetAddr $ addrToBase58 a
    coinM <- getBy $ CoinOutPoint (fromIntegral h) (fromIntegral i)
    case coinM of
        Just (Entity ci coin) -> do
            if isJust $ dbCoinSpent coin 
                then left "dbImportIn: Coin already spent" 
                else do
                    replace ci coin{ dbCoinSpent = (Just $ txidHex txid) }
                    return (dbCoinAccount coin,dbCoinValue coin,False)
        Nothing -> do
            time  <- liftIO $ getCurrentTime
            insert_ $ DbCoin (txidHex h) (fromIntegral i) 
                             0 "" 
                             (Just $ txidHex txid) 
                             (dbAddressAccount addr) 
                             True time
            return (dbAddressAccount addr,0,True)
    where txidHex = bsToHex . BS.reverse . encode'

dbImportOut :: PersistQuery m
            => Hash256 -> (TxOut,Int) -> EitherT String m (DbAccountId,Int)
dbImportOut txid ((TxOut v s), index) = do
    a    <- liftEither $ scriptRecipient s
    addr <- dbGetAddr $ addrToBase58 a
    coinM <- getBy $ CoinOutPoint (txidHex txid) index
    case coinM of
        Just (Entity ci coin) -> if dbCoinOrphan coin 
            then do
                replace ci coin{ dbCoinOrphan = False
                               , dbCoinValue  = (fromIntegral v)
                               , dbCoinScript = (bsToHex $ encodeScriptOps s)
                               }
                c <- count [ DbCoinSpent   ==. dbCoinSpent coin
                           , DbCoinAccount ==. dbCoinAccount coin
                           , DbCoinOrphan  ==. True
                           ]
                let keyId = fromJust $ dbCoinSpent coin
                    keyTx = UniqueTx keyId $ dbCoinAccount coin
                (Entity ti tx) <- liftMaybe txErr =<< getBy keyTx
                update ti $ if count == 0 
                    then [ DbTxValue +=. (fromIntegral v)
                         , DbTxOrphan =. False
                         ]
                    else [ DbTxValue +=. (fromIntegral v) ]
                return (dbAddressAccount addr, fromIntegral v)
            else left "dbImportOut: Coin already imported"
        Nothing -> do
            time  <- liftIO $ getCurrentTime
            insert_ $ DbCoin (txidHex txid) index 
                             (fromIntegral v)
                             (bsToHex $ encodeScriptOps s)
                             Nothing
                             (dbAddressAccount addr) 
                             False time
            return (dbAddressAccount addr, fromIntegral v)
    where txidHex = bsToHex . BS.reverse . encode'
          txErr = "dbImportOut: Orphan coin transaction not found"

-- |Extract coins from a transaction and save them in the database
dbImportTx :: MonadResource m => Tx -> WalletDB m [DBCoin]
dbImportTx tx = do
    newCoins   <- mapRights (dbImportCoin $ txid tx) $ zip (txOut tx) [0..]
    spentCoins <- mapRights dbSpendCoin $ txIn tx
    let xs = nubBy f $ newCoins ++ spentCoins
    forM xs $ \(DBCoin _ _ _ _ acPos) -> do
        acc <- dbGetAcc $ AccPos $ coinAccPos coin
        let aData = runAccData acc
            total = accTxCount aData
            id    = bsToString $ encode' $ txid tx
        acPos <- liftEither $ dbEncodeInt $ accPos aData
        exists <- dbExists $ concat ["txid_",acPos,"_",id]
        unless exists $ do
            dbPutTx $ DBTx tx (total + 1) $ accPos aData
            dbPutAcc acc{ runAccData = aData{ accTxCount = total + 1 } }
    return newCoins
    where f a b = (coinAccPos a) == (coinAccPos b)

getCoinNetAmnt :: [DBCoin] -> [DBCoin] -> (Word64,Bool)
getCoinNetAmnt new spent = 
    where credit = sum $ map (outValue . coinTxOut) new
          debit  = sum $ map (outValue . coinTxOut) spent


decodeIncTx :: Tx -> DBAccount -> WalletDB ResIO ([String],Word64)
decodeIncTx tx acc = do
    incSum <- sum <$> (mapRights f $ txOut tx)
    senders <- mapRights g $ txIn tx
    return (senders,incSum)
    where f (TxOut v s) = do
            str  <- liftEither $ scriptRecipient s
            addr <- dbGetAddr $ AddrBase58 str
            unless (addrAccPos addr == (accPos $ runAccData acc)) 
                left "decodeIncTx: Addr not in current account"
            return v
          g (TxIn _ s _) = liftEither $ scriptSender s 

decodeOutTx :: Tx -> DBAccount -> WalletDB ResIO ([String],Word64)
decodeOutTx tx acc = do
    myCoins <- mapRights f $ txIn tx
    if null myCoins then return [] else mapRights g $ txOut tx 
    where f (TxIn op s _) = do
            coin <- dbGetCoin $ CoinOutPoint op
            unless (coinAccPos op == (accPos $ runAccData acc))
                left "decodeOutTx: Coin not in current account"
            return coin
          g (TxOut v s) = do
            str <- liftEither $ scriptRecipient s
            return (str,v)

cmdListTx :: AccountName -> Command
cmdListTx name = dbGetAcc (AccName name) >>= \acc -> do
    guardValidAcc acc
    txs <- dbTxList $ accPos $ runAccData acc 
    forM txs $ \tx -> do
        coins <- mapRights (dbGetCoin . CoinOutPoint . prevOutPoint) $ txIn tx
        addr  <- mapRights (dbGetAddr . AddrBase58 =<<) . f $ txOut tx
        let outTotal = sum $ map (outValue . coinTxOut) coins
            outSum   = 
    return $ toJSON $ map (toJSON . dbTx) txs
    where f = liftEither . scriptRecipient . scriptOutput

cmdSend :: String -> Int -> AccountName -> Command
cmdSend a v name = dbGetAcc (AccName name) >>= \acc -> do
    guardValidAcc acc
    unspent <- dbCoinList $ accPos $ runAccData acc
    (coins,change) <- liftEither $ if isMSAcc acc
        then let msParam = (msReq acc,length $ msKeys acc)
             in chooseMSCoins (fromIntegral v) 10000 msParam unspent
        else chooseCoins (fromIntegral v) 10000 unspent
    recipients <- if change < 5000 then return [(a,fromIntegral v)] else do
        cAddr <- dbGenAddr (accPos $ runAccData acc) 1 True
        return $ [(a,fromIntegral v),(addrBase58 $ head cAddr,change)]
    tx <- liftEither $ buildAddrTx (map coinOutPoint coins) recipients
    ys <- mapM f coins
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
    bsTx <- liftEither $ buildToEither sigTx
    return $ object [ (T.pack "Payment Tx") .= (toJSON $ bsToHex $ encode' bsTx)
                    , (T.pack "Complete") .= isComplete sigTx
                    ]
    where f c = let s = scriptOutput $ coinTxOut c
                in dbGetSigData s (coinOutPoint c) $ SigAll False

cmdSignTx :: String -> AccountName -> Command
cmdSignTx str name = dbGetAcc (AccName name) >>= \acc -> do
    guardValidAcc acc
    tx      <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS str)
    txCoins <- mapM (dbGetCoin . CoinOutPoint) $ map prevOutput $ txIn tx
    let pos     = accPos $ runAccData acc
        -- For security, only sign this accounts coins
        myCoins = filter ((== pos) . coinAccPos) txCoins
    ys      <- mapM f myCoins
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
    bsTx <- liftEither $ buildToEither sigTx
    return $ object [ (T.pack "Tx") .= (toJSON $ bsToHex $ encode' bsTx)
                    , (T.pack "Complete") .= isComplete sigTx
                    ]
    where txErr = "cmdSignTx: Could not decode transaction"
          f c   = let s = scriptOutput $ coinTxOut c
                  in dbGetSigData s (coinOutPoint c) $ SigAll False

cmdImportTx :: String -> Command
cmdImportTx str = do
    tx    <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS str)
    coins <- dbImportTx tx
    accs  <- mapM (dbGetAcc . AccPos . coinAccPos) coins
    let json = map (\(c,a) -> yamlCoin c a) $ zip coins accs
    return $ object
        [ (T.pack "Import count") .= (toJSON $ length coins) 
        , (T.pack "Imported coins") .= toJSON json
        ]
    where txErr = "cmdImportTx: Could not decode transaction"
      
cmdDecodeTx :: String -> Command
cmdDecodeTx str = do
    tx <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS str)
    return $ toJSON (tx :: Tx)
    where txErr = "cmdDecodeTx: Could not decode transaction"

cmdBuildRawTx :: [(String,Int)] -> [(String,Int)] -> Command
cmdBuildRawTx os as = do
    ops <- mapM f os
    tx  <- liftEither $ buildAddrTx ops $ map (\(a,v) -> (a,fromIntegral v)) as
    return $ object [ (T.pack "Tx") .= (bsToHex $ encode' tx) ]
    where f (t,i) = do
            tid <- liftMaybe tidErr $ (decodeToMaybe . BS.reverse) =<< hexToBS t
            return $ OutPoint tid $ fromIntegral i
          tidErr  = "cmdBuildTx: Could not decode outpoint txid"

cmdSignRawTx :: String -> [(String,Int,String)] -> SigHash -> Command
cmdSignRawTx strTx xs sh = do
    tx <- liftMaybe txErr $ decodeToMaybe =<< (hexToBS strTx)
    ys <- mapRights f xs
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
    bsTx <- liftEither $ buildToEither sigTx
    return $ object [ (T.pack "Tx") .= (toJSON $ bsToHex $ encode' bsTx)
                    , (T.pack "Complete") .= isComplete sigTx
                    ]
    where f (t,i,s) = do
            sBS <- liftMaybe "Invalid script HEX encoding" $ hexToBS s
            tBS <- liftMaybe "Invalid txid HEX encoding" $ hexToBS t
            scp <- liftEither $ decodeScriptOps sBS
            tid <- liftEither $ decodeToEither $ BS.reverse tBS
            dbGetSigData scp (OutPoint tid $ fromIntegral i) sh
          txErr = "cmdSignTx: Could not decode transaction"

