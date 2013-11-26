{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
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
import Data.Word
import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as M
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
import Haskoin.Wallet.Store.DbAddress
import Haskoin.Wallet.Store.DbCoin
import Haskoin.Wallet.Store.Util
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

yamlTx :: DbTxGeneric b -> Value
yamlTx tx = object
    [ "Recipients" .= dbTxRecipients tx
    , "Value" .= dbTxValue tx
    , "Orphan" .= dbTxOrphan tx
    ]

txidHex :: Hash256 -> String
txidHex = bsToHex . BS.reverse . encode'

cmdImportTx :: ( PersistStore m
               , PersistQuery m 
               , PersistUnique m
               , PersistMonadBackend m ~ SqlBackend
               )
            => Tx -> EitherT String m Value
cmdImportTx tx = do
    inActions  <- mapRights (dbImportIn $ txid tx) $ txIn tx
    outActions <- mapRights (dbImportOut $ txid tx) $ zip (txOut tx) [0..]
    let inMap  = foldl fin M.empty inActions
        accMap = foldl fout inMap outActions
    accTx <- mapM build $ M.toList accMap
    insertMany accTx
    return $ toJSON $ map yamlTx accTx
    where fin acc (ai,vi,o) = flip (M.insert ai) acc $ case M.lookup ai acc of
              Just (vi',_,o',_) -> (vi+vi',0,o || o',[])
              _                 -> (vi,0,o,[])
          fout acc (ai,vo,addr) = flip (M.insert ai) acc $ case M.lookup ai acc of
              Just (vi,vo',o,xs) -> (vi,vo+vo',o,addr:xs)
              _                  -> (0,vo,False,[addr])
          recip  = rights $ map toAddr $ txOut tx
          toAddr = (addrToBase58 <$>) . scriptRecipient . scriptOutput
          build (ai,(vi,vo,orphan,xs)) = do
              time  <- liftIO $ getCurrentTime
              return $ DbTx (txidHex $ txid tx)
                            (if null xs then recip else xs)
                            (vo-vi) ai orphan time

dbImportIn :: ( PersistStore m
              , PersistQuery m 
              , PersistUnique m
              , PersistMonadBackend m ~ SqlBackend
              )
           => Hash256 -> TxIn -> EitherT String m (DbAccountId,Int,Bool)
dbImportIn txid (TxIn op@(OutPoint h i) s _) = do
    a <- liftEither $ scriptSender s   
    (Entity _ addr) <- dbGetAddr $ addrToBase58 a
    coinM <- getBy $ CoinOutPoint (txidHex h) (fromIntegral i)
    case coinM of
        Just (Entity ci coin) -> do
            update ci [ DbCoinSpent =. (Just $ txidHex txid) ]
            return (dbCoinAccount coin,dbCoinValue coin,dbCoinOrphan coin)
        Nothing -> do
            time  <- liftIO $ getCurrentTime
            insert_ $ DbCoin (txidHex h) (fromIntegral i) 
                             0 "" 
                             (Just $ txidHex txid) 
                             (dbAddressAccount addr) 
                             True time
            return (dbAddressAccount addr,0,True)

dbImportOut :: ( PersistStore m
               , PersistQuery m 
               , PersistUnique m
               , PersistMonadBackend m ~ SqlBackend
               )
            => Hash256 -> (TxOut,Int) 
            -> EitherT String m (DbAccountId,Int,String)
dbImportOut txid ((TxOut v s), index) = do
    a <- liftEither $ scriptRecipient s
    (Entity _ addr) <- dbGetAddr $ addrToBase58 a
    coinM <- getBy $ CoinOutPoint (txidHex txid) index
    case coinM of
        Just (Entity ci coin) -> 
            when (dbCoinOrphan coin) $ dbProcessOrphan v s ci coin
        Nothing -> do
            time  <- liftIO $ getCurrentTime
            insert_ $ DbCoin (txidHex txid) index 
                             (fromIntegral v)
                             (bsToHex $ encodeScriptOps s)
                             Nothing
                             (dbAddressAccount addr) 
                             False time
    return (dbAddressAccount addr, fromIntegral v, addrToBase58 a)

dbProcessOrphan :: ( PersistStore m
                   , PersistQuery m 
                   , PersistUnique m
                   , PersistMonadBackend m ~ SqlBackend
                   )
                => Word64 -> Script
                -> DbCoinId -> DbCoinGeneric SqlBackend
                -> EitherT String m ()
dbProcessOrphan v s ci coin = do
    replace ci coin{ dbCoinOrphan = False
                   , dbCoinValue  = (fromIntegral v)
                   , dbCoinScript = (bsToHex $ encodeScriptOps s)
                   }
    c <- count [ DbCoinSpent   ==. dbCoinSpent coin
               , DbCoinAccount ==. dbCoinAccount coin
               , DbCoinOrphan  ==. True
               ]
    let keyTx = UniqueTx (fromJust $ dbCoinSpent coin) (dbCoinAccount coin)
    (Entity ti tx) <- liftMaybe txErr =<< getBy keyTx
    update ti $ concat
        [ [DbTxValue +=. (fromIntegral v)]
        , if c == 0 then [DbTxOrphan =. False] else []
        ]
  where 
    txErr = "dbImportOut: Orphan coin transaction not found"
    
{-
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
-}

