{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Haskoin.Wallet.Store.DbTx
( cmdImportTx 
, dbImportTx
, cmdListTx
, cmdSend
, cmdSendMany
, dbSendTx
, dbSendSolution
, dbSendCoins
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Data.Time
import Data.Yaml
import Data.Word
import Data.List
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
               , PersistMonadBackend m ~ b
               , b ~ SqlBackend
               )
            => Tx -> EitherT String m Value
cmdImportTx tx = do
    accTx <- dbImportTx tx
    return $ toJSON $ map yamlTx $ sortBy f accTx
  where
    f a b = (dbTxCreated a) `compare` (dbTxCreated b)

dbImportTx :: ( PersistStore m
              , PersistQuery m 
              , PersistUnique m
              , PersistMonadBackend m ~ b
              , b ~ SqlBackend
              )
           => Tx -> EitherT String m [DbTxGeneric b]
dbImportTx tx = do
    inActions  <- mapRights (dbImportIn $ txid tx) $ txIn tx
    outActions <- mapRights (dbImportOut $ txid tx) $ zip (txOut tx) [0..]
    let inMap  = foldl fin M.empty inActions
        accMap = foldl fout inMap outActions
    accTx <- mapM build $ M.toList accMap
    -- insert account transactions into database, rewriting if they exist
    forM accTx $ \tx -> do
        prev <- getBy $ UniqueTx (dbTxTxid tx) (dbTxAccount tx)
        if isNothing prev 
            then insert_ tx 
            else replace (entityKey $ fromJust prev) tx
    -- insertUnique to ignore errors when duplicates detected (normal)
    time <- liftIO $ getCurrentTime
    insertUnique $ DbTxBlob (txidHex $ txid tx) (encode' tx) time
    -- Re-import transactions spending which are spending this one
    let txids = nub $ catMaybes $ map (\(_,_,_,x) -> x) outActions
    txBlobs <- mapM dbGetTxBlob txids
    let f = liftEither . decodeToEither . dbTxBlobValue . entityVal
    reImported <- forM txBlobs $ (dbImportTx =<<) . f
    return $ accTx ++ (concat reImported)
  where 
    fin acc (ai,vi,o) = flip (M.insert ai) acc $ case M.lookup ai acc of
        Just (vi',_,o',_) -> (vi+vi',0,o || o',[])
        _                 -> (vi,0,o,[])
    fout acc (ai,vo,addr,_) = flip (M.insert ai) acc $ case M.lookup ai acc of
        Just (vi,vo',o,xs) -> (vi,vo+vo',o,xs ++ [addr])
        _                  -> (0,vo,False,[addr])
    allRecip  = rights $ map toAddr $ txOut tx
    toAddr = (addrToBase58 <$>) . scriptRecipient . scriptOutput
    build (ai,(vi,vo,orphan,xs)) = do
        time  <- liftIO $ getCurrentTime
        let tot   | orphan    = 0
                  | otherwise = vo - vi
            recip | null xs   = allRecip
                  | orphan    = allRecip \\ xs -- Most likely an outgoing tx
                  | tot < 0   = allRecip \\ xs -- Remove the change
                  | otherwise = xs
        return $ DbTx (txidHex $ txid tx) recip tot ai orphan time

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
            time <- liftIO $ getCurrentTime
            rdm  <- dbGetRedeem addr
            insert_ $ DbCoin (txidHex h) (fromIntegral i) 
                             0 "" rdm
                             (addrToBase58 a)
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
            -> EitherT String m (DbAccountId,Int,String,Maybe String)
dbImportOut txid ((TxOut v s), index) = do
    a <- liftEither $ scriptRecipient s
    (Entity _ addr) <- dbGetAddr $ addrToBase58 a
    coinM <- getBy $ CoinOutPoint (txidHex txid) index
    toImport <- case coinM of
        Just (Entity ci coin) -> do
            replace ci coin{ dbCoinOrphan = False
                           , dbCoinValue  = (fromIntegral v)
                           , dbCoinScript = (bsToHex $ encodeScriptOps s)
                           }
            return $ dbCoinSpent coin
        Nothing -> do
            time <- liftIO $ getCurrentTime
            rdm  <- dbGetRedeem addr
            insert_ $ DbCoin (txidHex txid) index 
                             (fromIntegral v)
                             (bsToHex $ encodeScriptOps s)
                             rdm (addrToBase58 a) Nothing
                             (dbAddressAccount addr) 
                             False time
            dbAdjustGap addr -- Generate addresses if addr is within the gap
            return Nothing
    return (dbAddressAccount addr, fromIntegral v, addrToBase58 a, toImport)

dbGetRedeem :: ( PersistStore m
               , PersistQuery m 
               , PersistUnique m
               , PersistMonadBackend m ~ b
               , b ~ SqlBackend
               ) 
            => DbAddressGeneric b
            -> EitherT String m (Maybe String)
dbGetRedeem addr = do
    acc <- liftMaybe accErr =<< (get $ dbAddressAccount addr)
    rdm <- if isMSAcc acc 
        then Just <$> liftMaybe rdmErr (getRdm acc)
        else return Nothing
    return $ bsToHex . encodeScriptOps . encodeOutput <$> rdm
  where
    getRdm acc = do
        key      <- loadPubAcc =<< (xPubImport $ dbAccountKey acc)
        msKeys   <- mapM xPubImport $ dbAccountMsKeys acc
        addrKeys <- f key msKeys $ fromIntegral $ dbAddressIndex addr
        let pks = map (xPubKey . runAddrPubKey) addrKeys
        sortMulSig . (PayMulSig pks) <$> dbAccountMsRequired acc
      where
        f = if dbAddressInternal addr then intMulSigKey else extMulSigKey
    accErr = "dbImportOut: Invalid address account"
    rdmErr = "dbGetRedeem: Could not generate redeem script"

cmdListTx :: ( PersistQuery m 
             , PersistUnique m
             )
          => AccountName -> EitherT String m Value
cmdListTx name = do
    (Entity ai acc) <- dbGetAcc name
    txs <- selectList [ DbTxAccount ==. ai
                      ] 
                      [ Asc DbTxCreated ]
    return $ toJSON $ map (yamlTx . entityVal) txs

cmdSend :: ( PersistStore m
           , PersistQuery m 
           , PersistUnique m
           , PersistMonadBackend m ~ SqlBackend
           )
        => AccountName -> String -> Int -> Int
        -> EitherT String m Value
cmdSend name a v fee = do
    (tx,complete) <- dbSendTx name [(a,fromIntegral v)] (fromIntegral fee)
    return $ object [ "Payment Tx" .= (toJSON $ bsToHex $ encode' tx)
                    , "Complete"   .= complete
                    ]

cmdSendMany :: ( PersistStore m
               , PersistQuery m 
               , PersistUnique m
               , PersistMonadBackend m ~ SqlBackend
               )
            => AccountName -> [(String,Int)] -> Int
            -> EitherT String m Value
cmdSendMany name dests fee = do
    (tx,complete) <- dbSendTx name dests' (fromIntegral fee)
    return $ object [ "Payment Tx" .= (toJSON $ bsToHex $ encode' tx)
                    , "Complete"   .= complete
                    ]
    where dests' = map (\(a,b) -> (a,fromIntegral b)) dests

dbSendTx :: ( PersistStore m
            , PersistQuery m 
            , PersistUnique m
            , PersistMonadBackend m ~ SqlBackend
            )
         => AccountName -> [(String,Word64)] -> Word64
         -> EitherT String m (Tx, Bool)
dbSendTx name dests fee = do
    (coins,recip) <- dbSendSolution name dests fee
    dbSendCoins coins recip (SigAll False)

dbSendSolution :: ( PersistStore m
                  , PersistQuery m 
                  , PersistUnique m
                  , PersistMonadBackend m ~ SqlBackend
                  )
               => AccountName -> [(String,Word64)] -> Word64
               -> EitherT String m ([Coin],[(String,Word64)])
dbSendSolution name dests fee = do
    (Entity ai acc) <- dbGetAcc name
    unspent <- liftEither . (mapM toCoin) =<< dbCoins ai
    (coins,change) <- liftEither $ if isMSAcc acc
        then let msParam = ( fromJust $ dbAccountMsRequired acc
                           , fromJust $ dbAccountMsTotal acc
                           )
             in chooseMSCoins tot fee msParam unspent
        else chooseCoins tot fee unspent
    recip <- if change < 5000 then return dests else do
        cAddr <- dbGenIntAddrs name 1
        return $ dests ++ [(dbAddressBase58 $ head cAddr,change)]
    return (coins,recip)
  where
    tot = sum $ map snd dests
    
dbSendCoins :: ( PersistStore m
               , PersistQuery m 
               , PersistUnique m
               )
            => [Coin] -> [(String,Word64)] -> SigHash
            -> EitherT String m (Tx, Bool)
dbSendCoins coins recipients sh = do
    tx <- liftEither $ buildAddrTx (map coinOutPoint coins) recipients
    ys <- mapM (dbGetSigData sh) coins
    let sigTx = detSignTx tx (map fst ys) (map snd ys)
    bsTx <- liftEither $ buildToEither sigTx
    return (bsTx, isComplete sigTx)

dbGetSigData :: ( PersistStore m
                , PersistUnique m
                , PersistQuery m
                )
             => SigHash -> Coin -> EitherT String m (SigInput,PrvKey)
dbGetSigData sh coin = do
    (Entity _ w) <- dbGetWallet "main"
    mst <- liftMaybe mstErr $ loadMasterKey =<< xPrvImport (dbWalletMaster w)
    a   <- liftEither $ scriptRecipient out
    (Entity _ addr) <- dbGetAddr $ addrToBase58 a
    acc  <- liftMaybe accErr =<< get (dbAddressAccount addr)
    aKey <- liftMaybe prvErr $ accPrvKey mst $ fromIntegral $ dbAccountIndex acc
    let g = if dbAddressInternal addr then intPrvKey else extPrvKey
    sigKey <- liftMaybe addErr $ g aKey $ fromIntegral $ dbAddressIndex addr
    return (sigi, xPrvKey $ runAddrPrvKey sigKey)
  where
    out    = scriptOutput $ coinTxOut coin
    rdm    = coinRedeem coin
    sigi | isJust rdm = SigInputSH out (coinOutPoint coin) (fromJust rdm) sh
         | otherwise  = SigInput out (coinOutPoint coin) sh
    mstErr = "dbGetSigData: Could not load master key"
    accErr = "dbGetSigData: Could not load address account"
    prvErr = "dbGetSigData: Invalid account derivation index"
    addErr = "dbGetSigData: Invalid address derivation index"

{-
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

