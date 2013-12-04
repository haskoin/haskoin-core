{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Haskoin.Wallet.Store.DbAccount 
( dbGetAcc
, dbNewAcc
, dbNewMS
, dbAddKeys
, cmdAccInfo
, cmdListAcc
, cmdDumpKeys
, yamlAcc
, isMSAcc
) where

import Control.Applicative
import Control.Monad
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
import Haskoin.Wallet.Store.Util
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

yamlAcc :: DbAccountGeneric b -> Value
yamlAcc acc = object $ concat
    [ [ "Name" .= dbAccountName acc
      , "Tree" .= dbAccountTree acc
      ]
    , datType, datWarn
    ]
    where msReq = fromJust $ dbAccountMsRequired acc
          msTot = fromJust $ dbAccountMsTotal acc
          ms    = unwords [show msReq,"of",show msTot]
          miss  = msTot - length (dbAccountMsKeys acc) - 1
          datType | isMSAcc acc = ["Type" .= unwords [ "Multisig", ms ]]
                  | otherwise   = ["Type" .= ("Regular" :: String)]
          datWarn | isMSAcc acc && miss > 0 =
                      [ (T.pack "Warning") .= 
                          unwords [show miss,"multisig keys missing"]
                      ]
                  | otherwise = []

isMSAcc :: DbAccountGeneric b -> Bool
isMSAcc acc = (isJust $ dbAccountMsRequired acc) && 
              (isJust $ dbAccountMsTotal acc) 

dbGetAcc :: ( PersistUnique m 
            , PersistMonadBackend m ~ b
            )
         => String 
         -> EitherT String m (Entity (DbAccountGeneric b))
dbGetAcc name = liftMaybe accErr =<< (getBy $ UniqueAccName name)
  where 
    accErr = unwords ["dbGetAcc: Invalid account", name]

dbNewAcc :: ( PersistUnique m
            , PersistQuery m
            , PersistMonadBackend m ~ b
            ) 
         => String -> EitherT String m (DbAccountGeneric b)
dbNewAcc name = do
    time <- liftIO getCurrentTime
    (Entity wk w) <- dbGetWallet "main"
    let keyM = loadMasterKey =<< (xPrvImport $ dbWalletMaster w)
    master <- liftMaybe keyErr keyM
    let deriv = fromIntegral $ dbWalletAccIndex w + 1
        (k,i) = head $ accPubKeys master deriv
        acc   = DbAccount name 
                          (fromIntegral i) 
                          (concat ["m/",show i,"'/"])
                          (xPubExport $ runAccPubKey k)
                          (-1) (-1) (-1) (-1)
                          Nothing Nothing [] wk time
    insert_ acc
    update wk [DbWalletAccIndex =. fromIntegral i]
    return acc
  where 
    keyErr = "dbNewAcc: Could not load master key"

dbNewMS :: ( PersistUnique m
           , PersistQuery m
           , PersistMonadBackend m ~ b
           )
        => String -> Int -> Int -> [XPubKey]
        -> EitherT String m (DbAccountGeneric b)
dbNewMS name m n mskeys = do
    time <- liftIO getCurrentTime
    let keys = nub mskeys
    unless (n >= 1 && n <= 16 && m >= 1 && m <= n) $ left
        "cmdNewMS: Invalid multisig parameters"
    unless (length keys < n) $ left 
        "cmdNewMS: Too many keys"
    (Entity wk w) <- dbGetWallet "main"
    let keyM = loadMasterKey =<< (xPrvImport $ dbWalletMaster w)
    master <- liftMaybe keyErr keyM
    let deriv = fromIntegral $ dbWalletAccIndex w + 1
        (k,i) = head $ accPubKeys master deriv
        acc   = DbAccount name 
                          (fromIntegral i) 
                          (concat ["m/",show i,"'/"])
                          (xPubExport $ runAccPubKey k)
                          (-1) (-1) (-1) (-1) 
                          (Just m) (Just n) 
                          (map xPubExport keys)
                          wk time
    insert_ acc
    update wk [DbWalletAccIndex =. fromIntegral i]
    return acc
  where 
    keyErr = "dbNewMS: Could not load master key"

dbAddKeys :: ( PersistStore m
             , PersistUnique m
             , PersistQuery m
             , PersistMonadBackend m ~ b
             )
          => AccountName -> [XPubKey] 
          -> EitherT String m (DbAccountGeneric b)
dbAddKeys name keys 
    | null keys = left "dbAddKeys: Keys can not be empty"
    | otherwise = do
        (Entity ai acc) <- dbGetAcc name
        unless (isMSAcc acc) $ left $ 
            "dbAddKeys: Can only add keys to a multisig account"
        exists <- mapM (\x -> count [DbAccountKey ==. (xPubExport x)]) keys
        unless (sum exists == 0) $ left $
            "dbAddKeys: Can not add your own keys to a multisig account"
        prevKeys <- liftMaybe keyErr $ mapM xPubImport $ dbAccountMsKeys acc
        when (length prevKeys == (fromJust $ dbAccountMsTotal acc) - 1) $ left $
            "dbAddKeys: Account is complete. No more keys can be added"
        let newKeys = nub $ prevKeys ++ keys
            newAcc  = acc{ dbAccountMsKeys = map xPubExport newKeys }
        unless (length newKeys < (fromJust $ dbAccountMsTotal acc)) $ left $
            "dbAddKeys: Too many keys"
        replace ai newAcc
        return newAcc
  where 
    keyErr = "dbAddKeys: Invalid keys found in account"

cmdAccInfo :: PersistUnique m => AccountName -> EitherT String m Value
cmdAccInfo name = yamlAcc . entityVal <$> dbGetAcc name

cmdListAcc :: PersistQuery m => EitherT String m Value
cmdListAcc = toJSON . (map (yamlAcc . entityVal)) <$> selectList [] []

cmdDumpKeys :: (PersistStore m, PersistUnique m) 
            => AccountName -> EitherT String m Value
cmdDumpKeys name = do
    (Entity _ acc) <- dbGetAcc name
    w <- liftMaybe walErr =<< (get $ dbAccountWallet acc)
    let keyM = loadMasterKey =<< (xPrvImport $ dbWalletMaster w)
    master <- liftMaybe keyErr keyM
    prv <- liftMaybe prvErr $ 
        accPrvKey master (fromIntegral $ dbAccountIndex acc)
    let prvKey = runAccPrvKey prv
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

