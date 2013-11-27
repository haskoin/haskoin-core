{-# LANGUAGE OverloadedStrings #-}
module Haskoin.Wallet.Store.Units (tests) where

import System.IO.Error

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Exception (tryJust)

import Data.Maybe
import Data.Yaml
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Database.Persist
import Database.Persist.Sqlite

import Haskoin.Wallet
import Haskoin.Wallet.Store
import Haskoin.Util

tests =
    [ testGroup "Wallet persistence tests" 
        [ testCase "Wallet tests" runTests
        ] 
    ]

runTests :: Assertion
runTests = do
    runSqlite ":memory:" $ runEitherT $ do
        lift $ runMigration migrateAll
        liftIO . (assertBool "Pre init") . isRight =<< runEitherT testPreInit
        liftIO . (assertBool "Init") . isRight =<< runEitherT testInit
        liftIO . (assertBool "New Acc") . isRight =<< runEitherT testNewAcc
        liftIO . (assertBool "New MS") . isRight =<< runEitherT testNewMS
    return ()

testPreInit :: (PersistStore m, PersistUnique m, PersistQuery m) 
            => EitherT String m ()
testPreInit = do 
    res1 <- runEitherT $ cmdNewAcc "default"
    res2 <- runEitherT $ cmdList "default" 0 1
    res3 <- runEitherT $ cmdList "default" (-1) 1
    res4 <- runEitherT $ cmdList "default" 0 0
    liftIO $ do
        assertEqual "Invalid wallet" 
            (Left "dbGetWallet: Invalid wallet main") res1
        assertEqual "Invalid account" 
            (Left "dbGetAcc: Invalid account default") res2
        assertEqual "Invalid page number" 
            (Left "cmdList: Invalid page number -1") res3
        assertEqual "Invalid results per page" 
            (Left "cmdList: Invalid results per page 0") res4
    return ()

testInit :: (PersistStore m, PersistUnique m, PersistQuery m) 
         => EitherT String m ()
testInit = do 
    res1 <- runEitherT $ cmdInit ""
    liftIO $ assertEqual "Init empty seed" 
        (Left "cmdInit: seed can not be empty") res1
    cmdInit "Hello World" 
    walletE <- getBy $ UniqueWalletName "main"
    liftIO $ assertBool "Wallet init"$ isJust walletE
    let (Entity _ w) = fromJust walletE
        key          = fromJust $ makeMasterKey $ stringToBS "Hello World"
        keystr       = xPrvExport $ runMasterKey key
    liftIO $ do
        assertEqual "Wallet master key"  keystr $ dbWalletMaster w
        assertEqual "Wallet name" "main" $ dbWalletName w
    res2 <- runEitherT $ cmdGenAddr "default" 5
    res3 <- count ([] :: [Filter (DbAddressGeneric b)])
    liftIO $ do
        assertEqual "Invalid account" 
            (Left "dbGetAcc: Invalid account default") res2
        assertEqual "Address count" 0 res3

testNewAcc :: (PersistStore m, PersistUnique m, PersistQuery m) 
           => EitherT String m ()
testNewAcc = do 
    res1 <- cmdNewAcc ""
    res2 <- cmdNewAcc "acc1"
    res3 <- cmdNewAcc "acc2"
    res4 <- cmdListAcc
    res5 <- cmdDumpKeys "acc1"
    res6 <- cmdAccInfo "acc2"
    res7 <- count ([] :: [Filter (DbAccountGeneric b)])
    let val1 = object [ "Name" .= T.pack ""
                      , "Tree" .= T.pack "m/0'/"
                      , "Type" .= T.pack "Regular"
                      ] 
        val2 = object [ "Name" .= T.pack "acc1"
                      , "Tree" .= T.pack "m/1'/"
                      , "Type" .= T.pack "Regular"
                      ] 
        val3 = object [ "Name" .= T.pack "acc2"
                      , "Tree" .= T.pack "m/2'/"
                      , "Type" .= T.pack "Regular"
                      ] 
        val4 = toJSON $ [val1,val2,val3]
        mstKey = fromJust $ makeMasterKey $ stringToBS "Hello World"
        prvKey = runAccPrvKey $ fromJust $ accPrvKey mstKey 1
        pubKey = deriveXPubKey prvKey
        val5 = object [ "Account" .= val2
                      , "PubKey"  .= xPubExport pubKey
                      , "PrvKey"  .= xPrvExport prvKey
                      ]
        val6 = val3
    liftIO $ do
        assertEqual "New acc 1" val1 res1
        assertEqual "New acc 2" val2 res2
        assertEqual "New acc 3" val3 res3
        assertEqual "List accs" val4 res4
        assertEqual "Dump keys" val5 res5
        assertEqual "Acc info"  val6 res6
        assertEqual "Acc count" 3 res7

testNewMS :: (PersistStore m, PersistUnique m, PersistQuery m) 
          => EitherT String m ()
testNewMS = do 
    res1 <- cmdNewMS "ms1" 2 3 []
    res2 <- cmdAccInfo "ms1"
    res3 <- count ([] :: [Filter (DbAccountGeneric b)])
    let val1 = object [ "Name" .= T.pack "ms1"
                      , "Tree" .= T.pack "m/3'/"
                      , "Type" .= T.pack "Multisig 2 of 3"
                      , "Warning" .= T.pack "2 multisig keys missing"
                      ] 
        val2 = val1
        mstKey = fromJust $ makeMasterKey $ stringToBS "Hello World"
        mstKey2 = fromJust $ makeMasterKey $ stringToBS "Hello World 2"
        prvs = map (runAccPrvKey . fst) $ accPrvKeys mstKey 0
        pubs = map deriveXPubKey prvs
        prvs2 = map (runAccPrvKey . fst) $ accPrvKeys mstKey2 0
        pubs2 = map deriveXPubKey prvs2
    res4 <- runEitherT $ cmdAddKeys "ms1" []
    res5 <- runEitherT $ cmdAddKeys "acc1" [pubs !! 0]
    res6 <- runEitherT $ cmdAddKeys "ms1" [pubs !! 0]
    res7 <- runEitherT $ cmdAddKeys "ms1" [pubs2 !! 0,pubs !! 1,pubs2 !! 1]
    res8 <- runEitherT $ cmdAddKeys "ms1" $ take 4 pubs2
    res9 <- cmdAccInfo "ms1"
    res10 <- cmdAddKeys "ms1" [pubs2 !! 0]
    res11 <- cmdDumpKeys "ms1"
    res12 <- cmdAddKeys "ms1" [pubs2 !! 1]
    res13 <- cmdDumpKeys "ms1"
    let val9  = val1
        val10 = object [ "Name" .= T.pack "ms1"
                       , "Tree" .= T.pack "m/3'/"
                       , "Type" .= T.pack "Multisig 2 of 3"
                       , "Warning" .= T.pack "1 multisig keys missing"
                       ] 
        val11 = object [ "Account" .= val10
                       , "PubKey"  .= (xPubExport $ pubs !! 3)
                       , "PrvKey"  .= (xPrvExport $ prvs !! 3)
                       , "MSKeys"  .= toJSON [xPubExport $ pubs2 !! 0]
                       ]
        val12 = object [ "Name" .= T.pack "ms1"
                       , "Tree" .= T.pack "m/3'/"
                       , "Type" .= T.pack "Multisig 2 of 3"
                       ] 
        val13 = object [ "Account" .= val12
                       , "PubKey"  .= (xPubExport $ pubs !! 3)
                       , "PrvKey"  .= (xPrvExport $ prvs !! 3)
                       , "MSKeys"  .= toJSON [ xPubExport $ pubs2 !! 0
                                             , xPubExport $ pubs2 !! 1
                                             ]
                       ]
    liftIO $ do
        assertEqual "New ms 1" val1 res1
        assertEqual "MS info" val2 res2
        assertEqual "MS count" 4 res3
        assertEqual "Empty addKey" 
            (Left "cmdAddKeys: Keys can not be empty") res4
        assertEqual "Invalid addKey 1" 
            (Left "cmdAddKeys: Not a multisig account") res5
        assertEqual "Invalid addKey 2" 
            (Left "cmdAddKeys: Can not add your own keys") res6
        assertEqual "Invalid addKey 3" 
            (Left "cmdAddKeys: Can not add your own keys") res7
        assertEqual "Invalid addKey 4" 
            (Left "cmdAddKeys: Too many keys") res8
        assertEqual "MS info 2" val9 res9
        assertEqual "MS addkey 1" val10 res10
        assertEqual "MS dumpkey 2" val11 res11
        assertEqual "MS addkey 2" val12 res12
        assertEqual "MS dumpkey 2" val13 res13


