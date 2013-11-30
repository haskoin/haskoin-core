{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
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
import Data.Yaml as YAML
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Database.Persist
import Database.Persist.Sqlite

import Haskoin.Wallet
import Haskoin.Wallet.Store
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
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
        liftIO . (assertBool "Gen Addr") . isRight =<< runEitherT testGenAddr
        liftIO . (assertBool "Import Tx") . isRight =<< runEitherT testImport
        liftIO . (assertBool "Orphan Tx") . isRight =<< runEitherT testOrphan
    return ()

testPreInit :: (PersistStore m, PersistUnique m, PersistQuery m) 
            => EitherT String m ()
testPreInit = do 
    -- Creating a new account without initializing the wallet should fail
    runEitherT (cmdNewAcc "default") >>= 
        liftIO . assertEqual "Invalid wallet" 
            (Left "dbGetWallet: Invalid wallet main")

    -- Listing addresses without initializing the wallet should fail
    runEitherT (cmdList "default" 0 1) >>= 
        liftIO . assertEqual "Invalid account" 
            (Left "dbGetAcc: Invalid account default")

    -- Displaying page numbe -1 should fail
    runEitherT (cmdList "default" (-1) 1) >>= 
        liftIO . assertEqual "Invalid page number" 
            (Left "cmdList: Invalid page number -1")

    -- Displaying 0 results per page should fail
    runEitherT (cmdList "default" 0 0) >>= 
        liftIO . assertEqual "Invalid results per page" 
            (Left "cmdList: Invalid results per page 0")

testInit :: (PersistStore m, PersistUnique m, PersistQuery m) 
         => EitherT String m ()
testInit = do 

    -- Initializing the wallet with an empty seed should fail
    runEitherT (cmdInit "") >>= liftIO . assertEqual "Init empty seed" 
        (Left "cmdInit: seed can not be empty") 

    -- Initialize wallet with seed "Hello World"
    cmdInit "Hello World" 

    walletE <- getBy $ UniqueWalletName "main"
    
    -- Get the "main" wallet from the database
    liftIO $ assertBool "Wallet init" $ isJust walletE

    let (Entity _ w) = fromJust walletE
        key          = fromJust $ makeMasterKey $ stringToBS "Hello World"
        keystr       = xPrvExport $ runMasterKey key

    -- Check wallet master key
    liftIO $ assertEqual "Wallet master key" keystr $ dbWalletMaster w

    -- Check wallet name
    liftIO $ assertEqual "Wallet name" "main" $ dbWalletName w

    -- Generate addresses on an invalid account should fail
    runEitherT (cmdGenAddrs "default" 5) >>= liftIO . 
        assertEqual "Invalid account" 
        (Left "dbGetAcc: Invalid account default") 

    -- Address count in the database should be 0 at this point
    count ([] :: [Filter (DbAddressGeneric b)]) >>= 
        liftIO . assertEqual "Address count" 0

testNewAcc :: (PersistStore m, PersistUnique m, PersistQuery m) 
           => EitherT String m ()
testNewAcc = do 

    -- Create a new account with empty name ""
    cmdNewAcc "" >>= liftIO . assertEqual "New acc 1"
       ( object 
           [ "Name" .= T.pack ""
           , "Tree" .= T.pack "m/0'/"
           , "Type" .= T.pack "Regular"
           ] 
       )

    -- Check the wallet account index
    (dbWalletAccIndex . entityVal <$> dbGetWallet "main") >>= 
        liftIO . assertEqual "acc index 0" 0

    -- Create a new account named "acc1"
    cmdNewAcc "acc1" >>= liftIO . assertEqual "New acc 2"
        ( object 
            [ "Name" .= T.pack "acc1"
            , "Tree" .= T.pack "m/1'/"
            , "Type" .= T.pack "Regular"
            ] 
        )

    -- Check the wallet account index
    (dbWalletAccIndex . entityVal <$> dbGetWallet "main") >>= 
        liftIO . assertEqual "acc index 1" 1

    -- Create a new account named "acc2"
    cmdNewAcc "acc2" >>= liftIO . assertEqual "New acc 3"
        ( object 
            [ "Name" .= T.pack "acc2"
            , "Tree" .= T.pack "m/2'/"
            , "Type" .= T.pack "Regular"
            ] 
        )

    -- Check the wallet account index
    (dbWalletAccIndex . entityVal <$> dbGetWallet "main") >>= 
        liftIO . assertEqual "acc index 2" 2

    -- List all accounts created up to now
    cmdListAcc >>= liftIO . assertEqual "List accs"
        ( toJSON
            [ object 
                [ "Name" .= T.pack ""
                , "Tree" .= T.pack "m/0'/"
                , "Type" .= T.pack "Regular"
                ] 
            , object 
                [ "Name" .= T.pack "acc1"
                , "Tree" .= T.pack "m/1'/"
                , "Type" .= T.pack "Regular"
                ] 
            , object 
                [ "Name" .= T.pack "acc2"
                , "Tree" .= T.pack "m/2'/"
                , "Type" .= T.pack "Regular"
                ] 
            ]
        )

    -- Initialize some keys for future tests
    let mstKey = fromJust $ makeMasterKey $ stringToBS "Hello World"
        prvKey = runAccPrvKey $ fromJust $ accPrvKey mstKey 1
        pubKey = deriveXPubKey prvKey
       
    -- Dump keys for account "acc1"
    cmdDumpKeys "acc1" >>= liftIO . assertEqual "Dump keys"
        ( object 
            [ "Account" .= object 
                [ "Name" .= T.pack "acc1"
                , "Tree" .= T.pack "m/1'/"
                , "Type" .= T.pack "Regular"
                ] 
            , "PubKey"  .= xPubExport pubKey
            , "PrvKey"  .= xPrvExport prvKey
            ]
        )

    -- Display account information for account "acc2"
    cmdAccInfo "acc2" >>= liftIO . assertEqual "Acc info" 
        ( object 
            [ "Name" .= T.pack "acc2"
            , "Tree" .= T.pack "m/2'/"
            , "Type" .= T.pack "Regular"
            ] 
        )

    -- Count accounts in the database created up to now
    count ([] :: [Filter (DbAccountGeneric b)]) >>= 
        liftIO . assertEqual "Acc count" 3

    -- Check that address-related data and gaps are correct
    (Entity a1 acc1) <- dbGetAcc ""
    (Entity a2 acc2) <- dbGetAcc "acc1"
    (Entity a3 acc3) <- dbGetAcc "acc2"

    count [DbAddressAccount ==. a1, DbAddressInternal ==. True] >>=
        liftIO . assertEqual "Int gap address count" 30

    count [DbAddressAccount ==. a1, DbAddressInternal ==. False] >>=
        liftIO . assertEqual "Ext gap address count" 30

    count [DbAddressAccount ==. a2, DbAddressInternal ==. True] >>=
        liftIO . assertEqual "Int gap address count 2" 30

    count [DbAddressAccount ==. a2, DbAddressInternal ==. False] >>=
        liftIO . assertEqual "Ext gap address count 2" 30

    count [DbAddressAccount ==. a3, DbAddressInternal ==. True] >>=
        liftIO . assertEqual "Int gap address count 3" 30

    count [DbAddressAccount ==. a3, DbAddressInternal ==. False] >>=
        liftIO . assertEqual "Ext gap address count 3" 30

    liftIO $ assertEqual "ExtIndex acc 1" (-1) (dbAccountExtIndex acc1)
    liftIO $ assertEqual "IntIndex acc 1" (-1) (dbAccountIntIndex acc1)
    liftIO $ assertEqual "ExtGap acc 1"    29  (dbAccountExtGap acc1)
    liftIO $ assertEqual "IntGap acc 1"    29  (dbAccountIntGap acc1)

    liftIO $ assertEqual "ExtIndex acc 2" (-1) (dbAccountExtIndex acc2)
    liftIO $ assertEqual "IntIndex acc 2" (-1) (dbAccountIntIndex acc2)
    liftIO $ assertEqual "ExtGap acc 2"    29  (dbAccountExtGap acc2)
    liftIO $ assertEqual "IntGap acc 2"    29  (dbAccountIntGap acc2)

    liftIO $ assertEqual "ExtIndex acc 3" (-1) (dbAccountExtIndex acc3)
    liftIO $ assertEqual "IntIndex acc 3" (-1) (dbAccountIntIndex acc3)
    liftIO $ assertEqual "ExtGap acc 3"    29  (dbAccountExtGap acc3)
    liftIO $ assertEqual "IntGap acc 3"    29  (dbAccountIntGap acc3)

    cmdList "" 0 5 >>= liftIO . assertEqual "check empty addrs 1"
        ( object
            [ "Addresses" .= toJSON ([] :: [Value])
            , "Page results" .= object
                [ "Current page" .= (1 :: Int)
                , "Results per page" .= (5 :: Int)
                , "Total pages" .= (1 :: Int)
                , "Total addresses" .= (0 :: Int)
                ]
            ]
        )

    cmdList "acc1" 0 5 >>= liftIO . assertEqual "check empty addrs 2"
        ( object
            [ "Addresses" .= toJSON ([] :: [Value])
            , "Page results" .= object
                [ "Current page" .= (1 :: Int)
                , "Results per page" .= (5 :: Int)
                , "Total pages" .= (1 :: Int)
                , "Total addresses" .= (0 :: Int)
                ]
            ]
        )

    cmdList "acc2" 0 5 >>= liftIO . assertEqual "check empty addrs 3"
        ( object
            [ "Addresses" .= toJSON ([] :: [Value])
            , "Page results" .= object
                [ "Current page" .= (1 :: Int)
                , "Results per page" .= (5 :: Int)
                , "Total pages" .= (1 :: Int)
                , "Total addresses" .= (0 :: Int)
                ]
            ]
        )
        

testNewMS :: (PersistStore m, PersistUnique m, PersistQuery m) 
          => EitherT String m ()
testNewMS = do 

    -- Creating invalid multisig accounts should fail
    runEitherT (cmdNewMS "ms1" (-1) 0 []) >>= liftIO . assertEqual "Invalid ms 1"
        (Left "cmdNewMS: Invalid multisig parameters")

    -- Creating invalid multisig accounts should fail
    runEitherT (cmdNewMS "ms1" 0 (-1) []) >>= liftIO . assertEqual "Invalid ms 2"
        (Left "cmdNewMS: Invalid multisig parameters")

    -- Creating invalid multisig accounts should fail
    runEitherT (cmdNewMS "ms1" 0 0 []) >>= liftIO . assertEqual "Invalid ms 3"
        (Left "cmdNewMS: Invalid multisig parameters")

    -- Creating invalid multisig accounts should fail
    runEitherT (cmdNewMS "ms1" 0 3 []) >>= liftIO . assertEqual "Invalid ms 4"
        (Left "cmdNewMS: Invalid multisig parameters")

    -- Creating invalid multisig accounts should fail
    runEitherT (cmdNewMS "ms1" 3 0 []) >>= liftIO . assertEqual "Invalid ms 5"
        (Left "cmdNewMS: Invalid multisig parameters")

    -- Creating invalid multisig accounts should fail
    runEitherT (cmdNewMS "ms1" 3 17 []) >>= liftIO . assertEqual "Invalid ms 6"
        (Left "cmdNewMS: Invalid multisig parameters")

    -- Creating invalid multisig accounts should fail
    runEitherT (cmdNewMS "ms1" 17 3 []) >>= liftIO . assertEqual "Invalid ms 7"
        (Left "cmdNewMS: Invalid multisig parameters")

    -- Creating invalid multisig accounts should fail
    runEitherT (cmdNewMS "ms1" 3 2 []) >>= liftIO . assertEqual "Invalid ms 8"
        (Left "cmdNewMS: Invalid multisig parameters")

    -- Create a new 2 of 3 multisig account name "ms1"
    cmdNewMS "ms1" 2 3 [] >>= liftIO . assertEqual "New ms 1" 
        ( object 
            [ "Name" .= T.pack "ms1"
            , "Tree" .= T.pack "m/3'/"
            , "Type" .= T.pack "Multisig 2 of 3"
            , "Warning" .= T.pack "2 multisig keys missing"
            ] 
        )

    -- Check the wallet account index
    (dbWalletAccIndex . entityVal <$> dbGetWallet "main") >>= 
        liftIO . assertEqual "acc index 3" 3

    -- Display account information for account "ms1"
    cmdAccInfo "ms1" >>= liftIO . assertEqual "MS info"
        ( object 
            [ "Name" .= T.pack "ms1"
            , "Tree" .= T.pack "m/3'/"
            , "Type" .= T.pack "Multisig 2 of 3"
            , "Warning" .= T.pack "2 multisig keys missing"
            ] 
        )

    -- List all accounts created up to now (include the "ms1")
    cmdListAcc >>= liftIO . assertEqual "List accs"
        ( toJSON
            [ object 
                [ "Name" .= T.pack ""
                , "Tree" .= T.pack "m/0'/"
                , "Type" .= T.pack "Regular"
                ] 
            , object 
                [ "Name" .= T.pack "acc1"
                , "Tree" .= T.pack "m/1'/"
                , "Type" .= T.pack "Regular"
                ] 
            , object 
                [ "Name" .= T.pack "acc2"
                , "Tree" .= T.pack "m/2'/"
                , "Type" .= T.pack "Regular"
                ] 
            , object 
                [ "Name" .= T.pack "ms1"
                , "Tree" .= T.pack "m/3'/"
                , "Type" .= T.pack "Multisig 2 of 3"
                , "Warning" .= T.pack "2 multisig keys missing"
                ] 
            ]
        )

    -- Initialize some keys for the next tests
    let mstKey = fromJust $ makeMasterKey $ stringToBS "Hello World"
        mstKey2 = fromJust $ makeMasterKey $ stringToBS "Hello World 2"
        prvs = map (runAccPrvKey . fst) $ accPrvKeys mstKey 0
        pubs = map deriveXPubKey prvs
        prvs2 = map (runAccPrvKey . fst) $ accPrvKeys mstKey2 0
        pubs2 = map deriveXPubKey prvs2

    -- Count the number of accounts in the database
    count ([] :: [Filter (DbAccountGeneric b)]) >>= 
        liftIO . assertEqual "MS count" 4

    -- Adding empty key list should fail
    runEitherT (cmdAddKeys "ms1" []) >>= liftIO . assertEqual "Empty addKey"
        (Left "dbAddKeys: Keys can not be empty")

    -- Adding keys to a non-multisig account should fail
    runEitherT (cmdAddKeys "acc1" [pubs !! 0]) >>= 
        liftIO . assertEqual "Invalid addKey 1" 
            (Left "dbAddKeys: Can only add keys to a multisig account")

    -- Adding your own keys to a multisig account should fail
    runEitherT (cmdAddKeys "ms1" [pubs !! 0]) >>=
        liftIO . assertEqual "Invalid addKey 2" 
            (Left "dbAddKeys: Can not add your own keys to a multisig account")

    -- Adding your own keys to a multisig account should fail
    runEitherT (cmdAddKeys "ms1" [pubs2 !! 0,pubs !! 1]) >>=
        liftIO . assertEqual "Invalid addKey 3" 
            (Left "dbAddKeys: Can not add your own keys to a multisig account")
        
    -- Adding too many keys to a multisig account should fail
    runEitherT (cmdAddKeys "ms1" $ take 4 pubs2) >>=
        liftIO . assertEqual "Invalid addKey 4" 
            (Left "dbAddKeys: Too many keys")

    -- Display account information for account "ms1". Shold not have changed
    cmdAccInfo "ms1" >>= liftIO . assertEqual "MS info 2"
        ( object 
            [ "Name" .= T.pack "ms1"
            , "Tree" .= T.pack "m/3'/"
            , "Type" .= T.pack "Multisig 2 of 3"
            , "Warning" .= T.pack "2 multisig keys missing"
            ] 
        )
    
    -- Check that address-related data and gaps are correct
    (Entity a1 acc1) <- dbGetAcc "ms1"

    count [DbAddressAccount ==. a1, DbAddressInternal ==. True] >>=
        liftIO . assertEqual "Int gap address count MS 1" 0

    count [DbAddressAccount ==. a1, DbAddressInternal ==. False] >>=
        liftIO . assertEqual "Ext gap address count MS 1" 0

    liftIO $ assertEqual "ExtIndex acc MS" (-1) (dbAccountExtIndex acc1)
    liftIO $ assertEqual "IntIndex acc MS" (-1) (dbAccountIntIndex acc1)
    liftIO $ assertEqual "ExtGap acc MS"   (-1)  (dbAccountExtGap acc1)
    liftIO $ assertEqual "IntGap acc MS"   (-1)  (dbAccountIntGap acc1)

    cmdList "ms1" 0 5 >>= liftIO . assertEqual "check empty addrs MS 1"
        ( object
            [ "Addresses" .= toJSON ([] :: [Value])
            , "Page results" .= object
                [ "Current page" .= (1 :: Int)
                , "Results per page" .= (5 :: Int)
                , "Total pages" .= (1 :: Int)
                , "Total addresses" .= (0 :: Int)
                ]
            ]
        )

    -- Adding a key to the multisig account "ms1". One should still be missing
    cmdAddKeys "ms1" [pubs2 !! 0] >>= liftIO . assertEqual "MS addkey 1"
        ( object 
            [ "Name" .= T.pack "ms1"
            , "Tree" .= T.pack "m/3'/"
            , "Type" .= T.pack "Multisig 2 of 3"
            , "Warning" .= T.pack "1 multisig keys missing"
            ] 
        )

    -- Dump the keys of account "ms1". One account should be listed under MSKeys
    cmdDumpKeys "ms1" >>= liftIO . assertEqual "MS dumpkey 2" 
        ( object 
            [ "Account" .= object 
                [ "Name" .= T.pack "ms1"
                , "Tree" .= T.pack "m/3'/"
                , "Type" .= T.pack "Multisig 2 of 3"
                , "Warning" .= T.pack "1 multisig keys missing"
                ] 
            , "PubKey"  .= (xPubExport $ pubs !! 3)
            , "PrvKey"  .= (xPrvExport $ prvs !! 3)
            , "MSKeys"  .= toJSON [xPubExport $ pubs2 !! 0]
            ]
        )

    -- Check that address-related data and gaps are correct
    (Entity a2 acc2) <- dbGetAcc "ms1"

    count [DbAddressAccount ==. a2, DbAddressInternal ==. True] >>=
        liftIO . assertEqual "Int gap address count MS 2" 0

    count [DbAddressAccount ==. a2, DbAddressInternal ==. False] >>=
        liftIO . assertEqual "Ext gap address count MS 2" 0

    liftIO $ assertEqual "ExtIndex acc MS 2" (-1) (dbAccountExtIndex acc2)
    liftIO $ assertEqual "IntIndex acc MS 2" (-1) (dbAccountIntIndex acc2)
    liftIO $ assertEqual "ExtGap acc MS 2"   (-1)  (dbAccountExtGap acc2)
    liftIO $ assertEqual "IntGap acc MS 2"   (-1)  (dbAccountIntGap acc2)

    cmdList "ms1" 0 5 >>= liftIO . assertEqual "check empty addrs MS 2"
        ( object
            [ "Addresses" .= toJSON ([] :: [Value])
            , "Page results" .= object
                [ "Current page" .= (1 :: Int)
                , "Results per page" .= (5 :: Int)
                , "Total pages" .= (1 :: Int)
                , "Total addresses" .= (0 :: Int)
                ]
            ]
        )

    -- Add second key to account "ms1". Account should be complete now
    cmdAddKeys "ms1" [pubs2 !! 1] >>= liftIO . assertEqual "MS addkey 2" 
        ( object 
            [ "Name" .= T.pack "ms1"
            , "Tree" .= T.pack "m/3'/"
            , "Type" .= T.pack "Multisig 2 of 3"
            ] 
        )

    -- Dumping "ms1" keys should now display all keys
    cmdDumpKeys "ms1" >>= liftIO . assertEqual "MS dumpkey 2" 
        ( object 
            [ "Account" .= object 
                [ "Name" .= T.pack "ms1"
                , "Tree" .= T.pack "m/3'/"
                , "Type" .= T.pack "Multisig 2 of 3"
                ] 
            , "PubKey"  .= (xPubExport $ pubs !! 3)
            , "PrvKey"  .= (xPrvExport $ prvs !! 3)
            , "MSKeys"  .= toJSON [ xPubExport $ pubs2 !! 0
                                  , xPubExport $ pubs2 !! 1
                                  ]
            ]
        )

    -- Check that address-related data and gaps are correct
    (Entity a3 acc3) <- dbGetAcc "ms1"

    count [DbAddressAccount ==. a3, DbAddressInternal ==. True] >>=
        liftIO . assertEqual "Int gap address count MS 3" 30

    count [DbAddressAccount ==. a3, DbAddressInternal ==. False] >>=
        liftIO . assertEqual "Ext gap address count MS 3" 30

    liftIO $ assertEqual "ExtIndex acc MS 3" (-1) (dbAccountExtIndex acc3)
    liftIO $ assertEqual "IntIndex acc MS 3" (-1) (dbAccountIntIndex acc3)
    liftIO $ assertEqual "ExtGap acc MS 3"    29  (dbAccountExtGap acc3)
    liftIO $ assertEqual "IntGap acc MS 3"    29  (dbAccountIntGap acc3)

    cmdList "ms1" 0 5 >>= liftIO . assertEqual "check empty addrs MS 3"
        ( object
            [ "Addresses" .= toJSON ([] :: [Value])
            , "Page results" .= object
                [ "Current page" .= (1 :: Int)
                , "Results per page" .= (5 :: Int)
                , "Total pages" .= (1 :: Int)
                , "Total addresses" .= (0 :: Int)
                ]
            ]
        )

    -- Adding another key now should fail as the account "ms1" is complete
    runEitherT (cmdAddKeys "ms1" [pubs2 !! 2]) >>=
        liftIO . assertEqual "Invalid addKey 5" 
            (Left "dbAddKeys: Account is complete. No more keys can be added")

testGenAddr :: (PersistStore m, PersistUnique m, PersistQuery m) 
          => EitherT String m ()
testGenAddr = do 

    -- List addresses from an empty account
    cmdList "" 0 5 >>= liftIO . assertEqual "list empty addr" 
        ( object
            [ "Addresses" .= toJSON ([] :: [Value])
            , "Page results" .= object
                [ "Current page" .= (1 :: Int)
                , "Results per page" .= (5 :: Int)
                , "Total pages" .= (1 :: Int)
                , "Total addresses" .= (0 :: Int)
                ]
            ]
        )

    -- Generate 5 addresses on the account "" (empty string)
    cmdGenAddrs "" 5 >>= liftIO . assertEqual "gen addr"
        ( toJSON 
            [ object [ "Addr" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                     , "Key"  .= (0 :: Int)
                     , "Tree" .= T.pack "m/0'/0/0/"
                     ]
            , object [ "Addr" .= T.pack "1NkXvbrHPGC2vjtmL2mup1sWi2TU8LW6XB"
                     , "Key"  .= (1 :: Int)
                     , "Tree" .= T.pack "m/0'/0/1/"
                     ]
            , object [ "Addr" .= T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                     , "Key"  .= (2 :: Int)
                     , "Tree" .= T.pack "m/0'/0/2/"
                     ]
            , object [ "Addr" .= T.pack "1Erwcuqn1dHm8r6fNxogGmCHYuNfwKNwer"
                     , "Key"  .= (3 :: Int)
                     , "Tree" .= T.pack "m/0'/0/3/"
                     ]
            , object [ "Addr" .= T.pack "18aT8MJ15VV26nx29xmbu5fzvqE6sqh6i9"
                     , "Key"  .= (4 :: Int)
                     , "Tree" .= T.pack "m/0'/0/4/"
                     ]
            ]
        )

    -- Check account external index
    (dbAccountExtIndex . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc ext index" 4

    -- Check account internal index
    (dbAccountIntIndex . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc int index" (-1)

    -- Check account external gap
    (dbAccountExtGap . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc ext index" 34

    -- Check account internal gap
    (dbAccountIntGap . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc int index" 29

    -- Count all addresses in the Address table
    -- 60*4 + 5
    count ([] :: [Filter (DbAddressGeneric b)]) >>= 
        liftIO . assertEqual "Count addr" 245

    let f x = (dbAddressBase58 x,dbAddressTree x,dbAddressIndex x)
    (map f <$> dbGenIntAddrs "" 4) >>= liftIO . assertEqual "Internal addr"
        [ ("19RtLtmuuxscgg5TXkCsSJ7bCdEzci5XTm","m/0'/1/0/",0)
        , ("1E75f3kuDanTHeTa8nvJCxYF8MXaud4QPE","m/0'/1/1/",1)
        , ("1NXkqUpqM23p6u44nAhoP1wVd2BdCEr4Zm","m/0'/1/2/",2)
        , ("1AjBQfprusZGGnD4jCmexizJxKBcAw4cdc","m/0'/1/3/",3)
        ]

    -- Check account external index
    (dbAccountExtIndex . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc ext index 2" 4

    -- Check account internal index
    (dbAccountIntIndex . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc int index" 3

    -- Check account external gap
    (dbAccountExtGap . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc ext index" 34

    -- Check account internal gap
    (dbAccountIntGap . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc int index" 33

    -- Count all addresses in the Address table
    -- 60*4 + 5
    count ([] :: [Filter (DbAddressGeneric b)]) >>= 
        liftIO . assertEqual "Count addr" 249

    -- List addresses from the ms1 account. Should be empty
    cmdList "ms1" 0 5 >>= liftIO . assertEqual "list empty addr 2"
        ( object
            [ "Addresses" .= toJSON ([] :: [Value])
            , "Page results" .= object
                [ "Current page" .= (1 :: Int)
                , "Results per page" .= (5 :: Int)
                , "Total pages" .= (1 :: Int)
                , "Total addresses" .= (0 :: Int)
                ]
            ]
        )

    -- List page 1 with 1 result per page
    cmdList "" 1 1 >>= liftIO . assertEqual "list addr 1"
        ( object 
            [ "Addresses" .= toJSON 
                [ object [ "Addr" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                         , "Key"  .= (0 :: Int)
                         , "Tree" .= T.pack "m/0'/0/0/"
                         ]
                ]
            , "Page results" .= object
                [ "Current page" .= (1 :: Int)
                , "Results per page" .= (1 :: Int)
                , "Total pages" .= (5 :: Int)
                , "Total addresses" .= (5 :: Int)
                ]
            ]
        )

    -- List page 2 with 1 result per page
    cmdList "" 2 1 >>= liftIO . assertEqual "list addr 2"
        ( object 
            [ "Addresses" .= toJSON 
               [ object [ "Addr" .= T.pack "1NkXvbrHPGC2vjtmL2mup1sWi2TU8LW6XB"
                        , "Key"  .= (1 :: Int)
                        , "Tree" .= T.pack "m/0'/0/1/"
                        ]
               ]
            , "Page results" .= object
                [ "Current page" .= (2 :: Int)
                , "Results per page" .= (1 :: Int)
                , "Total pages" .= (5 :: Int)
                , "Total addresses" .= (5 :: Int)
                ]
            ]
        )

    -- List page 0 (last page) with 1 result per page
    cmdList "" 0 1 >>= liftIO . assertEqual "list addr 2"
        ( object 
            [ "Addresses" .= toJSON 
               [ object [ "Addr" .= T.pack "18aT8MJ15VV26nx29xmbu5fzvqE6sqh6i9"
                        , "Key"  .= (4 :: Int)
                        , "Tree" .= T.pack "m/0'/0/4/"
                        ]
               ]
            , "Page results" .= object
                [ "Current page" .= (5 :: Int)
                , "Results per page" .= (1 :: Int)
                , "Total pages" .= (5 :: Int)
                , "Total addresses" .= (5 :: Int)
                ]
            ]
        )

    -- Listing page > maxpage should fail
    runEitherT (cmdList "" 6 1) >>= liftIO . assertEqual "list addr 2"
        (Left "cmdList: Page number too high")

    -- List page 0 (last page) with 3 result per page
    cmdList "" 0 3 >>= liftIO . assertEqual "list addr 2"
        ( object 
            [ "Addresses" .= toJSON 
                [ object [ "Addr" .= T.pack "1Erwcuqn1dHm8r6fNxogGmCHYuNfwKNwer"
                         , "Key"  .= (3 :: Int)
                         , "Tree" .= T.pack "m/0'/0/3/"
                         ]
                , object [ "Addr" .= T.pack "18aT8MJ15VV26nx29xmbu5fzvqE6sqh6i9"
                         , "Key"  .= (4 :: Int)
                         , "Tree" .= T.pack "m/0'/0/4/"
                         ]
                ]
            , "Page results" .= object
                [ "Current page" .= (2 :: Int)
                , "Results per page" .= (3 :: Int)
                , "Total pages" .= (2 :: Int)
                , "Total addresses" .= (5 :: Int)
                ]
            ]
        )

    -- Generating multisig addresses with labels
    cmdGenWithLabel "ms1" ["","addr1","addr2","Two Words"] >>= 
        liftIO . assertEqual "gen ms addr" 
        ( toJSON 
            [ object [ "Addr" .= T.pack "32VCGK4pbvVsFvSGfmLpmNTu4JjhuR3WmM"
                     , "Key"  .= (0 :: Int)
                     , "Tree" .= T.pack "m/3'/0/0/"
                     ]
            , object [ "Addr"  .= T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                     , "Key"   .= (1 :: Int)
                     , "Tree"  .= T.pack "m/3'/0/1/"
                     , "Label" .= T.pack "addr1"
                     ]
            , object [ "Addr"  .= T.pack "3E3qvGPki6sypXdyL6CMxBQwzFkY7iKrGW"
                     , "Key"   .= (2 :: Int)
                     , "Tree"  .= T.pack "m/3'/0/2/"
                     , "Label" .= T.pack "addr2"
                     ]
            , object [ "Addr"  .= T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                     , "Key"   .= (3 :: Int)
                     , "Tree"  .= T.pack "m/3'/0/3/"
                     , "Label" .= T.pack "Two Words"
                     ]
            ]
        )

    -- Check account external index
    (dbAccountExtIndex . entityVal <$> dbGetAcc "ms1") >>= 
        liftIO . assertEqual "acc ext index 3" 3

    -- Check account internal index
    (dbAccountIntIndex . entityVal <$> dbGetAcc "ms1") >>= 
        liftIO . assertEqual "acc int index 3" (-1)

    -- Check account external gap
    (dbAccountExtGap . entityVal <$> dbGetAcc "ms1") >>= 
        liftIO . assertEqual "acc ext index 3" 33

    -- Check account internal gap
    (dbAccountIntGap . entityVal <$> dbGetAcc "ms1") >>= 
        liftIO . assertEqual "acc int index 3" 29

    -- Count addresses
    count ([] :: [Filter (DbAddressGeneric b)]) >>= 
        liftIO . assertEqual "Count addr 3" 253

    let f x = (dbAddressBase58 x,dbAddressTree x,dbAddressIndex x)
    (map f <$> dbGenIntAddrs "ms1" 6) >>= liftIO . assertEqual "Internal addr 2"
        [ ("19RtLtmuuxscgg5TXkCsSJ7bCdEzci5XTm","m/3'/1/0/",0)
        , ("1E75f3kuDanTHeTa8nvJCxYF8MXaud4QPE","m/3'/1/1/",1)
        , ("1NXkqUpqM23p6u44nAhoP1wVd2BdCEr4Zm","m/3'/1/2/",2)
        , ("1AjBQfprusZGGnD4jCmexizJxKBcAw4cdc","m/3'/1/3/",3)
        ]

    -- Rename the first multisig address label
    cmdLabel "ms1" 0 "alpha" >>= liftIO . assertEqual "label ms"
        ( object 
            [ "Addr"  .= T.pack "32VCGK4pbvVsFvSGfmLpmNTu4JjhuR3WmM"
            , "Key"   .= (0 :: Int)
            , "Tree"  .= T.pack "m/3'/0/0/"
            , "Label" .= T.pack "alpha"
            ]
        )

    -- Rename another multisig address label
    cmdLabel "ms1" 3 "beta"

    -- Setting a label on an invalid address should fail
    runEitherT (cmdLabel "ms1" (-1) "theta") >>= 
        liftIO . assertEqual "set label fail"
            (Left "cmdLabel: Key -1 does not exist")

    -- Setting a label on an invalid address should fail
    runEitherT (cmdLabel "ms1" 4 "theta") >>= 
        liftIO . assertEqual "set label fail 2"
            (Left "cmdLabel: Key 4 does not exist")

    -- Setting a label on an invalid address should fail
    runEitherT (cmdLabel "ms1" 100 "theta") >>= 
        liftIO . assertEqual "set label fail 3"
            (Left "cmdLabel: Key 100 does not exist")

    -- List page 2 with 1 result per page
    cmdList "ms1" 1 5 >>= liftIO . assertEqual "list ms"
        ( object 
            [ "Addresses" .= toJSON 
               [ object [ "Addr" .= T.pack "32VCGK4pbvVsFvSGfmLpmNTu4JjhuR3WmM"
                        , "Key"  .= (0 :: Int)
                        , "Tree" .= T.pack "m/3'/0/0/"
                        , "Label" .= T.pack "alpha"
                        ]
               , object [ "Addr"  .= T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                        , "Key"   .= (1 :: Int)
                        , "Tree"  .= T.pack "m/3'/0/1/"
                        , "Label" .= T.pack "addr1"
                        ]
               , object [ "Addr"  .= T.pack "3E3qvGPki6sypXdyL6CMxBQwzFkY7iKrGW"
                        , "Key"   .= (2 :: Int)
                        , "Tree"  .= T.pack "m/3'/0/2/"
                        , "Label" .= T.pack "addr2"
                        ]
               , object [ "Addr"  .= T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                        , "Key"   .= (3 :: Int)
                        , "Tree"  .= T.pack "m/3'/0/3/"
                        , "Label" .= T.pack "beta"
                        ]
               ]
            , "Page results" .= object
                [ "Current page" .= (1 :: Int)
                , "Results per page" .= (5 :: Int)
                , "Total pages" .= (1 :: Int)
                , "Total addresses" .= (4 :: Int)
                ]
            ]
        )

{- Payments sent to:
 - 1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi : 100000 (in wallet)
 - 1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY : 200000 (in wallet)
 - 19W372PnyKZUgYMj1f5qEsJS2xkSjSzXRA : 240000 (not in wallet)
 - 1NCSUHC7Dt6exeNd65PEjELdN4hk7QtN1m : 122000 (not in wallet)
 - 38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki : 150000 (in wallet)
 - 3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q : 400000 (in wallet)
 -}

tx1 :: String
tx1 = "010000000100000000000000000000000000000000000000000000000000000000000000010100000000ffffffff06a0860100000000001976a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac400d0300000000001976a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac80a90300000000001976a9145d3ed34bf8654225eb8dc35c15a47d25572a62be88ac90dc0100000000001976a914e8847a85898158cd24c7914c4dd8bfd175ab9de888acf04902000000000017a9144d769c08d79eed22532e044213bef3174f05158487801a06000000000017a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd08700000000"

tx2 :: String
tx2 = "010000000100000000000000000000000000000000000000000000000000000000000000020100000000ffffffff06a0860100000000001976a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac400d0300000000001976a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac80a90300000000001976a9145d3ed34bf8654225eb8dc35c15a47d25572a62be88ac90dc0100000000001976a914e8847a85898158cd24c7914c4dd8bfd175ab9de888acf04902000000000017a9144d769c08d79eed22532e044213bef3174f05158487801a06000000000017a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd08700000000"

testImport :: ( PersistStore m
              , PersistUnique m
              , PersistQuery m
              , PersistMonadBackend m ~ SqlBackend
              ) 
           => EitherT String m ()
testImport = do 

    -- Importin transaction sending funds to two different accounts
    cmdImportTx (decode' $ fromJust $ hexToBS tx1) >>= 
        liftIO . assertEqual "Import tx 1"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                             , T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                             ]
                         , "Value"      .= (300000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                             , T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                             ]
                         , "Value"      .= (550000 :: Int)
                         , "Orphan"     .= False
                         ]
                ]
            )

    -- Importin similar transaction sending funds to two different accounts
    cmdImportTx (decode' $ fromJust $ hexToBS tx2) >>= 
        liftIO . assertEqual "Import tx 2"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                             , T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                             ]
                         , "Value"      .= (300000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                             , T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                             ]
                         , "Value"      .= (550000 :: Int)
                         , "Orphan"     .= False
                         ]
                ]
            )

    -- List transactions of account ""
    cmdListTx "" >>= liftIO . assertEqual "List tx 1"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                            [ T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                            , T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                            ]
                        , "Value"      .= (300000 :: Int)
                        , "Orphan"     .= False
                        ]
            , object [ "Recipients" .= toJSON
                            [ T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                            , T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                            ]
                        , "Value"      .= (300000 :: Int)
                        , "Orphan"     .= False
                        ]
            ]
        )

    -- List transactions of account "ms1"
    cmdListTx "ms1" >>= liftIO . assertEqual "List tx 2"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                         , T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                         ]
                     , "Value"      .= (550000 :: Int)
                     , "Orphan"     .= False
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                         , T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                         ]
                     , "Value"      .= (550000 :: Int)
                     , "Orphan"     .= False
                     ]
            ]
        )

    -- Verify the balance of account ""
    cmdBalance "" 
        >>= liftIO . assertEqual "Balance 1" (toJSON (600000 :: Int))

    -- Verify the balance of account "ms1"
    cmdBalance "ms1" 
        >>= liftIO . assertEqual "Balance 2" (toJSON (1100000 :: Int))

    -- Get coins of account ""
    cmdCoins "" >>= liftIO . assertEqual "Get coins 1"
        ( toJSON
            [ object
                [ "TxID"    .= T.pack "4088f8ab36e3a3aaa067d54a37fca74478b4c52c16bdc3a7c4e6423ca3b46d86"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "4088f8ab36e3a3aaa067d54a37fca74478b4c52c16bdc3a7c4e6423ca3b46d86"
                , "Index"   .= (1 :: Int)
                , "Value"   .= (200000 :: Int)
                , "Script"  .= T.pack "76a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                ]
            , object
                [ "TxID"    .= T.pack "984092e0ba079812fe1d05ec653f775beb5ea7a1ae9ad1d35d6c9f39ec507a40"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "984092e0ba079812fe1d05ec653f775beb5ea7a1ae9ad1d35d6c9f39ec507a40"
                , "Index"   .= (1 :: Int)
                , "Value"   .= (200000 :: Int)
                , "Script"  .= T.pack "76a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                ]
            ]
        )

    -- Get coins of account "ms1"
    cmdCoins "ms1" >>= liftIO . assertEqual "Get coins 2"
        ( toJSON
            [ object
                [ "TxID"   .= T.pack "4088f8ab36e3a3aaa067d54a37fca74478b4c52c16bdc3a7c4e6423ca3b46d86"
                , "Index"   .= (4 :: Int)
                , "Value"   .= (150000 :: Int)
                , "Script"  .= T.pack "a9144d769c08d79eed22532e044213bef3174f05158487"
                , "Redeem"  .= T.pack "5221026e294fcecdcbae12a0aba1685db35c54ddfc1375d48f96ff1b8805a4bb57bfc921028bc8d8377f44de8ac8beff0dc9ccefde4cd5dded7b8cd8babe02c7147a90ba6c21039cf5d06e79871043c420fabc652f8082e702e0094f91ec14c020e9fcf48fa4d853ae"
                , "Orphan"  .= False
                , "Address" .= T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                ]
            , object
                [ "TxID"   .= T.pack "4088f8ab36e3a3aaa067d54a37fca74478b4c52c16bdc3a7c4e6423ca3b46d86"
                , "Index"   .= (5 :: Int)
                , "Value"   .= (400000 :: Int)
                , "Script"  .= T.pack "a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd087"
                , "Redeem"  .= T.pack "5221020f7ead178316e8414d128712a23cde2e843d1a0f66afc0bfa600ab90deefd5f321023182b240cb2607ed03f76c9dca37c4b9fcb3b763b776223cc94808f7e67fb03a2102648dbcbc9f44fb55a992efe7b3ab214306cc72cdcae2a7cf6f9d44262a53c3b353ae"
                , "Orphan"  .= False
                , "Address" .= T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                ]
            , object
                [ "TxID"   .= T.pack "984092e0ba079812fe1d05ec653f775beb5ea7a1ae9ad1d35d6c9f39ec507a40"
                , "Index"   .= (4 :: Int)
                , "Value"   .= (150000 :: Int)
                , "Script"  .= T.pack "a9144d769c08d79eed22532e044213bef3174f05158487"
                , "Redeem"  .= T.pack "5221026e294fcecdcbae12a0aba1685db35c54ddfc1375d48f96ff1b8805a4bb57bfc921028bc8d8377f44de8ac8beff0dc9ccefde4cd5dded7b8cd8babe02c7147a90ba6c21039cf5d06e79871043c420fabc652f8082e702e0094f91ec14c020e9fcf48fa4d853ae"
                , "Orphan"  .= False
                , "Address" .= T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                ]
            , object
                [ "TxID"   .= T.pack "984092e0ba079812fe1d05ec653f775beb5ea7a1ae9ad1d35d6c9f39ec507a40"
                , "Index"   .= (5 :: Int)
                , "Value"   .= (400000 :: Int)
                , "Script"  .= T.pack "a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd087"
                , "Redeem"  .= T.pack "5221020f7ead178316e8414d128712a23cde2e843d1a0f66afc0bfa600ab90deefd5f321023182b240cb2607ed03f76c9dca37c4b9fcb3b763b776223cc94808f7e67fb03a2102648dbcbc9f44fb55a992efe7b3ab214306cc72cdcae2a7cf6f9d44262a53c3b353ae"
                , "Orphan"  .= False
                , "Address" .= T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                ]
            ]
        )

    -- Input: 200000 and 200000 = 400000
    -- Output = 340000 + 50000 change = 390000
    -- Fee = 10000

    let txRes = "0100000002407a50ec399f6c5dd3d19aaea1a75eeb5b773f65ec051dfe129807bae0924098010000006a47304402207034db48e459406e68078d48905b8ec7517b51225b511907ff1d7432cf9b4ab9022059db7a313f266673f4e5cfbaed8c374c2ea55bee7df315369fd5e8cd340c4f7f01210250f4e42bb94ed8b27c6c8b728c0bd02828af1d4ebf8e3f0a6e7da3f53369c104ffffffff866db4a33c42e6c4a7c3bd162cc5b47844a7fc374ad567a0aaa3e336abf88840010000006a47304402203683c2a55dc11adbbc2f2a53a460f69719aa54ff557ccc5dbb1425a65e33af4302207446e598a8475380805afa1f6f5bf2ef01977e0dd48f83f1c5866dddb21ea84c01210250f4e42bb94ed8b27c6c8b728c0bd02828af1d4ebf8e3f0a6e7da3f53369c104ffffffff04c0d401000000000017a9144d769c08d79eed22532e044213bef3174f0515848780380100000000001976a914980b9c708958bbe4cc05d0b302d4f12625a5d88c88ace0220200000000001976a9140f05c6d0dadc442028258856a1f3d1e6d3e166ff88ac50c30000000000001976a914bb498fb0f0d0639193660b40a9a91e1b3eb60bab88ac00000000"

    cmdSendMany "" 
        [ ("38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki",120000) -- In wallet
        , ("1Erwcuqn1dHm8r6fNxogGmCHYuNfwKNwer", 80000) -- In wallet
        , ("12NS4FB1S76bkRrLUxHWSjzZaqEdUxYreJ",140000) -- Not in wallet
        ] 10000 >>= liftIO . assertEqual "sendMany tx"
            (object 
                [ "Payment Tx" .= T.pack txRes
               , "Complete" .= True
               ]
            )

    (cmdImportTx $ decode' $ fromJust $ hexToBS txRes) >>=
        liftIO . assertEqual "import send tx"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                             , T.pack "12NS4FB1S76bkRrLUxHWSjzZaqEdUxYreJ"
                             ]
                         , "Value"      .= (-270000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki" ]
                         , "Value"      .= (120000 :: Int)
                         , "Orphan"     .= False
                         ]
                ]
            )

    -- Get coins of account ""
    cmdCoins "" >>= liftIO . assertEqual "Get coins 2"
        ( toJSON
            [ object
                [ "TxID"    .= T.pack "4088f8ab36e3a3aaa067d54a37fca74478b4c52c16bdc3a7c4e6423ca3b46d86"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "984092e0ba079812fe1d05ec653f775beb5ea7a1ae9ad1d35d6c9f39ec507a40"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "b4b7c7b5a39cce981629f46eb0245b6ac53ef024b8a83b5a26166c7bcaf7c1f1"
                , "Index"   .= (1 :: Int)
                , "Value"   .= (80000 :: Int)
                , "Script"  .= T.pack "76a914980b9c708958bbe4cc05d0b302d4f12625a5d88c88ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1Erwcuqn1dHm8r6fNxogGmCHYuNfwKNwer"
                ]
            , object
                [ "TxID"    .= T.pack "b4b7c7b5a39cce981629f46eb0245b6ac53ef024b8a83b5a26166c7bcaf7c1f1"
                , "Index"   .= (3 :: Int)
                , "Value"   .= (50000 :: Int)
                , "Script"  .= T.pack "76a914bb498fb0f0d0639193660b40a9a91e1b3eb60bab88ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1J5HV12wGbPj5SUryku2zFoaxnC1AngqcH"
                ]
            ]
        )

    -- List transactions of account ""
    cmdListTx "" >>= liftIO . assertEqual "List tx 2"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                         , T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                         ]
                     , "Value"      .= (300000 :: Int)
                     , "Orphan"     .= False
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                         , T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                         ]
                     , "Value"      .= (300000 :: Int)
                     , "Orphan"     .= False
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                         , T.pack "12NS4FB1S76bkRrLUxHWSjzZaqEdUxYreJ"
                         ]
                     , "Value"      .= (-270000 :: Int)
                     , "Orphan"     .= False
                     ]
            ]
        )
            
    -- Verify the balance of account ""
    cmdBalance "" 
        >>= liftIO . assertEqual "Balance 3" (toJSON (330000 :: Int))

    -- Verify the balance of account "ms1"
    cmdBalance "ms1" 
        >>= liftIO . assertEqual "Balance 4" (toJSON (1220000 :: Int))

-- Building tx link:
-- txA : outside => acc1
-- txB : acc1 => (acc1,acc2,outside)
-- txC : acc2 => (acc1,outside) 
-- tcD : acc1 => outside
-- import order: txC, tcB, txA

{- TxID: bbe4d14cf36346d6b02e42b48bd149e2076d4059d1effb90f402f5d2a1e50a30
 - Payments sent to:
 - 1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL : 100000
 - 1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw : 200000
 - 17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG : 300000
 -}

txA :: String
txA = "010000000100000000000000000000000000000000000000000000000000000000000000090300000000ffffffff03a0860100000000001976a9148062ab5c3fdf5f8f0d41fccacbb3ea8058b911ae88ac400d0300000000001976a9148727e4552058a555d0ce269d8cf8c850785666f688ace0930400000000001976a9144c769509bb3e22c2275cd025fcb55ebc5dc1e39f88ac00000000"

{- TxID: 4c9bda948a7a99b648b238cb9c9fc2b036e9431a6053948747add7765659ea2b
 - inputs: acc1 (index 1 and index 2 = 500000)
 - Payments sent to:
 - 14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS : 100000 (acc1)
 - 16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M : 200000 (acc2)
 - 14bbbNEnXgqEWF3GaVLewvPYHAenStYawj : 150000 (outside)
 - 1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2 :  40000 (change to acc1)
 -}

txB :: String
txB = "0100000002300ae5a1d2f502f490fbefd159406d07e249d18bb4422eb0d64663f34cd1e4bb010000006b4830450221009dd1b747ff237dec0fd29eb853e8db06fc1ec7b777938d64723476763a3c2677022077ed2b238849aad5444044aaac0f52f3526b7974a543e51bd36fe99d14a47931012103a40f6bd1d59440a007aa8ec93875d07f234ffebe76df311c1b610fc1c0d22dd9ffffffff300ae5a1d2f502f490fbefd159406d07e249d18bb4422eb0d64663f34cd1e4bb020000006a47304402205928db75d0aeed4eb6bcb7cc436754e4ffb926fa13f86f050ae683eda5e879bb0220717bc45db34f4ff150d00e4f2f618bf66c7543ec08d56c05a83b7f7d061dd714012103a0d2cdf936eca39cfd6407393f8f0f2af932bdea6271d559308075b77d2ec080ffffffff04a0860100000000001976a914243d03889d49470ee721d44596fd146440e1167c88ac400d0300000000001976a9143f53fba59a1c17f17cf3c4b5cfcf15fa0087f1c188acf0490200000000001976a9142773657caec521b20d23536e7d7901de8c45652988ac409c0000000000001976a914740eb168ae243882a73f5467b9024431443ef12988ac00000000"

{- TxID: 92e83da9496d4b8fd26826b6bf8895e68487fea3d627227827b832ad2b7fdf56
 - inputs: acc2 (index 1 = 200000)
 - Payments sent to:
 - 14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb : 120000 (acc1)
 - 1P5vxvRYdtkucT2JeXk4XEvp5xvs4XLV13 :  40000 (outside)
 - 13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J :  30000 (change to acc2)
 -}

txC :: String
txC = "01000000012bea595676d7ad47879453601a43e936b0c29f9ccb38b248b6997a8a94da9b4c010000006a473044022070deb5ccd4dd99a599150f821d6500517468a65d33f959bfffdbc3f90b2ce303022006fcbca5e09ab35bd1aba042ce83fede27a589fa990445a62ae8e7e694469e2f012103b4b925d5967a00d2540115f035aa10290853d39e3c20591d711680cac5e2b4efffffffff03c0d40100000000001976a91424cba659aad4563de9199f3fe273bac07f170eb088ac409c0000000000001976a914f2417a64be9f74aea983488dbb031e39cb15bca888ac30750000000000001976a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac00000000"

{- TxID: 92e83da9496d4b8fd26826b6bf8895e68487fea3d627227827b832ad2b7fdf56
 - inputs: acc1 (index 0 = 120000)
 - Payments sent to:
 - 1GPF1XuzKtCiy6QGCYtdRvTnNKCB82zkFK : 110000 (outside)
 -}

txD :: String
txD = "010000000156df7f2bad32b827782227d6a3fe8784e69588bfb62668d28f4b6d49a93de892000000006b483045022100965df46f1b89fed2e70cbf441ff75510bf508524b469db29b6235e2cd508f26002200e1ddd4abf0de739c140ebf8200a22c3f59a710164c9bedec3278ff660417b02012103a104bc20b43f7f10f89f0519b12bd828dfb7c7969e4c833eadfc6badda334bc0ffffffff01b0ad0100000000001976a914a8bebb715cea2ec4435464e2c42b838b28a24db788ac00000000"

testOrphan :: ( PersistStore m
              , PersistUnique m
              , PersistQuery m
              , PersistMonadBackend m ~ SqlBackend
              ) 
           => EitherT String m ()
testOrphan = do 
    cmdGenAddrs "acc1" 5
    cmdGenAddrs "acc2" 5
    cmdImportTx (decode' $ fromJust $ hexToBS txA)
    cmdSendMany "acc1" 
        [ ("14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS",100000) -- acc1
        , ("16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M",200000) -- acc2
        , ("14bbbNEnXgqEWF3GaVLewvPYHAenStYawj",150000) -- outside
        ] 10000
    cmdImportTx (decode' $ fromJust $ hexToBS txB)
    cmdSendMany "acc2" 
        [ ("14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb",120000) --acc1
        , ("1P5vxvRYdtkucT2JeXk4XEvp5xvs4XLV13", 40000) --outside
        ] 10000
    cmdImportTx (decode' $ fromJust $ hexToBS txC)
    cmdSend "acc1" "1GPF1XuzKtCiy6QGCYtdRvTnNKCB82zkFK" 110000 10000 --outside
    cmdImportTx (decode' $ fromJust $ hexToBS txD)
    return ()




