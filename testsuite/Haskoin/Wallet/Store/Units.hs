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

import Data.Time
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
        liftIO . (assertBool "Send Tx") . isRight =<< runEitherT testSend
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
        liftIO . assertEqual "acc ext gap" 34

    -- Check account internal gap
    (dbAccountIntGap . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc int gap" 29

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
        liftIO . assertEqual "acc ext gap" 34

    -- Check account internal gap
    (dbAccountIntGap . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc int gap" 33

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
        liftIO . assertEqual "acc ext gap 3" 33

    -- Check account internal gap
    (dbAccountIntGap . entityVal <$> dbGetAcc "ms1") >>= 
        liftIO . assertEqual "acc int gap 3" 29

    -- Count addresses
    count ([] :: [Filter (DbAddressGeneric b)]) >>= 
        liftIO . assertEqual "Count addr 3" 253

    let f x = (dbAddressBase58 x,dbAddressTree x,dbAddressIndex x)
    (map f <$> dbGenIntAddrs "ms1" 6) >>= liftIO . assertEqual "Internal addr 2"
        [ ("34rWmp9DmxFbqXLHvzhMGATWDfsnLF8wiR","m/3'/1/0/",0)
        , ("3As9nWqHcWavv3MxSeZKYjMQ9SgE8zVaJ1","m/3'/1/1/",1)
        , ("3QYVvQrjtK5w8s6uE8T8ZLeXBEe7aTtVdj","m/3'/1/2/",2)
        , ("3GS2h9uZ3akS9GQbXDGZtWgAvzLFnjWrsS","m/3'/1/3/",3)
        , ("3CAF8PpJirGsqTxzK2aPao2MQSGkiC3gPN","m/3'/1/4/",4)
        , ("3Kcd2Nz1XYv25U6xUzjAycVBz9rp6r2Tf2","m/3'/1/5/",5)
        ]

    -- Check account external index
    (dbAccountExtIndex . entityVal <$> dbGetAcc "ms1") >>= 
        liftIO . assertEqual "acc ext index 4" 3

    -- Check account internal index
    (dbAccountIntIndex . entityVal <$> dbGetAcc "ms1") >>= 
        liftIO . assertEqual "acc int index 4" 5

    -- Check account external gap
    (dbAccountExtGap . entityVal <$> dbGetAcc "ms1") >>= 
        liftIO . assertEqual "acc ext gap 4" 33

    -- Check account internal gap
    (dbAccountIntGap . entityVal <$> dbGetAcc "ms1") >>= 
        liftIO . assertEqual "acc int gap 4" 35

    -- Count addresses
    count ([] :: [Filter (DbAddressGeneric b)]) >>= 
        liftIO . assertEqual "Count addr 4" 259

    -- Rename the first multisig address label
    cmdLabel "ms1" 0 "alpha" >>= liftIO . assertEqual "label ms"
        ( object 
            [ "Addr"  .= T.pack "32VCGK4pbvVsFvSGfmLpmNTu4JjhuR3WmM"
            , "Key"   .= (0 :: Int)
            , "Tree"  .= T.pack "m/3'/0/0/"
            , "Label" .= T.pack "alpha"
            ]
        )

    -- Rename the last multisig address label
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

    -- List page 1 with 5 result per page
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
 - 1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi:100000 (in wallet)
 - 1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY:200000 (in wallet)
 - 1Azso1Fz77buNc8p7sm3myVXZxqwQopMtp:240000 (not in wallet)
 - 1L1ryKs82ucjNmGwKT9kAsxeSVX1mhJyo5:122000 (not in wallet)
 - 38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki:150000 (in wallet)
 - 3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q:400000 (in wallet)
 -}

-- ID: 753c9dee0fc4742fccf62783740643edeed6280bf333499bd4a8f086887611d0
tx1 :: String
tx1 = "010000000100000000000000000000000000000000000000000000000000000000000000010100000000ffffffff06a0860100000000001976a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac400d0300000000001976a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac80a90300000000001976a9146dab3dec58a7ab13267c4ec8c60b516cbe7a3c9f88ac90dc0100000000001976a914d0941a8b2ce829d8692bf6af24f67c485ff9a20b88acf04902000000000017a9144d769c08d79eed22532e044213bef3174f05158487801a06000000000017a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd08700000000"

-- ID: 46b0e8d7759c06670e70b7dd000cc7b3d92e205ecc7f759b03be6d68049ade70
tx2 :: String
tx2 = "010000000100000000000000000000000000000000000000000000000000000000000000020100000000ffffffff06a0860100000000001976a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac400d0300000000001976a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac80a90300000000001976a9146dab3dec58a7ab13267c4ec8c60b516cbe7a3c9f88ac90dc0100000000001976a914d0941a8b2ce829d8692bf6af24f67c485ff9a20b88acf04902000000000017a9144d769c08d79eed22532e044213bef3174f05158487801a06000000000017a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd08700000000"

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
                [ "TxID"    .= T.pack "753c9dee0fc4742fccf62783740643edeed6280bf333499bd4a8f086887611d0"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "753c9dee0fc4742fccf62783740643edeed6280bf333499bd4a8f086887611d0"
                , "Index"   .= (1 :: Int)
                , "Value"   .= (200000 :: Int)
                , "Script"  .= T.pack "76a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                ]
            , object
                [ "TxID"    .= T.pack "46b0e8d7759c06670e70b7dd000cc7b3d92e205ecc7f759b03be6d68049ade70"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "46b0e8d7759c06670e70b7dd000cc7b3d92e205ecc7f759b03be6d68049ade70"
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
                [ "TxID"   .= T.pack "753c9dee0fc4742fccf62783740643edeed6280bf333499bd4a8f086887611d0"
                , "Index"   .= (4 :: Int)
                , "Value"   .= (150000 :: Int)
                , "Script"  .= T.pack "a9144d769c08d79eed22532e044213bef3174f05158487"
                , "Redeem"  .= T.pack "5221026e294fcecdcbae12a0aba1685db35c54ddfc1375d48f96ff1b8805a4bb57bfc921028bc8d8377f44de8ac8beff0dc9ccefde4cd5dded7b8cd8babe02c7147a90ba6c21039cf5d06e79871043c420fabc652f8082e702e0094f91ec14c020e9fcf48fa4d853ae"
                , "Orphan"  .= False
                , "Address" .= T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                ]
            , object
                [ "TxID"   .= T.pack "753c9dee0fc4742fccf62783740643edeed6280bf333499bd4a8f086887611d0"
                , "Index"   .= (5 :: Int)
                , "Value"   .= (400000 :: Int)
                , "Script"  .= T.pack "a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd087"
                , "Redeem"  .= T.pack "5221020f7ead178316e8414d128712a23cde2e843d1a0f66afc0bfa600ab90deefd5f321023182b240cb2607ed03f76c9dca37c4b9fcb3b763b776223cc94808f7e67fb03a2102648dbcbc9f44fb55a992efe7b3ab214306cc72cdcae2a7cf6f9d44262a53c3b353ae"
                , "Orphan"  .= False
                , "Address" .= T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                ]
            , object
                [ "TxID"   .= T.pack "46b0e8d7759c06670e70b7dd000cc7b3d92e205ecc7f759b03be6d68049ade70"
                , "Index"   .= (4 :: Int)
                , "Value"   .= (150000 :: Int)
                , "Script"  .= T.pack "a9144d769c08d79eed22532e044213bef3174f05158487"
                , "Redeem"  .= T.pack "5221026e294fcecdcbae12a0aba1685db35c54ddfc1375d48f96ff1b8805a4bb57bfc921028bc8d8377f44de8ac8beff0dc9ccefde4cd5dded7b8cd8babe02c7147a90ba6c21039cf5d06e79871043c420fabc652f8082e702e0094f91ec14c020e9fcf48fa4d853ae"
                , "Orphan"  .= False
                , "Address" .= T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                ]
            , object
                [ "TxID"   .= T.pack "46b0e8d7759c06670e70b7dd000cc7b3d92e205ecc7f759b03be6d68049ade70"
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

    let txRes = "010000000270de9a04686dbe039b757fcc5e202ed9b3c70c00ddb7700e67069c75d7e8b046010000006b483045022100f21c2704ac49f4f687e82d5c4e353173445a90b357072048db2e263f0be674cd022057b07fb08f36b2ded3541cade6febc1c9a6151a49f4dbe21ff85703a6488e58701210250f4e42bb94ed8b27c6c8b728c0bd02828af1d4ebf8e3f0a6e7da3f53369c104ffffffffd011768886f0a8d49b4933f30b28d6eeed4306748327f6cc2f74c40fee9d3c75010000006b4830450221008140562ade35d7f3f7ea413620f1c1af118e45c27bd4d22309f67a06d9de1edc022073bc7f70feb2ab3f90c0493e45d27498998f44f85ba7344ee5c832c5161581ad01210250f4e42bb94ed8b27c6c8b728c0bd02828af1d4ebf8e3f0a6e7da3f53369c104ffffffff04c0d401000000000017a9144d769c08d79eed22532e044213bef3174f0515848780380100000000001976a914980b9c708958bbe4cc05d0b302d4f12625a5d88c88ace0220200000000001976a9147f77d7a91f8e53a387530c58139290211579dd2b88ac50c30000000000001976a914bb498fb0f0d0639193660b40a9a91e1b3eb60bab88ac00000000"

    cmdSendMany "" 
        [ ("38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki",120000) -- In wallet
        , ("1Erwcuqn1dHm8r6fNxogGmCHYuNfwKNwer", 80000) -- In wallet
        , ("1CczPAHXrwyiCeJfx5Bifaoo4NBQr3TPpJ",140000) -- Not in wallet
        ] 10000 >>= liftIO . assertEqual "sendMany tx"
            (object 
               [ "Payment Tx" .= T.pack txRes
               , "Complete" .= True
               ]
            )

    -- Check that the internal change address was correctly generated
    (dbAddressIndex . entityVal) <$> 
        (dbGetAddr "1J5HV12wGbPj5SUryku2zFoaxnC1AngqcH") >>= 
            liftIO . assertEqual "check internal address" 4

    (dbAddressTree . entityVal) <$> 
        (dbGetAddr "1J5HV12wGbPj5SUryku2zFoaxnC1AngqcH") >>= 
            liftIO . assertEqual "check internal address tree" "m/0'/1/4/"

    (dbAddressInternal . entityVal) <$> 
        (dbGetAddr "1J5HV12wGbPj5SUryku2zFoaxnC1AngqcH") >>= 
            liftIO . assertEqual "check internal address tree" True

    -- Check account external index
    (dbAccountExtIndex . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc ext index 5" 4

    -- Check account internal index
    (dbAccountIntIndex . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc int index 5" 4

    -- Check account external gap
    (dbAccountExtGap . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc ext gap 5" 34

    -- Check account internal gap
    (dbAccountIntGap . entityVal <$> dbGetAcc "") >>= 
        liftIO . assertEqual "acc int gap 5" 34

    -- Count addresses
    count ([] :: [Filter (DbAddressGeneric b)]) >>= 
        liftIO . assertEqual "Count addr 5" 260


    (cmdImportTx $ decode' $ fromJust $ hexToBS txRes) >>=
        liftIO . assertEqual "import send tx"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                             , T.pack "1CczPAHXrwyiCeJfx5Bifaoo4NBQr3TPpJ"
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
                [ "TxID"    .= T.pack "753c9dee0fc4742fccf62783740643edeed6280bf333499bd4a8f086887611d0"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "46b0e8d7759c06670e70b7dd000cc7b3d92e205ecc7f759b03be6d68049ade70"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "7c22633b8999e548df416410ac163e3a429852e8924f1e4eeac07f9de293f69c"
                , "Index"   .= (1 :: Int)
                , "Value"   .= (80000 :: Int)
                , "Script"  .= T.pack "76a914980b9c708958bbe4cc05d0b302d4f12625a5d88c88ac"
                , "Orphan"  .= False
                , "Address" .= T.pack "1Erwcuqn1dHm8r6fNxogGmCHYuNfwKNwer"
                ]
            , object
                [ "TxID"    .= T.pack "7c22633b8999e548df416410ac163e3a429852e8924f1e4eeac07f9de293f69c"
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
                         , T.pack "1CczPAHXrwyiCeJfx5Bifaoo4NBQr3TPpJ"
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
 - 1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL : 100000 (acc1)
 - 1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw : 200000 (acc1)
 - 17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG : 300000 (acc1)
 -}

txA :: String
txA = "010000000100000000000000000000000000000000000000000000000000000000000000090300000000ffffffff03a0860100000000001976a9148062ab5c3fdf5f8f0d41fccacbb3ea8058b911ae88ac400d0300000000001976a9148727e4552058a555d0ce269d8cf8c850785666f688ace0930400000000001976a9144c769509bb3e22c2275cd025fcb55ebc5dc1e39f88ac00000000"

{- TxID: d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c
 - inputs: acc1 (index 1 and index 2 = 500000)
 - Payments sent to:
 - 14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS : 100000 (acc1)
 - 16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M : 200000 (acc2)
 - 1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86 : 150000 (outside)
 - 1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2 :  40000 (change to acc1)
 -}

txB :: String
txB = "0100000002300ae5a1d2f502f490fbefd159406d07e249d18bb4422eb0d64663f34cd1e4bb010000006b4830450221009d5a3f9df597da4412466be55ccbc0b058b63507e6716e718b8af7a3e3656353022022f1e747a765b9a39888da2516f3efa7e1f9c728a510b63e88e6e6066cbdd465012103a40f6bd1d59440a007aa8ec93875d07f234ffebe76df311c1b610fc1c0d22dd9ffffffff300ae5a1d2f502f490fbefd159406d07e249d18bb4422eb0d64663f34cd1e4bb020000006b483045022100d5e70bd37192edbe7be4214500aab6fccd232c383f9bb658bd593375b33ffe4402205513fc737fe74d980245188b5dca7a9b2b0f062b6148a7af2e80018cff089a79012103a0d2cdf936eca39cfd6407393f8f0f2af932bdea6271d559308075b77d2ec080ffffffff04a0860100000000001976a914243d03889d49470ee721d44596fd146440e1167c88ac400d0300000000001976a9143f53fba59a1c17f17cf3c4b5cfcf15fa0087f1c188acf0490200000000001976a9149d3e18f3cd8edf442c31cb5cc5b0acf8e4e96a1e88ac409c0000000000001976a914740eb168ae243882a73f5467b9024431443ef12988ac00000000"

{- TxID: f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953
 - inputs: acc2 (index 1 = 200000)
 - Payments sent to:
 - 14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb : 120000 (acc1)
 - 13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n :  40000 (outside)
 - 13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J :  30000 (change to acc2)
 -}

txC :: String
txC = "01000000015c48a55db18d73a93f106ecbb1441a410727a5d514efa54b0a251f7c337afdd1010000006b483045022100c1782e52714104c75db350e052aa414ae2651d2acbacd8f8d5ca5374a85cf28f022049a6cfd44189452a35871f9b5b16ceb0dcf423cfc9726b01d54ed29c1316681c012103b4b925d5967a00d2540115f035aa10290853d39e3c20591d711680cac5e2b4efffffffff03c0d40100000000001976a91424cba659aad4563de9199f3fe273bac07f170eb088ac409c0000000000001976a91418dc74eb38930493ce81b8f1d3fd0f15e43e96b288ac30750000000000001976a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac00000000"

{- TxID: e2974336b16235d3ddddd15be19dcd3a9522d521601051063afdd542a1f34967
 - inputs: acc1 (index 0 = 120000)
 - Payments sent to:
 - 13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf : 110000 (outside)
 -}

txD :: String
txD = "01000000015339e5714d323a7923484eba38c57b971072e2ffe907767288fc1b4b5d568ef0000000006b483045022100d615482a9b22bf10505418772f39714931cb0bdd11419be69de97573a3ec4be10220207d9157c5c5853614280ddeaccb42f16c0ecc6f3fd6dbea20890c05a43305fd012103a104bc20b43f7f10f89f0519b12bd828dfb7c7969e4c833eadfc6badda334bc0ffffffff01b0ad0100000000001976a9141f69921a4f95254ee2aaa181381439bbc8b2645788ac00000000"

testOrphan :: ( PersistStore m
              , PersistUnique m
              , PersistQuery m
              , PersistMonadBackend m ~ SqlBackend
              ) 
           => EitherT String m ()
testOrphan = do 

    -- import transaction D
    cmdImportTx (decode' $ fromJust $ hexToBS txD) >>= 
        liftIO . assertEqual "import txD"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (0 :: Int)
                         , "Orphan"     .= True
                         ]
                ]
            )

    -- import transaction D a second time. Operation should be idempotent
    cmdImportTx (decode' $ fromJust $ hexToBS txD) >>= 
        liftIO . assertEqual "import txD"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (0 :: Int)
                         , "Orphan"     .= True
                         ]
                ]
            )

    (Entity ai1 acc1) <- dbGetAcc "acc1"
    (Entity ai2 acc2) <- dbGetAcc "acc2"

    let f (Entity _ c) = ( dbCoinTxid c, dbCoinPos c
                         , dbCoinValue c, dbCoinScript c
                         , dbCoinRdmScript c, dbCoinAddress c
                         , dbCoinSpent c, dbCoinOrphan c
                         )
        g (Entity _ t) = ( dbTxTxid t, dbTxRecipients t
                         , dbTxValue t, dbTxOrphan t
                         )

    ((map f) <$> selectList [DbCoinAccount ==. ai1] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check import txD coins"
            [ ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , 0, 0, "", Nothing 
              , "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
              , Just "e2974336b16235d3ddddd15be19dcd3a9522d521601051063afdd542a1f34967"
              , True
              )
            ]

    ((map g) <$> selectList [DbTxAccount ==. ai1] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check import txD tx"
            [ ( "e2974336b16235d3ddddd15be19dcd3a9522d521601051063afdd542a1f34967"
              , ["13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"]
              , 0
              , True
              )
            ]

    cmdCoins "acc1" >>= liftIO . assertEqual "Check empty coins txD" 
        (toJSON ([] :: [DbCoinGeneric b]))

    cmdListTx "acc1" >>= liftIO . assertEqual "Check listTx txD"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                         ]
                     , "Value"      .= (0 :: Int)
                     , "Orphan"     .= True
                     ]
            ]
        )

    cmdBalance "acc1" >>= liftIO . assertEqual "Check 0 balance txD" 
        (toJSON (0 :: Int))

    -- import transaction C
    cmdImportTx (decode' $ fromJust $ hexToBS txC) >>= 
        liftIO . assertEqual "import txC"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (0 :: Int)
                         , "Orphan"     .= True
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (-120000 :: Int)
                         , "Orphan"     .= False
                         ]
                ]
            )

    -- import transaction C again (operation should be idempotent)
    cmdImportTx (decode' $ fromJust $ hexToBS txC) >>= 
        liftIO . assertEqual "import txC 2"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (0 :: Int)
                         , "Orphan"     .= True
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (-120000 :: Int)
                         , "Orphan"     .= False
                         ]
                ]
            )

    -- The previous orphaned coin should be un-orphaned now
    ((map f) <$> selectList [DbCoinAccount ==. ai1] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check import txC coins 2"
            [ ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , 0, 120000
              , "76a91424cba659aad4563de9199f3fe273bac07f170eb088ac", Nothing 
              , "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
              , Just "e2974336b16235d3ddddd15be19dcd3a9522d521601051063afdd542a1f34967"
              , False
              )
            ]

    -- The creation time of the transactions should reflect their dependencies
    ((map g) <$> selectList [DbTxAccount ==. ai1] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check import txC tx"
            [ ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , ["14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"]
              , 120000
              , False
              )
            , ( "e2974336b16235d3ddddd15be19dcd3a9522d521601051063afdd542a1f34967"
              , ["13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"]
              , -120000
              , False
              )
            ]

    -- Check coins of account 2
    ((map f) <$> selectList [DbCoinAccount ==. ai2] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check import txC coins 3"
            [ ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , 1, 0
              , "", Nothing 
              , "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
              , Just "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953" 
              , True
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , 2, 30000
              , "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac", Nothing 
              , "13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J"
              , Nothing , False
              )
            ]

    -- Check transactions of account 2
    ((map g) <$> selectList [DbTxAccount ==. ai2] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check import txC tx 2"
            [ ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , [ "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                , "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                ]
              , 0, True
              )
            ]

    -- list cmdCoins of acc 1 (should be empty)
    cmdCoins "acc1" >>= liftIO . assertEqual "Check cmdCoins txC 1" 
        (toJSON ([] :: [DbCoinGeneric b]))

    -- list cmdCoins of acc 2
    cmdCoins "acc2" >>= liftIO . assertEqual "Check cmdCoins txC 2" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
                     , "Index"   .= (2 :: Int)
                     , "Value"   .= (30000 :: Int)
                     , "Script"  .= T.pack "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac"
                     , "Orphan"  .= False 
                     , "Address" .= T.pack "13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J"
                     ] 
            ]
        )

    -- list cmdListTx of acc1
    cmdListTx "acc1" >>= liftIO . assertEqual "Check listTx txC 1"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                         ]
                     , "Value"      .= (120000 :: Int)
                     , "Orphan"     .= False
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                         ]
                     , "Value"      .= (-120000 :: Int)
                     , "Orphan"     .= False
                     ]
            ]
        )

    -- list cmdListTx of acc2
    cmdListTx "acc2" >>= liftIO . assertEqual "Check listTx txC 2"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                         , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                         ]
                     , "Value"      .= (0 :: Int)
                     , "Orphan"     .= True
                     ]
            ]
        )

    -- Check balance of acc1
    cmdBalance "acc1" >>= liftIO . assertEqual "Check balance txC 1" 
        (toJSON (0 :: Int))

    -- Balance of acc2 is 30000 but pending orphaned coins
    cmdBalance "acc2" >>= liftIO . assertEqual "Check balance txC 2" 
        (toJSON (30000 :: Int))

    -- Importing txB
    cmdImportTx (decode' $ fromJust $ hexToBS txB) >>= 
        liftIO . assertEqual "import txB 1"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                             , T.pack "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                             ]
                         , "Value"      .= (0 :: Int)
                         , "Orphan"     .= True
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
                         , "Value"      .= (200000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (-170000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (-120000 :: Int)
                         , "Orphan"     .= False
                         ]
                ]
            )

    -- Importing txB again. Import is idempotent so this should work fine
    cmdImportTx (decode' $ fromJust $ hexToBS txB) >>= 
        liftIO . assertEqual "import txB 1"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                             , T.pack "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                             ]
                         , "Value"      .= (0 :: Int)
                         , "Orphan"     .= True
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
                         , "Value"      .= (200000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (-170000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (-120000 :: Int)
                         , "Orphan"     .= False
                         ]
                ]
            )

    -- List coins of account 1 after importing txB
    ((map f) <$> selectList [DbCoinAccount ==. ai1] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check import txB coins"
            [ ( "bbe4d14cf36346d6b02e42b48bd149e2076d4059d1effb90f402f5d2a1e50a30"
              , 1, 0
              , "", Nothing 
              , "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
              , Just "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , True
              )
            , ( "bbe4d14cf36346d6b02e42b48bd149e2076d4059d1effb90f402f5d2a1e50a30"
              , 2, 0
              , "", Nothing 
              , "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
              , Just "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , True
              )
            , ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , 0, 100000
              , "76a914243d03889d49470ee721d44596fd146440e1167c88ac", Nothing 
              , "14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS"
              , Nothing
              , False
              )
            , ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , 3, 40000
              , "76a914740eb168ae243882a73f5467b9024431443ef12988ac", Nothing 
              , "1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2"
              , Nothing
              , False
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , 0, 120000
              , "76a91424cba659aad4563de9199f3fe273bac07f170eb088ac", Nothing 
              , "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
              , Just "e2974336b16235d3ddddd15be19dcd3a9522d521601051063afdd542a1f34967"
              , False
              )
            ]

    -- List of transactions for account 1
    ((map g) <$> selectList [DbTxAccount ==. ai1] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check transactions import txB"
            [ ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                , "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                ]
              , 0
              , True
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , ["14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"]
              , 120000
              , False
              )
            , ( "e2974336b16235d3ddddd15be19dcd3a9522d521601051063afdd542a1f34967"
              , ["13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"]
              , -120000
              , False
              )
            ]

    -- List coins of account 2. The first coin is not orphaned anymore
    ((map f) <$> selectList [DbCoinAccount ==. ai2] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check coins txB 2"
            [ ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , 1, 200000
              , "76a9143f53fba59a1c17f17cf3c4b5cfcf15fa0087f1c188ac", Nothing 
              , "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
              , Just "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953" 
              , False
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , 2, 30000
              , "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac", Nothing 
              , "13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J"
              , Nothing , False
              )
            ]

    -- Check transactions of account 2
    ((map g) <$> selectList [DbTxAccount ==. ai2] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check transactions import txB 2"
            [ ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
              , 200000, False
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , [ "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                , "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                ]
              , -170000, False
              )
            ]

    -- list cmdCoins of acc 1
    cmdCoins "acc1" >>= liftIO . assertEqual "Check cmdCoins txB 1" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
                     , "Index"   .= (0 :: Int)
                     , "Value"   .= (100000 :: Int)
                     , "Script"  .= T.pack "76a914243d03889d49470ee721d44596fd146440e1167c88ac"
                     , "Orphan"  .= False 
                     , "Address" .= T.pack "14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS"
                     ] 
            , object [ "TxID"    .= T.pack "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
                     , "Index"   .= (3 :: Int)
                     , "Value"   .= (40000 :: Int)
                     , "Script"  .= T.pack "76a914740eb168ae243882a73f5467b9024431443ef12988ac"
                     , "Orphan"  .= False 
                     , "Address" .= T.pack "1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2"
                     ] 
            ]
        )

    -- list cmdCoins of acc 2
    cmdCoins "acc2" >>= liftIO . assertEqual "Check cmdCoins txB 2" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
                     , "Index"   .= (2 :: Int)
                     , "Value"   .= (30000 :: Int)
                     , "Script"  .= T.pack "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac"
                     , "Orphan"  .= False 
                     , "Address" .= T.pack "13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J"
                     ] 
            ]
        )

    -- list cmdListTx of acc1
    cmdListTx "acc1" >>= liftIO . assertEqual "Check listTx txB 1"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                         , T.pack "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                         ]
                     , "Value"      .= (0 :: Int)
                     , "Orphan"     .= True
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                         ]
                     , "Value"      .= (120000 :: Int)
                     , "Orphan"     .= False
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                         ]
                     , "Value"      .= (-120000 :: Int)
                     , "Orphan"     .= False
                     ]
            ]
        )

    -- list cmdListTx of acc2
    cmdListTx "acc2" >>= liftIO . assertEqual "Check listTx txB 2"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
                     , "Value"      .= (200000 :: Int)
                     , "Orphan"     .= False
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                         , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                         ]
                     , "Value"      .= (-170000 :: Int)
                     , "Orphan"     .= False
                     ]
            ]
        )

    -- Check balance of acc1
    cmdBalance "acc1" >>= liftIO . assertEqual "Check balance txB 1" 
        (toJSON (140000 :: Int))

    -- Balance of acc2 
    cmdBalance "acc2" >>= liftIO . assertEqual "Check balance txB 2" 
        (toJSON (30000 :: Int))


    -- Importing txA
    cmdImportTx (decode' $ fromJust $ hexToBS txA) >>= 
        liftIO . assertEqual "import txA 1"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                             , T.pack "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
                             , T.pack "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
                             ]
                         , "Value"      .= (600000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                             , T.pack "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                             ]
                         , "Value"      .= (-360000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
                         , "Value"      .= (200000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (-170000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (-120000 :: Int)
                         , "Orphan"     .= False
                         ]
                ]
            )

    -- Importing txA a second time
    cmdImportTx (decode' $ fromJust $ hexToBS txA) >>= 
        liftIO . assertEqual "import txA 2"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                             , T.pack "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
                             , T.pack "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
                             ]
                         , "Value"      .= (600000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                             , T.pack "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                             ]
                         , "Value"      .= (-360000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
                         , "Value"      .= (200000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (-170000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (-120000 :: Int)
                         , "Orphan"     .= False
                         ]
                ]
            )

    -- List coins of account 1 after importing txA
    ((map f) <$> selectList [DbCoinAccount ==. ai1] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check import txB coins"
            [ ( "bbe4d14cf36346d6b02e42b48bd149e2076d4059d1effb90f402f5d2a1e50a30"
              , 0, 100000
              , "76a9148062ab5c3fdf5f8f0d41fccacbb3ea8058b911ae88ac", Nothing 
              , "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
              , Nothing
              , False
              )
            , ( "bbe4d14cf36346d6b02e42b48bd149e2076d4059d1effb90f402f5d2a1e50a30"
              , 1, 200000
              , "76a9148727e4552058a555d0ce269d8cf8c850785666f688ac", Nothing 
              , "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
              , Just "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , False
              )
            , ( "bbe4d14cf36346d6b02e42b48bd149e2076d4059d1effb90f402f5d2a1e50a30"
              , 2, 300000
              , "76a9144c769509bb3e22c2275cd025fcb55ebc5dc1e39f88ac", Nothing 
              , "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
              , Just "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , False
              )
            , ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , 0, 100000
              , "76a914243d03889d49470ee721d44596fd146440e1167c88ac", Nothing 
              , "14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS"
              , Nothing
              , False
              )
            , ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , 3, 40000
              , "76a914740eb168ae243882a73f5467b9024431443ef12988ac", Nothing 
              , "1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2"
              , Nothing
              , False
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , 0, 120000
              , "76a91424cba659aad4563de9199f3fe273bac07f170eb088ac", Nothing 
              , "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
              , Just "e2974336b16235d3ddddd15be19dcd3a9522d521601051063afdd542a1f34967"
              , False
              )
            ]

    -- List of transactions for account 1
    ((map g) <$> selectList [DbTxAccount ==. ai1] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check transactions import txA"
            [ ( "bbe4d14cf36346d6b02e42b48bd149e2076d4059d1effb90f402f5d2a1e50a30"
              , [ "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                , "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
                , "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
                ]
              , 600000
              , False
              )
            , ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                , "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                ]
              , -360000
              , False
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , ["14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"]
              , 120000
              , False
              )
            , ( "e2974336b16235d3ddddd15be19dcd3a9522d521601051063afdd542a1f34967"
              , ["13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"]
              , -120000
              , False
              )
            ]

    -- List coins of account 2
    ((map f) <$> selectList [DbCoinAccount ==. ai2] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check coins txA 2"
            [ ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , 1, 200000
              , "76a9143f53fba59a1c17f17cf3c4b5cfcf15fa0087f1c188ac", Nothing 
              , "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
              , Just "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953" 
              , False
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , 2, 30000
              , "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac", Nothing 
              , "13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J"
              , Nothing , False
              )
            ]

    -- Check transactions of account 2
    ((map g) <$> selectList [DbTxAccount ==. ai2] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check transactions import txA 2"
            [ ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
              , 200000, False
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , [ "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                , "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                ]
              , -170000, False
              )
            ]

    -- list cmdCoins of acc 1
    cmdCoins "acc1" >>= liftIO . assertEqual "Check cmdCoins txA 1" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "bbe4d14cf36346d6b02e42b48bd149e2076d4059d1effb90f402f5d2a1e50a30"
                     , "Index"   .= (0 :: Int)
                     , "Value"   .= (100000 :: Int)
                     , "Script"  .= T.pack "76a9148062ab5c3fdf5f8f0d41fccacbb3ea8058b911ae88ac"
                     , "Orphan"  .= False 
                     , "Address" .= T.pack "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                     ] 
            , object [ "TxID"    .= T.pack "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
                     , "Index"   .= (0 :: Int)
                     , "Value"   .= (100000 :: Int)
                     , "Script"  .= T.pack "76a914243d03889d49470ee721d44596fd146440e1167c88ac"
                     , "Orphan"  .= False 
                     , "Address" .= T.pack "14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS"
                     ] 
            , object [ "TxID"    .= T.pack "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
                     , "Index"   .= (3 :: Int)
                     , "Value"   .= (40000 :: Int)
                     , "Script"  .= T.pack "76a914740eb168ae243882a73f5467b9024431443ef12988ac"
                     , "Orphan"  .= False 
                     , "Address" .= T.pack "1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2"
                     ] 
            ]
        )

    -- list cmdCoins of acc 2
    cmdCoins "acc2" >>= liftIO . assertEqual "Check cmdCoins txA 2" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
                     , "Index"   .= (2 :: Int)
                     , "Value"   .= (30000 :: Int)
                     , "Script"  .= T.pack "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac"
                     , "Orphan"  .= False 
                     , "Address" .= T.pack "13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J"
                     ] 
            ]
        )

    -- list cmdListTx of acc1
    cmdListTx "acc1" >>= liftIO . assertEqual "Check listTx txA 1"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                         , T.pack "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
                         , T.pack "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
                         ]
                     , "Value"      .= (600000 :: Int)
                     , "Orphan"     .= False
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                         , T.pack "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                         ]
                     , "Value"      .= (-360000 :: Int)
                     , "Orphan"     .= False
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                         ]
                     , "Value"      .= (120000 :: Int)
                     , "Orphan"     .= False
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                         ]
                     , "Value"      .= (-120000 :: Int)
                     , "Orphan"     .= False
                     ]
            ]
        )

    -- list cmdListTx of acc2
    cmdListTx "acc2" >>= liftIO . assertEqual "Check listTx txA 2"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
                     , "Value"      .= (200000 :: Int)
                     , "Orphan"     .= False
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                         , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                         ]
                     , "Value"      .= (-170000 :: Int)
                     , "Orphan"     .= False
                     ]
            ]
        )

    -- Check balance of acc1
    cmdBalance "acc1" >>= liftIO . assertEqual "Check balance txA 1" 
        (toJSON (240000 :: Int))

    -- Balance of acc2 
    cmdBalance "acc2" >>= liftIO . assertEqual "Check balance txA 2" 
        (toJSON (30000 :: Int))

    -- Re-Importing a transaction in the middle of the chain
    cmdImportTx (decode' $ fromJust $ hexToBS txC) >>= 
        liftIO . assertEqual "import txC BIS"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (-170000 :: Int)
                         , "Orphan"     .= False
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (-120000 :: Int)
                         , "Orphan"     .= False
                         ]
                ]
            )

    -- Verify that the transaction orders haven't changed
    ((map g) <$> selectList [DbTxAccount ==. ai1] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Last order verification"
            [ ( "bbe4d14cf36346d6b02e42b48bd149e2076d4059d1effb90f402f5d2a1e50a30"
              , [ "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                , "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
                , "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
                ]
              , 600000
              , False
              )
            , ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                , "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                ]
              , -360000
              , False
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , ["14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"]
              , 120000
              , False
              )
            , ( "e2974336b16235d3ddddd15be19dcd3a9522d521601051063afdd542a1f34967"
              , ["13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"]
              , -120000
              , False
              )
            ]

    -- Verify that the transaction order haven't changed
    ((map g) <$> selectList [DbTxAccount ==. ai2] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Last order verification 2"
            [ ( "d1fd7a337c1f250a4ba5ef14d5a52707411a44b1cb6e103fa9738db15da5485c"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
              , 200000, False
              )
            , ( "f08e565d4b1bfc88727607e9ffe27210977bc538ba4e4823793a324d71e53953"
              , [ "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                , "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                ]
              , -170000, False
              )
            ]


testSend :: ( PersistStore m
            , PersistUnique m
            , PersistQuery m
            , PersistMonadBackend m ~ SqlBackend
            ) 
         => EitherT String m ()
testSend = do 

    -- Send some money to an outside address
    cmdSendMany "acc1" 
        [ ("19w9Btacp9tYgbhWE9d8yEdhR15XcjE9XZ", 20000) -- outside
        , ("1D8KWhk1x2EGqEUXi1GMJmqmRRWebH8XmT", 30000) -- outside
        ] 10000 >>= liftIO . assertEqual "send normal tx"
            ( object [ "Payment Tx" .= T.pack "0100000001300ae5a1d2f502f490fbefd159406d07e249d18bb4422eb0d64663f34cd1e4bb000000006a4730440220797af18bf7fab837fb1da3e6336888a31d95923b11991a89af60ed40b17267220220793302283c195c9653c6f5b9a391eaa3f07eb5e2b5b0cf6e05809ed175f2f9270121020ab91e1cdcaf0d13ca13f1b0b975184882a90ee369f4ac6eab00caacca36b423ffffffff03204e0000000000001976a91461fe4d90fbb250c29e24aaff95f14218c0d39f3388ac30750000000000001976a9148503dfd0281b679af0770d5894cdf0402047befa88ac409c0000000000001976a9146d1523de2ac60a29e60f6584956032054302e73a88ac00000000"
                     , "Complete" .= True
                     ]
            )

    -- Same as before but set the amount such that the change would
    -- result in dust. The dust should be donated as tx fee
    cmdSendMany "acc1" 
        [ ("19w9Btacp9tYgbhWE9d8yEdhR15XcjE9XZ", 44000) -- outside
        , ("1D8KWhk1x2EGqEUXi1GMJmqmRRWebH8XmT", 44000) -- outside
        ] 10000 >>= liftIO . assertEqual "send normal tx no change"
            ( object [ "Payment Tx" .= T.pack "0100000001300ae5a1d2f502f490fbefd159406d07e249d18bb4422eb0d64663f34cd1e4bb000000006b483045022100bfaa5b7722eccc63572612d33ad89d862d1ce4e84e99f52225ff36afea59dece022055772a11923391deda445cdc79163c48034e51fa04aabb986b657baf9e54fab60121020ab91e1cdcaf0d13ca13f1b0b975184882a90ee369f4ac6eab00caacca36b423ffffffff02e0ab0000000000001976a91461fe4d90fbb250c29e24aaff95f14218c0d39f3388ace0ab0000000000001976a9148503dfd0281b679af0770d5894cdf0402047befa88ac00000000"
                     , "Complete" .= True
                     ]
            )

    let emptyTx = "0100000001300ae5a1d2f502f490fbefd159406d07e249d18bb4422eb0d64663f34cd1e4bb0000000000ffffffff02e0ab0000000000001976a91461fe4d90fbb250c29e24aaff95f14218c0d39f3388ace0ab0000000000001976a9148503dfd0281b679af0770d5894cdf0402047befa88ac00000000"

    -- Sign an empty shell transaction with cmdSignTx
    cmdSignTx "acc1" (decode' $ fromJust $ hexToBS emptyTx) (SigAll False) >>= 
        liftIO . assertEqual "sign empty transaction"
            ( object [ "Tx"       .= T.pack "0100000001300ae5a1d2f502f490fbefd159406d07e249d18bb4422eb0d64663f34cd1e4bb000000006b483045022100bfaa5b7722eccc63572612d33ad89d862d1ce4e84e99f52225ff36afea59dece022055772a11923391deda445cdc79163c48034e51fa04aabb986b657baf9e54fab60121020ab91e1cdcaf0d13ca13f1b0b975184882a90ee369f4ac6eab00caacca36b423ffffffff02e0ab0000000000001976a91461fe4d90fbb250c29e24aaff95f14218c0d39f3388ace0ab0000000000001976a9148503dfd0281b679af0770d5894cdf0402047befa88ac00000000"
                     , "Complete" .= True
                     ]
            )



