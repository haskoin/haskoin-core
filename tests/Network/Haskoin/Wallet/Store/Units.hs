{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Store.Units (tests) where

import Test.HUnit (Assertion, assertBool, assertEqual)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.Either (EitherT, runEitherT)

import Data.Maybe (fromJust, isJust)
import Data.Yaml as YAML
    ( Value
    , object 
    , toJSON
    , (.=)
    )
import qualified Data.Text as T (pack)

import Database.Persist 
    ( PersistStore
    , PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , Entity(..)
    , entityVal
    , getBy
    , selectList
    , count
    , (==.)
    , Filter
    , SelectOpt(Asc)
    )
import Database.Persist.Sqlite (SqlBackend, runSqlite, runMigration)

import Network.Haskoin.Wallet
import Network.Haskoin.Wallet.Store
import Network.Haskoin.Wallet.Store.DbAddress
import Network.Haskoin.Wallet.Store.DbAccount
import Network.Haskoin.Wallet.Store.Util
import Network.Haskoin.Script
import Network.Haskoin.Util

tests :: [Test]
tests =
    [ testGroup "Wallet persistence tests" 
        [ testCase "Wallet tests" runTests
        ] 
    ]

runTests :: Assertion
runTests = do
    _ <- runSqlite ":memory:" $ runEitherT $ do
        lift $ runMigration migrateAll
        liftIO . (assertBool "Pre init") . isRight =<< runEitherT testPreInit
        liftIO . (assertBool "Init") . isRight =<< runEitherT testInit
        liftIO . (assertBool "New Acc") . isRight =<< runEitherT testNewAcc
        liftIO . (assertBool "New MS") . isRight =<< runEitherT testNewMS
        liftIO . (assertBool "Gen Addr") . isRight =<< runEitherT testGenAddr
        liftIO . (assertBool "Import Tx") . isRight =<< runEitherT testImport
        liftIO . (assertBool "Orphan Tx") . isRight =<< runEitherT testOrphan
        liftIO . (assertBool "Send Tx") . isRight =<< runEitherT testSend
        liftIO . (assertBool "Utilities") . isRight =<< runEitherT testUtil
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
    _ <- cmdInit "Hello World" 

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

    let h x = (dbAddressBase58 x,dbAddressTree x,dbAddressIndex x)
    (map h <$> dbGenIntAddrs "ms1" 6) >>= liftIO . assertEqual "Internal addr 2"
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
    _ <- cmdLabel "ms1" 3 "beta"

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

-- ID: a156f308b7c3dab42d98729fefc3dfa944d84f3ac48c7e603a58885d2b34a996
tx1 :: String
tx1 = "01000000010000000000000000000000000000000000000000000000000000000000000001010000006b483045022100bf1c6e0720284bcefa2e104b5eea27fb3f11a8ebce1d7c06a99567473f9524a202201ed0baafcac25f9aa3c81fb26a3476f559169f4c7f5a64e43a2eebb6b0a1aae001210290a14bce9d363667574a29da1b2e38d106968969f449588713bb271e28a9a4a0ffffffff06a0860100000000001976a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac400d0300000000001976a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac80a90300000000001976a9146dab3dec58a7ab13267c4ec8c60b516cbe7a3c9f88ac90dc0100000000001976a914d0941a8b2ce829d8692bf6af24f67c485ff9a20b88acf04902000000000017a9144d769c08d79eed22532e044213bef3174f05158487801a06000000000017a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd08700000000"

-- ID: 319f26fb4638f038008e3a9da98177f7a7cfaee64bab77328cf69e39c7fce785
tx2 :: String
tx2 = "01000000010000000000000000000000000000000000000000000000000000000000000002010000006b483045022100bf1c6e0720284bcefa2e104b5eea27fb3f11a8ebce1d7c06a99567473f9524a202201ed0baafcac25f9aa3c81fb26a3476f559169f4c7f5a64e43a2eebb6b0a1aae001210290a14bce9d363667574a29da1b2e38d106968969f449588713bb271e28a9a4a0ffffffff06a0860100000000001976a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac400d0300000000001976a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac80a90300000000001976a9146dab3dec58a7ab13267c4ec8c60b516cbe7a3c9f88ac90dc0100000000001976a914d0941a8b2ce829d8692bf6af24f67c485ff9a20b88acf04902000000000017a9144d769c08d79eed22532e044213bef3174f05158487801a06000000000017a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd08700000000"

testImport :: ( PersistStore m, PersistUnique m, PersistQuery m
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
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                             , T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                             ]
                         , "Value"      .= (550000 :: Int)
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
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                             , T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                             ]
                         , "Value"      .= (550000 :: Int)
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
                        ]
            , object [ "Recipients" .= toJSON
                            [ T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                            , T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                            ]
                        , "Value"      .= (300000 :: Int)
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
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                         , T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                         ]
                     , "Value"      .= (550000 :: Int)
                     ]
            ]
        )

    -- Verify the balance of account ""
    cmdBalance "" >>= liftIO . assertEqual "Balance 1" 
        (object ["Balance" .= (600000 :: Int)])

    -- Verify the balance of account "ms1"
    cmdBalance "ms1" >>= liftIO . assertEqual "Balance 2" 
        (object ["Balance" .= (1100000 :: Int)])

    -- Get coins of account ""
    cmdCoins "" >>= liftIO . assertEqual "Get coins 1"
        ( toJSON
            [ object
                [ "TxID"    .= T.pack "a156f308b7c3dab42d98729fefc3dfa944d84f3ac48c7e603a58885d2b34a996"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "a156f308b7c3dab42d98729fefc3dfa944d84f3ac48c7e603a58885d2b34a996"
                , "Index"   .= (1 :: Int)
                , "Value"   .= (200000 :: Int)
                , "Script"  .= T.pack "76a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac"
                , "Address" .= T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                ]
            , object
                [ "TxID"    .= T.pack "319f26fb4638f038008e3a9da98177f7a7cfaee64bab77328cf69e39c7fce785"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "319f26fb4638f038008e3a9da98177f7a7cfaee64bab77328cf69e39c7fce785"
                , "Index"   .= (1 :: Int)
                , "Value"   .= (200000 :: Int)
                , "Script"  .= T.pack "76a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac"
                , "Address" .= T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                ]
            ]
        )

    -- Get coins of account "ms1"
    cmdCoins "ms1" >>= liftIO . assertEqual "Get coins 2"
        ( toJSON
            [ object
                [ "TxID"   .= T.pack "a156f308b7c3dab42d98729fefc3dfa944d84f3ac48c7e603a58885d2b34a996"
                , "Index"   .= (4 :: Int)
                , "Value"   .= (150000 :: Int)
                , "Script"  .= T.pack "a9144d769c08d79eed22532e044213bef3174f05158487"
                , "Redeem"  .= T.pack "5221026e294fcecdcbae12a0aba1685db35c54ddfc1375d48f96ff1b8805a4bb57bfc921028bc8d8377f44de8ac8beff0dc9ccefde4cd5dded7b8cd8babe02c7147a90ba6c21039cf5d06e79871043c420fabc652f8082e702e0094f91ec14c020e9fcf48fa4d853ae"
                , "Address" .= T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                ]
            , object
                [ "TxID"   .= T.pack "a156f308b7c3dab42d98729fefc3dfa944d84f3ac48c7e603a58885d2b34a996"
                , "Index"   .= (5 :: Int)
                , "Value"   .= (400000 :: Int)
                , "Script"  .= T.pack "a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd087"
                , "Redeem"  .= T.pack "5221020f7ead178316e8414d128712a23cde2e843d1a0f66afc0bfa600ab90deefd5f321023182b240cb2607ed03f76c9dca37c4b9fcb3b763b776223cc94808f7e67fb03a2102648dbcbc9f44fb55a992efe7b3ab214306cc72cdcae2a7cf6f9d44262a53c3b353ae"
                , "Address" .= T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                ]
            , object
                [ "TxID"   .= T.pack "319f26fb4638f038008e3a9da98177f7a7cfaee64bab77328cf69e39c7fce785"
                , "Index"   .= (4 :: Int)
                , "Value"   .= (150000 :: Int)
                , "Script"  .= T.pack "a9144d769c08d79eed22532e044213bef3174f05158487"
                , "Redeem"  .= T.pack "5221026e294fcecdcbae12a0aba1685db35c54ddfc1375d48f96ff1b8805a4bb57bfc921028bc8d8377f44de8ac8beff0dc9ccefde4cd5dded7b8cd8babe02c7147a90ba6c21039cf5d06e79871043c420fabc652f8082e702e0094f91ec14c020e9fcf48fa4d853ae"
                , "Address" .= T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                ]
            , object
                [ "TxID"   .= T.pack "319f26fb4638f038008e3a9da98177f7a7cfaee64bab77328cf69e39c7fce785"
                , "Index"   .= (5 :: Int)
                , "Value"   .= (400000 :: Int)
                , "Script"  .= T.pack "a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd087"
                , "Redeem"  .= T.pack "5221020f7ead178316e8414d128712a23cde2e843d1a0f66afc0bfa600ab90deefd5f321023182b240cb2607ed03f76c9dca37c4b9fcb3b763b776223cc94808f7e67fb03a2102648dbcbc9f44fb55a992efe7b3ab214306cc72cdcae2a7cf6f9d44262a53c3b353ae"
                , "Address" .= T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                ]
            ]
        )

    -- ID: 9062c5ae9a4e84e37e712387a111f2dba6be57de4235df8dd1fae67cc2071771
    -- Input: 200000 and 200000 = 400000
    -- Output = 340000 + 50000 change = 390000
    -- Fee = 10000

    let txRes = "010000000285e7fcc7399ef68c3277ab4be6aecfa7f77781a99d3a8e0038f03846fb269f31010000006a47304402200f4bc4dba8e47e810362d48502329a75624d8863d8bd22618bd47c5fff8bd9e402200531318470a60b4d898b2061c3eef96ad9129ca758e7f55bd120a81569a7196201210250f4e42bb94ed8b27c6c8b728c0bd02828af1d4ebf8e3f0a6e7da3f53369c104ffffffff96a9342b5d88583a607e8cc43a4fd844a9dfc3ef9f72982db4dac3b708f356a1010000006a4730440220566d57667786d93551976f2bf890de9d623820c02c2e6d4476a71c27b771eae6022004227d27972a1f0933bf50472d85644cc5dbbc277f1370c171f80e95ed834ca401210250f4e42bb94ed8b27c6c8b728c0bd02828af1d4ebf8e3f0a6e7da3f53369c104ffffffff04c0d401000000000017a9144d769c08d79eed22532e044213bef3174f0515848780380100000000001976a914980b9c708958bbe4cc05d0b302d4f12625a5d88c88ace0220200000000001976a9147f77d7a91f8e53a387530c58139290211579dd2b88ac50c30000000000001976a914bb498fb0f0d0639193660b40a9a91e1b3eb60bab88ac00000000"

    cmdSendMany "" 
        [ ("38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki",120000) -- In wallet
        , ("1Erwcuqn1dHm8r6fNxogGmCHYuNfwKNwer", 80000) -- In wallet
        , ("1CczPAHXrwyiCeJfx5Bifaoo4NBQr3TPpJ",140000) -- Not in wallet
        ] 10000 >>= liftIO . assertEqual "sendMany tx"
            (object 
               [ "Tx" .= T.pack txRes
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
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki" ]
                         , "Value"      .= (120000 :: Int)
                         ]
                ]
            )

    -- Get coins of account ""
    cmdCoins "" >>= liftIO . assertEqual "Get coins 2"
        ( toJSON
            [ object
                [ "TxID"    .= T.pack "a156f308b7c3dab42d98729fefc3dfa944d84f3ac48c7e603a58885d2b34a996"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "319f26fb4638f038008e3a9da98177f7a7cfaee64bab77328cf69e39c7fce785"
                , "Index"   .= (0 :: Int)
                , "Value"   .= (100000 :: Int)
                , "Script"  .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Address" .= T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                ]
            , object
                [ "TxID"    .= T.pack "9062c5ae9a4e84e37e712387a111f2dba6be57de4235df8dd1fae67cc2071771"
                , "Index"   .= (1 :: Int)
                , "Value"   .= (80000 :: Int)
                , "Script"  .= T.pack "76a914980b9c708958bbe4cc05d0b302d4f12625a5d88c88ac"
                , "Address" .= T.pack "1Erwcuqn1dHm8r6fNxogGmCHYuNfwKNwer"
                ]
            , object
                [ "TxID"    .= T.pack "9062c5ae9a4e84e37e712387a111f2dba6be57de4235df8dd1fae67cc2071771"
                , "Index"   .= (3 :: Int)
                , "Value"   .= (50000 :: Int)
                , "Script"  .= T.pack "76a914bb498fb0f0d0639193660b40a9a91e1b3eb60bab88ac"
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
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi"
                         , T.pack "1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY"
                         ]
                     , "Value"      .= (300000 :: Int)
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki"
                         , T.pack "1CczPAHXrwyiCeJfx5Bifaoo4NBQr3TPpJ"
                         ]
                     , "Value"      .= (-270000 :: Int)
                     ]
            ]
        )
            
    -- Verify the balance of account ""
    cmdBalance "" >>= liftIO . assertEqual "Balance 3" 
        (object ["Balance" .= (330000 :: Int)])

    -- Verify the balance of account "ms1"
    cmdBalance "ms1" >>= liftIO . assertEqual "Balance 4" 
        (object ["Balance" .= (1220000 :: Int)])

-- Building tx link:
-- txA : outside => acc1
-- txB : acc1 => (acc1,acc2,outside)
-- txC : acc2 => (acc1,outside) 
-- tcD : acc1 => outside
-- import order: txC, tcB, txA

{- TxID: 23c5dcac6ef51d36ca63cf7f3dc30163939bb208eb8e12b92183ccc1333c20bd
 - Payments sent to:
 - 1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL : 100000 (acc1)
 - 1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw : 200000 (acc1)
 - 17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG : 300000 (acc1)
 -}

txA :: String
txA = "01000000010000000000000000000000000000000000000000000000000000000000000001090000006a4730440220464686ba44f82d76bc3687399f971fe86661241b9698308711e099fab785ef140220074de9e525ae379fc7ab676fbeb475e7c8aa7268afb0c3f48ea18137bf069f7d01210290a14bce9d363667574a29da1b2e38d106968969f449588713bb271e28a9a4a0ffffffff03a0860100000000001976a9148062ab5c3fdf5f8f0d41fccacbb3ea8058b911ae88ac400d0300000000001976a9148727e4552058a555d0ce269d8cf8c850785666f688ace0930400000000001976a9144c769509bb3e22c2275cd025fcb55ebc5dc1e39f88ac00000000"

{- TxID: 53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0
 - inputs: acc1 (index 1 and index 2 = 500000)
 - Payments sent to:
 - 14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS : 100000 (acc1)
 - 16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M : 200000 (acc2)
 - 1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86 : 150000 (outside)
 - 1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2 :  40000 (change to acc1)
 -}

txB :: String
txB = "0100000002bd203c33c1cc8321b9128eeb08b29b936301c33d7fcf63ca361df56eacdcc523010000006b4830450221009c061ca53ccfdd379f883857466103308f54c764e7acd94f5eb23d40bb5ce1cc022074f40ed51bbdc295307db177c15679939e104bcc8944b54eb3ac681992f27a10012103a40f6bd1d59440a007aa8ec93875d07f234ffebe76df311c1b610fc1c0d22dd9ffffffffbd203c33c1cc8321b9128eeb08b29b936301c33d7fcf63ca361df56eacdcc523020000006a47304402204e3c85a423066ef44a7d25d0ee0352d8dd5e7628e989aa9f0833abe681405610022030638452839cbb210bf7589e16beb0a737ecbc4a6560e28cd3b11517c83ff225012103a0d2cdf936eca39cfd6407393f8f0f2af932bdea6271d559308075b77d2ec080ffffffff04a0860100000000001976a914243d03889d49470ee721d44596fd146440e1167c88ac400d0300000000001976a9143f53fba59a1c17f17cf3c4b5cfcf15fa0087f1c188acf0490200000000001976a9149d3e18f3cd8edf442c31cb5cc5b0acf8e4e96a1e88ac409c0000000000001976a914740eb168ae243882a73f5467b9024431443ef12988ac00000000"

{- TxID: 2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150
 - inputs: acc2 (index 1 = 200000)
 - Payments sent to:
 - 14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb : 120000 (acc1)
 - 13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n :  40000 (outside)
 - 13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J :  30000 (change to acc2)
 -}

txC :: String
txC = "0100000001d0a3cbf9246be9519374c96b551545050b01d53c9d33e2ff3af59e571411fe53010000006b483045022100ff41f0cb1ce0f7f6d07b93c9f3c43480f843092a14618b2b20c2fad90d003216022049069460cbf2355f03ed31ac25cc086057aa752bd53f9de558f33e565369d11e012103b4b925d5967a00d2540115f035aa10290853d39e3c20591d711680cac5e2b4efffffffff03c0d40100000000001976a91424cba659aad4563de9199f3fe273bac07f170eb088ac409c0000000000001976a91418dc74eb38930493ce81b8f1d3fd0f15e43e96b288ac30750000000000001976a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac00000000"

{- TxID: 03e0ce2206a300bfc718afd25e1d34bc3a0b61e570913c4ab94884219b031582
 - inputs: acc1 (index 0 = 120000)
 - Payments sent to:
 - 13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf : 110000 (outside)
 -}

txD :: String
txD = "010000000150b18a410fdc69cf463134a357c6ef0c5b0651723dda6164c300ffe356db5522000000006b483045022100f9a43bc03aa44ea2e97873d73f522680a7967f71ad9406de380aafd8f1afc1260220790f82a126d2a5a5404ea2632c99e90e55484e1d16dba1842884bc740add3f23012103a104bc20b43f7f10f89f0519b12bd828dfb7c7969e4c833eadfc6badda334bc0ffffffff01b0ad0100000000001976a9141f69921a4f95254ee2aaa181381439bbc8b2645788ac00000000"

testOrphan :: ( PersistStore m, PersistUnique m, PersistQuery m
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
        liftIO . assertEqual "import txD 2"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (0 :: Int)
                         , "Orphan"     .= True
                         ]
                ]
            )

    (Entity ai1 _) <- dbGetAcc "acc1"
    (Entity ai2 _) <- dbGetAcc "acc2"

    let f (Entity _ c) = ( dbCoinTxid c, dbCoinPos c
                         , dbCoinValue c, dbCoinScript c
                         , dbCoinRdmScript c, dbCoinAddress c
                         , dbCoinStatus c, dbCoinOrphan c
                         )
        g (Entity _ t) = ( dbTxTxid t, dbTxRecipients t
                         , dbTxValue t, dbTxOrphan t, dbTxPartial t
                         )

    ((map f) <$> selectList [DbCoinAccount ==. ai1] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check import txD coins"
            [ ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , 0, 0, "", Nothing 
              , "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
              , Spent "03e0ce2206a300bfc718afd25e1d34bc3a0b61e570913c4ab94884219b031582"
              , True
              )
            ]

    ((map g) <$> selectList [DbTxAccount ==. ai1] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check import txD tx"
            [ ( "03e0ce2206a300bfc718afd25e1d34bc3a0b61e570913c4ab94884219b031582"
              , ["13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"]
              , 0, True, False
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
        (object ["Balance" .= (0 :: Int)])

    -- import transaction C
    cmdImportTx (decode' $ fromJust $ hexToBS txC) >>= 
        liftIO . assertEqual "import txC"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
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
                         ]
                ]
            )

    -- The previous orphaned coin should be un-orphaned now
    ((map f) <$> selectList [DbCoinAccount ==. ai1] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check import txC coins 2"
            [ ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , 0, 120000
              , "76a91424cba659aad4563de9199f3fe273bac07f170eb088ac", Nothing 
              , "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
              , Spent "03e0ce2206a300bfc718afd25e1d34bc3a0b61e570913c4ab94884219b031582"
              , False
              )
            ]

    -- The creation time of the transactions should reflect their dependencies
    ((map g) <$> selectList [DbTxAccount ==. ai1] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check import txC tx"
            [ ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , ["14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"]
              , 120000, False, False
              )
            , ( "03e0ce2206a300bfc718afd25e1d34bc3a0b61e570913c4ab94884219b031582"
              , ["13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"]
              , -120000, False, False
              )
            ]

    -- Check coins of account 2
    ((map f) <$> selectList [DbCoinAccount ==. ai2] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check import txC coins 3"
            [ ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , 1, 0
              , "", Nothing 
              , "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
              , Spent "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150" 
              , True
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , 2, 30000
              , "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac", Nothing 
              , "13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J"
              , Unspent , False
              )
            ]

    -- Check transactions of account 2
    ((map g) <$> selectList [DbTxAccount ==. ai2] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check import txC tx 2"
            [ ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , [ "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                , "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                ]
              , 0, True, False
              )
            ]

    -- list cmdCoins of acc 1 (should be empty)
    cmdCoins "acc1" >>= liftIO . assertEqual "Check cmdCoins txC 1" 
        (toJSON ([] :: [DbCoinGeneric b]))

    -- list cmdCoins of acc 2
    cmdCoins "acc2" >>= liftIO . assertEqual "Check cmdCoins txC 2" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
                     , "Index"   .= (2 :: Int)
                     , "Value"   .= (30000 :: Int)
                     , "Script"  .= T.pack "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac"
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
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                         ]
                     , "Value"      .= (-120000 :: Int)
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
        (object ["Balance" .= (0 :: Int)])

    -- Balance of acc2 is 30000 but pending orphaned coins
    cmdBalance "acc2" >>= liftIO . assertEqual "Check balance txC 2" 
        (object ["Balance" .= (30000 :: Int)])

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
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (-170000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (-120000 :: Int)
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
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (-170000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (-120000 :: Int)
                         ]
                ]
            )

    -- List coins of account 1 after importing txB
    ((map f) <$> selectList [DbCoinAccount ==. ai1] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check import txB coins"
            [ ( "23c5dcac6ef51d36ca63cf7f3dc30163939bb208eb8e12b92183ccc1333c20bd"
              , 1, 0
              , "", Nothing 
              , "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
              , Spent "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , True
              )
            , ( "23c5dcac6ef51d36ca63cf7f3dc30163939bb208eb8e12b92183ccc1333c20bd"
              , 2, 0
              , "", Nothing 
              , "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
              , Spent "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , True
              )
            , ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , 0, 100000
              , "76a914243d03889d49470ee721d44596fd146440e1167c88ac", Nothing 
              , "14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS"
              , Unspent
              , False
              )
            , ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , 3, 40000
              , "76a914740eb168ae243882a73f5467b9024431443ef12988ac", Nothing 
              , "1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2"
              , Unspent
              , False
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , 0, 120000
              , "76a91424cba659aad4563de9199f3fe273bac07f170eb088ac", Nothing 
              , "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
              , Spent "03e0ce2206a300bfc718afd25e1d34bc3a0b61e570913c4ab94884219b031582"
              , False
              )
            ]

    -- List of transactions for account 1
    ((map g) <$> selectList [DbTxAccount ==. ai1] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check transactions import txB"
            [ ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                , "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                ]
              , 0, True, False
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , ["14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"]
              , 120000, False, False
              )
            , ( "03e0ce2206a300bfc718afd25e1d34bc3a0b61e570913c4ab94884219b031582"
              , ["13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"]
              , -120000, False, False
              )
            ]

    -- List coins of account 2. The first coin is not orphaned anymore
    ((map f) <$> selectList [DbCoinAccount ==. ai2] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check coins txB 2"
            [ ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , 1, 200000
              , "76a9143f53fba59a1c17f17cf3c4b5cfcf15fa0087f1c188ac", Nothing 
              , "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
              , Spent "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150" 
              , False
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , 2, 30000
              , "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac", Nothing 
              , "13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J"
              , Unspent , False
              )
            ]

    -- Check transactions of account 2
    ((map g) <$> selectList [DbTxAccount ==. ai2] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check transactions import txB 2"
            [ ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
              , 200000, False, False
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , [ "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                , "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                ]
              , -170000, False, False
              )
            ]

    -- list cmdCoins of acc 1
    cmdCoins "acc1" >>= liftIO . assertEqual "Check cmdCoins txB 1" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
                     , "Index"   .= (0 :: Int)
                     , "Value"   .= (100000 :: Int)
                     , "Script"  .= T.pack "76a914243d03889d49470ee721d44596fd146440e1167c88ac"
                     , "Address" .= T.pack "14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS"
                     ] 
            , object [ "TxID"    .= T.pack "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
                     , "Index"   .= (3 :: Int)
                     , "Value"   .= (40000 :: Int)
                     , "Script"  .= T.pack "76a914740eb168ae243882a73f5467b9024431443ef12988ac"
                     , "Address" .= T.pack "1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2"
                     ] 
            ]
        )

    -- list cmdCoins of acc 2
    cmdCoins "acc2" >>= liftIO . assertEqual "Check cmdCoins txB 2" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
                     , "Index"   .= (2 :: Int)
                     , "Value"   .= (30000 :: Int)
                     , "Script"  .= T.pack "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac"
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
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                         ]
                     , "Value"      .= (-120000 :: Int)
                     ]
            ]
        )

    -- list cmdListTx of acc2
    cmdListTx "acc2" >>= liftIO . assertEqual "Check listTx txB 2"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
                     , "Value"      .= (200000 :: Int)
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                         , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                         ]
                     , "Value"      .= (-170000 :: Int)
                     ]
            ]
        )

    -- Check balance of acc1
    cmdBalance "acc1" >>= liftIO . assertEqual "Check balance txB 1" 
        (object ["Balance" .= (140000 :: Int)])

    -- Balance of acc2 
    cmdBalance "acc2" >>= liftIO . assertEqual "Check balance txB 2" 
        (object ["Balance" .= (30000 :: Int)])

    -- Importing txA (Partial)
    cmdImportTx (decode' $ fromJust $ hexToBS txA) >>= 
        liftIO . assertEqual "import txA 1"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                             , T.pack "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
                             , T.pack "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
                             ]
                         , "Value"      .= (600000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                             , T.pack "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                             ]
                         , "Value"      .= (-360000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
                         , "Value"      .= (200000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (-170000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf" ]
                         , "Value"      .= (-120000 :: Int)
                         ]
                ]
            )

    -- Importing txA a second time
    cmdImportTx (decode' $ fromJust $ hexToBS txA) >>= 
        liftIO . assertEqual "import txA 1"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                             , T.pack "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
                             , T.pack "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
                             ]
                         , "Value"      .= (600000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                             , T.pack "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                             ]
                         , "Value"      .= (-360000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
                         , "Value"      .= (200000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (-170000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf" ]
                         , "Value"      .= (-120000 :: Int)
                         ]
                ]
            )

    -- List coins of account 1 after importing txA
    ((map f) <$> selectList [DbCoinAccount ==. ai1] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check import txB coins"
            [ ( "23c5dcac6ef51d36ca63cf7f3dc30163939bb208eb8e12b92183ccc1333c20bd"
              , 0, 100000
              , "76a9148062ab5c3fdf5f8f0d41fccacbb3ea8058b911ae88ac", Nothing 
              , "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
              , Unspent, False
              )
            , ( "23c5dcac6ef51d36ca63cf7f3dc30163939bb208eb8e12b92183ccc1333c20bd"
              , 1, 200000
              , "76a9148727e4552058a555d0ce269d8cf8c850785666f688ac", Nothing 
              , "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
              , Spent "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , False
              )
            , ( "23c5dcac6ef51d36ca63cf7f3dc30163939bb208eb8e12b92183ccc1333c20bd"
              , 2, 300000
              , "76a9144c769509bb3e22c2275cd025fcb55ebc5dc1e39f88ac", Nothing 
              , "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
              , Spent "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , False
              )
            , ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , 0, 100000
              , "76a914243d03889d49470ee721d44596fd146440e1167c88ac", Nothing 
              , "14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS"
              , Unspent, False
              )
            , ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , 3, 40000
              , "76a914740eb168ae243882a73f5467b9024431443ef12988ac", Nothing 
              , "1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2"
              , Unspent, False
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , 0, 120000
              , "76a91424cba659aad4563de9199f3fe273bac07f170eb088ac", Nothing 
              , "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
              , Spent "03e0ce2206a300bfc718afd25e1d34bc3a0b61e570913c4ab94884219b031582"
              , False
              )
            ]

    -- List of transactions for account 1
    ((map g) <$> selectList [DbTxAccount ==. ai1] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check transactions import txA"
            [ ( "23c5dcac6ef51d36ca63cf7f3dc30163939bb208eb8e12b92183ccc1333c20bd"
              , [ "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                , "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
                , "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
                ]
              , 600000, False, False
              )
            , ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                , "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                ]
              , -360000, False, False
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , ["14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"]
              , 120000, False, False
              )
            , ( "03e0ce2206a300bfc718afd25e1d34bc3a0b61e570913c4ab94884219b031582"
              , ["13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"]
              , -120000, False, False
              )
            ]

    -- List coins of account 2
    ((map f) <$> selectList [DbCoinAccount ==. ai2] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check coins txA 2"
            [ ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , 1, 200000
              , "76a9143f53fba59a1c17f17cf3c4b5cfcf15fa0087f1c188ac", Nothing 
              , "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
              , Spent "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150" 
              , False
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , 2, 30000
              , "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac", Nothing 
              , "13wD1L9PvEgBytP5X6ykiuhB8gRP58CB5J"
              , Unspent , False
              )
            ]

    -- Check transactions of account 2
    ((map g) <$> selectList [DbTxAccount ==. ai2] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check transactions import txA 2"
            [ ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
              , 200000, False, False
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , [ "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                , "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                ]
              , -170000, False, False
              )
            ]

    -- list cmdCoins of acc 1
    cmdCoins "acc1" >>= liftIO . assertEqual "Check cmdCoins txA 1" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "23c5dcac6ef51d36ca63cf7f3dc30163939bb208eb8e12b92183ccc1333c20bd"
                     , "Index"   .= (0 :: Int)
                     , "Value"   .= (100000 :: Int)
                     , "Script"  .= T.pack "76a9148062ab5c3fdf5f8f0d41fccacbb3ea8058b911ae88ac"
                     , "Address" .= T.pack "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                     ] 
            , object [ "TxID"    .= T.pack "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
                     , "Index"   .= (0 :: Int)
                     , "Value"   .= (100000 :: Int)
                     , "Script"  .= T.pack "76a914243d03889d49470ee721d44596fd146440e1167c88ac"
                     , "Address" .= T.pack "14JcRDidCbYFBwWjP9PGJL1MRKCzUWCmaS"
                     ] 
            , object [ "TxID"    .= T.pack "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
                     , "Index"   .= (3 :: Int)
                     , "Value"   .= (40000 :: Int)
                     , "Script"  .= T.pack "76a914740eb168ae243882a73f5467b9024431443ef12988ac"
                     , "Address" .= T.pack "1Baez98Lapiu7mQLfXuUCjBreEAwFrWNd2"
                     ] 
            ]
        )

    -- list cmdCoins of acc 2
    cmdCoins "acc2" >>= liftIO . assertEqual "Check cmdCoins txA 2" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
                     , "Index"   .= (2 :: Int)
                     , "Value"   .= (30000 :: Int)
                     , "Script"  .= T.pack "76a9142030bc3fec2b3783acd9e83f6e24d57568be69a988ac"
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
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                         , T.pack "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                         ]
                     , "Value"      .= (-360000 :: Int)
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                         ]
                     , "Value"      .= (120000 :: Int)
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                         ]
                     , "Value"      .= (-120000 :: Int)
                     ]
            ]
        )

    -- list cmdListTx of acc2
    cmdListTx "acc2" >>= liftIO . assertEqual "Check listTx txA 2"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
                     , "Value"      .= (200000 :: Int)
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                         , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                         ]
                     , "Value"      .= (-170000 :: Int)
                     ]
            ]
        )

    -- Check balance of acc1
    cmdBalance "acc1" >>= liftIO . assertEqual "Check balance txA 1" 
        (object ["Balance" .= (240000 :: Int)])

    -- Balance of acc2 
    cmdBalance "acc2" >>= liftIO . assertEqual "Check balance txA 2" 
        (object ["Balance" .= (30000 :: Int)])

    -- Re-Importing a transaction in the middle of the chain
    cmdImportTx (decode' $ fromJust $ hexToBS txC) >>= 
        liftIO . assertEqual "import txC BIS"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             ]
                         , "Value"      .= (120000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                             , T.pack "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                             ]
                         , "Value"      .= (-170000 :: Int)
                         ]
                , object [ "Recipients" .= toJSON
                             [ T.pack "13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"
                             ]
                         , "Value"      .= (-120000 :: Int)
                         ]
                ]
            )

    -- Verify that the transaction orders haven't changed
    ((map g) <$> selectList [DbTxAccount ==. ai1] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Last order verification"
            [ ( "23c5dcac6ef51d36ca63cf7f3dc30163939bb208eb8e12b92183ccc1333c20bd"
              , [ "1ChqhDvLVx5bjRHbdUweCsd4mgwD4fvdGL"
                , "1DKe1dvRznGqmBFsHY7MmvJy3DtcfBDZbw"
                , "17yJQpBHpWyVHNshgGpCK876Nb8qqvp3rG"
                ]
              , 600000, False, False
              )
            , ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M"
                , "1FLRUj4iGRZHpr9WJq3RG64cL2vEbccQ86"
                ]
              , -360000, False, False
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , ["14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"]
              , 120000, False, False
              )
            , ( "03e0ce2206a300bfc718afd25e1d34bc3a0b61e570913c4ab94884219b031582"
              , ["13s6R8TRWTk5DZaSQ2pKn3hfdugvEZEdZf"]
              , -120000, False, False
              )
            ]

    -- Verify that the transaction order haven't changed
    ((map g) <$> selectList [DbTxAccount ==. ai2] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Last order verification 2"
            [ ( "53fe1114579ef53affe2339d3cd5010b054515556bc9749351e96b24f9cba3d0"
              , [ "16mrBKvB9DV5YAYw7a8kYDvqwh1tJGMR5M" ]
              , 200000, False, False
              )
            , ( "2255db56e3ff00c36461da3d7251065b0cefc657a3343146cf69dc0f418ab150"
              , [ "14MZHk4dkM3ZM7bBcET4ELuyozXqCCsQpb"
                , "13GTKCtWbpRpPGca3uhTjwZWiQcqAMPh6n"
                ]
              , -170000, False, False
              )
            ]


testSend :: ( PersistStore m, PersistUnique m, PersistQuery m
            , PersistMonadBackend m ~ SqlBackend
            ) 
         => EitherT String m ()
testSend = do 

    -- Send some money to an outside address
    cmdSendMany "acc1" 
        [ ("19w9Btacp9tYgbhWE9d8yEdhR15XcjE9XZ", 20000) -- outside
        , ("1D8KWhk1x2EGqEUXi1GMJmqmRRWebH8XmT", 30000) -- outside
        ] 10000 >>= liftIO . assertEqual "send normal tx"
            ( object [ "Tx" .= T.pack "0100000001bd203c33c1cc8321b9128eeb08b29b936301c33d7fcf63ca361df56eacdcc523000000006b483045022100b45842dae5f62c6564c9133891bab410ec8b9129580bf98762fc66ae6be45b6c02207cf90bdc82e2d538a46e3580a7b639737c2d334cdd74df65ee24770d2ac8fb940121020ab91e1cdcaf0d13ca13f1b0b975184882a90ee369f4ac6eab00caacca36b423ffffffff03204e0000000000001976a91461fe4d90fbb250c29e24aaff95f14218c0d39f3388ac30750000000000001976a9148503dfd0281b679af0770d5894cdf0402047befa88ac409c0000000000001976a9146d1523de2ac60a29e60f6584956032054302e73a88ac00000000"
                     , "Complete" .= True
                     ]
            )

    -- Same as before but set the amount such that the change would
    -- result in dust. The dust should be donated as tx fee
    cmdSendMany "acc1" 
        [ ("19w9Btacp9tYgbhWE9d8yEdhR15XcjE9XZ", 44000) -- outside
        , ("1D8KWhk1x2EGqEUXi1GMJmqmRRWebH8XmT", 44000) -- outside
        ] 10000 >>= liftIO . assertEqual "send normal tx no change"
            ( object [ "Tx" .= T.pack "0100000001bd203c33c1cc8321b9128eeb08b29b936301c33d7fcf63ca361df56eacdcc523000000006a47304402207a68a43941c387787a611b7775e487dced559f7331f00aa143d87bf4c6dc447802203d56aa3c9e3b209da4396e771c734d68b6b99c52528c8255dc552fe42a7d67be0121020ab91e1cdcaf0d13ca13f1b0b975184882a90ee369f4ac6eab00caacca36b423ffffffff02e0ab0000000000001976a91461fe4d90fbb250c29e24aaff95f14218c0d39f3388ace0ab0000000000001976a9148503dfd0281b679af0770d5894cdf0402047befa88ac00000000"
                     , "Complete" .= True
                     ]
            )

    let emptyTx = "0100000001bd203c33c1cc8321b9128eeb08b29b936301c33d7fcf63ca361df56eacdcc5230000000000ffffffff02e0ab0000000000001976a91461fe4d90fbb250c29e24aaff95f14218c0d39f3388ace0ab0000000000001976a9148503dfd0281b679af0770d5894cdf0402047befa88ac00000000"

    -- Sign an empty shell transaction with cmdSignTx
    cmdSignTx "acc1" (decode' $ fromJust $ hexToBS emptyTx) (SigAll False) >>= 
        liftIO . assertEqual "sign empty transaction"
            ( object [ "Tx"       .= T.pack "0100000001bd203c33c1cc8321b9128eeb08b29b936301c33d7fcf63ca361df56eacdcc523000000006a47304402207a68a43941c387787a611b7775e487dced559f7331f00aa143d87bf4c6dc447802203d56aa3c9e3b209da4396e771c734d68b6b99c52528c8255dc552fe42a7d67be0121020ab91e1cdcaf0d13ca13f1b0b975184882a90ee369f4ac6eab00caacca36b423ffffffff02e0ab0000000000001976a91461fe4d90fbb250c29e24aaff95f14218c0d39f3388ace0ab0000000000001976a9148503dfd0281b679af0770d5894cdf0402047befa88ac00000000"
                     , "Complete" .= True
                     ]
            )

    -- Sign an empty shell transaction with cmdSignTx. Should fail as
    -- acc2 does not have any of the inputs
    cmdSignTx "acc2" (decode' $ fromJust $ hexToBS emptyTx) (SigAll False) >>= 
        liftIO . assertEqual "sign empty transaction 2"
            ( object [ "Tx"       .= T.pack emptyTx
                     , "Complete" .= False
                     ]
            )

    -- Sending more coins than you have should fail
    (runEitherT $ cmdSendMany "acc1" 
        [ ("15WAh7HRms3HDFLEAtMuBM4aUcwvN7B2QY", 300000) -- outside
        , ("1CzjVJfn1RmNes4ZEKGRA8VJcpCB1xfNvp", 110000) -- outside
        ] 10000) >>= liftIO . assertEqual "Send too many coins"
            (Left "chooseCoins: No solution found")

    -- Spending coins from a multisignature account
    cmdSendMany "ms1" 
        [ ("15WAh7HRms3HDFLEAtMuBM4aUcwvN7B2QY", 300000) -- outside
        , ("1CzjVJfn1RmNes4ZEKGRA8VJcpCB1xfNvp", 110000) -- outside
        ] 10000 >>= liftIO . assertEqual "Send coins from multisig coins"
            ( object [ "Tx" .= T.pack "0100000003711707c27ce6fad18ddf3542de57bea6dbf211a18723717ee3844e9aaec5629000000000b50047304402206fa37ea2fcc6467c788b321be6c8db6b52c1c328dfe3397c1d0c3f06b13ef1bc02203afdcca94843391019cf0a38331a85f7ae5b24df28c397adb4ebecbd5390db2901004c695221026e294fcecdcbae12a0aba1685db35c54ddfc1375d48f96ff1b8805a4bb57bfc921028bc8d8377f44de8ac8beff0dc9ccefde4cd5dded7b8cd8babe02c7147a90ba6c21039cf5d06e79871043c420fabc652f8082e702e0094f91ec14c020e9fcf48fa4d853aeffffffff85e7fcc7399ef68c3277ab4be6aecfa7f77781a99d3a8e0038f03846fb269f3104000000b600483045022100caeda000f0a7144721439020edee8c9f20f41afc63288ea732b2483f1f3b77a202207d727f06e2a5661eff977a7f29bf563a9e3fc11bc3e191a246a722cfc9ab9a6601004c695221026e294fcecdcbae12a0aba1685db35c54ddfc1375d48f96ff1b8805a4bb57bfc921028bc8d8377f44de8ac8beff0dc9ccefde4cd5dded7b8cd8babe02c7147a90ba6c21039cf5d06e79871043c420fabc652f8082e702e0094f91ec14c020e9fcf48fa4d853aeffffffff96a9342b5d88583a607e8cc43a4fd844a9dfc3ef9f72982db4dac3b708f356a104000000b500473044022070bdccc43981a4a341f27b4e8ed146301ce82c1c571f483b7d457f20bc754b4402201deefab82e52a0235c336fd24e7ccf1ec47b5751f4b8a472b0a23543d9b7e42401004c695221026e294fcecdcbae12a0aba1685db35c54ddfc1375d48f96ff1b8805a4bb57bfc921028bc8d8377f44de8ac8beff0dc9ccefde4cd5dded7b8cd8babe02c7147a90ba6c21039cf5d06e79871043c420fabc652f8082e702e0094f91ec14c020e9fcf48fa4d853aeffffffff02e0930400000000001976a9143164a7f65ce9d354a8ded150547a9523df056b3588acb0ad0100000000001976a91483948f86afa40df4836e88f402aff1e540469ba588ac00000000"
                     , "Complete" .= False
                     ]
            )

    _ <- cmdNewMS "signms" 2 3 []
    
    _ <- cmdAddKeys "signms" [fromJust $ xPubImport "xpub69QmF5x2m5duxo856Dg622vDjsxtDCwMAaLyhVjBZDDTaKbenBfMByNJVMYKAVTEpGPDePdgjDpDyPvBPx4WpFV8zmNEa6Cx7Lk4Bn9PDcE"]

    _ <- cmdAddKeys "signms" [fromJust $ xPubImport "xpub69QmF5x2m5dv1r2xNBLTq6HGKjFDyE4iAhxhdL4FgVR8vQprruLcauxJNdASANJaZc65bmoD1snK4db9DDYAuEBo3ANZik9SHnTAKQZLDt1"]

--    cmdNewMS "test2" 2 3 []
--    cmdNewMS "test3" 2 3 []
--
--    deleteBy $ UniqueAccName "signms"
--    deleteBy $ UniqueAccName "test2"
--
--    cmdAddKeys "test3" [fromJust $ xPubImport "xpub69QmF5x2m5duvyfWu3DjU9r9CkTKp5poGCnVLBmRBgJaye7LQ9yPs55z7zPZDpp6X4BHru9EpF8hqQGNa2ipEmBx9rYq6LdvNht6M2qqxUm"]
--
--    cmdAddKeys "test3" [fromJust $ xPubImport "xpub69QmF5x2m5duxo856Dg622vDjsxtDCwMAaLyhVjBZDDTaKbenBfMByNJVMYKAVTEpGPDePdgjDpDyPvBPx4WpFV8zmNEa6Cx7Lk4Bn9PDcE"]

{- TxID: 62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589
 - Input: 03-01
 - Outputs:
 - 3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E : 100000
 - 3EBJj6Z2FrrGUytJabTrJRD6mxCgF3d7JW : 200000
 - 3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR : 150000
 - 3AWd6mZHrvMbocVRp752ExTGnUhh1huWQp : 180000 
 -}

    let fundingTx = decode' $ fromJust $ hexToBS "01000000010000000000000000000000000000000000000000000000000000000000000001030000006a47304402206992bec0f95102a9994dd77e0b910af0b19b0b94d94423c68bb49ed57890238402203b15cbb6e1edcb686891138b057acac4cb67247f6bf693f38bd35d8c6605bca201210290a14bce9d363667574a29da1b2e38d106968969f449588713bb271e28a9a4a0ffffffff04a08601000000000017a91491b00b8d4d4b8909d5d17fe8da3b09fbd64e003c87400d03000000000017a91488fb2eb63c1f992d801f538340180527510464c787f04902000000000017a9148c0ad01947abab4985837a24be0d2141dcba06398720bf02000000000017a91460c1eb9034130684ece4594384e984325d1280ad8700000000"

{- TxID: 74ea8680000dd5e4f0122e47c73de7b0b02aae478a03930c5f547f67ed3dabcb
 - Inputs: 1,2,3
 - Outputs: 
 - 13LjjLQGGiTL8AYMbYqBmQVyAMyQnjPZK3 : 300000
 - 1MK3xFQSekQPuety1oaVTmvLRusFYsTq3d : 140000
 - 3CDgiQ2TnKvy1AovxwFy2jsDqURN8bq856 :  80000 (Change)
 -}
    let partialTx = "01000000038935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76202000000b6004830450221009d876833374da1714aa42945b73fe2af9ee37fdf32b4db9dd10297c0a9a18257022065618d04ead4ba5e4201bf15f149f1429e83173d42e31246d3164f2072e6605901004c69522102779d361a5d534dfca9d480eb60a1b14741a98d29f5c984fc1afd68a8c50981232102f579364c0d0971ec9ae52f2fc870204bce00438be1b6fa695d27c9d01cfb0956210383787221b68f601aa4695c27e7eebb0d15de2ed7ef7b9981fba8cad71ae2d84253aeffffffff8935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76203000000b600483045022100f8f9274fcf04200031578f5bfa0ecd108f5c945d7efef5f4fe519943d945594502200941f22088458750894005366a74857b72fb59197405bd2298947d96456d165b01004c6952210285bd8161363c6e2d2c075e08ed8a956c4706ccd77497a31327bf92b2208c3587210286e3177a8ca61d2d9426b4230735631e8552cc4d6e87e1277960461b0d26aac42103e2e138589e064f347238c2ca46c1d7deb8dd9472d065d7c5cb0f91aa5cc210a953aeffffffff8935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76201000000b50047304402200469180f9e94bf6d1d03ce07411bfd107b78f2fb1b1a862fc39dfeece417d14b02206696f963f97d3a6aaddc295c8eaf1846fef07f44c46d3aa97f09a4c0b72476c101004c695221023206a385544defd500a54d3d3fd8982d35fa7f877fa721beb59e440186183e8a210352b7c1f82593abebcb4ad0bbf28a4317b49efc1cc3e647453c09c6375122e8f621035c9ca1f7825d2cf025f42e636f57f193dbaae5c18b08dca10bf996ddb58eb8e853aeffffffff03e0930400000000001976a91419abd2efb4cd44b132ffd6206aa35e60f9d3e4b988ace0220200000000001976a914decc77b87d2199a51532034ad63f59cae6e6c4f588ac803801000000000017a914737e1e10bbf4290c82681745b5a54449ca83d1848700000000"

-- ID: a7274a15719099c5de37741bfd70efa15bb22a9de5507d966ccdf528a4f8f4c3

    let finalTx = "01000000038935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76202000000fdfd00004830450221009d876833374da1714aa42945b73fe2af9ee37fdf32b4db9dd10297c0a9a18257022065618d04ead4ba5e4201bf15f149f1429e83173d42e31246d3164f2072e6605901473044022065316243de4edd6e1741580a11489a5445b4fc35efcb13aa6189f1a524f6745a022003fdf5273c90ac7d906cba33f42661dcac45e80aab9aecd4fcd5ed5e9595c239014c69522102779d361a5d534dfca9d480eb60a1b14741a98d29f5c984fc1afd68a8c50981232102f579364c0d0971ec9ae52f2fc870204bce00438be1b6fa695d27c9d01cfb0956210383787221b68f601aa4695c27e7eebb0d15de2ed7ef7b9981fba8cad71ae2d84253aeffffffff8935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76203000000fdfd000047304402205c1bd8303745cbcc3f3412ee6f19090519ef2b72a03c799325ebd2188783bc670220298b8b13cf726beccf0909c83016a22c8429df4a2982f7ea54b24f40dd6a27ac01483045022100f8f9274fcf04200031578f5bfa0ecd108f5c945d7efef5f4fe519943d945594502200941f22088458750894005366a74857b72fb59197405bd2298947d96456d165b014c6952210285bd8161363c6e2d2c075e08ed8a956c4706ccd77497a31327bf92b2208c3587210286e3177a8ca61d2d9426b4230735631e8552cc4d6e87e1277960461b0d26aac42103e2e138589e064f347238c2ca46c1d7deb8dd9472d065d7c5cb0f91aa5cc210a953aeffffffff8935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76201000000fdfd000047304402200469180f9e94bf6d1d03ce07411bfd107b78f2fb1b1a862fc39dfeece417d14b02206696f963f97d3a6aaddc295c8eaf1846fef07f44c46d3aa97f09a4c0b72476c101483045022100e865ba910c96823cc9ddf6b65d62f6d107ca5589a866f3814d048ada70a01b3102201025101604ccf2f639a7f7176517665656b04c9a531d3e542eb495095148b802014c695221023206a385544defd500a54d3d3fd8982d35fa7f877fa721beb59e440186183e8a210352b7c1f82593abebcb4ad0bbf28a4317b49efc1cc3e647453c09c6375122e8f621035c9ca1f7825d2cf025f42e636f57f193dbaae5c18b08dca10bf996ddb58eb8e853aeffffffff03e0930400000000001976a91419abd2efb4cd44b132ffd6206aa35e60f9d3e4b988ace0220200000000001976a914decc77b87d2199a51532034ad63f59cae6e6c4f588ac803801000000000017a914737e1e10bbf4290c82681745b5a54449ca83d1848700000000"

    liftIO $ assertBool "Partial transaction not complete" $ 
        not $ isTxComplete (decode' $ fromJust $ hexToBS partialTx)

    liftIO $ assertBool "Final transaction complete" $ 
        isTxComplete (decode' $ fromJust $ hexToBS finalTx)

    -- trying to sign the multisig transaction before importing the transaction
    -- funding the account. This should return the original transaction
    cmdSignTx "signms" (decode' $ fromJust $ hexToBS partialTx) (SigAll False)
        >>= liftIO . assertEqual "Sign partial multisig without coins" 
            ( object [ "Tx"       .= T.pack partialTx
                     , "Complete" .= False
                     ]
            )

    -- Import the funding transaction into the wallet
    cmdImportTx fundingTx >>= liftIO . assertEqual "Importing funding tx"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
                         , T.pack "3EBJj6Z2FrrGUytJabTrJRD6mxCgF3d7JW"
                         , T.pack "3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR"
                         , T.pack "3AWd6mZHrvMbocVRp752ExTGnUhh1huWQp"
                         ]
                     , "Value"      .= (630000:: Int)
                     ]
            ]
        )

    -- Import the funding transaction again. The process should be idempotent
    cmdImportTx fundingTx >>= liftIO . assertEqual "Importing funding tx"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
                         , T.pack "3EBJj6Z2FrrGUytJabTrJRD6mxCgF3d7JW"
                         , T.pack "3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR"
                         , T.pack "3AWd6mZHrvMbocVRp752ExTGnUhh1huWQp"
                         ]
                     , "Value"      .= (630000:: Int)
                     ]
            ]
        )

    -- We should have 4 coins in the database
    cmdCoins "signms" >>= liftIO . assertEqual "Check coins after funding tx import" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
                     , "Index"   .= (0 :: Int)
                     , "Value"   .= (100000 :: Int)
                     , "Script"  .= T.pack "a91491b00b8d4d4b8909d5d17fe8da3b09fbd64e003c87"
                     , "Redeem"  .= T.pack "522102690c830a595721558e2bf6c58f6ed269c3b9fa194523326bb26d9a20b0cf49532102c98a1bde30c9fd0b71e88792b512963ce8b956f8735dbb0b1a2d6da7f1385e8d2102e7166192bf2bb2433daad4ab396fc2dc09785df716ca992f8bf76ff0aed6695053ae"
                     , "Address" .= T.pack "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
                     ] 
            , object [ "TxID"    .= T.pack "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
                     , "Index"   .= (1 :: Int)
                     , "Value"   .= (200000 :: Int)
                     , "Script"  .= T.pack "a91488fb2eb63c1f992d801f538340180527510464c787"
                     , "Redeem"  .= T.pack "5221023206a385544defd500a54d3d3fd8982d35fa7f877fa721beb59e440186183e8a210352b7c1f82593abebcb4ad0bbf28a4317b49efc1cc3e647453c09c6375122e8f621035c9ca1f7825d2cf025f42e636f57f193dbaae5c18b08dca10bf996ddb58eb8e853ae"
                     , "Address" .= T.pack "3EBJj6Z2FrrGUytJabTrJRD6mxCgF3d7JW"
                     ] 
            , object [ "TxID"    .= T.pack "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
                     , "Index"   .= (2 :: Int)
                     , "Value"   .= (150000 :: Int)
                     , "Script"  .= T.pack "a9148c0ad01947abab4985837a24be0d2141dcba063987"
                     , "Redeem"  .= T.pack "522102779d361a5d534dfca9d480eb60a1b14741a98d29f5c984fc1afd68a8c50981232102f579364c0d0971ec9ae52f2fc870204bce00438be1b6fa695d27c9d01cfb0956210383787221b68f601aa4695c27e7eebb0d15de2ed7ef7b9981fba8cad71ae2d84253ae"
                     , "Address" .= T.pack "3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR"
                     ] 
            , object [ "TxID"    .= T.pack "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
                     , "Index"   .= (3 :: Int)
                     , "Value"   .= (180000 :: Int)
                     , "Script"  .= T.pack "a91460c1eb9034130684ece4594384e984325d1280ad87"
                     , "Redeem"  .= T.pack "52210285bd8161363c6e2d2c075e08ed8a956c4706ccd77497a31327bf92b2208c3587210286e3177a8ca61d2d9426b4230735631e8552cc4d6e87e1277960461b0d26aac42103e2e138589e064f347238c2ca46c1d7deb8dd9472d065d7c5cb0f91aa5cc210a953ae"
                     , "Address" .= T.pack "3AWd6mZHrvMbocVRp752ExTGnUhh1huWQp"
                     ] 
            ]
        )

    -- Import the partially signed transaction into the wallet
    -- This should yield a partial transaction and no output coins
    cmdImportTx (decode' $ fromJust $ hexToBS $ partialTx) >>= 
        liftIO . assertEqual "Importing partial tx 1"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                        [ T.pack "13LjjLQGGiTL8AYMbYqBmQVyAMyQnjPZK3"
                        , T.pack "1MK3xFQSekQPuety1oaVTmvLRusFYsTq3d"
                        ]
                    , "Value"      .= (-450000 :: Int)
                    , "Partial"    .= True
                    ]
            ]
        )

    -- Re-importing the partial transaction should be an idempotent process
    cmdImportTx (decode' $ fromJust $ hexToBS $ partialTx) >>= 
        liftIO . assertEqual "Importing partial tx 2"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                        [ T.pack "13LjjLQGGiTL8AYMbYqBmQVyAMyQnjPZK3"
                        , T.pack "1MK3xFQSekQPuety1oaVTmvLRusFYsTq3d"
                        ]
                    , "Value"      .= (-450000 :: Int)
                    , "Partial"    .= True
                    ]
            ]
        )

    -- After importing the partially signed transaction in the wallet, we should
    -- have some of the coins in "reserved" status. Additionally, the change
    -- coin should not be displayed here as importing partial transactions does
    -- not produce coins (would not make sense as TxID is wrong)
    cmdCoins "signms" >>= liftIO . assertEqual "Check coins after importing partial tx" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
                     , "Index"   .= (0 :: Int)
                     , "Value"   .= (100000 :: Int)
                     , "Script"  .= T.pack "a91491b00b8d4d4b8909d5d17fe8da3b09fbd64e003c87"
                     , "Redeem"  .= T.pack "522102690c830a595721558e2bf6c58f6ed269c3b9fa194523326bb26d9a20b0cf49532102c98a1bde30c9fd0b71e88792b512963ce8b956f8735dbb0b1a2d6da7f1385e8d2102e7166192bf2bb2433daad4ab396fc2dc09785df716ca992f8bf76ff0aed6695053ae"
                     , "Address" .= T.pack "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
                     ] 
            ]
        )

    -- Now we should have the funding transaction followed by a
    -- a partially signed multisig transaction
    cmdListTx "signms" >>= liftIO . assertEqual "Final tx check MS"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
                         , T.pack "3EBJj6Z2FrrGUytJabTrJRD6mxCgF3d7JW"
                         , T.pack "3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR"
                         , T.pack "3AWd6mZHrvMbocVRp752ExTGnUhh1huWQp"
                         ]
                     , "Value"      .= (630000 :: Int)
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "13LjjLQGGiTL8AYMbYqBmQVyAMyQnjPZK3"
                         , T.pack "1MK3xFQSekQPuety1oaVTmvLRusFYsTq3d"
                         ]
                     , "Value"      .= (-450000 :: Int)
                     , "Partial"    .= True
                     ]
            ]
        )


    let f (Entity _ c) = ( dbCoinTxid c, dbCoinPos c
                         , dbCoinValue c, dbCoinScript c
                         , dbCoinRdmScript c, dbCoinAddress c
                         , dbCoinStatus c, dbCoinOrphan c
                         )
        g (Entity _ t) = ( dbTxTxid t, dbTxRecipients t
                         , dbTxValue t, dbTxOrphan t, dbTxPartial t
                         )

    (Entity msi _) <- dbGetAcc "signms"

    -- Checking the coins directly in the database
    ((map f) <$> selectList [DbCoinAccount ==. msi] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check DB coins after importing Partial tx"
            [ ( "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
              , 0, 100000, "a91491b00b8d4d4b8909d5d17fe8da3b09fbd64e003c87"
              , Just "522102690c830a595721558e2bf6c58f6ed269c3b9fa194523326bb26d9a20b0cf49532102c98a1bde30c9fd0b71e88792b512963ce8b956f8735dbb0b1a2d6da7f1385e8d2102e7166192bf2bb2433daad4ab396fc2dc09785df716ca992f8bf76ff0aed6695053ae"
              , "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
              , Unspent
              , False
              )
            , ( "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
              , 1, 200000, "a91488fb2eb63c1f992d801f538340180527510464c787"
              , Just "5221023206a385544defd500a54d3d3fd8982d35fa7f877fa721beb59e440186183e8a210352b7c1f82593abebcb4ad0bbf28a4317b49efc1cc3e647453c09c6375122e8f621035c9ca1f7825d2cf025f42e636f57f193dbaae5c18b08dca10bf996ddb58eb8e853ae"
              , "3EBJj6Z2FrrGUytJabTrJRD6mxCgF3d7JW"
              , Reserved "74ea8680000dd5e4f0122e47c73de7b0b02aae478a03930c5f547f67ed3dabcb"
              , False
              )
            , ( "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
              , 2, 150000, "a9148c0ad01947abab4985837a24be0d2141dcba063987"
              , Just "522102779d361a5d534dfca9d480eb60a1b14741a98d29f5c984fc1afd68a8c50981232102f579364c0d0971ec9ae52f2fc870204bce00438be1b6fa695d27c9d01cfb0956210383787221b68f601aa4695c27e7eebb0d15de2ed7ef7b9981fba8cad71ae2d84253ae"
              , "3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR"
              , Reserved "74ea8680000dd5e4f0122e47c73de7b0b02aae478a03930c5f547f67ed3dabcb"
              , False
              )
            , ( "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
              , 3, 180000, "a91460c1eb9034130684ece4594384e984325d1280ad87"
              , Just "52210285bd8161363c6e2d2c075e08ed8a956c4706ccd77497a31327bf92b2208c3587210286e3177a8ca61d2d9426b4230735631e8552cc4d6e87e1277960461b0d26aac42103e2e138589e064f347238c2ca46c1d7deb8dd9472d065d7c5cb0f91aa5cc210a953ae"
              , "3AWd6mZHrvMbocVRp752ExTGnUhh1huWQp"
              , Reserved "74ea8680000dd5e4f0122e47c73de7b0b02aae478a03930c5f547f67ed3dabcb"
              , False
              )
            ]

    -- Checking the transactions directly in the database
    ((map g) <$> selectList [DbTxAccount ==. msi] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check DB transactions after importing Partial tx"
            [ ( "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
              , [ "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
                , "3EBJj6Z2FrrGUytJabTrJRD6mxCgF3d7JW"
                , "3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR"
                , "3AWd6mZHrvMbocVRp752ExTGnUhh1huWQp"
                ]
              , 630000 , False, False
              )
            , ( "74ea8680000dd5e4f0122e47c73de7b0b02aae478a03930c5f547f67ed3dabcb"
              , [ "13LjjLQGGiTL8AYMbYqBmQVyAMyQnjPZK3"
                , "1MK3xFQSekQPuety1oaVTmvLRusFYsTq3d"
                ]
              , -450000, False, True
              )
            ]

    -- Completing a multisig transaction
    cmdSignTx "signms" (decode' $ fromJust $ hexToBS partialTx) (SigAll False) 
        >>= liftIO . assertEqual "Sign partial multisig" 
            ( object [ "Tx"       .= T.pack finalTx
                     , "Complete" .= True
                     ]
            )

    -- this should return the partial signature as the account is wrong
    cmdSignTx "ms1" (decode' $ fromJust $ hexToBS partialTx) (SigAll False) 
        >>= liftIO . assertEqual "Sign partial multisig bad acc" 
            ( object [ "Tx"       .= T.pack partialTx
                     , "Complete" .= False
                     ]
            )

    -- Importing the full transaction into the wallet
    cmdImportTx (decode' $ fromJust $ hexToBS finalTx) >>= 
        liftIO . assertEqual "Final tx import"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                            [ T.pack "13LjjLQGGiTL8AYMbYqBmQVyAMyQnjPZK3"
                            , T.pack "1MK3xFQSekQPuety1oaVTmvLRusFYsTq3d"
                            ]
                        , "Value"      .= (-450000 :: Int)
                        ]
                ]
            )

    -- Importing the full transaction into the wallet (again)
    cmdImportTx (decode' $ fromJust $ hexToBS finalTx) >>= 
        liftIO . assertEqual "Final tx import again"
            ( toJSON
                [ object [ "Recipients" .= toJSON
                            [ T.pack "13LjjLQGGiTL8AYMbYqBmQVyAMyQnjPZK3"
                            , T.pack "1MK3xFQSekQPuety1oaVTmvLRusFYsTq3d"
                            ]
                        , "Value"      .= (-450000 :: Int)
                        ]
                ]
            )

    -- The change coin should now be there
    cmdCoins "signms" >>= liftIO . assertEqual "Check coins after importing final tx" 
        (toJSON 
            [ object [ "TxID"    .= T.pack "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
                     , "Index"   .= (0 :: Int)
                     , "Value"   .= (100000 :: Int)
                     , "Script"  .= T.pack "a91491b00b8d4d4b8909d5d17fe8da3b09fbd64e003c87"
                     , "Redeem"  .= T.pack "522102690c830a595721558e2bf6c58f6ed269c3b9fa194523326bb26d9a20b0cf49532102c98a1bde30c9fd0b71e88792b512963ce8b956f8735dbb0b1a2d6da7f1385e8d2102e7166192bf2bb2433daad4ab396fc2dc09785df716ca992f8bf76ff0aed6695053ae"
                     , "Address" .= T.pack "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
                     ] 
            , object [ "TxID"    .= T.pack "a7274a15719099c5de37741bfd70efa15bb22a9de5507d966ccdf528a4f8f4c3"
                     , "Index"   .= (2 :: Int)
                     , "Value"   .= (80000 :: Int)
                     , "Script"  .= T.pack "a914737e1e10bbf4290c82681745b5a54449ca83d18487"
                     , "Redeem"  .= T.pack "5221021190e8331245f9455d0b027cd9f53e2e5f530ffa48aaa4f6b9fc7657c8cd5ebe21030e08ad2233845f90e958988768b42f8e53d358efc371149049a5399ae0d3b334210380f8240eb12a1bbe20c2f441d6c2216d5eb84bbb4b926aa801180de13dbdf7c553ae"
                     , "Address" .= T.pack "3CDgiQ2TnKvy1AovxwFy2jsDqURN8bq856"
                     ] 
            ]
        )

    -- The transaction list should be the funding transaction followed by
    -- the multisig spending transaction
    cmdListTx "signms" >>= liftIO . assertEqual "Check tx after importing final tx"
        ( toJSON
            [ object [ "Recipients" .= toJSON
                         [ T.pack "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
                         , T.pack "3EBJj6Z2FrrGUytJabTrJRD6mxCgF3d7JW"
                         , T.pack "3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR"
                         , T.pack "3AWd6mZHrvMbocVRp752ExTGnUhh1huWQp"
                         ]
                     , "Value"      .= (630000 :: Int)
                     ]
            , object [ "Recipients" .= toJSON
                         [ T.pack "13LjjLQGGiTL8AYMbYqBmQVyAMyQnjPZK3"
                         , T.pack "1MK3xFQSekQPuety1oaVTmvLRusFYsTq3d"
                         ]
                     , "Value"      .= (-450000 :: Int)
                     ]
            ]
        )

    -- Checking the coins directly in the database
    ((map f) <$> selectList [DbCoinAccount ==. msi] [Asc DbCoinCreated]) >>= 
        liftIO . assertEqual "Check DB coins after importing Final tx"
            [ ( "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
              , 0, 100000, "a91491b00b8d4d4b8909d5d17fe8da3b09fbd64e003c87"
              , Just "522102690c830a595721558e2bf6c58f6ed269c3b9fa194523326bb26d9a20b0cf49532102c98a1bde30c9fd0b71e88792b512963ce8b956f8735dbb0b1a2d6da7f1385e8d2102e7166192bf2bb2433daad4ab396fc2dc09785df716ca992f8bf76ff0aed6695053ae"
              , "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
              , Unspent
              , False
              )
            , ( "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
              , 1, 200000, "a91488fb2eb63c1f992d801f538340180527510464c787"
              , Just "5221023206a385544defd500a54d3d3fd8982d35fa7f877fa721beb59e440186183e8a210352b7c1f82593abebcb4ad0bbf28a4317b49efc1cc3e647453c09c6375122e8f621035c9ca1f7825d2cf025f42e636f57f193dbaae5c18b08dca10bf996ddb58eb8e853ae"
              , "3EBJj6Z2FrrGUytJabTrJRD6mxCgF3d7JW"
              , Spent "a7274a15719099c5de37741bfd70efa15bb22a9de5507d966ccdf528a4f8f4c3"
              , False
              )
            , ( "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
              , 2, 150000, "a9148c0ad01947abab4985837a24be0d2141dcba063987"
              , Just "522102779d361a5d534dfca9d480eb60a1b14741a98d29f5c984fc1afd68a8c50981232102f579364c0d0971ec9ae52f2fc870204bce00438be1b6fa695d27c9d01cfb0956210383787221b68f601aa4695c27e7eebb0d15de2ed7ef7b9981fba8cad71ae2d84253ae"
              , "3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR"
              , Spent "a7274a15719099c5de37741bfd70efa15bb22a9de5507d966ccdf528a4f8f4c3"
              , False
              )
            , ( "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
              , 3, 180000, "a91460c1eb9034130684ece4594384e984325d1280ad87"
              , Just "52210285bd8161363c6e2d2c075e08ed8a956c4706ccd77497a31327bf92b2208c3587210286e3177a8ca61d2d9426b4230735631e8552cc4d6e87e1277960461b0d26aac42103e2e138589e064f347238c2ca46c1d7deb8dd9472d065d7c5cb0f91aa5cc210a953ae"
              , "3AWd6mZHrvMbocVRp752ExTGnUhh1huWQp"
              , Spent "a7274a15719099c5de37741bfd70efa15bb22a9de5507d966ccdf528a4f8f4c3"
              , False
              )
            , ( "a7274a15719099c5de37741bfd70efa15bb22a9de5507d966ccdf528a4f8f4c3"
              , 2, 80000, "a914737e1e10bbf4290c82681745b5a54449ca83d18487"
              , Just "5221021190e8331245f9455d0b027cd9f53e2e5f530ffa48aaa4f6b9fc7657c8cd5ebe21030e08ad2233845f90e958988768b42f8e53d358efc371149049a5399ae0d3b334210380f8240eb12a1bbe20c2f441d6c2216d5eb84bbb4b926aa801180de13dbdf7c553ae"
              , "3CDgiQ2TnKvy1AovxwFy2jsDqURN8bq856"
              , Unspent
              , False
              )
            ]

    -- Checking the transactions directly in the database
    ((map g) <$> selectList [DbTxAccount ==. msi] [Asc DbTxCreated]) >>=
        liftIO . assertEqual "Check DB transactions after importing Final tx"
            [ ( "62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589"
              , [ "3EyLoBudu2fNY7hnQDX7iZ3kn1d9wyPc2E"
                , "3EBJj6Z2FrrGUytJabTrJRD6mxCgF3d7JW"
                , "3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR"
                , "3AWd6mZHrvMbocVRp752ExTGnUhh1huWQp"
                ]
              , 630000 , False, False
              )
            , ( "a7274a15719099c5de37741bfd70efa15bb22a9de5507d966ccdf528a4f8f4c3"
              , [ "13LjjLQGGiTL8AYMbYqBmQVyAMyQnjPZK3"
                , "1MK3xFQSekQPuety1oaVTmvLRusFYsTq3d"
                ]
              , -450000, False, False
              )
            ]

testUtil :: ( PersistStore m, PersistUnique m, PersistQuery m
            , PersistMonadBackend m ~ SqlBackend
            ) 
         => EitherT String m ()
testUtil = do 

    cmdBuildRawTx "[{\"txid\":\"0100000000000000000000000000000000000000000000000000000000000000\",\"vout\":10},{\"txid\":\"0200000000000000000000000000000000000000000000000000000000000000\",\"vout\":11}]" "{\"1EWdRnLVsncoJjrfShHQxPDJPZNGNGFKgx\":100000,\"3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR\":200000}" >>= liftIO . assertEqual "Build Raw Tx"
        (toJSON $ object ["Tx" .= T.pack "010000000200000000000000000000000000000000000000000000000000000000000000010a00000000ffffffff00000000000000000000000000000000000000000000000000000000000000020b00000000ffffffff02400d03000000000017a9148c0ad01947abab4985837a24be0d2141dcba063987a0860100000000001976a91494341b2515efda20f26d37a2f4f5abd424cf71f688ac00000000"])

    let partialTx = "01000000038935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76202000000b6004830450221009d876833374da1714aa42945b73fe2af9ee37fdf32b4db9dd10297c0a9a18257022065618d04ead4ba5e4201bf15f149f1429e83173d42e31246d3164f2072e6605901004c69522102779d361a5d534dfca9d480eb60a1b14741a98d29f5c984fc1afd68a8c50981232102f579364c0d0971ec9ae52f2fc870204bce00438be1b6fa695d27c9d01cfb0956210383787221b68f601aa4695c27e7eebb0d15de2ed7ef7b9981fba8cad71ae2d84253aeffffffff8935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76203000000b600483045022100f8f9274fcf04200031578f5bfa0ecd108f5c945d7efef5f4fe519943d945594502200941f22088458750894005366a74857b72fb59197405bd2298947d96456d165b01004c6952210285bd8161363c6e2d2c075e08ed8a956c4706ccd77497a31327bf92b2208c3587210286e3177a8ca61d2d9426b4230735631e8552cc4d6e87e1277960461b0d26aac42103e2e138589e064f347238c2ca46c1d7deb8dd9472d065d7c5cb0f91aa5cc210a953aeffffffff8935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76201000000b50047304402200469180f9e94bf6d1d03ce07411bfd107b78f2fb1b1a862fc39dfeece417d14b02206696f963f97d3a6aaddc295c8eaf1846fef07f44c46d3aa97f09a4c0b72476c101004c695221023206a385544defd500a54d3d3fd8982d35fa7f877fa721beb59e440186183e8a210352b7c1f82593abebcb4ad0bbf28a4317b49efc1cc3e647453c09c6375122e8f621035c9ca1f7825d2cf025f42e636f57f193dbaae5c18b08dca10bf996ddb58eb8e853aeffffffff03e0930400000000001976a91419abd2efb4cd44b132ffd6206aa35e60f9d3e4b988ace0220200000000001976a914decc77b87d2199a51532034ad63f59cae6e6c4f588ac803801000000000017a914737e1e10bbf4290c82681745b5a54449ca83d1848700000000"

    let resultTx = "01000000038935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76202000000fdfd00004830450221009d876833374da1714aa42945b73fe2af9ee37fdf32b4db9dd10297c0a9a18257022065618d04ead4ba5e4201bf15f149f1429e83173d42e31246d3164f2072e6605901473044022065316243de4edd6e1741580a11489a5445b4fc35efcb13aa6189f1a524f6745a022003fdf5273c90ac7d906cba33f42661dcac45e80aab9aecd4fcd5ed5e9595c239014c69522102779d361a5d534dfca9d480eb60a1b14741a98d29f5c984fc1afd68a8c50981232102f579364c0d0971ec9ae52f2fc870204bce00438be1b6fa695d27c9d01cfb0956210383787221b68f601aa4695c27e7eebb0d15de2ed7ef7b9981fba8cad71ae2d84253aeffffffff8935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76203000000b600483045022100f8f9274fcf04200031578f5bfa0ecd108f5c945d7efef5f4fe519943d945594502200941f22088458750894005366a74857b72fb59197405bd2298947d96456d165b01004c6952210285bd8161363c6e2d2c075e08ed8a956c4706ccd77497a31327bf92b2208c3587210286e3177a8ca61d2d9426b4230735631e8552cc4d6e87e1277960461b0d26aac42103e2e138589e064f347238c2ca46c1d7deb8dd9472d065d7c5cb0f91aa5cc210a953aeffffffff8935da5b1084b96a26d931aa9a6e9c844191c014c190d7981979eb977c89e76201000000fdfd000047304402200469180f9e94bf6d1d03ce07411bfd107b78f2fb1b1a862fc39dfeece417d14b02206696f963f97d3a6aaddc295c8eaf1846fef07f44c46d3aa97f09a4c0b72476c101483045022100e865ba910c96823cc9ddf6b65d62f6d107ca5589a866f3814d048ada70a01b3102201025101604ccf2f639a7f7176517665656b04c9a531d3e542eb495095148b802014c695221023206a385544defd500a54d3d3fd8982d35fa7f877fa721beb59e440186183e8a210352b7c1f82593abebcb4ad0bbf28a4317b49efc1cc3e647453c09c6375122e8f621035c9ca1f7825d2cf025f42e636f57f193dbaae5c18b08dca10bf996ddb58eb8e853aeffffffff03e0930400000000001976a91419abd2efb4cd44b132ffd6206aa35e60f9d3e4b988ace0220200000000001976a914decc77b87d2199a51532034ad63f59cae6e6c4f588ac803801000000000017a914737e1e10bbf4290c82681745b5a54449ca83d1848700000000"

    let sigi = concat
            [ "[{"
            , "\"txid\":\"62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589\","
            , "\"vout\":2,"
            , "\"scriptPubKey\":\"a9148c0ad01947abab4985837a24be0d2141dcba063987\","
            , "\"scriptRedeem\":\"522102779d361a5d534dfca9d480eb60a1b14741a98d29f5c984fc1afd68a8c50981232102f579364c0d0971ec9ae52f2fc870204bce00438be1b6fa695d27c9d01cfb0956210383787221b68f601aa4695c27e7eebb0d15de2ed7ef7b9981fba8cad71ae2d84253ae\""
            , "},{"
            , "\"txid\":\"62e7897c97eb791998d790c114c09141849c6e9aaa31d9266ab984105bda3589\","
            , "\"vout\":1,"
            , "\"scriptPubKey\":\"a91488fb2eb63c1f992d801f538340180527510464c787\","
            , "\"scriptRedeem\":\"5221023206a385544defd500a54d3d3fd8982d35fa7f877fa721beb59e440186183e8a210352b7c1f82593abebcb4ad0bbf28a4317b49efc1cc3e647453c09c6375122e8f621035c9ca1f7825d2cf025f42e636f57f193dbaae5c18b08dca10bf996ddb58eb8e853ae\""
            , "}]"
            ]

    -- WIF for 3ETVUh7mn1sSiLrhUQYMByfuo32DauHKdR (input 0)
    let prv = concat
            [ "["
            , "\"L4rxfXmwUDpmkpfmUAaJo7ChfPgAAj18pdpF8GTVkJRheJZaFikf\","
            , "\"L5ZencGKZqtiDk2DjFn23PjMdzGo9ZNpDw5wpcgN2cX1Qzjg6zzr\""
            , "]"
            ]
        tx  = decode' $ fromJust $ hexToBS partialTx

    cmdSignRawTx tx sigi prv (SigAll False) >>=
        liftIO . assertEqual "Sign raw transaction"
        ( object [ (T.pack "Tx") .= T.pack resultTx
                 , (T.pack "Complete") .= False
                 ]
        )

