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
        liftIO . (assertBool "Gen Addr") . isRight =<< runEitherT testGenAddr
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
    runEitherT (cmdGenAddr "default" 5) >>= liftIO . 
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

    -- Create a new account named "acc1"
    cmdNewAcc "acc1" >>= liftIO . assertEqual "New acc 2"
        ( object 
            [ "Name" .= T.pack "acc1"
            , "Tree" .= T.pack "m/1'/"
            , "Type" .= T.pack "Regular"
            ] 
        )

    -- Create a new account named "acc2"
    cmdNewAcc "acc2" >>= liftIO . assertEqual "New acc 3"
        ( object 
            [ "Name" .= T.pack "acc2"
            , "Tree" .= T.pack "m/2'/"
            , "Type" .= T.pack "Regular"
            ] 
        )

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

testNewMS :: (PersistStore m, PersistUnique m, PersistQuery m) 
          => EitherT String m ()
testNewMS = do 

    -- Create a new 2 of 3 multisig account name "ms1"
    cmdNewMS "ms1" 2 3 [] >>= liftIO . assertEqual "New ms 1" 
        ( object 
            [ "Name" .= T.pack "ms1"
            , "Tree" .= T.pack "m/3'/"
            , "Type" .= T.pack "Multisig 2 of 3"
            , "Warning" .= T.pack "2 multisig keys missing"
            ] 
        )

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
        (Left "cmdAddKeys: Keys can not be empty")

    -- Adding keys to a non-multisig account should fail
    runEitherT (cmdAddKeys "acc1" [pubs !! 0]) >>= 
        liftIO . assertEqual "Invalid addKey 1" 
            (Left "cmdAddKeys: Not a multisig account")

    -- Adding your own keys to a multisig account should fail
    runEitherT (cmdAddKeys "ms1" [pubs !! 0]) >>=
        liftIO . assertEqual "Invalid addKey 2" 
            (Left "cmdAddKeys: Can not add your own keys")

    -- Adding your own keys to a multisig account should fail
    runEitherT (cmdAddKeys "ms1" [pubs2 !! 0,pubs !! 1]) >>=
        liftIO . assertEqual "Invalid addKey 3" 
            (Left "cmdAddKeys: Can not add your own keys")
        
    -- Adding too many keys to a multisig account should fail
    runEitherT (cmdAddKeys "ms1" $ take 4 pubs2) >>=
        liftIO . assertEqual "Invalid addKey 4" 
            (Left "cmdAddKeys: Too many keys")

    -- Display account information for account "ms1". Shold not have changed
    cmdAccInfo "ms1" >>= liftIO . assertEqual "MS info 2"
        ( object 
            [ "Name" .= T.pack "ms1"
            , "Tree" .= T.pack "m/3'/"
            , "Type" .= T.pack "Multisig 2 of 3"
            , "Warning" .= T.pack "2 multisig keys missing"
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

    -- Adding another key now should fail as the account "ms1" is complete
    runEitherT (cmdAddKeys "ms1" [pubs2 !! 2]) >>=
        liftIO . assertEqual "Invalid addKey 5" 
            (Left "cmdAddKeys: Too many keys")

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
    cmdGenAddr "" 5 >>= liftIO . assertEqual "gen addr"
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

    -- Count all addresses in the Address table
    count ([] :: [Filter (DbAddressGeneric b)]) >>= 
        liftIO . assertEqual "Count addr" 5

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

    -- Listing page > maxpage should print the last page
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

    -- Count addresses
    count ([] :: [Filter (DbAddressGeneric b)]) >>= 
        liftIO . assertEqual "Count addr 2" 9

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
    cmdLabel "ms1" 2 "beta"

    -- Setting a label on an invalid address should fail
    runEitherT (cmdLabel "ms1" (-1) "theta") >>= 
        liftIO . assertEqual "set label fail"
            (Left "cmdLabel: Key -1 does not exist")

    -- Setting a label on an invalid address should fail
    runEitherT (cmdLabel "ms1" 4 "theta") >>= 
        liftIO . assertEqual "set label fail 2"
            (Left "cmdLabel: Key 4 does not exist")

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
                        , "Label" .= T.pack "beta"
                        ]
               , object [ "Addr"  .= T.pack "3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q"
                        , "Key"   .= (3 :: Int)
                        , "Tree"  .= T.pack "m/3'/0/3/"
                        , "Label" .= T.pack "Two Words"
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




