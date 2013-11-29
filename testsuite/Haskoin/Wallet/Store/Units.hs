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
        liftIO . (assertBool "Import Tx") . isRight =<< runEitherT testImport
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

formatStr :: String -> IO ()
formatStr str = forM_ (lines str) putStrLn

{- Payments sent to:
 - 1LaPZtFWAWRP8eLNZRLLPGaB3dn19Nb6wi:100000 (in wallet)
 - 1AZimU5FfTQyF4GMsEKLZ32773TtPKczdY:200000 (in wallet)
 - 19W372PnyKZUgYMj1f5qEsJS2xkSjSzXRA:240000 (not in wallet)
 - 1NCSUHC7Dt6exeNd65PEjELdN4hk7QtN1m:122000 (not in wallet)
 - 38kc3Sw4fwkvXMyGPmjQqp7WXMdGQG3Lki:150000 (in wallet)
 - 3QqkesBZx7WBSLcdy5e1PmRU1QLdYTG49Q:400000 (in wallet)
 -}

tx1 :: String
tx1 = "010000000100000000000000000000000000000000000000000000000000000000000000010100000000ffffffff06a0860100000000001976a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac400d0300000000001976a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac80a90300000000001976a9145d3ed34bf8654225eb8dc35c15a47d25572a62be88ac90dc0100000000001976a914e8847a85898158cd24c7914c4dd8bfd175ab9de888acf04902000000000017a9144d769c08d79eed22532e044213bef3174f05158487801a06000000000017a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd08700000000"

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

    -- Verify the balance of account ""
    cmdBalance "" 
        >>= liftIO . assertEqual "Balance 1" (toJSON (300000 :: Int))

    -- Verify the balance of account "ms1"
    cmdBalance "ms1" 
        >>= liftIO . assertEqual "Balance 2" (toJSON (550000 :: Int))

    -- Get coins of account ""
    cmdCoins "" >>= liftIO . assertEqual "Get coins 1"
        ( toJSON
            [ object
                [ "TxID"   .= T.pack "4088f8ab36e3a3aaa067d54a37fca74478b4c52c16bdc3a7c4e6423ca3b46d86"
                , "Index"  .= (0 :: Int)
                , "Value"  .= (100000 :: Int)
                , "Script" .= T.pack "76a914d6baf45f52b4cccc7ac1ba3a35dd739497f8e98988ac"
                , "Orphan" .= False
                ]
            , object
                [ "TxID"   .= T.pack "4088f8ab36e3a3aaa067d54a37fca74478b4c52c16bdc3a7c4e6423ca3b46d86"
                , "Index"  .= (1 :: Int)
                , "Value"  .= (200000 :: Int)
                , "Script" .= T.pack "76a91468e94ed1e88f7e942bf4aaa25fcf5930f517730888ac"
                , "Orphan" .= False
                ]
            ]
        )

    -- Get coins of account "ms1"
    cmdCoins "ms1" >>= liftIO . assertEqual "Get coins 2"
        ( toJSON
            [ object
                [ "TxID"   .= T.pack "4088f8ab36e3a3aaa067d54a37fca74478b4c52c16bdc3a7c4e6423ca3b46d86"
                , "Index"  .= (4 :: Int)
                , "Value"  .= (150000 :: Int)
                , "Script" .= T.pack "a9144d769c08d79eed22532e044213bef3174f05158487"
                , "Orphan" .= False
                ]
            , object
                [ "TxID"   .= T.pack "4088f8ab36e3a3aaa067d54a37fca74478b4c52c16bdc3a7c4e6423ca3b46d86"
                , "Index"  .= (5 :: Int)
                , "Value"  .= (400000 :: Int)
                , "Script" .= T.pack "a914fdf1e3c1a936ab1dde0d7a305d28df396949ffd087"
                , "Orphan" .= False
                ]
            ]
        )

     


