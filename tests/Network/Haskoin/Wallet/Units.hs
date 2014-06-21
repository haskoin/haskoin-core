{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Units (tests) where

import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (liftM, guard)
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Logger (MonadLogger, NoLoggingT)
import Control.Exception (Exception, handleJust)
import Control.Monad.Trans.Resource (ResourceT)

import Data.Maybe (fromJust, isJust)
import Data.Yaml as YAML
    ( Value
    , object 
    , toJSON
    , (.=)
    )
import qualified Data.Text as T (pack)
import qualified Data.ByteString as BS 
    ( empty
    , pack
    )

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
import Database.Persist.Sqlite 
    ( SqlBackend
    , runSqlite
    , runMigrationSilent
    , SqlPersistT
    )

import Network.Haskoin.Wallet.Commands
import Network.Haskoin.Wallet.DbAddress
import Network.Haskoin.Wallet.DbAccount
import Network.Haskoin.Wallet.DbTx
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util

type App = SqlPersistT (NoLoggingT (ResourceT IO))

tests :: [Test]
tests =
    [ testGroup "Wallet pre-initialization tests" 
        [ testCase "cmdNewAcc fails if the wallet is not initialized" $ 
            assertException
                (InitializationException "Wallet main is not initialized") 
                (cmdNewAcc "default")

        , testCase "cmdNewMS fails if the wallet is not initialized" $ 
            assertException
                (InitializationException "Wallet main is not initialized") 
                (cmdNewMS "default" 2 3 [])

        , testCase "cmdList fails if the wallet is not initialized" $
            assertException
                (InitializationException "Wallet main is not initialized") 
                (cmdList "default" 0 1)

        , testCase "cmdBalances fails if the wallet is not initialized" $
            assertException
                (InitializationException "Wallet main is not initialized") 
                cmdBalances

        , testCase "cmdGenAddrs fails if the wallet is not initialized" $
            assertException
                (InitializationException "Wallet main is not initialized") 
                (cmdGenAddrs "default" 3)

        , testCase "cmdAllCoins fails if the wallet is not initialized" $
            assertException
                (InitializationException "Wallet main is not initialized") 
                cmdAllCoins

        , testCase "cmdImportTx fails if the wallet is not initialized" $
            assertException
                (InitializationException "Wallet main is not initialized") $
                cmdImportTx (Tx 1 [] [] 0)
        ] 
    , testGroup "Wallet initialization tests"
        [ testCase "Calling cmdInitMnemo with a bad mnemonic should fail" $
            assertException
                (InitializationException 
                    "fromMnemonic: wrong number of words: 1") 
                (cmdInitMnemo "password" (Just "hello"))

        , testCase "Calling cmdInit with an empty seed should fail" $
            assertException
                (InitializationException "The seed is empty") 
                (cmdInit BS.empty)

        , testCase "Calling cmdInit twice should fail" $
            assertException
                (InitializationException "The wallet is already initialized") 
                (cmdInit (BS.pack [0]) >> cmdInit (BS.pack [0]))
        ]
    , testGroup "Account tests"
        [ testCase "Invalid multisig parameters (0 of 1)" $
            assertException
                (AccountSetupException "Invalid multisig parameters") 
                (cmdInit (BS.pack [0]) >> cmdNewMS "ms" 0 1 [])
                
        , testCase "Invalid multisig parameters (2 of 1)" $
            assertException
                (AccountSetupException "Invalid multisig parameters") 
                (cmdInit (BS.pack [0]) >> cmdNewMS "ms" 2 1 [])

        , testCase "Invalid multisig parameters (16 of 17)" $
            assertException
                (AccountSetupException "Invalid multisig parameters") 
                (cmdInit (BS.pack [0]) >> cmdNewMS "ms" 16 17 [])

        , testCase "To many multisig keys (2 keys for 1 of 2)" $
            assertException
                (AccountSetupException "Too many keys") 
                ( cmdInit (BS.pack [0]) >> cmdNewMS "ms" 1 2 
                    [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1])
                    , deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [2])
                    ]
                )

        , testCase "Calling cmdAddKeys with an empty key list should fail" $
            assertException
                (AccountSetupException "Thirdparty key list can not be empty") 
                (cmdInit (BS.pack [0]) >> cmdAddKeys "default" [])

        , testCase "Calling cmdAddKeys on a non-multisig account should fail" $
            assertException
                (AccountSetupException 
                    "Can only add keys to a multisig account") $ do
                    cmdInit (BS.pack [0])
                    cmdNewAcc "default" 
                    cmdAddKeys "default"
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1]) ]

        , testCase "Calling cmdNewMS with keys in your wallet should fail" $
            assertException
                (AccountSetupException 
                    "Can not add your own keys to a multisig account") $ do
                    cmdInit (BS.pack [0])
                    cmdNewAcc "default" 
                    let master = fromJust $ makeMasterKey $ BS.pack [0]
                        accKey = fromJust $ accPubKey master 0
                    cmdNewMS "ms" 1 2 [ getAccPubKey accKey ]

        , testCase "Calling cmdAddKeys with keys in your wallet should fail" $
            assertException
                (AccountSetupException 
                    "Can not add your own keys to a multisig account") $ do
                    cmdInit (BS.pack [0])
                    cmdNewAcc "default" 
                    let master = fromJust $ makeMasterKey $ BS.pack [0]
                        accKey = fromJust $ accPubKey master 0
                    cmdNewMS "ms" 1 2 []
                    cmdAddKeys "ms" [getAccPubKey accKey]

        , testCase "Adding keys to a complete multisig account should fail" $
            assertException
                (AccountSetupException 
                    "The account is complete and no further keys can be added") $ do
                    cmdInit (BS.pack [0])
                    cmdNewMS "ms" 2 3 
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1])
                        , deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [2])
                        ]
                    cmdAddKeys "ms" 
                        [deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [3])]

        , testCase "Adding more keys than the account can hold should fail" $
            assertException
                (AccountSetupException 
                    "Adding too many keys to the account") $ do
                    cmdInit (BS.pack [0])
                    cmdNewMS "ms" 2 3 
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1])
                        ]
                    cmdAddKeys "ms" 
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [2])
                        , deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [3])
                        ]

        , testCase "Displaying a non-existing account should fail" $
            assertException
                (InvalidAccountException "Account default does not exist") 
                (cmdInit (BS.pack [0]) >> cmdAccInfo "default")

        , testCase "Dumping keys of a non-existing account should fail" $
            assertException
                (InvalidAccountException "Account default does not exist") 
                (cmdInit (BS.pack [0]) >> cmdDumpKeys "default")

        , testCase "Listing addresses of a non-existing account should fail" $
            assertException
                (InvalidAccountException "Account default does not exist") 
                (cmdInit (BS.pack [0]) >> cmdList "default" 0 1)
                
        ]
    , testGroup "Address tests"
        [ testCase "Displaying page -1 should fail" $
            assertException
                (InvalidPageException "Invalid page number: -1") 
                (cmdInit (BS.pack [0]) >> cmdList "default" (-1) 1)

        , testCase "Displaying 0 results per page should fail" $
            assertException
                (InvalidPageException "Invalid results per page: 0") 
                (cmdInit (BS.pack [0]) >> cmdList "default" 0 0)

        , testCase "Displaying a page number that is too high should fail" $
            assertException
                (InvalidPageException "The page number 2 is too high") $ do
                    cmdInit $ BS.pack [0] 
                    cmdNewAcc "default"
                    cmdGenAddrs "default" 5
                    cmdList "default" 2 5

        , testCase "Generating less than 1 address should fail" $
            assertException
                (AddressGenerationException "Can not generate less than 1 address") $ do
                    cmdInit $ BS.pack [0] 
                    cmdNewAcc "default"
                    cmdGenAddrs "default" 0

        , testCase "Generating addresses with empty label list should fail" $
            assertException
                (AddressGenerationException "Labels can not be empty") $ do
                    cmdInit $ BS.pack [0] 
                    cmdNewAcc "default"
                    cmdGenWithLabel "default" []

        , testCase "Setting a label on an invalid address key should fail" $
            assertException
                (InvalidAddressException "The address key does not exist") $ do
                    cmdInit $ BS.pack [0] 
                    cmdNewAcc "default"
                    cmdGenAddrs "default" 5
                    cmdLabel "default" 5 "Gym membership"

        , testCase "Requesting the WIF on an invalid address key should fail" $
            assertException
                (InvalidAddressException "The address key does not exist") $ do
                    cmdInit $ BS.pack [0] 
                    cmdNewAcc "default"
                    cmdGenAddrs "default" 5
                    cmdPrvKey "default" 5
        ]
    , testGroup "Transaction tests"
        [ testCase "Tx import" $ runUnit testImportTx
        ]
    ]

assertException :: ( Exception e
                   , Eq e
                   ) => e -> App a -> Assertion
assertException ex action = 
    handleJust matchEx (const $ return ()) $ do
        runUnit action
        assertFailure $ "Expecting exception: " ++ show ex
  where
    matchEx = guard . (== ex)

runUnit :: App a -> Assertion
runUnit action = do
    _ <- runSqlite ":memory:" $ do
        _ <- runMigrationSilent migrateWallet 
        action
    return ()

mnemo :: String
mnemo = unwords
    [ "mass", "coast", "dance"
    , "birth", "online", "various"
    , "renew", "alert", "crunch" 
    , "middle", "absurd", "health"
    ]

pass :: String
pass = "passw0rd"

testImportTx :: App ()
testImportTx = do
    cmdInitMnemo pass $ Just mnemo
    cmdNewAcc "a"
    cmdGenAddrs "a" 5 
    cmdNewAcc "b"
    cmdGenAddrs "b" 5 
    let fundingTx = 
            Tx 0 [ TxIn (OutPoint 0 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" -- a
                 , TxOut 15000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "141gt8RK8uiohKXGNDw25Zih4ugCsW4iUt" -- b
                 , TxOut 20000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" -- a
                 , TxOut 25000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "16rEcC1w39g7VpVbmkqiPZZi4oJ1tkQnjU" -- b
                 ] maxBound 
    let fundingTx2 = 
            Tx 0 [ TxIn (OutPoint 0 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "13r5SQ9YQ5Xxk1RUbTvZ7VB9xB7EMprWNj"
                 ] maxBound 
    res <- dbImportTx fundingTx
    liftIO $ print $ bsToHex $ encode' fundingTx2
    --TODO: Test res
    return ()
    



