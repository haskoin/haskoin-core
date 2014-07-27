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

import Network.Haskoin.Wallet
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Tx
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
    [ testGroup "Wallet creation tests"
        [ testCase "Calling newWalletMnemo with a bad mnemonic should fail" $
            assertException
                (WalletException 
                    "fromMnemonic: wrong number of words: 1") 
                (newWalletMnemo "main" "password" (Just "hello"))

        , testCase "Calling newWallet with an empty seed should fail" $
            assertException
                (WalletException "The seed is empty") 
                (newWallet "main" BS.empty)

        , testCase "Creating two wallets with the same name should fail" $
            assertException
                (WalletException "Wallet main already exists") 
                (newWallet "main" (BS.pack [0]) 
                    >> newWallet "main" (BS.pack [1]))
        ]
    , testGroup "Account tests"
        [ testCase "Creating two accounts with the same name should fail" $
            assertException
                (WalletException "Account acc already exists") $ do
                newWallet "main" (BS.pack [1])
                newAccount "main" "acc" 
                newAccount "main" "acc" 
        , testCase "Invalid multisig parameters (0 of 1)" $
            assertException
                (WalletException "Invalid multisig parameters") 
                (newWallet "main" (BS.pack [0]) 
                    >> newMSAccount "main" "ms" 0 1 [])
                
        , testCase "Invalid multisig parameters (2 of 1)" $
            assertException
                (WalletException "Invalid multisig parameters") 
                (newWallet "main" (BS.pack [0]) 
                    >> newMSAccount "main" "ms" 2 1 [])

        , testCase "Invalid multisig parameters (16 of 17)" $
            assertException
                (WalletException "Invalid multisig parameters") 
                (newWallet "main" (BS.pack [0]) 
                    >> newMSAccount "main" "ms" 16 17 [])

        , testCase "To many multisig keys (2 keys for 1 of 2)" $
            assertException
                (WalletException "Too many keys") 
                ( newWallet "main" (BS.pack [0]) 
                    >> newMSAccount "main" "ms" 1 2 
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1])
                        , deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [2])
                        ]
                )

        , testCase "Calling addAccountKeys with an empty key list should fail" $
            assertException
                (WalletException "Thirdparty key list can not be empty") 
                (newWallet "main" (BS.pack [0]) >> addAccountKeys "default" [])

        , testCase "Calling addAccountKeys on a non-multisig account should fail" $
            assertException
                (WalletException 
                    "Can only add keys to a multisig account") $ do
                    newWallet "main" (BS.pack [0])
                    newAccount "main" "default" 
                    addAccountKeys "default"
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1]) ]

        , testCase "Calling newMSAccount with keys in your wallet should fail" $
            assertException
                (WalletException 
                    "Can not add your own keys to a multisig account") $ do
                    newWallet "main" (BS.pack [0])
                    newAccount "main" "default" 
                    let master = fromJust $ makeMasterKey $ BS.pack [0]
                        accKey = fromJust $ accPubKey master 0
                    newMSAccount "main" "ms" 1 2 [ getAccPubKey accKey ]

        , testCase "Calling addAccountKeys with keys in your wallet should fail" $
            assertException
                (WalletException 
                    "Can not add your own keys to a multisig account") $ do
                    newWallet "main" (BS.pack [0])
                    newAccount "main" "default" 
                    let master = fromJust $ makeMasterKey $ BS.pack [0]
                        accKey = fromJust $ accPubKey master 0
                    newMSAccount "main" "ms" 1 2 []
                    addAccountKeys "ms" [getAccPubKey accKey]

        , testCase "Adding keys to a complete multisig account should fail" $
            assertException
                (WalletException 
                    "The account is complete and no further keys can be added") $ do
                    newWallet "main" (BS.pack [0])
                    newMSAccount "main" "ms" 2 3 
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1])
                        , deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [2])
                        ]
                    addAccountKeys "ms" 
                        [deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [3])]

        , testCase "Adding more keys than the account can hold should fail" $
            assertException
                (WalletException 
                    "Adding too many keys to the account") $ do
                    newWallet "main" (BS.pack [0])
                    newMSAccount "main" "ms" 2 3 
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1])
                        ]
                    addAccountKeys "ms" 
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [2])
                        , deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [3])
                        ]

        , testCase "Getting a non-existing account should fail" $
            assertException
                (WalletException "Account default does not exist") 
                (newWallet "main" (BS.pack [0]) >> getAccount "default")

        , testCase "Dumping keys of a non-existing account should fail" $
            assertException
                (WalletException "Account default does not exist") 
                (newWallet "main" (BS.pack [0]) >> accountPrvKey "default")

        , testCase "Listing addresses of a non-existing account should fail" $
            assertException
                (WalletException "Account default does not exist") 
                (newWallet "main" (BS.pack [0]) >> addressPage "default" 0 1)
                
        ]
    , testGroup "Address tests"
        [ testCase "Displaying page -1 should fail" $
            assertException
                (WalletException "Invalid page number: -1") 
                (newWallet "main" (BS.pack [0]) >> addressPage "default" (-1) 1)

        , testCase "Displaying 0 results per page should fail" $
            assertException
                (WalletException "Invalid results per page: 0") 
                (newWallet "main" (BS.pack [0]) >> addressPage "default" 0 0)

        , testCase "Displaying a page number that is too high should fail" $
            assertException
                (WalletException "The page number 2 is too high") $ do
                    newWallet "main" $ BS.pack [0] 
                    newAccount "main" "default"
                    newAddrs "default" 5
                    addressPage "default" 2 5

        , testCase "Generating less than 1 address should fail" $
            assertException
                (WalletException "Can not generate less than 1 address") $ do
                    newWallet "main" $ BS.pack [0] 
                    newAccount "main" "default"
                    newAddrs "default" 0

        , testCase "Setting a label on an invalid address key should fail" $
            assertException
                (WalletException "The address has not been generated yet") $ do
                    newWallet "main" $ BS.pack [0] 
                    newAccount "main" "default"
                    newAddrs "default" 5
                    setAddrLabel "default" 5 "Gym membership"

        , testCase "Requesting the private key on an invalid address key should fail" $
            assertException
                (WalletException "The address has not been generated yet") $ do
                    newWallet "main" $ BS.pack [0] 
                    newAccount "main" "default"
                    newAddrs "default" 5
                    addressPrvKey "default" 5
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
    newWalletMnemo "main" pass $ Just mnemo
    newAccount "main" "a"
    newAddrs "a" 5 
    newAccount "main" "b"
    newAddrs "b" 5 
    let fundingTx = 
            Tx 1 [ TxIn (OutPoint 0 0) (BS.pack [1]) maxBound ] -- dummy input
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
                 ] 0
    --TODO: Test res
    importTx fundingTx NetworkSource
    b1 <- balance "a"
    liftIO $ print b1
    importTx fundingTx WalletSource
    b2 <- balance "a"
    liftIO $ print b2
    



