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
import Network.Haskoin.Wallet.Model
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
        ]
    , testGroup "Address page tests"
        [ testCase "Displaying page -1 should fail" $
            assertException
                (InvalidPageException "Invalid page number: -1") 
                (cmdList "default" (-1) 1)

        , testCase "Displaying 0 results per page should fail" $
            assertException
                (InvalidPageException "Invalid results per page: 0") 
                (cmdList "default" 0 0)

        ]
    ]

assertException :: ( Exception e
                   , Eq e
                   ) => e -> App a -> IO ()
assertException ex action = 
    handleJust matchEx (const $ return ()) $ do
        runUnit action
        assertFailure $ "Expecting exception: " ++ show ex
  where
    matchEx = guard . (== ex)

runUnit :: App a -> Assertion
runUnit action = do
    _ <- runSqlite ":memory:" $ do
        _ <- runMigrationSilent migrateAll 
        action
    return ()

