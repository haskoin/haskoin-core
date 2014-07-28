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

import Data.Word (Word32)
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

import Network.Haskoin.Node.HeaderChain

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
        [ testCase "Tx double spend" $ runUnit testDoubleSpend
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
        initWalletDB
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

-- create a fake block for testing at height 1
fakeNode :: Word32 -> BlockHash -> BlockHeaderNode
fakeNode i h = BlockHeaderNode
    { nodeBlockHash = h
    , nodeHeader = BlockHeader 1 0 0 0 0 0
    , nodeHeaderHeight = i
    , nodeChainWork = 0
    , nodeParent = 0
    , nodeChild = Nothing
    , nodeMedianTimes = []
    , nodeMinWork = 0
    }

testDoubleSpend :: App ()
testDoubleSpend = do
    newWalletMnemo "test" pass $ Just mnemo
    newAccount "test" "acc1"
    newAddrs "acc1" 5 
    let fundingTx = 
            Tx 1 [ TxIn (OutPoint 1 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" 
                 , TxOut 20000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" 
                 ] 0
        -- These two transactions are double spending the same coins and
        -- sending them to two different addresses within the wallet
        -- sendTx "acc1" [("1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ",50000)] 10000
        -- sendTx "acc1" [("184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF",50000)] 10000
        spend1 = decode' $ fromJust $ hexToBS "010000000177c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be000000006b483045022100ccac8d72db2fe883dabb4452dcd7d522025225c68d78c27ae2c4362de4a98726022071970ef99969631fb3ff73880fb7e44da42e0b273deca0d149f58dd64cb1d39101210320e6fef44dc34322ce8e5d0a20efe55ae1308c321fab6496eece4473b9f12dd6ffffffff0250c30000000000001976a914bbc24a1dbb213c82dc6bd3e008e411e7a22ba74488ac20ac9700000000001976a91478046f37173d0a16deb1491b8566e26f0cb4894488ac00000000" :: Tx
        spend2 = decode' $ fromJust $ hexToBS "010000000177c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be000000006b4830450221008b6a328f5403f97ac154b543f23a203a711708a2b5d2c4886f110773450723b402205ed70b0a49f797b2e989874ad26ca5cedd677e7e12d7c8844f67eebee5cd8a9601210320e6fef44dc34322ce8e5d0a20efe55ae1308c321fab6496eece4473b9f12dd6ffffffff0250c30000000000001976a9144d816754accc18bb7b2cef479d948be74399337788ac20ac9700000000001976a91478046f37173d0a16deb1491b8566e26f0cb4894488ac00000000" :: Tx
    -- Import funding transaction
    importTx fundingTx NetworkSource >>=
        liftIO . (assertEqual "Confidence is not pending" (Just TxPending))
    spendableCoins "acc1" >>= 
        liftIO . (assertEqual "Spendable coins is not 2" 2) . length

    -- Import first conflicting transaction
    importTx spend1 NetworkSource >>=
        liftIO . (assertEqual "Confidence is not pending" (Just TxPending))
    spendableCoins "acc1" >>= 
        liftIO . (assertEqual "Spendable coins is not 2" 2) . length
    balance "acc1" >>= liftIO . (assertEqual "Balance is not 20050000" 20050000)

    -- Import second conflicting transaction
    importTx spend2 NetworkSource >>=
        liftIO . (assertEqual "Confidence is not pending" (Just TxPending))
    spendableCoins "acc1" >>= 
        liftIO . (assertEqual "Spendable coins is not 1" 1) . length
    balance "acc1" >>= 
        liftIO . (assertEqual "Balance is not 20000000" 20000000)
    
    --Import fake block
    importBlocks [(BestBlock $ fakeNode 0 0x01, [])]
    importBlocks [(BestBlock $ fakeNode 1 0x02, [txHash spend2])]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    balance "acc1" >>= liftIO . (assertEqual "Balance is not 20050000" 20050000)
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins "acc1")
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash fundingTx, txHash spend2])

    --Create a fork. Nothing should change from the tests above
    importBlocks [(SideBlock $ fakeNode 1 0x03, [txHash spend1])]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    balance "acc1" >>= liftIO . (assertEqual "Balance is not 20050000" 20050000)
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins "acc1")
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash fundingTx, txHash spend2])

    -- Trigger a reorg
    let s = fakeNode 0 0x01
        o = [fakeNode 0 0x01, fakeNode 1 0x02]
        n = [fakeNode 0 0x01, fakeNode 1 0x03, fakeNode 2 0x04] 
    importBlocks [(BlockReorg s o n, [])]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend2)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    balance "acc1" >>= liftIO . (assertEqual "Balance is not 20050000" 20050000)
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins "acc1")
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash fundingTx, txHash spend1])

    -- Trigger another reorg
    importBlocks [(SideBlock $ fakeNode 2 0x05, [])]
    let s' = fakeNode 0 0x01
        o' = [fakeNode 0 0x01, fakeNode 1 0x03, fakeNode 2 0x04]
        n' = [fakeNode 0 0x01, fakeNode 1 0x02, fakeNode 2 0x05, fakeNode 3 0x06] 
    importBlocks [(BlockReorg s' o' n', [])]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    balance "acc1" >>= liftIO . (assertEqual "Balance is not 20050000" 20050000)
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins "acc1")
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash fundingTx, txHash spend2])

