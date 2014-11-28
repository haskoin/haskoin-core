{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Units (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (liftM, guard)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Exception (Exception, handleJust)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (NoLoggingT)

import Data.Word (Word32, Word64)
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS 
    ( ByteString
    , empty
    , pack
    )

import Database.Persist 
    ( Entity(..)
    , entityVal
    , selectList
    , selectFirst
    , (==.)
    )
import Database.Persist.Sqlite 
    ( runSqlite
    , runMigrationSilent
    , SqlPersistT
    )

import Network.Haskoin.Wallet
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Model

import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Util

type App = SqlPersistT (NoLoggingT (ResourceT IO))

tests :: [Test]
tests =
    [ testGroup "Wallet creation tests"
        [ testCase "Calling newWallet with an empty seed should fail" $
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
                (WalletException "Account acc already exists in wallet main") $ do
                _ <- newWallet "main" (BS.pack [1])
                _ <- newAccount "main" "acc" 
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
                (newWallet "main" (BS.pack [0]) >> addAccountKeys "main" "default" [])

        , testCase "Calling addAccountKeys on a non-multisig account should fail" $
            assertException
                (WalletException 
                    "Can only add keys to a multisig account") $ do
                    _ <- newWallet "main" (BS.pack [0])
                    _ <- newAccount "main" "default" 
                    addAccountKeys "main" "default"
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1]) ]

        , testCase "Calling newMSAccount with keys in your wallet should fail" $
            assertException
                (WalletException 
                    "Can not add your own keys to an account") $ do
                    _ <- newWallet "main" (BS.pack [0])
                    _ <- newAccount "main" "default" 
                    let master = fromJust $ makeMasterKey $ BS.pack [0]
                        accKey = fromJust $ accPubKey master 0
                    newMSAccount "main" "ms" 1 2 [ getAccPubKey accKey ]

        , testCase "Calling addAccountKeys with keys in your wallet should fail" $
            assertException
                (WalletException 
                    "Can not add your own keys to an account") $ do
                    _ <- newWallet "main" (BS.pack [0])
                    _ <- newAccount "main" "default" 
                    let master = fromJust $ makeMasterKey $ BS.pack [0]
                        accKey = fromJust $ accPubKey master 0
                    _ <- newMSAccount "main" "ms" 1 2 []
                    addAccountKeys "main" "ms" [getAccPubKey accKey]

        , testCase "Adding keys to a complete multisig account should fail" $
            assertException
                (WalletException 
                    "The account is complete and no further keys can be added") $ do
                    _ <- newWallet "main" (BS.pack [0])
                    _ <- newMSAccount "main" "ms" 2 3 
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1])
                        , deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [2])
                        ]
                    addAccountKeys "main" "ms" 
                        [deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [3])]

        , testCase "Adding more keys than the account can hold should fail" $
            assertException
                (WalletException 
                    "Adding too many keys to the account") $ do
                    _ <- newWallet "main" (BS.pack [0])
                    _ <- newMSAccount "main" "ms" 2 3 
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [1])
                        ]
                    addAccountKeys "main" "ms" 
                        [ deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [2])
                        , deriveXPubKey $ fromJust $ makeXPrvKey (BS.pack [3])
                        ]

        , testCase "Getting a non-existing account should fail" $
            assertException
                (WalletException "Account default does not exist") 
                (newWallet "main" (BS.pack [0]) >> getAccount "main" "default")

        , testCase "Dumping keys of a non-existing account should fail" $
            assertException
                (WalletException "Account default does not exist") 
                (newWallet "main" (BS.pack [0]) >> accountPrvKey "main" "default")

        , testCase "Listing addresses of a non-existing account should fail" $
            assertException
                (WalletException "Account default does not exist") 
                (newWallet "main" (BS.pack [0]) >> addressPage "main" "default" 0 1 False)
                
        ]
    , testGroup "Address tests"
        [ testCase "Displaying page -1 should fail" $
            assertException
                (WalletException "Invalid page number: -1") 
                (newWallet "main" (BS.pack [0]) >> addressPage "main" "default" (-1) 1 False)

        , testCase "Displaying 0 results per page should fail" $
            assertException
                (WalletException "Invalid results per page: 0") 
                (newWallet "main" (BS.pack [0]) >> addressPage "main" "default" 0 0 False)

        , testCase "Displaying a page number that is too high should fail" $
            assertException
                (WalletException "The page number 2 is too high") $ do
                    _ <- newWallet "main" $ BS.pack [0] 
                    _ <- newAccount "main" "default"
                    _ <- addLookAhead "main" "default" 5
                    addressPage "main" "default" 2 5 False

        , testCase "Setting a label on an invalid address key should fail" $
            assertException
                (WalletException "The address has not been generated yet") $ do
                    _ <- newWallet "main" $ BS.pack [0] 
                    _ <- newAccount "main" "default"
                    _ <- addLookAhead "main" "default" 5
                    setAddrLabel "main" "default" 5 "Gym membership"

        , testCase "Requesting the private key on an invalid address key should fail" $
            assertException
                (WalletException "The address has not been generated yet") $ do
                    _ <- newWallet "main" $ BS.pack [0] 
                    _ <- newAccount "main" "default"
                    _ <- addLookAhead "main" "default" 5
                    addressPrvKey "main" "default" 5
        ]
    , testGroup "Transaction import tests"
        [ testCase "Importing orphan tx" $ runUnit testImportOrphan
        , testCase "Importing multisig tx" $ runUnit testImportMultisig
        , testCase "Importing multisig tx 2" $ runUnit testImportMultisig2
        ]
    , testGroup "Double spend tests"
        [ testCase "Outgoing tx double spend" $ runUnit testOutDoubleSpend
        , testCase "Incoming tx double spend" $ runUnit testInDoubleSpend
        , testCase "Chain of double spent tx" $ runUnit testDoubleSpendChain
        , testCase "Groups of double spent tx" $ runUnit testDoubleSpendGroup
        , testCase "Wallet double spend" testWalletDoubleSpend
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

bs1 :: BS.ByteString
bs1 = fromRight $ mnemonicToSeed pass $ unwords
    [ "mass", "coast", "dance"
    , "birth", "online", "various"
    , "renew", "alert", "crunch" 
    , "middle", "absurd", "health"
    ]

bs2 :: BS.ByteString
bs2 = fromRight $ mnemonicToSeed pass $ unwords
    [ "couple", "wrong", "toss"
    , "light", "trust", "abandon"
    , "define", "copy", "radar"
    , "power", "useful", "simple"
    ]

pass :: String
pass = "passw0rd"

testImportOrphan :: App ()
testImportOrphan = do
    _ <- newWallet "test" bs1
    _ <- newAccount "test" "acc1"
    addLookAhead "test" "acc1" 30
    let fundingTx = 
            Tx 1 [ TxIn (OutPoint 1 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" 
                 , TxOut 20000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" 
                 ] 0
        -- sendTx "acc1" [("1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ",9990000)] 10000
        -- sendTx "acc1" [("184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF",19990000)] 10000
        tx1 = decode' $ fromJust $ hexToBS "010000000177c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be000000006a47304402203e79947165a72de92581a6a53afaa552593a966db52477d18e4d97b5338d9451022037696e5e6f6792b92e796bfac6eb37ad16038dbbe019b8fffc9ad5407c1bf34a01210320e6fef44dc34322ce8e5d0a20efe55ae1308c321fab6496eece4473b9f12dd6ffffffff01706f9800000000001976a914bbc24a1dbb213c82dc6bd3e008e411e7a22ba74488ac00000000" :: Tx
        tx2 = decode' $ fromJust $ hexToBS "010000000177c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be010000006b483045022100e73c4bac3519d6dc42a0410d1a2caad3b2445a2be82ed4588cb94443550e3afc022077a590e6d49f534db74a2c30e97f735d7affb0142a91d60455a53b02afaba7dd01210206ac706bccb9a4ba7c1a6f133d5f17d847875d5ff29019913224cb32f6c57fa7ffffffff01f0053101000000001976a9144d816754accc18bb7b2cef479d948be74399337788ac00000000" :: Tx

    checkSpendableBalance 0 "test" "acc1" 0
    checkAccountBalance 0 "test" "acc1" (Balance 0) 0
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

    -- import first orphan
    importTx tx1 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not Nothing" Nothing)
    liftM (map (dbOrphanHash . entityVal)) (selectList [] [])
        >>= liftIO . (assertEqual "Wrong orphans" [txHash tx1])

    checkSpendableBalance 0 "test" "acc1" 0
    checkAccountBalance 0 "test" "acc1" (Balance 0) 0
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

    -- import second orphan
    importTx tx2 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not Nothing" Nothing)
    liftM (map (dbOrphanHash . entityVal)) (selectList [] [])
        >>= liftIO . (assertEqual "Wrong orphans" [txHash tx1, txHash tx2])

    checkSpendableBalance 0 "test" "acc1" 0
    checkAccountBalance 0 "test" "acc1" (Balance 0) 0
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

    importTx fundingTx NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (txHash fundingTx, TxPending, True)))
    liftM (map (dbOrphanHash . entityVal)) (selectList [] [])
        >>= liftIO . (assertEqual "Orphan list should be empty" [])
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash fundingTx)
        >>= liftIO . (assertEqual "Confidence is not TxPending" TxPending) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxPending" TxPending) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxPending" TxPending) 

    checkSpendableBalance 0 "test" "acc1" 29980000
    checkAccountBalance 0 "test" "acc1" (Balance 29980000) 0
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 10000000) 1 1 0
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 0) (Balance 20000000) 1 1 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 9990000) (Balance 9990000) 1 0 0
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 19990000) (Balance 19990000) 1 0 0

    -- The 1-conf balance should be all 0
    checkSpendableBalance 1 "test" "acc1" 0
    checkAccountBalance 1 "test" "acc1" (Balance 0) 0
    checkAddressBalance 1 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 1 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 1 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 1 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

-- Creates fake testing blocks
fakeNode :: Word32 -> BlockHash -> BlockHeaderNode
fakeNode i h = BlockHeaderNode
    { nodeBlockHash = h
    , nodeHeader = BlockHeader 1 0 0 0 0 0
    , nodeHeaderHeight = i
    , nodeChainWork = 0
    , nodeMedianTimes = []
    , nodeMinWork = 0
    }

testOutDoubleSpend :: App ()
testOutDoubleSpend = do
    _ <- newWallet "test" bs1
    _ <- newAccount "test" "acc1"
    Entity wk _ <- getWalletEntity "test"
    Entity ai _ <- getAccountEntity wk "acc1"
    addLookAhead "test" "acc1" 30
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
    importTx fundingTx NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (txHash fundingTx, TxPending, True)))
    spendableCoins ai 0 >>= 
        liftIO . (assertEqual "Spendable coins is not 2" 2) . length

    checkSpendableBalance 0 "test" "acc1" 30000000
    checkAccountBalance 0 "test" "acc1" (Balance 30000000) 0
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 10000000) (Balance 10000000) 1 0 0
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

    -- Import first conflicting transaction
    importTx spend1 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (txHash spend1, TxPending, True)))
    spendableCoins ai 0 >>= 
        liftIO . (assertEqual "Spendable coins is not 3" 3) . length

    checkSpendableBalance 0 "test" "acc1" 29990000
    checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 10000000) 1 1 0
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 50000) (Balance 50000) 1 0 0
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

    -- Import second conflicting transaction
    importTx spend2 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (txHash spend2, TxPending, True)))
    spendableCoins ai 0 >>= 
        liftIO . (assertEqual "Spendable coins is not 1" 1) . length

    checkSpendableBalance 0 "test" "acc1" 20000000
    checkAccountBalance 0 "test" "acc1" BalanceConflict 2
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" BalanceConflict (Balance 10000000) 1 0 2
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" BalanceConflict BalanceConflict 0 0 2
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" BalanceConflict BalanceConflict 0 0 2

    -- Check 1-conf balance
    checkSpendableBalance 1 "test" "acc1" 0
    checkAccountBalance 1 "test" "acc1" (Balance 0) 0
    checkAddressBalance 1 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 1 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 1 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 1 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0
    
    --Confirm funding transaction
    importBlock (BestBlock $ fakeNode 0 0x01) [txHash fundingTx]

    checkSpendableBalance 0 "test" "acc1" 20000000
    checkAccountBalance 0 "test" "acc1" BalanceConflict 2
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" BalanceConflict (Balance 10000000) 1 0 2
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" BalanceConflict BalanceConflict 0 0 2
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" BalanceConflict BalanceConflict 0 0 2

    checkSpendableBalance 1 "test" "acc1" 20000000
    checkAccountBalance 1 "test" "acc1" BalanceConflict 2
    checkAddressBalance 1 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" BalanceConflict (Balance 10000000) 1 0 2
    checkAddressBalance 1 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 1 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 1 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

    checkSpendableBalance 2 "test" "acc1" 0
    checkAccountBalance 2 "test" "acc1" (Balance 0) 0
    checkAddressBalance 2 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 2 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 2 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 2 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

    importBlock (BestBlock $ fakeNode 1 0x02) [txHash spend2]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" 
            [txHash fundingTx, txHash spend2, txHash spend2])

    checkSpendableBalance 0 "test" "acc1" 29990000
    checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 10000000) 1 1 0
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 50000) (Balance 50000) 1 0 0

    --Create a fork. Nothing should change from the tests above
    importBlock (SideBlock $ fakeNode 1 0x03) [txHash spend1]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" 
            [txHash fundingTx, txHash spend2, txHash spend2])

    checkSpendableBalance 0 "test" "acc1" 29990000
    checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 10000000) 1 1 0
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 50000) (Balance 50000) 1 0 0

    -- Trigger a reorg
    let s = fakeNode 0 0x01
        o = [fakeNode 0 0x01, fakeNode 1 0x02]
        n = [fakeNode 0 0x01, fakeNode 1 0x03, fakeNode 2 0x04] 
    importBlock (BlockReorg s o n) []
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend2)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" 
            [txHash fundingTx, txHash spend1, txHash spend1])

    checkSpendableBalance 0 "test" "acc1" 29990000
    checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 10000000) 1 1 0
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 50000) (Balance 50000) 1 0 0
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

    -- Trigger another reorg
    importBlock (SideBlock $ fakeNode 2 0x05) []
    let s' = fakeNode 0 0x01
        o' = [fakeNode 0 0x01, fakeNode 1 0x03, fakeNode 2 0x04]
        n' = [fakeNode 0 0x01, fakeNode 1 0x02, fakeNode 2 0x05, fakeNode 3 0x06] 
    importBlock (BlockReorg s' o' n') []
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash spend2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" 
            [txHash fundingTx, txHash spend2, txHash spend2])

    checkSpendableBalance 0 "test" "acc1" 29990000
    checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
    checkAddressBalance 0 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 10000000) 1 1 0
    checkAddressBalance 0 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 0 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 0 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 50000) (Balance 50000) 1 0 0

    -- Check 3-conf balance
    checkSpendableBalance 3 "test" "acc1" 29990000
    checkAccountBalance 3 "test" "acc1" (Balance 29990000) 0
    checkAddressBalance 3 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 10000000) 1 1 0
    checkAddressBalance 3 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 3 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 3 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 50000) (Balance 50000) 1 0 0

    -- Check 4-conf balance
    checkSpendableBalance 4 "test" "acc1" 20000000
    checkAccountBalance 4 "test" "acc1" (Balance 20000000) 0
    checkAddressBalance 4 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 10000000) 1 1 0
    checkAddressBalance 4 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 20000000) (Balance 20000000) 1 0 0
    checkAddressBalance 4 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 4 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

    -- Check 5-conf balance
    checkSpendableBalance 5 "test" "acc1" 0
    checkAccountBalance 5 "test" "acc1" (Balance 0) 0
    checkAddressBalance 5 "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 5 "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 5 "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ" (Balance 0) (Balance 0) 0 0 0
    checkAddressBalance 5 "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF" (Balance 0) (Balance 0) 0 0 0

testInDoubleSpend :: App ()
testInDoubleSpend = do
    _ <- newWallet "test" bs1
    _ <- newAccount "test" "acc1"
    Entity wk _ <- getWalletEntity "test"
    Entity ai _ <- getAccountEntity wk "acc1"
    addLookAhead "test" "acc1" 30
    let tx1 = Tx 1 [ TxIn (OutPoint 5 5) (BS.pack [1]) maxBound ] 
                   [ TxOut 10000000 $
                      encodeOutputBS $ PayPKHash $ fromJust $ 
                      base58ToAddr "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" 
                   ] 0
        tx2 = Tx 1 [ TxIn (OutPoint 5 5) (BS.pack [1]) maxBound ]
                   [ TxOut 20000000 $
                      encodeOutputBS $ PayPKHash $ fromJust $ 
                      base58ToAddr "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" 
                   ] 0

    -- Import first conflicting transaction
    importTx tx1 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (txHash tx1, TxPending, True)))
    spendableCoins ai 0 >>= 
        liftIO . (assertEqual "Spendable coins is not 1" 1) . length
    checkAccountBalance 0 "test" "acc1" (Balance 10000000) 0
    checkSpendableBalance 0 "test" "acc1" 10000000

    -- Import second conflicting transaction
    importTx tx2 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (txHash tx2, TxPending, True)))
    spendableCoins ai 0 >>= 
        liftIO . (assertEqual "Spendable coins is not 0" 0) . length
    checkAccountBalance 0 "test" "acc1" BalanceConflict 2
    checkSpendableBalance 0 "test" "acc1" 0

    --Import fake block
    importBlock (BestBlock $ fakeNode 0 0x01) []
    importBlock (BestBlock $ fakeNode 1 0x02) [txHash tx2]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx2])
    checkAccountBalance 0 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 0 "test" "acc1" 20000000
    checkAccountBalance 1 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 1 "test" "acc1" 20000000
    checkAccountBalance 2 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 2 "test" "acc1" 0

    --Create a fork. Nothing should change from the tests above
    importBlock (SideBlock $ fakeNode 1 0x03) [txHash tx1]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx2])
    checkAccountBalance 0 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 0 "test" "acc1" 20000000
    checkAccountBalance 1 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 1 "test" "acc1" 20000000
    checkAccountBalance 2 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 2 "test" "acc1" 0

    -- Trigger a reorg
    let s = fakeNode 0 0x01
        o = [fakeNode 0 0x01, fakeNode 1 0x02]
        n = [fakeNode 0 0x01, fakeNode 1 0x03, fakeNode 2 0x04] 
    importBlock (BlockReorg s o n) []
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx1])
    checkAccountBalance 0 "test" "acc1" (Balance 10000000) 0
    checkSpendableBalance 0 "test" "acc1" 10000000
    checkAccountBalance 2 "test" "acc1" (Balance 10000000) 0
    checkSpendableBalance 2 "test" "acc1" 10000000
    checkAccountBalance 3 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 3 "test" "acc1" 0

    -- Trigger another reorg
    importBlock (SideBlock $ fakeNode 2 0x05) []
    let s' = fakeNode 0 0x01
        o' = [fakeNode 0 0x01, fakeNode 1 0x03, fakeNode 2 0x04]
        n' = [fakeNode 0 0x01, fakeNode 1 0x02, fakeNode 2 0x05, fakeNode 3 0x06] 
    importBlock (BlockReorg s' o' n') []
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx2])
    checkAccountBalance 0 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 0 "test" "acc1" 20000000
    checkAccountBalance 3 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 3 "test" "acc1" 20000000
    checkAccountBalance 4 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 4 "test" "acc1" 0

-- tx1, tx2 and tx3 form a chain, and tx4 is in conflict with tx1
testDoubleSpendChain :: App ()
testDoubleSpendChain = do
    _ <- newWallet "test" bs1
    _ <- newAccount "test" "acc1"
    Entity wk _ <- getWalletEntity "test"
    Entity ai _ <- getAccountEntity wk "acc1"
    addLookAhead "test" "acc1" 30
    let tx1 = Tx 1 [ TxIn (OutPoint 4 4) (BS.pack [1]) maxBound ] 
                   [ TxOut 10000000 $
                      encodeOutputBS $ PayPKHash $ fromJust $ 
                      base58ToAddr "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" 
                   ] 0
        -- sendTx "acc1" [("1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ",500000)] 10000
        -- sendTx "acc1" [("184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF",800000)] 10000
        tx2 = decode' $ fromJust $ hexToBS "0100000001f0fb1652c177b28328ea0b3eafcf78f4987004da30c9faa080cd8ba5f4db8164000000006b483045022100fea8d86e0bd4a041813a82d985a08402a7d59abfa26e6598f80ee79d5f92a60b02201feac0e031be6dfba08754f075f71538e235b59b7ccd323ace31acf90b4f944301210320e6fef44dc34322ce8e5d0a20efe55ae1308c321fab6496eece4473b9f12dd6ffffffff0220a10700000000001976a914bbc24a1dbb213c82dc6bd3e008e411e7a22ba74488ac50ce9000000000001976a91478046f37173d0a16deb1491b8566e26f0cb4894488ac00000000" :: Tx
        tx3 = decode' $ fromJust $ hexToBS "01000000010dcd892768a926dd2d0266ae5f835125436dd04c7f5ab2e769902b053ace71c4010000006b48304502210080286d4d6aacad96eb7edbfc3121f38c3408918063b02e18078bbdf354804098022029e837c55fe01a63d8df9a1286cd7f0415882ce65c464945b70901f1b46cc8b801210205edef805f3b3f197470ff16ded9156ddb086b5d8e43b32a7f43868baabf1f71ffffffff0200350c00000000001976a9144d816754accc18bb7b2cef479d948be74399337788ac40728400000000001976a9144122754fb5595e28e8c42eb039ab299350e9644788ac00000000" :: Tx
        tx4 = Tx 1 [ TxIn (OutPoint 4 4) (BS.pack [1]) maxBound ]
                   [ TxOut 20000000 $
                      encodeOutputBS $ PayPKHash $ fromJust $ 
                      base58ToAddr "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" 
                   ] 0

    -- Import first transaction
    importTx tx1 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (txHash tx1, TxPending, True)))
    spendableCoins ai 0 >>= 
        liftIO . (assertEqual "Spendable coins is not 1" 1) . length
    checkAccountBalance 0 "test" "acc1" (Balance 10000000) 0
    checkSpendableBalance 0 "test" "acc1" 10000000

    -- Now we spend our new coins
    importTx tx2 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (txHash tx2, TxPending, True)))
    spendableCoins ai 0 >>= 
        liftIO . (assertEqual "Spendable coins is not 2" 2) . length
    checkAccountBalance 0 "test" "acc1" (Balance 9990000) 0
    checkSpendableBalance 0 "test" "acc1" 9990000

    -- Let's confirm these two transactions
    importBlock (BestBlock $ fakeNode 0 0x01) []
    importBlock (BestBlock $ fakeNode 1 0x02) [txHash tx1, txHash tx2]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx2, txHash tx2])
    checkAccountBalance 0 "test" "acc1" (Balance 9990000) 0
    checkSpendableBalance 0 "test" "acc1" 9990000
    checkAccountBalance 1 "test" "acc1" (Balance 9990000) 0
    checkSpendableBalance 1 "test" "acc1" 9990000
    checkAccountBalance 2 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 2 "test" "acc1" 0

    -- Now let's add tx4 which is in conflict with tx1
    importTx tx4 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not dead" 
            (Just (txHash tx4, TxDead, False)))
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx4)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx2, txHash tx2])
    checkAccountBalance 0 "test" "acc1" (Balance 9990000) 0
    checkSpendableBalance 0 "test" "acc1" 9990000
    checkAccountBalance 1 "test" "acc1" (Balance 9990000) 0
    checkSpendableBalance 1 "test" "acc1" 9990000
    checkAccountBalance 2 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 2 "test" "acc1" 0

    -- Now we create a fork that contains tx4. Nothing should change
    importBlock (SideBlock $ fakeNode 1 0x03) [txHash tx4]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx4)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx2, txHash tx2])
    checkAccountBalance 0 "test" "acc1" (Balance 9990000) 0
    checkSpendableBalance 0 "test" "acc1" 9990000
    checkAccountBalance 1 "test" "acc1" (Balance 9990000) 0
    checkSpendableBalance 1 "test" "acc1" 9990000
    checkAccountBalance 2 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 2 "test" "acc1" 0

    -- Now we trigger a reorg that validates tx4. tx1 and tx2 should be dead
    let s = fakeNode 0 0x01
        o = [fakeNode 0 0x01, fakeNode 1 0x02]
        n = [fakeNode 0 0x01, fakeNode 1 0x03, fakeNode 2 0x04] 
    importBlock (BlockReorg s o n) []
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx4)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx4])
    checkAccountBalance 0 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 0 "test" "acc1" 20000000
    checkAccountBalance 2 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 2 "test" "acc1" 20000000
    checkAccountBalance 3 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 3 "test" "acc1" 0

    -- Now we add tx3 on top of tx2. It should be dead.
    importTx tx3 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not dead" 
            (Just (txHash tx3, TxDead, True)))
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx3)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx4)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx4])
    checkAccountBalance 0 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 0 "test" "acc1" 20000000
    checkAccountBalance 2 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 2 "test" "acc1" 20000000
    checkAccountBalance 3 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 3 "test" "acc1" 0

    -- Let's reorg back to the original chain. tx1, tx2 and tx3 should be
    -- building and tx4 should be dead.
    importBlock (SideBlock $ fakeNode 2 0x05) []
    let s' = fakeNode 0 0x01
        o' = [fakeNode 0 0x01, fakeNode 1 0x03, fakeNode 2 0x04]
        n' = [fakeNode 0 0x01, fakeNode 1 0x02, fakeNode 2 0x05, fakeNode 3 0x06] 
    importBlock (BlockReorg s' o' n') []
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx3)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxPending) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx4)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx2, txHash tx3, txHash tx3])
    checkAccountBalance 0 "test" "acc1" (Balance 9980000) 0
    checkSpendableBalance 0 "test" "acc1" 9980000
    checkAccountBalance 3 "test" "acc1" (Balance 500000) 0
    checkSpendableBalance 3 "test" "acc1" 500000
    checkAccountBalance 4 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 4 "test" "acc1" 0

-- We have 2 coins, c1 and c2. tx1 spends from c1, tx2 spends from c1 and c2,
-- tx3 spends from c2. So we can either have tx2 valid or tx1 and tx3 as valid.
testDoubleSpendGroup :: App ()
testDoubleSpendGroup = do
    _ <- newWallet "test" bs1
    _ <- newAccount "test" "acc1"
    Entity wk _ <- getWalletEntity "test"
    Entity ai _ <- getAccountEntity wk "acc1"
    addLookAhead "test" "acc1" 30
    let fundingTx = 
            Tx 1 [ TxIn (OutPoint 1 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" 
                 , TxOut 20000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" 
                 ] 0
        -- sendTx "acc1" [("1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ",9990000)] 10000
        -- sendTx "acc1" [("184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF",29990000)] 10000
        -- sendTx "acc1" [("1FkBfN2P6RdvSE6M4k1BGZqFYRLXMXyJen",19990000)] 10000
        tx1 = decode' $ fromJust $ hexToBS "010000000177c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be000000006a47304402203e79947165a72de92581a6a53afaa552593a966db52477d18e4d97b5338d9451022037696e5e6f6792b92e796bfac6eb37ad16038dbbe019b8fffc9ad5407c1bf34a01210320e6fef44dc34322ce8e5d0a20efe55ae1308c321fab6496eece4473b9f12dd6ffffffff01706f9800000000001976a914bbc24a1dbb213c82dc6bd3e008e411e7a22ba74488ac00000000" :: Tx
        tx2 = decode' $ fromJust $ hexToBS "010000000277c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be000000006a473044022041814327248f0d5dc3f2046e5d04d9429e08ba1364062755b381ef4b85a195f202200d6968e737ff15767af1663cf582e6954b56f8dd717de2b7c05ecb8460f6a13601210320e6fef44dc34322ce8e5d0a20efe55ae1308c321fab6496eece4473b9f12dd6ffffffff77c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be010000006a47304402200d03e75b7fb298c6025f3134834e937a4d3180c1fc29b18f888406ac117a19320220180ebebc8468c3750142f3b0df3d242d53237861fc987f16a94735686284f2e801210206ac706bccb9a4ba7c1a6f133d5f17d847875d5ff29019913224cb32f6c57fa7ffffffff01709cc901000000001976a9144d816754accc18bb7b2cef479d948be74399337788ac00000000" :: Tx
        tx3 = decode' $ fromJust $ hexToBS "010000000177c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be010000006a47304402205ebb211e7e073ab79a02af5db840d4829335e110ddffa81a26cacc91569f963802201bf901de4dc626b5ccce4f15ef22682cb210b7a22bb5817d310f0d65fbef20bc01210206ac706bccb9a4ba7c1a6f133d5f17d847875d5ff29019913224cb32f6c57fa7ffffffff01f0053101000000001976a914a1bc8c0a9a20bf4d629be585f8764a475090cae788ac00000000" :: Tx
    
    -- Import funding transaction
    importTx fundingTx NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (txHash fundingTx, TxPending, True)))
    spendableCoins ai 0 >>= 
        liftIO . (assertEqual "Spendable coins is not 2" 2) . length
    checkAccountBalance 0 "test" "acc1" (Balance 30000000) 0
    checkSpendableBalance 0 "test" "acc1" 30000000

    -- Import first transaction
    importTx tx1 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (txHash tx1, TxPending, True)))
    spendableCoins ai 0 >>= 
        liftIO . (assertEqual "Spendable coins is not 2" 2) . length
    checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
    checkSpendableBalance 0 "test" "acc1" 29990000

    -- Confirm the funding transaction
    importBlock (BestBlock $ fakeNode 0 0x01) [txHash fundingTx]
    checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
    checkSpendableBalance 0 "test" "acc1" 29990000
    checkAccountBalance 1 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 1 "test" "acc1" 20000000
    checkAccountBalance 2 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 2 "test" "acc1" 0

    -- Let's confirm the first transaction
    importBlock (BestBlock $ fakeNode 1 0x02) [txHash tx1]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash fundingTx, txHash tx1])
    checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
    checkSpendableBalance 0 "test" "acc1" 29990000
    checkAccountBalance 1 "test" "acc1" (Balance 29990000) 0
    checkSpendableBalance 1 "test" "acc1" 29990000
    checkAccountBalance 2 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 2 "test" "acc1" 20000000
    checkAccountBalance 3 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 3 "test" "acc1" 0

    -- Import second transaction
    importTx tx2 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not TxDead" 
            (Just (txHash tx2, TxDead, True)))
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash fundingTx, txHash tx1])
    checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
    checkSpendableBalance 0 "test" "acc1" 29990000
    checkAccountBalance 1 "test" "acc1" (Balance 29990000) 0
    checkSpendableBalance 1 "test" "acc1" 29990000
    checkAccountBalance 2 "test" "acc1" (Balance 20000000) 0
    checkSpendableBalance 2 "test" "acc1" 20000000
    checkAccountBalance 3 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 3 "test" "acc1" 0

    -- Import third transaction
    importTx tx3 NetworkSource Nothing >>=
        liftIO . (assertEqual "Confidence is not TxPending" 
            (Just (txHash tx3, TxPending, True)))
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx3)
        >>= liftIO . (assertEqual "Confidence is not TxPending" TxPending) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx1, txHash tx3])
    checkAccountBalance 0 "test" "acc1" (Balance 29980000) 0
    checkSpendableBalance 0 "test" "acc1" 29980000
    checkAccountBalance 1 "test" "acc1" (Balance 9990000) 0
    checkSpendableBalance 1 "test" "acc1" 9990000
    checkAccountBalance 2 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 2 "test" "acc1" 0

    -- Let's confirm tx3
    importBlock (BestBlock $ fakeNode 2 0x03) []
    importBlock (BestBlock $ fakeNode 3 0x04) [txHash tx3]
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx3)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx1, txHash tx3])
    checkAccountBalance 1 "test" "acc1" (Balance 29980000) 0
    checkSpendableBalance 1 "test" "acc1" 29980000
    checkAccountBalance 2 "test" "acc1" (Balance 9990000) 0
    checkSpendableBalance 2 "test" "acc1" 9990000
    checkAccountBalance 4 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 4 "test" "acc1" 0

    -- Now we unconfirm tx3. tx2 should remain dead because it it still in
    -- conflict with building tx1
    importBlock (SideBlock $ fakeNode 3 0x05) []
    let s = fakeNode 2 0x03
        o = [fakeNode 2 0x03, fakeNode 3 0x04]
        n = [fakeNode 2 0x03, fakeNode 3 0x05, fakeNode 4 0x06] 
    importBlock (BlockReorg s o n) []
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx3)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxPending) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx1, txHash tx3])
    checkAccountBalance 0 "test" "acc1" (Balance 29980000) 0
    checkSpendableBalance 0 "test" "acc1" 29980000
    checkAccountBalance 4 "test" "acc1" (Balance 9990000) 0
    checkSpendableBalance 4 "test" "acc1" 9990000
    checkAccountBalance 5 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 5 "test" "acc1" 0
    
    -- Now let's reorg on a new chain that makes tx2 valid. tx1 and tx3 should
    -- then be dead
    importBlock (SideBlock $ fakeNode 1 0x07) [txHash tx2]
    importBlock (SideBlock $ fakeNode 2 0x08) []
    importBlock (SideBlock $ fakeNode 3 0x09) []
    importBlock (SideBlock $ fakeNode 4 0x0a) []
    let s' = fakeNode 0 0x01
        o' = [ fakeNode 0 0x01, fakeNode 1 0x02
             , fakeNode 2 0x03, fakeNode 3 0x05
             , fakeNode 4 0x06
             ]
        n' = [ fakeNode 0 0x01, fakeNode 1 0x07
             , fakeNode 2 0x08, fakeNode 3 0x09
             , fakeNode 4 0x0a, fakeNode 5 0x0b
             ] 
    importBlock (BlockReorg s' o' n') []
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx1)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx2)
        >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxBuilding) 
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx3)
        >>= liftIO . (assertEqual "Confidence is not TxDead" TxDead) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx2])
    checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
    checkSpendableBalance 0 "test" "acc1" 29990000
    checkAccountBalance 5 "test" "acc1" (Balance 29990000) 0
    checkSpendableBalance 5 "test" "acc1" 29990000
    checkAccountBalance 6 "test" "acc1" (Balance 0) 0
    checkSpendableBalance 6 "test" "acc1" 0

testWalletDoubleSpend :: Assertion
testWalletDoubleSpend = do
    let fundingTx = 
            Tx 1 [ TxIn (OutPoint 1 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR" 
                 , TxOut 20000000 $
                    encodeOutputBS $ PayPKHash $ fromJust $ 
                    base58ToAddr "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r" 
                 ] 0
        -- sendTx "acc1" [("1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ",9990000)] 10000
        -- sendTx "acc1" [("184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF",29990000)] 10000
        -- sendTx "acc1" [("1FkBfN2P6RdvSE6M4k1BGZqFYRLXMXyJen",19990000)] 10000
        -- sendTx "acc1" [("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR",9980000)] 10000
        tx1 = decode' $ fromJust $ hexToBS "010000000177c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be000000006a47304402203e79947165a72de92581a6a53afaa552593a966db52477d18e4d97b5338d9451022037696e5e6f6792b92e796bfac6eb37ad16038dbbe019b8fffc9ad5407c1bf34a01210320e6fef44dc34322ce8e5d0a20efe55ae1308c321fab6496eece4473b9f12dd6ffffffff01706f9800000000001976a914bbc24a1dbb213c82dc6bd3e008e411e7a22ba74488ac00000000" :: Tx
        tx2 = decode' $ fromJust $ hexToBS "010000000277c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be000000006a473044022041814327248f0d5dc3f2046e5d04d9429e08ba1364062755b381ef4b85a195f202200d6968e737ff15767af1663cf582e6954b56f8dd717de2b7c05ecb8460f6a13601210320e6fef44dc34322ce8e5d0a20efe55ae1308c321fab6496eece4473b9f12dd6ffffffff77c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be010000006a47304402200d03e75b7fb298c6025f3134834e937a4d3180c1fc29b18f888406ac117a19320220180ebebc8468c3750142f3b0df3d242d53237861fc987f16a94735686284f2e801210206ac706bccb9a4ba7c1a6f133d5f17d847875d5ff29019913224cb32f6c57fa7ffffffff01709cc901000000001976a9144d816754accc18bb7b2cef479d948be74399337788ac00000000" :: Tx
        tx3 = decode' $ fromJust $ hexToBS "010000000177c50936caaa97a7cf69b45579252d7712760285b53751cb844c74af55bd33be010000006a47304402205ebb211e7e073ab79a02af5db840d4829335e110ddffa81a26cacc91569f963802201bf901de4dc626b5ccce4f15ef22682cb210b7a22bb5817d310f0d65fbef20bc01210206ac706bccb9a4ba7c1a6f133d5f17d847875d5ff29019913224cb32f6c57fa7ffffffff01f0053101000000001976a914a1bc8c0a9a20bf4d629be585f8764a475090cae788ac00000000" :: Tx
        tx4 = decode' $ fromJust $ hexToBS "01000000018f8beb42a53d2fab0614867e8aec484d3e0b1097ab91a6ad3967b5f753770eb2000000006b483045022100ccf17481bd37aecb9fc6130f9fcfa73914bb0ea535af45fc989e6b3faa3f271802201b90209622ecd01417fb7e263598278541ac359cb7572dd3e4dec9fca2bd01ad0121038fecbf5bca807297c0290d8912dcb4348b46de7272c942a71bb8465de7d63293ffffffff0160489800000000001976a9141bb874c1b168e928ff148f2cf7ae9ad69e3e49f988ac00000000" :: Tx

    assertException (WalletException "Can not import double-spending transaction") $ do
        _ <- newWallet "test" bs1
        _ <- newAccount "test" "acc1"
        addLookAhead "test" "acc1" 30
        -- Import funding transaction
        importTx fundingTx NetworkSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash fundingTx, TxPending, True)))
        -- Import first transaction
        importTx tx1 NetworkSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash tx1, TxPending, True)))
        -- Import second transaction
        importTx tx2 NetworkSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash tx2, TxPending, True)))
        -- Importing this transaction as a wallet transaction should fail as it
        -- double spends a coin
        importTx tx3 WalletSource Nothing 

    assertException (WalletException "Can not import double-spending transaction") $ do
        _ <- newWallet "test" bs1
        _ <- newAccount "test" "acc1"
        addLookAhead "test" "acc1" 30
        -- Import funding transaction
        importTx fundingTx NetworkSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash fundingTx, TxPending, True)))
        -- Import first transaction
        importTx tx1 NetworkSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash tx1, TxPending, True)))
        -- Import second transaction
        importTx tx2 NetworkSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash tx2, TxPending, True)))
        -- Importing this transaction as a wallet transaction should fail as it
        -- builds on top of a conflicting chain
        importTx tx4 WalletSource Nothing 

    -- Now we make tx2 dead so we can import tx3 and tx4 from the wallet
    runUnit $ do
        _ <- newWallet "test" bs1
        _ <- newAccount "test" "acc1"
        Entity wk _ <- getWalletEntity "test"
        Entity ai _ <- getAccountEntity wk "acc1"
        addLookAhead "test" "acc1" 30
        -- Import funding transaction
        importTx fundingTx NetworkSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash fundingTx, TxPending, True)))
        -- Import first transaction
        importTx tx1 NetworkSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash tx1, TxPending, True)))
        -- Import second transaction
        importTx tx2 NetworkSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash tx2, TxPending, True)))

        -- confirm the first transaction
        importBlock (BestBlock $ fakeNode 0 0x01) [txHash fundingTx]
        importBlock (BestBlock $ fakeNode 1 0x02) [txHash tx1]
        checkAccountBalance 0 "test" "acc1" (Balance 29990000) 0
        checkSpendableBalance 0 "test" "acc1" 29990000
        checkAccountBalance 2 "test" "acc1" (Balance 20000000) 0
        checkSpendableBalance 2 "test" "acc1" 20000000
        checkAccountBalance 3 "test" "acc1" (Balance 0) 0
        checkSpendableBalance 3 "test" "acc1" 0

        -- now we can import tx3
        importTx tx3 WalletSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash tx3, TxPending, True)))
        liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx3)
            >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxPending) 
        liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
            >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx1, txHash tx3])
        checkAccountBalance 0 "test" "acc1" (Balance 29980000) 0
        checkSpendableBalance 0 "test" "acc1" 29980000
        checkAccountBalance 1 "test" "acc1" (Balance 9990000) 0
        checkSpendableBalance 1 "test" "acc1" 9990000
        checkAccountBalance 2 "test" "acc1" (Balance 0) 0
        checkSpendableBalance 2 "test" "acc1" 0

        -- and tx4
        importTx tx4 WalletSource Nothing >>=
            liftIO . (assertEqual "Confidence is not TxPending" 
                (Just (txHash tx4, TxPending, False)))
        liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash tx4)
            >>= liftIO . (assertEqual "Confidence is not TxBuilding" TxPending) 
        liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
            >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash tx3, txHash tx4])
        checkAccountBalance 0 "test" "acc1" (Balance 29970000) 0
        checkSpendableBalance 0 "test" "acc1" 29970000
        checkAccountBalance 1 "test" "acc1" (Balance 0) 0
        checkSpendableBalance 1 "test" "acc1" 0

-- This test create a multisig account with the key of testImportMultisig2
testImportMultisig :: App ()
testImportMultisig = do
    --testImportMultisig2
    _ <- newWallet "test" bs1
    _ <- newMSAccount "test" "ms1" 2 2 $
        [fromJust $ xPubImport "xpub68yUKy6M9BSM3HMejgrwGipKXSn22QzTqhFguvcE4yksoHP2TJjCadfE2fHyvBAE9VpGkxygrqsDqohyeXMZUM8Fh3GxRGKpFXQiJ6vgrNG"]
    Entity wk _ <- getWalletEntity "test"
    Entity ai _ <- getAccountEntity wk "ms1"
    addLookAhead "test" "ms1" 30
    let fundingTx = 
            Tx 1 [ TxIn (OutPoint 1 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayScriptHash $ fromJust $ 
                    base58ToAddr "38pfoRLKzxUTcCdA4PTgeVQBePLtcC28iv" 
                 ] 0
        toImport = decode' $ fromJust $ hexToBS "0100000001d53c19abd25c333a0d348b10c10f1781e12ddc9fc82d95743b249b88cc50a72900000000da00483045022100ae08adb9dbb3974c95f39400f22b28b8f3920e131fe8c43b942632718c018b2902204510743685522f4e29bc0cac7938b7ece87ae4e2a93182e5bafc0e88bdf9e3c2014730440220351bafa1f3f0c82720d9f887d97c23681bfbded78119201cbed00b57e5eff73e02205da173d08be046d125cc3bbc35ce1be16652d0990f247662f7a171c49381badd014752210320e6fef44dc34322ce8e5d0a20efe55ae1308c321fab6496eece4473b9f12dd62103d9097c7e36d393672fd366f303e1c30c1421e1e72bedc73d49ae92e4ba5ed83552aeffffffff02404b4c000000000017a9143c8ea9e0b86430bed5805b86023ce11175c26ad38730244c000000000017a91473a92334bcf250c85a30fd3cb7fbebc49d822ccc8700000000" :: Tx

    _ <- importTx fundingTx NetworkSource Nothing
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash fundingTx)
        >>= liftIO . (assertEqual "Confidence is not TxPending" TxPending) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash fundingTx])
    checkAccountBalance 0 "test" "ms1" (Balance 10000000) 0
    checkSpendableBalance 0 "test" "ms1" 10000000

    (h,c,_) <- sendTx "test" "ms1" 0 
        [(fromJust $ base58ToAddr "37DDNVZZqU5i8XjyKyvZZv7edjCn3XrRsm", 5000000)] 10000
    liftIO $ assertEqual "Completed status is not False" False c
    liftM (dbTxConfidence . entityVal) (getTxEntity h)
        >>= liftIO . (assertEqual "Confidence is not TxOffline" TxOffline) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [])
    liftM (map accTxHash) (txList "test" "ms1") 
        >>= liftIO . (assertEqual "Wrong txhash in acc list" [txHash fundingTx, h])
    checkAccountBalance 0 "test" "ms1" (Balance 9990000) 0
    checkSpendableBalance 0 "test" "ms1" 0

    (h2,c2,_) <- signWalletTx "test" "ms1" toImport 
    liftIO $ assertEqual "Completed status is not True" True c2
    liftM (dbTxConfidence . entityVal) (getTxEntity h2)
        >>= liftIO . (assertEqual "Confidence is not TxPending" TxPending) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash toImport, txHash toImport])
    liftM (map accTxHash) (txList "test" "ms1") 
        >>= liftIO . (assertEqual "Wrong txhash in acc list" [txHash fundingTx, h2])
    checkAccountBalance 0 "test" "ms1" (Balance 9990000) 0
    checkSpendableBalance 0 "test" "ms1" 9990000


-- This test create a multisig account with the key of testImportMultisig1
testImportMultisig2 :: App ()
testImportMultisig2 = do
    _ <- newWallet "test" bs2
    _ <- newMSAccount "test" "ms1" 2 2 [fromJust $ xPubImport "xpub69iinth3CTrfh5efv7baTWwk9hHi4zqcQEsNFgVwEJvdaZVEPytZzmNxjYTnF5F5x2CamLXvmD1T4RhpsuaXSFPo2MnLN5VqWqrWb82U7ED"]
    Entity wk _ <- getWalletEntity "test"
    Entity ai _ <- getAccountEntity wk "ms1"
    addLookAhead "test" "ms1" 30
    let fundingTx = 
            Tx 1 [ TxIn (OutPoint 1 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayScriptHash $ fromJust $ 
                    base58ToAddr "38pfoRLKzxUTcCdA4PTgeVQBePLtcC28iv" 
                 ] 0
        toSign = decode' $ fromJust $ hexToBS "0100000001d53c19abd25c333a0d348b10c10f1781e12ddc9fc82d95743b249b88cc50a729000000009200483045022100ae08adb9dbb3974c95f39400f22b28b8f3920e131fe8c43b942632718c018b2902204510743685522f4e29bc0cac7938b7ece87ae4e2a93182e5bafc0e88bdf9e3c2014752210320e6fef44dc34322ce8e5d0a20efe55ae1308c321fab6496eece4473b9f12dd62103d9097c7e36d393672fd366f303e1c30c1421e1e72bedc73d49ae92e4ba5ed83552aeffffffff02404b4c000000000017a9143c8ea9e0b86430bed5805b86023ce11175c26ad38730244c000000000017a91473a92334bcf250c85a30fd3cb7fbebc49d822ccc8700000000" :: Tx
    _ <- importTx fundingTx NetworkSource Nothing
    liftM (dbTxConfidence . entityVal) (getTxEntity $ txHash fundingTx)
        >>= liftIO . (assertEqual "Confidence is not TxPending" TxPending) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [txHash fundingTx])
    return ()
    checkAccountBalance 0 "test" "ms1" (Balance 10000000) 0
    checkSpendableBalance 0 "test" "ms1" 10000000

    (h,c,_) <- signWalletTx "test" "ms1" toSign 
    liftIO $ assertEqual "Completed status is not True" True c
    liftM (dbTxConfidence . entityVal) (getTxEntity h)
        >>= liftIO . (assertEqual "Confidence is not TxPending" TxPending) 
    liftM (map (outPointHash . coinOutPoint)) (spendableCoins ai 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [h,h])
    liftM (map accTxHash) (txList "test" "ms1") 
        >>= liftIO . (assertEqual "Wrong txhash in acc list" [txHash fundingTx, h])
    checkAccountBalance 0 "test" "ms1" (Balance 9990000) 0
    checkSpendableBalance 0 "test" "ms1" 9990000

checkAddressBalance :: Word32 -> String ->  Balance -> Balance 
                    -> Int -> Int -> Int -> App ()
checkAddressBalance conf addrStr fb tr ft st ct = do
    addrM <- selectFirst [ DbAddressValue ==. (fromJust $ base58ToAddr addrStr) ] []
    let p = toPaymentAddr $ entityVal $ fromJust addrM
    BalanceAddress _ fb' tr' ft' st' ct' <- addressBalance p conf

    liftIO $ assertEqual ("Final balance is not " ++ show fb) fb fb'
    liftIO $ assertEqual ("Total received is not " ++ show tr) tr tr'
    liftIO $ assertEqual ("Funding txs length is not " ++ show ft) ft (length ft')
    liftIO $ assertEqual ("Spending txs length is not " ++ show st) st (length st')
    liftIO $ assertEqual ("Conflict txs length is not " ++ show ct) ct (length ct')

checkAccountBalance :: Word32 -> String -> String -> Balance -> Int -> App ()
checkAccountBalance conf wallet name b cs = do
    (b', cs') <- accountBalance wallet name conf
    liftIO $ assertEqual ( "Balance is not " ++ show b) b b'
    liftIO $ assertEqual ( "Conflict txs length is not " ++ show cs) cs (length cs')

checkSpendableBalance :: Word32 -> String -> String -> Word64 -> App ()
checkSpendableBalance conf wallet name b = do
    b' <- spendableAccountBalance wallet name conf
    liftIO $ assertEqual ( "Spendable balance is not " ++ show b) b b'

