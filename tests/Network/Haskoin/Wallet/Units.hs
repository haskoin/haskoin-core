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
import qualified Data.Map.Strict as M (fromList, empty)
import qualified Data.Text as T (Text)
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
    , getBy
    , (==.)
    )
import Database.Persist.Sqlite 
    ( runSqlite
    , runMigrationSilent
    , SqlPersistT
    )

import Network.Haskoin.Wallet.Internals

import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Util

type App = SqlPersistT (NoLoggingT (ResourceT IO))

tests :: [Test]
tests =
    [ testGroup "KeyRing creation"
        [ testCase "Calling newKeyRing with an empty seed should fail" $
            assertException
                (WalletException "The seed is empty") 
                (newKeyRing "main" BS.empty)

        , testCase "Creating two KeyRings with the same name should fail" $
            assertException
                (WalletException "KeyRing main already exists") $ do
                    newKeyRing "main" $ BS.pack [0]
                    newKeyRing "main" $ BS.pack [1]
        ]
    , testGroup "Account tests"
        [ testCase "Creating two accounts with the same name should fail" $
            assertException (WalletException "Account acc already exists") $ do
                newKeyRing "main" $ BS.pack [1]
                newAccount "main" "acc" 
                newAccount "main" "acc" 

        , testCase "Invalid multisig parameters (0 of 1)" $
            assertException (WalletException "Invalid multisig parameters") $ do
                newKeyRing "main" $ BS.pack [0]
                newAccountMultisig "main" "ms" [] 0 1 

        , testCase "Invalid multisig parameters (2 of 1)" $
            assertException (WalletException "Invalid multisig parameters") $ do
                newKeyRing "main" $ BS.pack [0] 
                newAccountMultisig "main" "ms" [] 2 1

        , testCase "Invalid multisig parameters (15 of 16)" $
            assertException (WalletException "Invalid multisig parameters") $ do
                newKeyRing "main" $ BS.pack [0]
                newAccountMultisig "main" "ms" [] 15 16

        , testCase "To many multisig keys (2 keys for 1 of 2)" $
            assertException
                (WalletException "Adding too many keys to account ms") $ do
                    newKeyRing "main" $ BS.pack [0]
                    newAccountMultisig 
                        "main" "ms" 
                        [ deriveXPubKey $ makeXPrvKey (BS.pack [1])
                        , deriveXPubKey $ makeXPrvKey (BS.pack [2])
                        ] 1 2

        , testCase "Calling addAccountKeys with an empty key list should fail" $
            assertException
                (WalletException "No keys have been provided") $ do
                    newKeyRing "main" $ BS.pack [0]
                    addAccountKeys "main" "default" []

        , testCase "Calling addAccountKeys on a non-multisig account should fail" $
            assertException
                (WalletException "Account default is not a multisig account") $ do
                    newKeyRing "main" $ BS.pack [0]
                    newAccount "main" "default" 
                    addAccountKeys "main" "default"
                        [ deriveXPubKey $ makeXPrvKey (BS.pack [1]) ]

        , testCase "Adding keys to a complete multisig account should fail" $
            assertException
                (WalletException "Adding too many keys to account ms") $ do
                    newKeyRing "main" $ BS.pack [0]
                    newAccountMultisig "main" "ms"
                        [ deriveXPubKey $ makeXPrvKey (BS.pack [1])
                        , deriveXPubKey $ makeXPrvKey (BS.pack [2])
                        ] 2 3
                    addAccountKeys "main" "ms" 
                        [ deriveXPubKey $ makeXPrvKey (BS.pack [3]) ]

        , testCase "Getting a non-existing account should fail" $
            assertException
                (WalletException "Account default does not exist") $ do
                    newKeyRing "main" $ BS.pack [0] 
                    getAccount "main" "default"

        , testCase "Listing addresses of a non-existing account should fail" $
            assertException (WalletException "Account default does not exist") $ do
                newKeyRing "main" $ BS.pack [0] 
                addressPage "main" "default" AddressExternal $ 
                    PageRequest 1 1 False
                
        ]
    , testGroup "Address tests"
        [ testCase "Displaying page 0 should fail" $
            assertException 
                (WalletException "Invalid page request (Page: 0, Page size: 1)" ) $ do 
                    newKeyRing "main" $ BS.pack [0]
                    addressPage "main" "default" AddressExternal $
                        PageRequest 0 1 False

        , testCase "Displaying 0 results per page should fail" $
            assertException
                (WalletException "Invalid page request (Page: 1, Page size: 0)" ) $ do 
                    newKeyRing "main" $ BS.pack [0]
                    addressPage "main" "default" AddressExternal $
                        PageRequest 1 0 False

        , testCase "Displaying a page number that is too high should fail" $
            assertException 
                (WalletException "Invalid page number 5") $ do
                    newKeyRing "main" $ BS.pack [0] 
                    newAccount "main" "default"
                    setAccountGap "main" "default" 10
                    addressPage "main" "default" AddressExternal $
                        PageRequest 5 3 False

        , testCase "Decreasing the address gap should fail" $
            assertException (WalletException "Can not decrease the gap from 15 to 14") $ do
                newKeyRing "main" $ BS.pack [0] 
                newAccount "main" "default"
                setAccountGap "main" "default" 15
                setAccountGap "main" "default" 14

        , testCase "Setting a label on a hidden address key should fail" $
            assertException (WalletException "Address index 10 is in the hidden gap") $ do
                newKeyRing "main" $ BS.pack [0] 
                newAccount "main" "default"
                setAccountGap "main" "default" 10
                setAddrLabel "main" "default" 10 AddressExternal "Gym membership"

        , testCase "Setting a label on an invalid address key should fail" $
            assertException (WalletException "Address index 20 does not exist") $ do
                newKeyRing "main" $ BS.pack [0] 
                newAccount "main" "default"
                setAccountGap "main" "default" 10
                setAddrLabel "main" "default" 20 AddressExternal "Gym membership"

        , testCase "Requesting an address prvkey on a read-only account should fail" $
            assertException
                (WalletException "Can not get private keys from read-only account default") $ do
                    newKeyRing "main" $ BS.pack [0] 
                    newAccountRead "main" "default" $
                        deriveXPubKey $ makeXPrvKey $ BS.pack [1]
                    setAccountGap "main" "default" 10
                    addressPrvKey "main" "default" 2 AddressExternal
        ]
    , testGroup "Wallet tests"
        [ testCase "Verify address derivations" $ runUnit testDerivations
        , testCase "Verify balances" $ runUnit testBalances
        , testCase "Verify balances in conflict" $ runUnit testConflictBalances
        , testCase "Offline transactions" $ runUnit testOffline
        , testCase "Kill an offline tx by spending his coins" $ runUnit testKillOffline
        , testCase "Offline transaction exceptions" testOfflineExceptions
        , testCase "Multisig test 1" $ runUnit testImportMultisig
        , testCase "Kill Tx" $ runUnit testKillTx
        ]
    ]

assertException :: (Exception e, Eq e) => e -> App a -> Assertion
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
        initWallet 0.0001
        action
    return ()

bs1 :: BS.ByteString
bs1 = fromRight $ mnemonicToSeed pass $ unwords
    [ "mass", "coast", "dance"
    , "birth", "online", "various"
    , "renew", "alert", "crunch" 
    , "middle", "absurd", "health"
    ]

pass :: String
pass = "passw0rd"

-- Creates fake testing blocks
fakeNode :: Word32 -> BlockHash -> BlockHeaderNode
fakeNode i h = BlockHeaderNode
    { nodeBlockHash = h
    , nodeHeader = BlockHeader 1 0 0 0 0 0
    , nodeHeaderHeight = i
    , nodeChainWork = 0
    , nodeChild = Nothing
    , nodeMedianTimes = []
    , nodeMinWork = 0
    }

fakeTx :: [(TxHash, Word32)] -> [(String, Word64)] -> Tx
fakeTx xs ys = 
    Tx 1 txi txo 0
  where
    txi = map (\(h,p) -> TxIn (OutPoint h p) (BS.pack [1]) maxBound) xs
    f = encodeOutputBS . PayPKHash . fromJust . base58ToAddr
    txo = map (\(a,v) -> TxOut v $ f a ) ys

testDerivations :: App ()
testDerivations = do
    newKeyRing "test" bs1
    newAccount "test" "acc1"
    setAccountGap "test" "acc1" 10

    addressUnused "test" "acc1" AddressExternal 
        >>= liftIO . assertEqual "Generated external addresses do not match"
            [ "13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR"
            , "1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r"
            , "1J7n7Lz1VKYdemEDWfyFoGQpSByK9doqeZ"
            , "184p3tofVNgFXfA7Ry3VU1uTPyr5dGCiUF"
            , "1FkBfN2P6RdvSE6M4k1BGZqFYRLXMXyJen"
            , "1MQRM1Luzq4rkrKV8ii7BiukjCa63wt91D"
            , "14zzWHCS5969DL4ZqphMrsG7p2gCSJnCV7"
            , "1FFCS3SzGduAv2MBM9Ak9tALT5snVySST"
            , "18VNX8vQre2hGneuCrXtXwB5D1NVTBUB46"
            , "17mE4ZUaWETvjyLXbTcgoyqTc3A1f7eWVs"
            ] . map (addrToBase58 . keyRingAddrAddress)

    addressUnused "test" "acc1" AddressInternal
        >>= liftIO . assertEqual "Generated internal addresses do not match"
            [ "1BwbQ8Wp7YUfaYeiQPgXu6br5e4ogKjuKd"
            , "16wQCfrqW5QegVe5pXpczHaxDmqTAn4ieM"
            , "1PZjbfPbGzvB7jvoRSkCQZfne154mjU3sY"
            , "152Nc7WrB24foAydrHJ7Sie954NgXCx5Tn"
            , "1HojKLGEQb9bZMMckXgujnv9HGCNxtowCP"
            , "13X9ds52rRYGvLwfbAvQDVU7K13j9cU7BR"
            , "1LSBEYAcmsZuxyPVpF1GqxXTRxpg4CaJPF"
            , "1MUcLFqrYhkSHjYcQdfZJRwnkEi9xWaGZU"
            , "12vgEgi8ExgCo7EBPG1kxwJGR5FCXmZpoB"
            , "1K14RjZ3he6erLHFNrPWwvmxm4nbr1MEYC"
            ] . map (addrToBase58 . keyRingAddrAddress)

-- -- We have 2 coins, c1 and c2. tx1 spends from c1, tx2 spends from c1 and c2,
-- -- tx3 spends from c2. So we can either have tx2 valid or tx1 and tx3 as valid.
testBalances :: App ()
testBalances = do
    newKeyRing "test" bs1
    newAccount "test" "acc1"
    setAccountGap "test" "acc1" 10
    Entity ai _ <- getAccount "test" "acc1"
    let fundingTx = fakeTx 
            [ (1, 0) ] 
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 10000000) 
            , ("1BECmeSVxBYCwL493wt9Vqx8mvaWozTF4r", 20000000)
            ]
    let tx1 = fakeTx
            [ (txHash fundingTx, 0)
            , (txHash fundingTx, 1)
            ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 30000000) ] -- external
        tx2 = fakeTx
            [ (txHash fundingTx, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 5000000) 
            , ("1BwbQ8Wp7YUfaYeiQPgXu6br5e4ogKjuKd", 5000000)
            ] -- external

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0

    -- Import funding transaction twice. This operation should be idempotent
    importNetTx fundingTx >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.fromList [(ai, 2)])))
    importNetTx fundingTx >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.empty)))

    spendableCoins ai 0 >>= 
        liftIO . (assertEqual "0-conf spendable coins is not 2" 2) . length
    spendableCoins ai 1 >>= 
        liftIO . (assertEqual "1-conf spendable coins is not 0" 0) . length

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 30000000") 30000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 inbalance is not 20000000") 20000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    importNetTx tx1 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.empty)))

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 inbalance is not 20000000") 20000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 outbalance is not 20000000") 20000000
            . keyRingAddrOutBalance . entityVal

    -- We re-import tx1. This operation has to be idempotent with respect to
    -- balances.
    importNetTx tx1 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.empty)))

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 inbalance is not 20000000") 20000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 outbalance is not 20000000") 20000000
            . keyRingAddrOutBalance . entityVal

    -- Importing tx2 twice. This operation has to be idempotent.
    importNetTx tx2 >>=
        liftIO . (assertEqual "Confidence is not dead" 
            (Just (TxDead, M.fromList [(ai, 1)])))
    importNetTx tx2 >>=
        liftIO . (assertEqual "Confidence is not dead" 
            (Just (TxDead, M.empty)))

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 inbalance is not 20000000") 20000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 outbalance is not 20000000") 20000000
            . keyRingAddrOutBalance . entityVal

    -- Confirm the funding transaction at height 1
    importMerkles ((BestChain [fakeNode 1 0x01])) [[txHash fundingTx]]

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 30000000") 30000000
    accountBalance ai 2 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 inbalance is not 20000000") 20000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 outbalance is not 20000000") 20000000
            . keyRingAddrOutBalance . entityVal

    -- Confirm tx1 at height 2
    importMerkles ((BestChain [fakeNode 2 0x02])) [[txHash tx1]]

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 2 >>= liftIO . (assertEqual "Balance is not 30000000") 30000000
    accountBalance ai 3 >>= liftIO . (assertEqual "Balance is not 0") 0

    -- Reorg on tx2
    let s = fakeNode 1 0x01
        o = [fakeNode 2 0x02]
        n = [fakeNode 2 0x03, fakeNode 3 0x04] 
    importMerkles (ChainReorg s o n) [[], [txHash tx2]]

    getBy (UniqueAccTx ai (txHash tx1)) >>= 
        liftIO . (assertEqual "Confidence is not dead" TxDead)
            . keyRingTxConfidence . entityVal . fromJust

    getBy (UniqueAccTx ai (txHash tx2)) >>= 
        liftIO . (assertEqual "Confidence is not building" TxBuilding)
            . keyRingTxConfidence . entityVal . fromJust

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance ai 2 >>= liftIO . (assertEqual "Balance is not 30000000") 30000000
    accountBalance ai 3 >>= liftIO . (assertEqual "Balance is not 30000000") 30000000
    accountBalance ai 4 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 inbalance is not 20000000") 20000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 5000000") 5000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    -- Reimporting tx2 should be idempotent and return TxBuilding
    importNetTx tx2 >>=
        liftIO . (assertEqual "Confidence is not building" 
            (Just (TxBuilding, M.empty)))

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance ai 2 >>= liftIO . (assertEqual "Balance is not 30000000") 30000000
    accountBalance ai 3 >>= liftIO . (assertEqual "Balance is not 30000000") 30000000
    accountBalance ai 4 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 inbalance is not 20000000") 20000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 5000000") 5000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    -- Reorg back onto tx1
    let s = fakeNode 1 0x01
        o = [fakeNode 2 0x03, fakeNode 3 0x04] 
        n = [fakeNode 2 0x02, fakeNode 3 0x05, fakeNode 4 0x06]
    importMerkles (ChainReorg s o n) [[txHash tx1], [], []]

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 2 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 3 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 4 >>= liftIO . (assertEqual "Balance is not 30000000") 30000000
    accountBalance ai 5 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 inbalance is not 20000000") 20000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 1 AddressExternal >>=
        liftIO . (assertEqual "Address 1 outbalance is not 20000000") 20000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 0") 0
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

-- tx1, tx2 and tx3 form a chain, and tx4 is in conflict with tx1
testConflictBalances :: App ()
testConflictBalances = do
    newKeyRing "test" bs1
    newAccount "test" "acc1"
    Entity ai _ <- getAccount "test" "acc1"
    setAccountGap "test" "acc1" 10
    let tx1 = fakeTx
            [ (4, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 10000000) ] 
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1BwbQ8Wp7YUfaYeiQPgXu6br5e4ogKjuKd", 4000000) -- change
            ] 
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (4, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 20000000) ] 

    -- Import first transaction
    importNetTx tx1 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.fromList [(ai, 1)])))

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    offlineBalance ai   >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    -- Import second transaction
    importNetTx tx2 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.fromList [(ai, 1)])))

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 4000000") 4000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    -- Let's confirm these two transactions
    importMerkles 
        (BestChain [fakeNode 1 0x01, fakeNode 2 0x02 ]) 
        [[txHash tx1], [txHash tx2]]

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 2 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 3 >>= liftIO . (assertEqual "Balance is not 0") 0

    -- Import third transaction
    importNetTx tx3 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.empty)))

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 2 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 3 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 4000000") 4000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 4000000") 4000000
            . keyRingAddrOutBalance . entityVal

    -- Now let's add tx4 which is in conflict with tx1
    importNetTx tx4 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxDead, M.empty)))

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 2 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 3 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 4000000") 4000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 4000000") 4000000
            . keyRingAddrOutBalance . entityVal

    -- Now we trigger a reorg that validates tx4. tx1, tx2 and tx3 should be dead
    let s = fakeNode 0 0x00
        o = [fakeNode 1 0x01, fakeNode 2 0x02]
        n = [fakeNode 1 0x03, fakeNode 2 0x04, fakeNode 3 0x05] 
    importMerkles (ChainReorg s o n) [[], [txHash tx4], []]

    getBy (UniqueAccTx ai $ txHash tx1) >>=
        liftIO . (assertEqual "tx1 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx2) >>=
        liftIO . (assertEqual "tx2 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx3) >>=
        liftIO . (assertEqual "tx3 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance ai 2 >>= liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance ai 3 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 20000000") 20000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 0") 0
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    -- Reorg back to tx1, tx2 and tx3
    let s = fakeNode 0 0x00
        o = [fakeNode 1 0x03, fakeNode 2 0x04, fakeNode 3 0x05] 
        n = [fakeNode 1 0x01, fakeNode 2 0x02, fakeNode 3 0x06, fakeNode 4 0x07]
    importMerkles (ChainReorg s o n) [[txHash tx1], [txHash tx2], [], []]

    getBy (UniqueAccTx ai $ txHash tx1) >>=
        liftIO . (assertEqual "tx1 confidence is not building") (Just TxBuilding)
            . fmap (keyRingTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx2) >>=
        liftIO . (assertEqual "tx2 confidence is not building") (Just TxBuilding)
            . fmap (keyRingTxConfidence . entityVal)

    -- Tx3 remains dead until it is included into a block. Dead transaction are
    -- only revived upon confirmations. They are not revived if they are not
    -- confirmed even if they have no conflicts anymore. 
    getBy (UniqueAccTx ai $ txHash tx3) >>=
        liftIO . (assertEqual "tx3 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx4) >>=
        liftIO . (assertEqual "tx4 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 2 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 3 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 4 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 5 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 4000000") 4000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

testOffline :: App ()
testOffline = do
    newKeyRing "test" bs1
    newAccount "test" "acc1"
    Entity ai _ <- getAccount "test" "acc1"
    setAccountGap "test" "acc1" 10
    let tx1 = fakeTx
            [ (4, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 10000000) ] 
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1BwbQ8Wp7YUfaYeiQPgXu6br5e4ogKjuKd", 4000000) -- change
            ] 
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (4, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 20000000) ] 

    -- Import first transaction
    importTx tx1 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx1, TxOffline)

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0
    offlineBalance ai   >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 0") 0
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline inbalance is not 10000000") 10000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline outbalance is not 0") 0
            . keyRingAddrOutOfflineBalance . entityVal

    -- Reimporting a transaction should me idempotent
    importTx tx1 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx1, TxOffline)

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0
    offlineBalance ai   >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 0") 0
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline inbalance is not 10000000") 10000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline outbalance is not 0") 0
            . keyRingAddrOutOfflineBalance . entityVal

    -- Import tx2
    importTx tx2 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx2, TxOffline)

    offlineBalance ai >>= liftIO . (assertEqual "Balance is not 4000000") 4000000

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline inbalance is not 10000000") 10000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline outbalance is not 10000000") 10000000
            . keyRingAddrOutOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 offline inbalance is not 4000000") 4000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 offline outbalance is not 0") 0
            . keyRingAddrOutOfflineBalance . entityVal

    -- Import tx3
    importTx tx3 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx3, TxOffline)

    offlineBalance ai >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline inbalance is not 10000000") 10000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline outbalance is not 10000000") 10000000
            . keyRingAddrOutOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 offline inbalance is not 4000000") 4000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 offline outbalance is not 4000000") 4000000
            . keyRingAddrOutOfflineBalance . entityVal

    -- Import tx4
    importTx tx4 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx4, TxOffline)

    getBy (UniqueAccTx ai $ txHash tx1) >>=
        liftIO . (assertEqual "tx1 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx2) >>=
        liftIO . (assertEqual "tx2 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx3) >>=
        liftIO . (assertEqual "tx3 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    offlineBalance ai >>= liftIO . (assertEqual "Balance is not 20000000") 20000000

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline inbalance is not 20000000") 20000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline outbalance is not 0") 0
            . keyRingAddrOutOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 offline inbalance is not 0") 0
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 offline outbalance is not 0") 0
            . keyRingAddrOutOfflineBalance . entityVal

    -- importTx should be idempotent
    importTx tx4 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx4, TxOffline)

    offlineBalance ai >>= liftIO . (assertEqual "Balance is not 20000000") 20000000

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline inbalance is not 20000000") 20000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 offline outbalance is not 0") 0
            . keyRingAddrOutOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 offline inbalance is not 0") 0
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 offline outbalance is not 0") 0
            . keyRingAddrOutOfflineBalance . entityVal

testKillOffline :: App ()
testKillOffline = do
    newKeyRing "test" bs1
    newAccount "test" "acc1"
    Entity ai _ <- getAccount "test" "acc1"
    setAccountGap "test" "acc1" 10
    let tx1 = fakeTx
            [ (4, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 10000000) ] 
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1BwbQ8Wp7YUfaYeiQPgXu6br5e4ogKjuKd", 4000000) -- change
            ] 
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 2000000) -- external
            , ("1BwbQ8Wp7YUfaYeiQPgXu6br5e4ogKjuKd", 3000000) -- change
            , ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 5000000) -- more change
            ] 

    -- Import tx1 as a network transaction
    importNetTx tx1 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.fromList [(ai, 1)])))

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    offlineBalance ai   >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 1 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    -- Import tx2 as offline
    importTx tx2 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx2, TxOffline)

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    offlineBalance ai   >>= liftIO . (assertEqual "Balance is not 4000000") 4000000

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 0") 0
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 4000000") 4000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutOfflineBalance . entityVal

    -- Import tx3 as offline
    importTx tx3 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx3, TxOffline)

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    offlineBalance ai   >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 0") 0
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 4000000") 4000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 4000000") 4000000
            . keyRingAddrOutOfflineBalance . entityVal

    -- Import tx4 as a network transaction. It should override tx2 and tx3.
    importNetTx tx4 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.empty)))

    getBy (UniqueAccTx ai (txHash tx2)) >>= 
        liftIO . (assertEqual "Confidence is not dead" TxDead)
            . keyRingTxConfidence . entityVal . fromJust

    getBy (UniqueAccTx ai (txHash tx3)) >>= 
        liftIO . (assertEqual "Confidence is not dead" TxDead)
            . keyRingTxConfidence . entityVal . fromJust

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 8000000") 8000000
    offlineBalance ai   >>= liftIO . (assertEqual "Offline balance is not 8000000") 8000000

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 15000000") 15000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 3000000") 3000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 15000000") 15000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 3000000") 3000000
            . keyRingAddrInOfflineBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutOfflineBalance . entityVal


testOfflineExceptions :: Assertion
testOfflineExceptions = do
    let tx1 = fakeTx
            [ (4, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 10000000) ] 
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1BwbQ8Wp7YUfaYeiQPgXu6br5e4ogKjuKd", 4000000) -- change
            ] 
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (4, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 20000000) ] 

    assertException (WalletException "importLocalTx: can not double spend coins") $ do
        newKeyRing "test" bs1
        newAccount "test" "acc1"
        Entity ai _ <- getAccount "test" "acc1"
        setAccountGap "test" "acc1" 10
        importNetTx tx1 >>=
            liftIO . (assertEqual "Confidence is not pending")
                (Just (TxPending, M.fromList [(ai, 1)]))
        importTx tx4 ai

    assertException (WalletException "importLocalTx: can not double spend coins") $ do
        newKeyRing "test" bs1
        newAccount "test" "acc1"
        Entity ai _ <- getAccount "test" "acc1"
        setAccountGap "test" "acc1" 10
        importNetTx tx4 >>=
            liftIO . (assertEqual "Confidence is not pending")
                (Just (TxPending, M.fromList [(ai, 1)]))
        importNetTx tx1 >>=
            liftIO . (assertEqual "Confidence is not dead")
                (Just (TxDead, M.empty))
        importNetTx tx2 >>=
            liftIO . (assertEqual "Confidence is not dead")
                (Just (TxDead, M.fromList [(ai, 1)]))
        importTx tx3 ai

    assertException (WalletException "importLocalTx: The transaction already exists and is not offline") $ do
        newKeyRing "test" bs1
        newAccount "test" "acc1"
        Entity ai _ <- getAccount "test" "acc1"
        setAccountGap "test" "acc1" 10
        importNetTx tx1 >>=
            liftIO . (assertEqual "Confidence is not pending")
                (Just (TxPending, M.fromList [(ai, 1)]))
        importTx tx1 ai
 
-- This test create a multisig account with the key of testImportMultisig2
testImportMultisig :: App ()
testImportMultisig = do
    newKeyRing "test" bs1
    newAccountMultisig 
        "test" "ms1"
        [fromJust $ xPubImport "xpub69iinth3CTrfkmijzhQXi3kwhGQjba31fncrBgA9vM9T9tv69qSwp525yDVYmX2BTAdeuYSZqkcWhkrqD5Xbsz5YHJZL6CzYGL2WACorpdS"] 2 2
    newAccountMultisig 
        "test" "ms2" 
        [fromJust $ xPubImport "xpub69iinth3CTrfh5efv7baTWwk9hHi4zqcQEsNFgVwEJvdaZVEPytZzmNxjYTnF5F5x2CamLXvmD1T4RhpsuaXSFPo2MnLN5VqWqrWb82U7ED"] 2 2
    setAccountGap "test" "ms1" 10
    setAccountGap "test" "ms2" 10
    Entity _ keyRing <- getKeyRing "test"
    accE1@(Entity ai1 acc1) <- getAccount "test" "ms1"
    accE2@(Entity ai2 acc2) <- getAccount "test" "ms2"

    let fundingTx = 
            Tx 1 [ TxIn (OutPoint 1 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayScriptHash $ fromJust $ 
                    base58ToAddr "3Dgz9gqsAMPr7i9qocLMNHU8wuoKqtUNoM" 
                 ] 0

    importNetTx fundingTx >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.fromList [(ai1, 1), (ai2, 1)])))

    -- Create a transaction which has 0 signatures in ms1
    (h,c) <- createTx "test" "ms1" 0 
        [(fromJust $ base58ToAddr "3C9fz8kDwX2rV25YeWC7YcDNHtTreAV52m", 5000000)] 10000 True
    tx1 <- liftM (entityVal . fromJust) $ getBy $ UniqueAccTx ai1 h
    liftIO $ assertEqual "Confidence is not offline" TxOffline c
    liftIO $ assertEqual "Confidence is not offline" TxOffline $ keyRingTxConfidence tx1
    liftM (map (keyRingCoinHash . entityVal)) (spendableCoins ai1 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [])
    liftM (map keyRingTxHash . fst) (txPage "test" "ms1" $ PageRequest 1 10 False) 
        >>= liftIO . (assertEqual "Wrong txhash in tx list" [txHash fundingTx, h])
    accountBalance ai1 0 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai1 1 >>= liftIO . (assertEqual "Balance is not 0") 0
    offlineBalance ai1   >>= liftIO . (assertEqual "Balance is not 9990000") 9990000

    -- Import the empty transaction in ms2
    (h2,c2) <- importTx (keyRingTxTx tx1) ai2
    -- This second import should be idempotent
    importTx (keyRingTxTx tx1) ai2
    tx2 <- liftM (entityVal . fromJust) $ getBy $ UniqueAccTx ai2 h2
    liftIO $ assertEqual "Txid do not match" h h2
    liftIO $ assertEqual "Confidence is not offline" TxOffline c2
    liftIO $ assertEqual "Confidence is not offline" TxOffline $ keyRingTxConfidence tx2
    liftM (map (keyRingCoinHash . entityVal)) (spendableCoins ai2 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [])
    liftM (map keyRingTxHash . fst) (txPage "test" "ms2" $ PageRequest 1 10 False) 
        >>= liftIO . (assertEqual "Wrong txhash in tx list" [txHash fundingTx, h2])
    accountBalance ai2 0 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai2 1 >>= liftIO . (assertEqual "Balance is not 0") 0
    offlineBalance ai2   >>= liftIO . (assertEqual "Balance is not 9990000") 9990000

    -- Sign the transaction in ms2
    (h3,c3) <- signKeyRingTx h2 keyRing accE2
    tx3 <- liftM (entityVal . fromJust) $ getBy $ UniqueAccTx ai2 h3
    liftIO $ assertEqual "Confidence is not pending" TxPending c3
    liftIO $ assertEqual "Confidence is not pending" TxPending $ keyRingTxConfidence tx3
    liftM (map (keyRingCoinHash . entityVal)) (spendableCoins ai2 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [h3, h3])
    liftM (map keyRingTxHash . fst) (txPage "test" "ms2" $ PageRequest 1 10 False) 
        >>= liftIO . (assertEqual "Wrong txhash in tx list" [txHash fundingTx, h3])
    accountBalance ai2 0 >>= liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance ai2 1 >>= liftIO . (assertEqual "Balance is not 0") 0
    offlineBalance ai2   >>= liftIO . (assertEqual "Balance is not 9990000") 9990000

    tx4 <- liftM (entityVal . fromJust) $ getBy $ UniqueAccTx ai1 h3
    liftIO $ assertEqual "Confidence is not pending" TxPending $ keyRingTxConfidence tx4
    liftM (map (keyRingCoinHash . entityVal)) (spendableCoins ai1 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [h3, h3])
    liftM (map keyRingTxHash . fst) (txPage "test" "ms1" $ PageRequest 1 10 False) 
        >>= liftIO . (assertEqual "Wrong txhash in tx list" [txHash fundingTx, h3])
    accountBalance ai1 0 >>= liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance ai1 1 >>= liftIO . (assertEqual "Balance is not 0") 0
    offlineBalance ai1   >>= liftIO . (assertEqual "Balance is not 9990000") 9990000

    -- Importing the transaction should have no effect as it was globally
    -- imported already in the previous step.
    (h5,c5) <- importTx (keyRingTxTx tx3) ai1
    tx5 <- liftM (entityVal . fromJust) $ getBy $ UniqueAccTx ai1 h5
    liftIO $ assertEqual "Confidence is not pending" TxPending c5
    liftIO $ assertEqual "Confidence is not pending" TxPending $ keyRingTxConfidence tx5
    liftM (map (keyRingCoinHash . entityVal)) (spendableCoins ai1 0)
        >>= liftIO . (assertEqual "Wrong txhash in coins" [h5, h5])
    liftM (map keyRingTxHash . fst) (txPage "test" "ms1" $ PageRequest 1 10 False) 
        >>= liftIO . (assertEqual "Wrong txhash in tx list" [txHash fundingTx, h5])
    accountBalance ai1 0 >>= liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance ai1 1 >>= liftIO . (assertEqual "Balance is not 0") 0
    offlineBalance ai1   >>= liftIO . (assertEqual "Balance is not 9990000") 9990000

testKillTx :: App ()
testKillTx = do
    newKeyRing "test" bs1
    newAccount "test" "acc1"
    Entity ai _ <- getAccount "test" "acc1"
    setAccountGap "test" "acc1" 10
    let tx1 = fakeTx
            [ (4, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 10000000) ] 
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1BwbQ8Wp7YUfaYeiQPgXu6br5e4ogKjuKd", 4000000) -- change
            ] 
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external

    importNetTx tx1 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.fromList [(ai, 1)])))
    importNetTx tx2 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.fromList [(ai, 1)])))
    importNetTx tx3 >>=
        liftIO . (assertEqual "Confidence is not pending" 
            (Just (TxPending, M.empty)))

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 0") 0

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 4000000") 4000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 4000000") 4000000
            . keyRingAddrOutBalance . entityVal

    killTx $ txHash tx2

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 0") 0
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    -- Killing a transaction should be idempotent
    killTx $ txHash tx2

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 0") 0
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    -- Killing a transaction should be idempotent
    killAccTx ai $ txHash tx2
    killAccTx ai $ txHash tx3
    killTx $ txHash tx3

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 10000000") 10000000

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 0") 0
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    reviveTx tx2

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 4000000") 4000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

    -- Reviving a transaction should be idempotent
    reviveTx tx2

    accountBalance ai 0 >>= liftIO . (assertEqual "Balance is not 4000000") 4000000

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 10000000") 10000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressExternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 10000000") 10000000
            . keyRingAddrOutBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 inbalance is not 4000000") 4000000
            . keyRingAddrInBalance . entityVal

    getAddress "test" "acc1" 0 AddressInternal >>=
        liftIO . (assertEqual "Address 0 outbalance is not 0") 0
            . keyRingAddrOutBalance . entityVal

