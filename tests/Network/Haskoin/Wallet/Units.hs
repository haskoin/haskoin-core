module Network.Haskoin.Wallet.Units (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (liftM, guard)
import Control.Monad.Trans (liftIO)
import Control.Exception (Exception, handleJust)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (NoLoggingT)

import Data.Word (Word32, Word64)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (fromList, empty)
import qualified Data.ByteString as BS
    ( ByteString
    , empty
    , pack
    )

import Database.Persist (Entity(..), entityVal, getBy)
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
        [ testCase "Calling _ <- newKeyRing with an empty seed should fail" $
            assertException
                (WalletException "The seed is empty")
                (newKeyRing "main" BS.empty)

        , testCase "Creating two KeyRings with the same name should fail" $
            assertException
                (WalletException "KeyRing main already exists") $ do
                    _ <- newKeyRing "main" $ BS.pack [0]
                    newKeyRing "main" $ BS.pack [1]
        ]
    , testGroup "Account tests"
        [ testCase "Creating two accounts with the same name should fail" $
            assertException (WalletException "Account acc already exists") $ do
                _ <- newKeyRing "main" $ BS.pack [1]
                _ <- newAccount "main" "acc" (AccountRegular False) []
                newAccount "main" "acc" (AccountRegular False) []

        , testCase "Invalid multisig parameters (0 of 1)" $
            assertException (WalletException "Invalid account type") $ do
                _ <- newKeyRing "main" $ BS.pack [0]
                newAccount "main" "ms" (AccountMultisig False 0 1) []

        , testCase "Invalid multisig parameters (2 of 1)" $
            assertException (WalletException "Invalid account type") $ do
                _ <- newKeyRing "main" $ BS.pack [0]
                newAccount "main" "ms" (AccountMultisig False 2 1) []

        , testCase "Invalid multisig parameters (15 of 16)" $
            assertException (WalletException "Invalid account type") $ do
                _ <- newKeyRing "main" $ BS.pack [0]
                newAccount "main" "ms" (AccountMultisig False 15 16) []

        , testCase "To many multisig keys (2 keys for 1 of 2)" $
            assertException
                (WalletException "Invalid account keys") $ do
                    _ <- newKeyRing "main" $ BS.pack [0]
                    newAccount "main" "ms" (AccountMultisig False 1 2)
                        [ deriveXPubKey $ makeXPrvKey (BS.pack [1])
                        , deriveXPubKey $ makeXPrvKey (BS.pack [2])
                        ]

        , testCase "Calling addAccountKeys with an empty key list should fail" $
            assertException
                (WalletException "Invalid account keys") $ do
                    _ <- newKeyRing "main" $ BS.pack [0]
                    _ <- newAccount "main" "default" (AccountRegular True) []
                    (_, accE) <- getAccount "main" "default"
                    addAccountKeys accE []

        , testCase "Calling addAccountKeys on a non-multisig account should fail" $
            assertException
                (WalletException "The account is already complete") $ do
                    _ <- newKeyRing "main" $ BS.pack [0]
                    _ <- newAccount "main" "default" (AccountRegular False) []
                    (_, accE) <- getAccount "main" "default"
                    addAccountKeys accE [ deriveXPubKey $ makeXPrvKey (BS.pack [1]) ]

        , testCase "Adding keys to a complete multisig account should fail" $
            assertException
                (WalletException "The account is already complete") $ do
                    _ <- newKeyRing "main" $ BS.pack [0]
                    _ <- newAccount "main" "ms" (AccountMultisig False 2 3)
                        [ deriveXPubKey $ makeXPrvKey (BS.pack [1])
                        , deriveXPubKey $ makeXPrvKey (BS.pack [2])
                        ]
                    (_, accE) <- getAccount "main" "ms"
                    addAccountKeys accE [deriveXPubKey $ makeXPrvKey (BS.pack [3])]

        , testCase "Getting a non-existing account should fail" $
            assertException
                (WalletException "Account default does not exist") $ do
                    _ <- newKeyRing "main" $ BS.pack [0]
                    getAccount "main" "default"

        ]
    , testGroup "Address tests"
        [ testCase "Displaying page 0 should fail" $
            assertException
                (WalletException "Invalid page request (Page: 0, Page size: 1)" ) $ do
                    _ <- newKeyRing "main" $ BS.pack [0]
                    addressPage "main" "default" AddressExternal $
                        PageRequest 0 1 False

        , testCase "Displaying 0 results per page should fail" $
            assertException
                (WalletException "Invalid page request (Page: 1, Page size: 0)" ) $ do
                    _ <- newKeyRing "main" $ BS.pack [0]
                    addressPage "main" "default" AddressExternal $
                        PageRequest 1 0 False

        , testCase "Displaying a page number that is too high should fail" $
            assertException
                (WalletException "Invalid page number 5") $ do
                    _ <- newKeyRing "main" $ BS.pack [0]
                    _ <- newAccount "main" "default" (AccountRegular False) []
                    addressPage "main" "default" AddressExternal $
                        PageRequest 5 3 False

        , testCase "Decreasing the address gap should fail" $
            assertException (WalletException "The gap of an account can only be increased") $ do
                _ <- newKeyRing "main" $ BS.pack [0]
                _ <- newAccount "main" "default" (AccountRegular False) []
                (_, acc1E) <- getAccount "main" "default"
                setAccountGap acc1E 15
                (_, acc2E) <- getAccount "main" "default"
                setAccountGap acc2E 14

        , testCase "Setting a label on a hidden address key should fail" $
            assertException (WalletException "Invalid address index 10") $ do
                _ <- newKeyRing "main" $ BS.pack [0]
                _ <- newAccount "main" "default" (AccountRegular False) []
                setAddrLabel "main" "default" 10 AddressExternal "Gym membership"

        , testCase "Setting a label on an invalid address key should fail" $
            assertException (WalletException "Invalid address index 20") $ do
                _ <- newKeyRing "main" $ BS.pack [0]
                _ <- newAccount "main" "default" (AccountRegular False) []
                setAddrLabel "main" "default" 20 AddressExternal "Gym membership"

        , testCase "Requesting an address prvkey on a read-only account should fail" $
            assertException
                (WalletException "Invalid address") $ do
                    _ <- newKeyRing "main" $ BS.pack [0]
                    _ <- newAccount "main" "default" (AccountRegular True)
                        [deriveXPubKey $ makeXPrvKey $ BS.pack [1]]
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
    _ <- newKeyRing "test" bs1
    _ <- newAccount "test" "acc1" (AccountRegular False) []

    unusedAddresses "test" "acc1" AddressExternal
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
            ] . map (addrToBase58 . keyRingAddrAddress . lst3)

    unusedAddresses "test" "acc1" AddressInternal
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
            ] . map (addrToBase58 . keyRingAddrAddress . lst3)

testBalances :: App ()
testBalances = do
    _ <- newKeyRing "test" bs1
    _ <- newAccount "test" "acc1" (AccountRegular False) []
    (_, Entity ai _) <- getAccount "test" "acc1"
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
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 5000000) -- external
            , ("1BwbQ8Wp7YUfaYeiQPgXu6br5e4ogKjuKd", 5000000) -- change
            ]

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    -- Import funding transaction twice. This operation should be idempotent
    importNetTx fundingTx >>=
        liftIO . (assertEqual "Confidence is not pending"
            (Just (TxPending, M.fromList [(ai, 2)])))
    importNetTx fundingTx >>=
        liftIO . (assertEqual "Confidence is not pending"
            (Just (TxPending, M.empty)))

    spendableCoins ai 0 (const . const []) >>=
        liftIO . (assertEqual "0-conf spendable coins is not 2" 2) . length
    spendableCoins ai 1 (const . const []) >>=
        liftIO . (assertEqual "1-conf spendable coins is not 0" 0) . length

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 30000000") 30000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, AddressBalance 10000000 0 1 0)]

    addressBalances "test" "acc1" 1 1 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 1 balance is not (20000000, 0, 1, 0)")
            [(1, AddressBalance 20000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 1 1 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 1 1-conf balance is not (0, 0, 0, 0)")
            [(1, AddressBalance 0 0 0 0)]

    importNetTx tx1 >>=
        liftIO . (assertEqual "Confidence is not pending"
            (Just (TxPending, M.empty)))

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 1 balance is not (20000000, 20000000, 1, 1)")
            [(1, AddressBalance 20000000 20000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 1 1 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 1 1-conf balance is not (0, 0, 0, 0)")
            [(1, AddressBalance 0 0 0 0)]

    -- We re-import tx1. This operation has to be idempotent with respect to
    -- balances.
    importNetTx tx1 >>=
        liftIO . (assertEqual "Confidence is not pending"
            (Just (TxPending, M.empty)))

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 1 balance is not (20000000, 20000000, 1, 1)")
            [(1, AddressBalance 20000000 20000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 1 1 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 1 1-conf balance is not (0, 0, 0, 0)")
            [(1, AddressBalance 0 0 0 0)]

    -- Importing tx2 twice. This operation has to be idempotent.
    importNetTx tx2 >>=
        liftIO . (assertEqual "Confidence is not dead"
            (Just (TxDead, M.fromList [(ai, 1)])))
    importNetTx tx2 >>=
        liftIO . (assertEqual "Confidence is not dead"
            (Just (TxDead, M.empty)))

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 1 balance is not (20000000, 20000000, 1, 1)")
            [(1, AddressBalance 20000000 20000000 1 1)]

    -- Confirm the funding transaction at height 1
    importMerkles ((BestChain [fakeNode 1 0x01])) [[txHash fundingTx]]

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 1 balance is not (20000000, 20000000, 1, 1)")
            [(1, AddressBalance 20000000 20000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 1 1-conf balance is not (20000000, 20000000, 1, 1)")
            [(1, AddressBalance 20000000 20000000 1 1)]

    -- Confirm tx1 at height 2
    importMerkles ((BestChain [fakeNode 2 0x02])) [[txHash tx1]]

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 2 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 2 False >>=
        liftIO . (assertEqual "Address 1 2-conf balance is not (20000000, 20000000, 1, 1)")
            [(1, AddressBalance 20000000 20000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressExternal 3 False >>=
        liftIO . (assertEqual "Address 0 3-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 1 1 AddressExternal 3 False >>=
        liftIO . (assertEqual "Address 1 3-conf balance is not (0, 0, 0, 0)")
            [(1, AddressBalance 0 0 0 0)]

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

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance "test" "acc1" 2 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance "test" "acc1" 3 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance "test" "acc1" 4 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 1 balance is not (20000000, 0, 1, 0)")
            [(1, AddressBalance 20000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 3 False >>=
        liftIO . (assertEqual "Address 0 3-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 3 False >>=
        liftIO . (assertEqual "Address 1 3-conf balance is not (20000000, 0, 1, 0)")
            [(1, AddressBalance 20000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (5000000, 0, 1, 0)")
            [(0, AddressBalance 5000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (5000000, 0, 1, 0)")
            [(0, AddressBalance 5000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 2 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    -- Reimporting tx2 should be idempotent and return TxBuilding
    importNetTx tx2 >>=
        liftIO . (assertEqual "Confidence is not building"
            (Just (TxBuilding, M.empty)))

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance "test" "acc1" 2 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance "test" "acc1" 3 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance "test" "acc1" 4 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 1 balance is not (20000000, 0, 1, 0)")
            [(1, AddressBalance 20000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 3 False >>=
        liftIO . (assertEqual "Address 0 3-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 3 False >>=
        liftIO . (assertEqual "Address 1 3-conf balance is not (20000000, 0, 1, 0)")
            [(1, AddressBalance 20000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (5000000, 0, 1, 0)")
            [(0, AddressBalance 5000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (5000000, 0, 1, 0)")
            [(0, AddressBalance 5000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 2 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    -- Reorg back onto tx1
    let s2 = fakeNode 1 0x01
        o2 = [fakeNode 2 0x03, fakeNode 3 0x04]
        n2 = [fakeNode 2 0x02, fakeNode 3 0x05, fakeNode 4 0x06]
    importMerkles (ChainReorg s2 o2 n2) [[txHash tx1], [], []]

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 4 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 5 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 1 balance is not (20000000, 20000000, 1, 1)")
            [(1, AddressBalance 20000000 20000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressExternal 4 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 1 1 AddressExternal 4 False >>=
        liftIO . (assertEqual "Address 1 2-conf balance is not (20000000, 20000000, 1, 1)")
            [(1, AddressBalance 20000000 20000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressExternal 5 False >>=
        liftIO . (assertEqual "Address 0 4-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 1 1 AddressExternal 5 False >>=
        liftIO . (assertEqual "Address 1 4-conf balance is not (0, 0, 0, 0)")
            [(1, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

-- tx1, tx2 and tx3 form a chain, and tx4 is in conflict with tx1
testConflictBalances :: App ()
testConflictBalances = do
    _ <- newKeyRing "test" bs1
    _ <- newAccount "test" "acc1" (AccountRegular False) []
    (_, Entity ai _) <- getAccount "test" "acc1"
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

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance "test" "acc1" 0 True  >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, AddressBalance 10000000 0 1 0)]

    -- Import second transaction
    importNetTx tx2 >>=
        liftIO . (assertEqual "Confidence is not pending"
            (Just (TxPending, M.fromList [(ai, 1)])))

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, AddressBalance 4000000 0 1 0)]

    -- Let's confirm these two transactions
    importMerkles
        (BestChain [fakeNode 1 0x01, fakeNode 2 0x02 ])
        [[txHash tx1], [txHash tx2]]

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance "test" "acc1" 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (4000000, 0, 1, 0)")
            [(0, AddressBalance 4000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 2 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 2 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    -- Import third transaction
    importNetTx tx3 >>=
        liftIO . (assertEqual "Confidence is not pending"
            (Just (TxPending, M.empty)))

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 4000000, 1, 1)")
            [(0, AddressBalance 4000000 4000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (4000000, 4000000, 1, 1)")
            [(0, AddressBalance 4000000 4000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressExternal 2 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 2 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    -- Now let's add tx4 which is in conflict with tx1
    importNetTx tx4 >>=
        liftIO . (assertEqual "Confidence is not pending"
            (Just (TxDead, M.empty)))

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 4000000, 1, 1)")
            [(0, AddressBalance 4000000 4000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressExternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 1 False >>=
        liftIO . (assertEqual "Address 0 1-conf balance is not (4000000, 4000000, 1, 1)")
            [(0, AddressBalance 4000000 4000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressExternal 2 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 2 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

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

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance "test" "acc1" 2 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance "test" "acc1" 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (20000000, 0, 1, 0)")
            [(0, AddressBalance 20000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 2 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (20000000, 0, 1, 0)")
            [(0, AddressBalance 20000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 3 False >>=
        liftIO . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    -- Reorg back to tx1, tx2 and tx3
    let s2 = fakeNode 0 0x00
        o2 = [fakeNode 1 0x03, fakeNode 2 0x04, fakeNode 3 0x05]
        n2 = [fakeNode 1 0x01, fakeNode 2 0x02, fakeNode 3 0x06, fakeNode 4 0x07]
    importMerkles (ChainReorg s2 o2 n2) [[txHash tx1], [txHash tx2], [], []]

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

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance "test" "acc1" 2 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance "test" "acc1" 3 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance "test" "acc1" 4 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 5 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, AddressBalance 4000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 3 False >>=
        liftIO . (assertEqual "Address 0 3-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 3 False >>=
        liftIO . (assertEqual "Address 0 3-conf balance is not (4000000, 0, 1, 0)")
            [(0, AddressBalance 4000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 4 False >>=
        liftIO . (assertEqual "Address 0 4-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 4 False >>=
        liftIO . (assertEqual "Address 0 4-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 5 False >>=
        liftIO . (assertEqual "Address 0 5-conf balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

testOffline :: App ()
testOffline = do
    _ <- newKeyRing "test" bs1
    _ <- newAccount "test" "acc1" (AccountRegular False) []
    (_, Entity ai _) <- getAccount "test" "acc1"
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

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 0 True  >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, AddressBalance 10000000 0 1 0)]

    -- Reimporting a transaction should me idempotent
    importTx tx1 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx1, TxOffline)

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "acc1" 0 True  >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, AddressBalance 10000000 0 1 0)]

    -- Import tx2
    importTx tx2 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx2, TxOffline)

    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, AddressBalance 4000000 0 1 0)]

    -- Import tx3
    importTx tx3 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx3, TxOffline)

    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 4000000, 1, 1)")
            [(0, AddressBalance 4000000 4000000 1 1)]

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

    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (20000000, 0, 1, 0)")
            [(0, AddressBalance 20000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    -- importTx should be idempotent
    importTx tx4 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx4, TxOffline)

    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (20000000, 0, 1, 0)")
            [(0, AddressBalance 20000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

testKillOffline :: App ()
testKillOffline = do
    _ <- newKeyRing "test" bs1
    _ <- newAccount "test" "acc1" (AccountRegular False) []
    (_, Entity ai _) <- getAccount "test" "acc1"
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

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
        [(0, AddressBalance 10000000 0 1 0)]

    -- Import tx2 as offline
    importTx tx2 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx2, TxOffline)

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Offline balance is not 4000000") 4000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "1-conf Balance is not 0") 0
    accountBalance "test" "acc1" 1 True >>=
        liftIO . (assertEqual "1-conf Offline balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, AddressBalance 10000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, AddressBalance 4000000 0 1 0)]

    -- Import tx3 as offline
    importTx tx3 ai >>=
        liftIO . (assertEqual "Confidence is not offline")
            (txHash tx3, TxOffline)

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Offline balance is not 0") 0
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "1-conf Balance is not 0") 0
    accountBalance "test" "acc1" 1 True >>=
        liftIO . (assertEqual "1-conf Offline balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, AddressBalance 10000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 4000000, 1, 1)")
            [(0, AddressBalance 4000000 4000000 1 1)]

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

    accountBalance "test" "acc1" 0 False >>=
        liftIO . (assertEqual "Balance is not 8000000") 8000000
    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Offline balance is not 8000000") 8000000
    accountBalance "test" "acc1" 1 False >>=
        liftIO . (assertEqual "1-conf Balance is not 0") 0
    accountBalance "test" "acc1" 1 True >>=
        liftIO . (assertEqual "1-conf Offline balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (15000000, 10000000, 2, 1)")
            [(0, AddressBalance 15000000 10000000 2 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (3000000, 0, 1, 0)")
            [(0, AddressBalance 3000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (15000000, 10000000, 2 1)")
            [(0, AddressBalance 15000000 10000000 2 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (3000000, 0, 1, 0)")
            [(0, AddressBalance 3000000 0 1 0)]

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

    assertException (WalletException "Could not import offline transaction") $ do
        _ <- newKeyRing "test" bs1
        _ <- newAccount "test" "acc1" (AccountRegular False) []
        (_, Entity ai _) <- getAccount "test" "acc1"
        importNetTx tx1 >>=
            liftIO . (assertEqual "Confidence is not pending")
                (Just (TxPending, M.fromList [(ai, 1)]))
        importTx tx4 ai

    assertException (WalletException "Could not import offline transaction") $ do
        _ <- newKeyRing "test" bs1
        _ <- newAccount "test" "acc1" (AccountRegular False) []
        (_, Entity ai _) <- getAccount "test" "acc1"
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

    assertException (WalletException "Could not import offline transaction") $ do
        _ <- newKeyRing "test" bs1
        _ <- newAccount "test" "acc1" (AccountRegular False) []
        (_, Entity ai _) <- getAccount "test" "acc1"
        importNetTx tx1 >>=
            liftIO . (assertEqual "Confidence is not pending")
                (Just (TxPending, M.fromList [(ai, 1)]))
        importTx tx1 ai

-- This test create a multisig account with the key of testImportMultisig2
testImportMultisig :: App ()
testImportMultisig = do
    _ <- newKeyRing "test" bs1
    _ <- newAccount "test" "ms1" (AccountMultisig False 2 2)
        [fromJust $ xPubImport "xpub69iinth3CTrfkmijzhQXi3kwhGQjba31fncrBgA9vM9T9tv69qSwp525yDVYmX2BTAdeuYSZqkcWhkrqD5Xbsz5YHJZL6CzYGL2WACorpdS"]
    _ <- newAccount "test" "ms2" (AccountMultisig False 2 2)
        [fromJust $ xPubImport "xpub69iinth3CTrfh5efv7baTWwk9hHi4zqcQEsNFgVwEJvdaZVEPytZzmNxjYTnF5F5x2CamLXvmD1T4RhpsuaXSFPo2MnLN5VqWqrWb82U7ED"]
    Entity _ keyRing <- getKeyRing "test"
    (_, Entity ai1 _) <- getAccount "test" "ms1"
    (_, accE2@(Entity ai2 _)) <- getAccount "test" "ms2"

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
        [(fromJust $ base58ToAddr "3C9fz8kDwX2rV25YeWC7YcDNHtTreAV52m", 5000000)] 10000 False True
    tx1 <- liftM (entityVal . fromJust) $ getBy $ UniqueAccTx ai1 h
    liftIO $ assertEqual "Confidence is not offline" TxOffline c
    liftIO $ assertEqual "Confidence is not offline" TxOffline $ keyRingTxConfidence tx1
    spendableCoins ai1 0 (const . const [])
        >>= liftIO . (assertEqual "Wrong txhash in coins" [])
            . map (keyRingCoinHash . entityVal . fst3)
    txPage "test" "ms1" (PageRequest 1 10 False)
        >>= liftIO . (assertEqual "Wrong txhash in tx list" [txHash fundingTx, h])
            . (map (keyRingTxHash . lst3)) . fst
    accountBalance "test" "ms1" 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance "test" "ms1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "ms1" 0 True >>=
        liftIO . (assertEqual "Offline balance is not 9990000") 9990000

    -- Import the empty transaction in ms2
    (h2,c2) <- importTx (keyRingTxTx tx1) ai2
    -- This second import should be idempotent
    _ <- importTx (keyRingTxTx tx1) ai2
    tx2 <- liftM (entityVal . fromJust) $ getBy $ UniqueAccTx ai2 h2
    liftIO $ assertEqual "Txid do not match" h h2
    liftIO $ assertEqual "Confidence is not offline" TxOffline c2
    liftIO $ assertEqual "Confidence is not offline" TxOffline $ keyRingTxConfidence tx2
    spendableCoins ai2 0 (const . const [])
        >>= liftIO . (assertEqual "Wrong txhash in coins" [])
            . map (keyRingCoinHash . entityVal . fst3)
    txPage "test" "ms2" (PageRequest 1 10 False)
        >>= liftIO . (assertEqual "Wrong txhash in tx list" [txHash fundingTx, h2])
            . (map (keyRingTxHash . lst3)) . fst
    accountBalance "test" "ms2" 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance "test" "ms2" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "ms2" 0 True >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

    -- Sign the transaction in ms2
    (h3,c3) <- signKeyRingTx keyRing accE2 h2
    tx3 <- liftM (entityVal . fromJust) $ getBy $ UniqueAccTx ai2 h3
    liftIO $ assertEqual "Confidence is not pending" TxPending c3
    liftIO $ assertEqual "Confidence is not pending" TxPending $ keyRingTxConfidence tx3
    spendableCoins ai2 0 (const . const [])
        >>= liftIO . (assertEqual "Wrong txhash in coins" [h3, h3])
            . map (keyRingCoinHash . entityVal . fst3)
    txPage "test" "ms2" (PageRequest 1 10 False)
        >>= liftIO . (assertEqual "Wrong txhash in tx list" [txHash fundingTx, h3])
            . (map (keyRingTxHash . lst3)) . fst
    accountBalance "test" "ms2" 0 False >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance "test" "ms2" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "ms2" 0 True  >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

    tx4 <- liftM (entityVal . fromJust) $ getBy $ UniqueAccTx ai1 h3
    liftIO $ assertEqual "Confidence is not pending" TxPending $ keyRingTxConfidence tx4
    spendableCoins ai1 0 (const . const [])
        >>= liftIO . (assertEqual "Wrong txhash in coins" [h3, h3])
            . map (keyRingCoinHash . entityVal . fst3)
    txPage "test" "ms1" (PageRequest 1 10 False)
        >>= liftIO . (assertEqual "Wrong txhash in tx list" [txHash fundingTx, h3])
            . (map (keyRingTxHash . lst3)) . fst
    accountBalance "test" "ms1" 0 False >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance "test" "ms1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "ms1" 0 True  >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

    -- Importing the transaction should have no effect as it was globally
    -- imported already in the previous step.
    (h5,c5) <- importTx (keyRingTxTx tx3) ai1
    tx5 <- liftM (entityVal . fromJust) $ getBy $ UniqueAccTx ai1 h5
    liftIO $ assertEqual "Confidence is not pending" TxPending c5
    liftIO $ assertEqual "Confidence is not pending" TxPending $ keyRingTxConfidence tx5
    spendableCoins ai1 0 (const . const [])
        >>= liftIO . (assertEqual "Wrong txhash in coins" [h5, h5])
            . map (keyRingCoinHash . entityVal . fst3)
    txPage "test" "ms1" (PageRequest 1 10 False)
        >>= liftIO . (assertEqual "Wrong txhash in tx list" [txHash fundingTx, h5])
            . (map (keyRingTxHash . lst3)) . fst
    accountBalance "test" "ms1" 0 False >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance "test" "ms1" 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance "test" "ms1" 0 True  >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

testKillTx :: App ()
testKillTx = do
    _ <- newKeyRing "test" bs1
    _ <- newAccount "test" "acc1" (AccountRegular False) []
    (_, Entity ai _) <- getAccount "test" "acc1"
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

    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 4000000, 1, 1)")
            [(0, AddressBalance 4000000 4000000 1 1)]

    killTxs [txHash tx2]

    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, AddressBalance 10000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    -- Killing a transaction should be idempotent
    killTxs [txHash tx2]

    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, AddressBalance 10000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    killTxs [txHash tx3]

    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, AddressBalance 10000000 0 1 0)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, AddressBalance 0 0 0 0)]

    reviveTx tx2

    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, AddressBalance 4000000 0 1 0)]

    -- Reviving a transaction should be idempotent
    reviveTx tx2

    accountBalance "test" "acc1" 0 True >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000

    addressBalances "test" "acc1" 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, AddressBalance 10000000 10000000 1 1)]

    addressBalances "test" "acc1" 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, AddressBalance 4000000 0 1 0)]

