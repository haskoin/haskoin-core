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
import Data.List (sort)
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
import Network.Haskoin.Node.HeaderTree

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
                    _ <- newKeyRing "main" $ BS.pack [0]
                    newKeyRing "main" $ BS.pack [1]
        ]
    , testGroup "Account tests"
        [ testCase "Creating two accounts with the same name should fail" $
            assertException (WalletException "Account acc already exists") $ do
                keyE <- newKeyRing "main" $ BS.pack [1]
                _ <- newAccount keyE "acc" (AccountRegular False) []
                newAccount keyE "acc" (AccountRegular False) []

        , testCase "Invalid multisig parameters (0 of 1)" $
            assertException (WalletException "Invalid account type") $ do
                keyE <- newKeyRing "main" $ BS.pack [0]
                newAccount keyE "ms" (AccountMultisig False 0 1) []

        , testCase "Invalid multisig parameters (2 of 1)" $
            assertException (WalletException "Invalid account type") $ do
                keyE <- newKeyRing "main" $ BS.pack [0]
                newAccount keyE "ms" (AccountMultisig False 2 1) []

        , testCase "Invalid multisig parameters (15 of 16)" $
            assertException (WalletException "Invalid account type") $ do
                keyE <- newKeyRing "main" $ BS.pack [0]
                newAccount keyE "ms" (AccountMultisig False 15 16) []

        , testCase "To many multisig keys (2 keys for 1 of 2)" $
            assertException
                (WalletException "Invalid account keys") $ do
                    keyE <- newKeyRing "main" $ BS.pack [0]
                    newAccount keyE "ms" (AccountMultisig False 1 2)
                        [ deriveXPubKey $ makeXPrvKey (BS.pack [1])
                        , deriveXPubKey $ makeXPrvKey (BS.pack [2])
                        ]

        , testCase "Calling addAccountKeys with an empty key list should fail" $
            assertException
                (WalletException "Invalid account keys") $ do
                    keyE <- newKeyRing "main" $ BS.pack [0]
                    accE <- newAccount keyE "default" (AccountRegular True) []
                    addAccountKeys accE []

        , testCase "Calling addAccountKeys on a non-multisig account should fail" $
            assertException
                (WalletException "The account is already complete") $ do
                    keyE <- newKeyRing "main" $ BS.pack [0]
                    _ <- newAccount keyE "default" (AccountRegular False) []
                    (_, accE) <- getAccount "main" "default"
                    addAccountKeys accE [ deriveXPubKey $ makeXPrvKey (BS.pack [1]) ]

        , testCase "Adding keys to a complete multisig account should fail" $
            assertException
                (WalletException "The account is already complete") $ do
                    keyE <- newKeyRing "main" $ BS.pack [0]
                    _ <- newAccount keyE "ms" (AccountMultisig False 2 3)
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
        [ testCase "Decreasing the address gap should fail" $
            assertException (WalletException "The gap of an account can only be increased") $ do
                keyE <- newKeyRing "main" $ BS.pack [0]
                _ <- newAccount keyE "default" (AccountRegular False) []
                (_, acc1E) <- getAccount "main" "default"
                _ <- setAccountGap acc1E 15
                (_, acc2E) <- getAccount "main" "default"
                setAccountGap acc2E 14

        , testCase "Setting a label on a hidden address key should fail" $
            assertException (WalletException "Invalid address index 10") $ do
                keyE <- newKeyRing "main" $ BS.pack [0]
                accE <- newAccount keyE "default" (AccountRegular False) []
                setAddrLabel accE 10 AddressExternal "Gym membership"

        , testCase "Setting a label on an invalid address key should fail" $
            assertException (WalletException "Invalid address index 20") $ do
                keyE <- newKeyRing "main" $ BS.pack [0]
                accE <- newAccount keyE "default" (AccountRegular False) []
                setAddrLabel accE 20 AddressExternal "Gym membership"

        , testCase "Requesting an address prvkey on a read-only account should fail" $
            assertException
                (WalletException "Invalid address") $ do
                    keyE@(Entity _ kr) <- newKeyRing "main" $ BS.pack [0]
                    accE <- newAccount keyE "default" (AccountRegular True)
                        [deriveXPubKey $ makeXPrvKey $ BS.pack [1]]
                    addressPrvKey kr accE 2 AddressExternal
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
bs1 = fromRight $ mnemonicToSeed pass
    "mass coast dance birth online various renew alert crunch middle absurd health"

pass :: BS.ByteString
pass = "passw0rd"

tid1 :: TxHash
tid1 = "0000000000000000000000000000000000000000000000000000000000000001"

bid0 :: BlockHash
bid0 = "0000000000000000000000000000000000000000000000000000000000000000"

bid1 :: BlockHash
bid1 = "0000000000000000000000000000000000000000000000000000000000000001"

bid2 :: BlockHash
bid2 = "0000000000000000000000000000000000000000000000000000000000000002"

bid3 :: BlockHash
bid3 = "0000000000000000000000000000000000000000000000000000000000000003"

bid4 :: BlockHash
bid4 = "0000000000000000000000000000000000000000000000000000000000000004"

bid5 :: BlockHash
bid5 = "0000000000000000000000000000000000000000000000000000000000000005"

bid6 :: BlockHash
bid6 = "0000000000000000000000000000000000000000000000000000000000000006"

bid7 :: BlockHash
bid7 = "0000000000000000000000000000000000000000000000000000000000000007"

-- Creates fake testing blocks
fakeNode :: Word32 -> BlockHash -> BlockHeaderNode
fakeNode i h = BlockHeaderNode
    { nodeBlockHash = h
    , nodeHeader = BlockHeader 1 z1 z2 0 0 0
    , nodeHeaderHeight = i
    , nodeChainWork = 0
    , nodeChild = Nothing
    , nodeMedianTimes = []
    , nodeMinWork = 0
    }
  where
    z1 = "0000000000000000000000000000000000000000000000000000000000000000"
    z2 = "0000000000000000000000000000000000000000000000000000000000000000"

fakeTx :: [(TxHash, Word32)] -> [(BS.ByteString, Word64)] -> Tx
fakeTx xs ys =
    Tx 1 txi txo 0
  where
    txi = map (\(h,p) -> TxIn (OutPoint h p) (BS.pack [1]) maxBound) xs
    f = encodeOutputBS . PayPKHash . fromJust . base58ToAddr
    txo = map (\(a,v) -> TxOut v $ f a ) ys

testDerivations :: App ()
testDerivations = do
    keyE <- newKeyRing "test" bs1
    accE <- newAccount keyE "acc1" (AccountRegular False) []

    unusedAddresses accE AddressExternal
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

    unusedAddresses accE AddressInternal
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

testBalances :: App ()
testBalances = do
    keyE <- newKeyRing "test" bs1
    accE@(Entity ai _) <- newAccount keyE "acc1" (AccountRegular False) []
    let fundingTx = fakeTx
            [ (tid1, 0) ]
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

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    -- Import funding transaction twice. This operation should be idempotent
    importNetTx fundingTx
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai, TxPending)], 2))
        . testTx
    importNetTx fundingTx
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai, TxPending)], 0))
        . testTx

    spendableCoins ai 0 (const . const []) >>=
        liftIO . (assertEqual "0-conf spendable coins is not 2" 2) . length
    spendableCoins ai 1 (const . const []) >>=
        liftIO . (assertEqual "1-conf spendable coins is not 0" 0) . length

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 30000000") 30000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    addressBalances accE 1 1 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 1 balance is not (20000000, 0, 1, 0)")
            [(1, BalanceInfo 20000000 0 1 0)]

    addressBalances accE 0 0 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 1 1 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 1 1-conf balance is not (0, 0, 0, 0)")
            [(1, BalanceInfo 0 0 0 0)]

    importNetTx tx1
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai, TxPending)], 0))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 1 balance is not (20000000, 20000000, 1, 1)")
            [(1, BalanceInfo 20000000 20000000 1 1)]

    addressBalances accE 0 0 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 1 1 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 1 1-conf balance is not (0, 0, 0, 0)")
            [(1, BalanceInfo 0 0 0 0)]

    -- We re-import tx1. This operation has to be idempotent with respect to
    -- balances.
    importNetTx tx1
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai, TxPending)], 0))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 1 balance is not (20000000, 20000000, 1, 1)")
            [(1, BalanceInfo 20000000 20000000 1 1)]

    addressBalances accE 0 0 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 1 1 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 1 1-conf balance is not (0, 0, 0, 0)")
            [(1, BalanceInfo 0 0 0 0)]

    -- Importing tx2 twice. This operation has to be idempotent.
    importNetTx tx2
        >>= liftIO
        . (assertEqual "Confidence is not dead"
            ([(ai, TxDead)], 1))
        . testTx
    importNetTx tx2
        >>= liftIO
        . (assertEqual "Confidence is not dead"
            ([(ai, TxDead)], 0))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 1 balance is not (20000000, 20000000, 1, 1)")
            [(1, BalanceInfo 20000000 20000000 1 1)]

    -- Confirm the funding transaction at height 1
    importMerkles ((BestChain [fakeNode 1 bid1])) [[txHash fundingTx]]

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 1 balance is not (20000000, 20000000, 1, 1)")
            [(1, BalanceInfo 20000000 20000000 1 1)]

    addressBalances accE 0 0 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 1 1-conf balance is not (20000000, 20000000, 1, 1)")
            [(1, BalanceInfo 20000000 20000000 1 1)]

    -- Confirm tx1 at height 2
    importMerkles (BestChain [fakeNode 2 bid2]) [[txHash tx1]]

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 2 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 2 False
        >>= liftIO
        . (assertEqual "Address 1 2-conf balance is not (20000000, 20000000, 1, 1)")
            [(1, BalanceInfo 20000000 20000000 1 1)]

    addressBalances accE 0 0 AddressExternal 3 False
        >>= liftIO
        . (assertEqual "Address 0 3-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 1 1 AddressExternal 3 False
        >>= liftIO
        . (assertEqual "Address 1 3-conf balance is not (0, 0, 0, 0)")
            [(1, BalanceInfo 0 0 0 0)]

    -- Reorg on tx2
    let s = fakeNode 1 bid1
        o = [fakeNode 2 bid2]
        n = [fakeNode 2 bid3, fakeNode 3 bid4]
    importMerkles (ChainReorg s o n) [[], [txHash tx2]]

    getBy (UniqueAccTx ai (txHash tx1))
        >>= liftIO
        . (assertEqual "Confidence is not dead" TxDead)
        . keyRingTxConfidence . entityVal . fromJust

    getBy (UniqueAccTx ai (txHash tx2))
        >>= liftIO
        . (assertEqual "Confidence is not building" TxBuilding)
        . keyRingTxConfidence . entityVal . fromJust

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance ai 2 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance ai 3 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance ai 4 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 1 balance is not (20000000, 0, 1, 0)")
            [(1, BalanceInfo 20000000 0 1 0)]

    addressBalances accE 0 0 AddressExternal 3 False
        >>= liftIO
        . (assertEqual "Address 0 3-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 3 False
        >>= liftIO
        . (assertEqual "Address 1 3-conf balance is not (20000000, 0, 1, 0)")
            [(1, BalanceInfo 20000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (5000000, 0, 1, 0)")
            [(0, BalanceInfo 5000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (5000000, 0, 1, 0)")
            [(0, BalanceInfo 5000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 2 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    -- Reimporting tx2 should be idempotent and return TxBuilding
    importNetTx tx2
        >>= liftIO
        . (assertEqual "Confidence is not building"
            ([(ai, TxBuilding)], 0))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 25000000") 25000000
    accountBalance ai 2 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance ai 3 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance ai 4 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 1 balance is not (20000000, 0, 1, 0)")
            [(1, BalanceInfo 20000000 0 1 0)]

    addressBalances accE 0 0 AddressExternal 3 False
        >>= liftIO
        . (assertEqual "Address 0 3-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 3 False
        >>= liftIO
        . (assertEqual "Address 1 3-conf balance is not (20000000, 0, 1, 0)")
            [(1, BalanceInfo 20000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (5000000, 0, 1, 0)")
            [(0, BalanceInfo 5000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (5000000, 0, 1, 0)")
            [(0, BalanceInfo 5000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 2 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    -- Reorg back onto tx1
    let s2 = fakeNode 1 bid1
        o2 = [fakeNode 2 bid3, fakeNode 3 bid4]
        n2 = [fakeNode 2 bid2, fakeNode 3 bid5, fakeNode 4 bid6]
    importMerkles (ChainReorg s2 o2 n2) [[txHash tx1], [], []]

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 4 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 5 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 1 balance is not (20000000, 20000000, 1, 1)")
            [(1, BalanceInfo 20000000 20000000 1 1)]

    addressBalances accE 0 0 AddressExternal 4 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 1 1 AddressExternal 4 False
        >>= liftIO
        . (assertEqual "Address 1 2-conf balance is not (20000000, 20000000, 1, 1)")
            [(1, BalanceInfo 20000000 20000000 1 1)]

    addressBalances accE 0 0 AddressExternal 5 False
        >>= liftIO
        . (assertEqual "Address 0 4-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 1 1 AddressExternal 5 False
        >>= liftIO
        . (assertEqual "Address 1 4-conf balance is not (0, 0, 0, 0)")
            [(1, BalanceInfo 0 0 0 0)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

-- tx1, tx2 and tx3 form a chain, and tx4 is in conflict with tx1
testConflictBalances :: App ()
testConflictBalances = do
    keyE <- newKeyRing "test" bs1
    accE@(Entity ai _) <- newAccount keyE "acc1" (AccountRegular False) []
    let tx1 = fakeTx
            [ (tid1, 4) ]
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
            [ (tid1, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 20000000) ]

    -- Import first transaction
    importNetTx tx1
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai, TxPending)], 1))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 0 True  >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    -- Import second transaction
    importNetTx tx2
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai, TxPending)], 1))
        . testTx

    accountBalance ai 0 False
        >>= liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 1 False
        >>= liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, BalanceInfo 4000000 0 1 0)]

    -- Let's confirm these two transactions
    importMerkles
        (BestChain [fakeNode 1 bid1, fakeNode 2 bid2 ])
        [[txHash tx1], [txHash tx2]]

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (4000000, 0, 1, 0)")
            [(0, BalanceInfo 4000000 0 1 0)]

    addressBalances accE 0 0 AddressExternal 2 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 2 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    -- Import third transaction
    importNetTx tx3
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai, TxPending)], 0))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (4000000, 4000000, 1, 1)")
            [(0, BalanceInfo 4000000 4000000 1 1)]

    addressBalances accE 0 0 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (4000000, 4000000, 1, 1)")
            [(0, BalanceInfo 4000000 4000000 1 1)]

    addressBalances accE 0 0 AddressExternal 2 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 2 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    -- Now let's add tx4 which is in conflict with tx1
    importNetTx tx4
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai, TxDead)], 0))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 2 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (4000000, 4000000, 1, 1)")
            [(0, BalanceInfo 4000000 4000000 1 1)]

    addressBalances accE 0 0 AddressExternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 1 False
        >>= liftIO
        . (assertEqual "Address 0 1-conf balance is not (4000000, 4000000, 1, 1)")
            [(0, BalanceInfo 4000000 4000000 1 1)]

    addressBalances accE 0 0 AddressExternal 2 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 2 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    -- Now we trigger a reorg that validates tx4. tx1, tx2 and tx3 should be dead
    let s = fakeNode 0 bid0
        o = [fakeNode 1 bid1, fakeNode 2 bid2]
        n = [fakeNode 1 bid3, fakeNode 2 bid4, fakeNode 3 bid5]
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

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance ai 2 False >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000
    accountBalance ai 3 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (20000000, 0, 1, 0)")
            [(0, BalanceInfo 20000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 0 0 AddressExternal 2 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (20000000, 0, 1, 0)")
            [(0, BalanceInfo 20000000 0 1 0)]

    addressBalances accE 0 0 AddressExternal 3 False
        >>= liftIO
        . (assertEqual "Address 0 2-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    -- Reorg back to tx1, tx2 and tx3
    let s2 = fakeNode 0 bid0
        o2 = [ fakeNode 1 bid3, fakeNode 2 bid4, fakeNode 3 bid5 ]
        n2 = [ fakeNode 1 bid1, fakeNode 2 bid2
             , fakeNode 3 bid6, fakeNode 4 bid7
             ]
    importMerkles (ChainReorg s2 o2 n2) [[txHash tx1], [txHash tx2], [], []]

    getBy (UniqueAccTx ai $ txHash tx1)
        >>= liftIO
        . (assertEqual "tx1 confidence is not building") (Just TxBuilding)
        . fmap (keyRingTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx2)
        >>= liftIO
        . (assertEqual "tx2 confidence is not building") (Just TxBuilding)
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

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 2 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 3 False >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000
    accountBalance ai 4 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 5 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, BalanceInfo 4000000 0 1 0)]

    addressBalances accE 0 0 AddressExternal 3 False
        >>= liftIO
        . (assertEqual "Address 0 3-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 3 False
        >>= liftIO
        . (assertEqual "Address 0 3-conf balance is not (4000000, 0, 1, 0)")
            [(0, BalanceInfo 4000000 0 1 0)]

    addressBalances accE 0 0 AddressExternal 4 False
        >>= liftIO
        . (assertEqual "Address 0 4-conf balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 4 False
        >>= liftIO
        . (assertEqual "Address 0 4-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 0 0 AddressExternal 5 False
        >>= liftIO
        . (assertEqual "Address 0 5-conf balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

testOffline :: App ()
testOffline = do
    keyE <- newKeyRing "test" bs1
    accE@(Entity ai _) <- newAccount keyE "acc1" (AccountRegular False) []
    let tx1 = fakeTx
            [ (tid1, 4) ]
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
            [ (tid1, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 20000000) ]

    -- Import first transaction
    importTx tx1 ai
        >>= liftIO
        . (assertEqual "Confidence is not offline"
            ([(ai, TxOffline)], 1))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 0 True  >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    -- Reimporting a transaction should me idempotent
    importTx tx1 ai
        >>= liftIO
        . (assertEqual "Confidence is not offline"
            ([(ai, TxOffline)], 0))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai 0 True  >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    -- Import tx2
    importTx tx2 ai
        >>= liftIO
        . (assertEqual "Confidence is not offline"
            ([(ai, TxOffline)], 1))
        . testTx

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, BalanceInfo 4000000 0 1 0)]

    -- Import tx3
    importTx tx3 ai
        >>= liftIO
        . (assertEqual "Confidence is not offline"
            ([(ai, TxOffline)], 0))
        . testTx

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 True
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 True
        >>= liftIO
        . (assertEqual "Address 0 balance is not (4000000, 4000000, 1, 1)")
            [(0, BalanceInfo 4000000 4000000 1 1)]

    -- Import tx4
    importTx tx4 ai
        >>= liftIO
        . (assertEqual "Confidence is not offline"
            ([(ai, TxOffline)], 0))
        . testTx

    getBy (UniqueAccTx ai $ txHash tx1) >>=
        liftIO . (assertEqual "tx1 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx2) >>=
        liftIO . (assertEqual "tx2 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx3) >>=
        liftIO . (assertEqual "tx3 confidence is not dead") (Just TxDead)
            . fmap (keyRingTxConfidence . entityVal)

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (20000000, 0, 1, 0)")
            [(0, BalanceInfo 20000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    -- importTx should be idempotent
    importTx tx4 ai
        >>= liftIO
        . (assertEqual "Confidence is not offline"
            ([(ai, TxOffline)], 0))
        . testTx

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 20000000") 20000000

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (20000000, 0, 1, 0)")
            [(0, BalanceInfo 20000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

testKillOffline :: App ()
testKillOffline = do
    keyE <- newKeyRing "test" bs1
    accE@(Entity ai _) <- newAccount keyE "acc1" (AccountRegular False) []
    let tx1 = fakeTx
            [ (tid1, 4) ]
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
    importNetTx tx1
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai, TxPending)], 1))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    -- Import tx2 as offline
    importTx tx2 ai
        >>= liftIO
        . (assertEqual "Confidence is not offline"
            ([(ai, TxOffline)], 1))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Offline balance is not 4000000") 4000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "1-conf Balance is not 0") 0
    accountBalance ai 1 True >>=
        liftIO . (assertEqual "1-conf Offline balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 0 0 AddressExternal 0 True
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 True
        >>= liftIO
        . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, BalanceInfo 4000000 0 1 0)]

    -- Import tx3 as offline
    importTx tx3 ai
        >>= liftIO
        . (assertEqual "Confidence is not offline"
            ([(ai, TxOffline)], 0))
        . testTx

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Offline balance is not 0") 0
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "1-conf Balance is not 0") 0
    accountBalance ai 1 True >>=
        liftIO . (assertEqual "1-conf Offline balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    addressBalances accE 0 0 AddressExternal 0 True
        >>= liftIO
        . (assertEqual "Address 0 balance is not (10000000, 10000000, 1 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 True
        >>= liftIO
        . (assertEqual "Address 0 balance is not (4000000, 4000000, 1, 1)")
            [(0, BalanceInfo 4000000 4000000 1 1)]

    -- Import tx4 as a network transaction. It should override tx2 and tx3.
    importNetTx tx4
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai, TxPending)], 0))
        . testTx

    getBy (UniqueAccTx ai (txHash tx2))
        >>= liftIO
        . (assertEqual "Confidence is not dead" TxDead)
        . keyRingTxConfidence . entityVal . fromJust

    getBy (UniqueAccTx ai (txHash tx3))
        >>= liftIO
        . (assertEqual "Confidence is not dead" TxDead)
        . keyRingTxConfidence . entityVal . fromJust

    accountBalance ai 0 False >>=
        liftIO . (assertEqual "Balance is not 8000000") 8000000
    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Offline balance is not 8000000") 8000000
    accountBalance ai 1 False >>=
        liftIO . (assertEqual "1-conf Balance is not 0") 0
    accountBalance ai 1 True >>=
        liftIO . (assertEqual "1-conf Offline balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (15000000, 10000000, 2, 1)")
            [(0, BalanceInfo 15000000 10000000 2 1)]

    addressBalances accE 0 0 AddressInternal 0 False
        >>= liftIO
        . (assertEqual "Address 0 balance is not (3000000, 0, 1, 0)")
            [(0, BalanceInfo 3000000 0 1 0)]

    addressBalances accE 0 0 AddressExternal 0 True
        >>= liftIO
        . (assertEqual "Address 0 balance is not (15000000, 10000000, 2 1)")
            [(0, BalanceInfo 15000000 10000000 2 1)]

    addressBalances accE 0 0 AddressInternal 0 True
        >>= liftIO
        . (assertEqual "Address 0 balance is not (3000000, 0, 1, 0)")
            [(0, BalanceInfo 3000000 0 1 0)]

testOfflineExceptions :: Assertion
testOfflineExceptions = do
    let tx1 = fakeTx
            [ (tid1, 4) ]
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
            [ (tid1, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 20000000) ]

    assertException (WalletException "Could not import offline transaction") $ do
        keyE <- newKeyRing "test" bs1
        _ <- newAccount keyE "acc1" (AccountRegular False) []
        (_, Entity ai _) <- getAccount "test" "acc1"
        importNetTx tx1
            >>= liftIO
            . (assertEqual "Confidence is not pending"
                ([(ai, TxPending)], 1))
            . testTx
        importTx tx4 ai

    assertException (WalletException "Could not import offline transaction") $ do
        keyE <- newKeyRing "test" bs1
        _ <- newAccount keyE "acc1" (AccountRegular False) []
        (_, Entity ai _) <- getAccount "test" "acc1"
        importNetTx tx4
            >>= liftIO
            . (assertEqual "Confidence is not pending"
                ([(ai, TxPending)], 1))
            . testTx
        importNetTx tx1
            >>= liftIO
            . (assertEqual "Confidence is not dead"
                ([(ai, TxDead)], 0))
            . testTx
        importNetTx tx2
            >>= liftIO
            . (assertEqual "Confidence is not dead"
                ([(ai, TxDead)], 1))
            . testTx
        importTx tx3 ai

    assertException (WalletException "Could not import offline transaction") $ do
        keyE <- newKeyRing "test" bs1
        _ <- newAccount keyE "acc1" (AccountRegular False) []
        (_, Entity ai _) <- getAccount "test" "acc1"
        importNetTx tx1
            >>= liftIO
            . (assertEqual "Confidence is not pending"
                ([(ai, TxPending)], 1))
            . testTx
        importTx tx1 ai

-- This test create a multisig account with the key of testImportMultisig2
testImportMultisig :: App ()
testImportMultisig = do
    keyE <- newKeyRing "test" bs1
    _ <- newAccount keyE "ms1" (AccountMultisig False 2 2)
        [fromJust $ xPubImport "xpub69iinth3CTrfkmijzhQXi3kwhGQjba31fncrBgA9vM9T9tv69qSwp525yDVYmX2BTAdeuYSZqkcWhkrqD5Xbsz5YHJZL6CzYGL2WACorpdS"]
    _ <- newAccount keyE "ms2" (AccountMultisig False 2 2)
        [fromJust $ xPubImport "xpub69iinth3CTrfh5efv7baTWwk9hHi4zqcQEsNFgVwEJvdaZVEPytZzmNxjYTnF5F5x2CamLXvmD1T4RhpsuaXSFPo2MnLN5VqWqrWb82U7ED"]
    Entity _ keyRing <- getKeyRing "test"
    (_, accE1@(Entity ai1 _)) <- getAccount "test" "ms1"
    (_, accE2@(Entity ai2 _)) <- getAccount "test" "ms2"

    let fundingTx =
            Tx 1 [ TxIn (OutPoint tid1 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayScriptHash $ fromJust $
                    base58ToAddr "3Dgz9gqsAMPr7i9qocLMNHU8wuoKqtUNoM"
                 ] 0

    importNetTx fundingTx
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai1, TxPending), (ai2, TxPending)], 2))
        . testTx

    -- Create a transaction which has 0 signatures in ms1
    (tx1, _) <- createTx keyRing accE1
        [ ( fromJust $ base58ToAddr "3C9fz8kDwX2rV25YeWC7YcDNHtTreAV52m"
          , 5000000
          )
        ] 10000 0 False True
    liftIO $ assertEqual "Confidence is not offline" TxOffline $
        keyRingTxConfidence tx1
    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins" [])
        . map (keyRingCoinHash . entityVal . inCoinDataCoin)
    txs ai1 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, keyRingTxHash tx1]))
        . sort . map keyRingTxHash . fst
    accountBalance ai1 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai1 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai1 0 True >>=
        liftIO . (assertEqual "Offline balance is not 9990000") 9990000

    -- Import the empty transaction in ms2
    (tx2:_, _) <- importTx (keyRingTxTx tx1) ai2
    -- This second import should be idempotent
    _ <- importTx (keyRingTxTx tx1) ai2
    liftIO $ assertEqual "Txid do not match"
        (keyRingTxHash tx1) (keyRingTxHash tx2)
    liftIO $ assertEqual "Confidence is not offline" TxOffline $
        keyRingTxConfidence tx2
    spendableCoins ai2 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins" [])
        . map (keyRingCoinHash . entityVal . inCoinDataCoin)
    txs ai2 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, keyRingTxHash tx2]))
        . sort . map keyRingTxHash . fst
    accountBalance ai2 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai2 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai2 0 True >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

    -- Sign the transaction in ms2
    (tx3:_, _) <- signKeyRingTx keyRing accE2 $ keyRingTxHash tx2
    liftIO $ assertEqual "Confidence is not pending" TxPending $
        keyRingTxConfidence tx3
    spendableCoins ai2 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins"
            [keyRingTxHash tx3, keyRingTxHash tx3])
        . map (keyRingCoinHash . entityVal . inCoinDataCoin)
    txs ai2 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, keyRingTxHash tx3]))
        . sort . map keyRingTxHash . fst
    accountBalance ai2 0 False >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance ai2 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai2 0 True  >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

    tx4 <- liftM (entityVal . fromJust) $
        getBy $ UniqueAccTx ai1 $ keyRingTxHash tx3
    liftIO $ assertEqual "Confidence is not pending" TxPending $
        keyRingTxConfidence tx4
    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins"
            [keyRingTxHash tx3, keyRingTxHash tx3])
        . map (keyRingCoinHash . entityVal . inCoinDataCoin)
    txs ai1 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, keyRingTxHash tx3]))
        . sort . map keyRingTxHash . fst
    accountBalance ai1 0 False >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance ai1 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai1 0 True  >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

    -- Importing the transaction should have no effect as it was globally
    -- imported already in the previous step.
    (tx5:_, _) <- importTx (keyRingTxTx tx3) ai1
    liftIO $ assertEqual "Confidence is not pending" TxPending $
        keyRingTxConfidence tx5
    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins"
            [keyRingTxHash tx5, keyRingTxHash tx5])
        . map (keyRingCoinHash . entityVal . inCoinDataCoin)
    txs ai1 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, keyRingTxHash tx5]))
        . sort . map keyRingTxHash . fst
    accountBalance ai1 0 False >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance ai1 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai1 0 True  >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

testKillTx :: App ()
testKillTx = do
    keyE <- newKeyRing "test" bs1
    _ <- newAccount keyE "acc1" (AccountRegular False) []
    (_, accE@(Entity ai _)) <- getAccount "test" "acc1"
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("13XaDQvvE4rqiVKMi4MApsaZwTcDNiwfuR", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1BwbQ8Wp7YUfaYeiQPgXu6br5e4ogKjuKd", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external

    importNetTx tx1
        >>= liftIO
        . (assertEqual "Confidence is not pending" ([(ai, TxPending)], 1))
        . testTx
    importNetTx tx2
        >>= liftIO
        . (assertEqual "Confidence is not pending" ([(ai, TxPending)], 1))
        . testTx
    importNetTx tx3
        >>= liftIO
        . (assertEqual "Confidence is not pending" ([(ai, TxPending)], 0))
        . testTx

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 0") 0

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 4000000, 1, 1)")
            [(0, BalanceInfo 4000000 4000000 1 1)]

    killTxs [txHash tx2]

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    -- Killing a transaction should be idempotent
    killTxs [txHash tx2]

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    killTxs [txHash tx3]

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

    reviveTx tx2

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, BalanceInfo 4000000 0 1 0)]

    -- Reviving a transaction should be idempotent
    reviveTx tx2

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 4000000") 4000000

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 10000000, 1, 1)")
            [(0, BalanceInfo 10000000 10000000 1 1)]

    addressBalances accE 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (4000000, 0, 1, 0)")
            [(0, BalanceInfo 4000000 0 1 0)]

testTx :: ([KeyRingTx], [KeyRingAddr])
       -> ([(KeyRingAccountId, TxConfidence)], Int)
testTx (txls, addrs) = (map f txls, length addrs)
  where
    f tx = (keyRingTxAccount tx, keyRingTxConfidence tx)

