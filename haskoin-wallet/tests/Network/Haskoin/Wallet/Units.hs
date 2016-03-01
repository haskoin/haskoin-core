module Network.Haskoin.Wallet.Units (tests) where

import Test.HUnit (Assertion, assertEqual, assertFailure, assertBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (liftM, guard)
import Control.Monad.Trans (liftIO)
import Control.Exception (Exception, handleJust)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (NoLoggingT)

import Data.String.Conversions (cs)
import Data.Word (Word32, Word64)
import Data.Maybe (fromJust, isJust)
import Data.List (sort)
import qualified Data.ByteString as BS
    ( ByteString
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

type App = SqlPersistT (NoLoggingT (ResourceT IO))

-- TODO: Add tests for accounts with no private key
tests :: [Test]
tests =
    [ testGroup "Account tests"
        [ testCase "Fail create account with wrong keys" $
            assertException
                (WalletException "Invalid account keys")
                ( newAccount NewAccount
                    { newAccountName = "fail-this"
                    , newAccountType = AccountRegular
                    -- This key does not correspond to the one below
                    , newAccountMaster = Just
                        "xprv9s21ZrQH143K33Ezpb81k5upGyhrVcwgqNzHRHnQ2kGBPHkJ3sLPjGwj4LML1kr1bLfguJiY21XrYfVrL1CGurfVoMKSPwRdmzt1LwBtVyR"
                    , newAccountDeriv = Nothing
                    , newAccountKeys =
                        ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                    , newAccountMnemonic = Nothing
                    , newAccountReadOnly = False
                    }
                )

        , testCase "Creating two accounts with the same data should fail" $
            assertException
                (WalletException "Account already exists") $ do
                    _ <- newAccount NewAccount
                        { newAccountName = "main"
                        , newAccountType = AccountRegular
                        , newAccountMaster = Just
                            "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                        , newAccountDeriv = Nothing
                        , newAccountKeys =
                            ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                        , newAccountMnemonic = Nothing
                        , newAccountReadOnly = False
                        }
                    newAccount NewAccount
                        { newAccountName = "main"
                        , newAccountType = AccountRegular
                        , newAccountMaster = Nothing
                        , newAccountDeriv = Nothing
                        , newAccountKeys =
                            ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                        , newAccountMnemonic = Nothing
                        , newAccountReadOnly = False
                        }
        , testCase "Invalid multisig parameters (0 of 1)" $
            assertException (WalletException "Invalid account type") $
                newAccount NewAccount
                    { newAccountName = "multisig-0-of-1"
                    , newAccountType = AccountMultisig 0 1
                    , newAccountMaster = Just
                        "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                    , newAccountDeriv = Nothing
                    , newAccountKeys =
                        ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                    , newAccountMnemonic = Nothing
                    , newAccountReadOnly = False
                    }

        , testCase "Invalid multisig parameters (2 of 1)" $
            assertException (WalletException "Invalid account type") $
                newAccount NewAccount
                    { newAccountName = "multisig-2-of-1"
                    , newAccountType = AccountMultisig 2 1
                    , newAccountMaster = Just
                        "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                    , newAccountDeriv = Nothing
                    , newAccountKeys =
                        ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                    , newAccountMnemonic = Nothing
                    , newAccountReadOnly = False
                    }

        , testCase "Invalid multisig parameters (15 of 16)" $
            assertException (WalletException "Invalid account type") $
                newAccount NewAccount
                    { newAccountName = "multisig-15-of-16"
                    , newAccountType = AccountMultisig 15 16
                    , newAccountMaster = Just
                        "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                    , newAccountDeriv = Nothing
                    , newAccountKeys =
                        ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                    , newAccountMnemonic = Nothing
                    , newAccountReadOnly = False
                    }

        , testCase "To many multisig keys (3 keys for 1 of 2)" $
            assertException (WalletException "Invalid account keys") $
                newAccount NewAccount
                    { newAccountName = "multisig-1-of-2-with-3"
                    , newAccountType = AccountMultisig 1 2
                    , newAccountMaster = Just
                        "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                    , newAccountDeriv = Nothing
                    , newAccountKeys =
                        [ "xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"
                        , "xpub661MyMwAqRbcFEPH5Aon6F7edspeu1v6a1Nw5qJgk1aX5XYg1ktBL9Azra2CKaAJ2bHXEXkeKHE3eFaCJktFiA5tSMDQDs6bi83maQtdYby"
                        , "xpub661MyMwAqRbcFtDszBWpawpg4KbNWL9qD4VdRwjd1L5cmcS8nXHWXpg9WL1Xc9Yh7HbQBwWDw37YJfc4AF3YEpvAHEBPBFQPFkUcFHnopw8"
                        ]
                    , newAccountMnemonic = Nothing
                    , newAccountReadOnly = False
                    }

        , testCase "Calling addAccountKeys with an empty key list should fail" $
            assertException
                (WalletException "Invalid account keys") $ do
                    res <- newAccount NewAccount
                        { newAccountName = "multisig-1-of-2-plus-empty"
                        , newAccountType = AccountMultisig 1 2
                        , newAccountMaster = Just
                            "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                        , newAccountDeriv = Nothing
                        , newAccountKeys =
                            ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                        , newAccountMnemonic = Nothing
                        , newAccountReadOnly = False
                        }
                    addAccountKeys (fst res) []

        , testCase "Calling addAccountKeys on a non-multisig account should fail" $
            assertException
                (WalletException "The account is already complete") $ do
                    res <- newAccount NewAccount
                        { newAccountName = "regular-plus-more"
                        , newAccountType = AccountRegular
                        , newAccountMaster = Just
                            "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                        , newAccountDeriv = Nothing
                        , newAccountKeys =
                            ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                        , newAccountMnemonic = Nothing
                        , newAccountReadOnly = False
                        }
                    addAccountKeys (fst res)
                        ["xpub661MyMwAqRbcFEPH5Aon6F7edspeu1v6a1Nw5qJgk1aX5XYg1ktBL9Azra2CKaAJ2bHXEXkeKHE3eFaCJktFiA5tSMDQDs6bi83maQtdYby"]

        , testCase "Adding keys to a complete multisig account should fail" $
            assertException
                (WalletException "The account is already complete") $ do
                    res <- newAccount NewAccount
                        { newAccountName = "regular-plus-more"
                        , newAccountType = AccountMultisig 1 2
                        , newAccountMaster = Just
                            "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                        , newAccountDeriv = Nothing
                        , newAccountKeys =
                            [ "xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"
                            , "xpub661MyMwAqRbcFEPH5Aon6F7edspeu1v6a1Nw5qJgk1aX5XYg1ktBL9Azra2CKaAJ2bHXEXkeKHE3eFaCJktFiA5tSMDQDs6bi83maQtdYby"
                            ]
                        , newAccountMnemonic = Nothing
                        , newAccountReadOnly = False
                        }
                    addAccountKeys (fst res)
                        ["xpub661MyMwAqRbcFtDszBWpawpg4KbNWL9qD4VdRwjd1L5cmcS8nXHWXpg9WL1Xc9Yh7HbQBwWDw37YJfc4AF3YEpvAHEBPBFQPFkUcFHnopw8"]

        , testCase "Getting a non-existing account should fail" $
            assertException
                (WalletException "Account inexistent does not exist") $
                    getAccount "inexistent"

        ]
    , testGroup "Address tests"
        [ testCase "Decreasing the address gap should fail" $
            assertException (WalletException "The gap of an account can only be increased") $ do
                res <- newAccount NewAccount
                    { newAccountName = "reduce-gap"
                    , newAccountType = AccountRegular
                    , newAccountMaster = Just
                        "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                    , newAccountDeriv = Nothing
                    , newAccountKeys =
                        ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                    , newAccountMnemonic = Nothing
                    , newAccountReadOnly = False
                    }
                accE' <- setAccountGap (fst res) 15
                setAccountGap accE' 14

        , testCase "Setting a label on a hidden address key should fail" $
            assertException (WalletException "Invalid address index 10") $ do
                res <- newAccount NewAccount
                    { newAccountName = "label-hidden"
                    , newAccountType = AccountRegular
                    , newAccountMaster = Just
                        "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                    , newAccountDeriv = Nothing
                    , newAccountKeys =
                        ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                    , newAccountMnemonic = Nothing
                    , newAccountReadOnly = False
                    }
                setAddrLabel (fst res) 10 AddressExternal "Gym membership"

        , testCase "Setting a label on an invalid address key should fail" $
            assertException (WalletException "Invalid address index 20") $ do
                res <- newAccount NewAccount
                    { newAccountName = "label-invalid"
                    , newAccountType = AccountRegular
                    , newAccountMaster = Just
                        "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"
                    , newAccountDeriv = Nothing
                    , newAccountKeys =
                        ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                    , newAccountMnemonic = Nothing
                    , newAccountReadOnly = False
                    }
                setAddrLabel (fst res) 20 AddressExternal "Gym membership"

        , testCase "Requesting an address prvkey on a read-only account should fail" $
            assertException
                (WalletException "Could not get private key") $ do
                    res <- newAccount NewAccount
                        { newAccountName = "label-invalid"
                        , newAccountType = AccountRegular
                        , newAccountMaster = Nothing
                        , newAccountDeriv = Nothing
                        , newAccountKeys =
                            ["xpub661MyMwAqRbcH49U84sbFSXEwforKeyukvkBuY9x2ruDvLUccaRf2SeUL1f6StQke7sCuvott5CjzFqw6aA49g2NSjaARBkQdHE18zjC5hB"]
                        , newAccountMnemonic = Nothing
                        , newAccountReadOnly = False
                        }
                    addressPrvKey (fst res) Nothing 2 AddressExternal
        ]
    , testGroup "Wallet tests"
        [ testCase "Verify address derivations" $ runUnit testDerivations
        , testCase "Verify balances" $ runUnit testBalances
        , testCase "Verify balances in conflict" $ runUnit testConflictBalances
        , testCase "Offline transactions" $ runUnit testOffline
        , testCase "Kill an offline tx by spending its coins" $ runUnit testKillOffline
        , testCase "Offline transaction exceptions" testOfflineExceptions
        , testCase "Multisig test 1" $ runUnit testImportMultisig
        , testCase "Kill Tx" $ runUnit testKillTx
        , testCase "Delete Tx" $ runUnit testDeleteTx
        , testCase "Delete Unsigned Tx" $ runUnit testDeleteUnsignedTx
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

ms :: Mnemonic
ms = "mass coast dance birth online various renew alert crunch middle absurd health"

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
    (accE, _) <- newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just (Deriv :| 0)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = []
        , newAccountReadOnly = False
        }

    unusedAddresses accE AddressExternal
        >>= liftIO . assertEqual "Generated external addresses do not match"
            [ "1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe"
            , "1NrcKe9UNtxjjgMSLdhBYaEgrQWQFnvJXX"
            , "123XgHRNxkprjec72EpxBFPytPim21u9Kc"
            , "16AuD3mAQMzsUHkMGfkQQapG6jhHmV3Rys"
            , "1AjF1GhsxyXCN5doPGLjSztDcuLbfYMkqw"
            , "1LPUYEjUd1u9dgjM3RqnoYj7Zt4j4dmZYA"
            , "1Kzyb5Fpj2VmMoCNWxLNMYeMSu5ocS7u7e"
            , "131L3UXV6WakXpyXvmqzqNkHwWJzWExR9i"
            , "19FWGTZERHzMePTqKoR8nB9y6w7S5u9yGr"
            , "135dwGc8JG2dmhy79onerHKdqoqibHShRJ"
            ] . map (addrToBase58 . walletAddrAddress)

    unusedAddresses accE AddressInternal
        >>= liftIO . assertEqual "Generated internal addresses do not match"
            [ "1PY7pWZ5FddWi747C6k5Y7okNHFUM2BKAm"
            , "1JVocGcqZvQFbfpeQUY92iL9ARLsAP4DjW"
            , "15yaPowqF2g9B3xbdyXaPygV9bfXFjYj2X"
            , "187xp9nQTsCa7QJLHYSMyaM1Mf5u2knJYY"
            , "1QDQwk4d4zVWH3TnUHMCvDvvrTqFSaA3hr"
            , "17ATGqWhkXYLxPdynWrFjnJBvD3EYxqC5A"
            , "1EHgPV5DEs3GwUSaRQD7hd2m5MkR4hdByb"
            , "1EvUYGoC7p7GC8BtYTz4RX2ERxjUsT3zLz"
            , "15857PJZQxUH8Jgomv83mYqJqDsVEUZwz1"
            , "18L5fQjr5yXdqqnK98v2UQi1WHAXfYHr7L"
            ] . map (addrToBase58 . walletAddrAddress)

testBalances :: App ()
testBalances = do
    (accE@(Entity ai _), _) <- newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just (Deriv :| 0)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = []
        , newAccountReadOnly = False
        }
    let fundingTx = fakeTx
            [ (tid1, 0) ]
            [ ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 10000000)
            , ("1NrcKe9UNtxjjgMSLdhBYaEgrQWQFnvJXX", 20000000)
            ]
    let tx1 = fakeTx
            [ (txHash fundingTx, 0)
            , (txHash fundingTx, 1)
            ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 30000000) ] -- external
        tx2 = fakeTx
            [ (txHash fundingTx, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 5000000) -- external
            , ("1PY7pWZ5FddWi747C6k5Y7okNHFUM2BKAm", 5000000) -- change
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
        . walletTxConfidence . entityVal . fromJust

    getBy (UniqueAccTx ai (txHash tx2))
        >>= liftIO
        . (assertEqual "Confidence is not building" TxBuilding)
        . walletTxConfidence . entityVal . fromJust

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
    (accE@(Entity ai _), _) <- newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just (Deriv :| 0)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = []
        , newAccountReadOnly = False
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1PY7pWZ5FddWi747C6k5Y7okNHFUM2BKAm", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (tid1, 4) ]
            [ ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 20000000) ]

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
            . fmap (walletTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx2) >>=
        liftIO . (assertEqual "tx2 confidence is not dead") (Just TxDead)
            . fmap (walletTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx3) >>=
        liftIO . (assertEqual "tx3 confidence is not dead") (Just TxDead)
            . fmap (walletTxConfidence . entityVal)

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
        . fmap (walletTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx2)
        >>= liftIO
        . (assertEqual "tx2 confidence is not building") (Just TxBuilding)
        . fmap (walletTxConfidence . entityVal)

    -- Tx3 remains dead until it is included into a block. Dead transaction are
    -- only revived upon confirmations. They are not revived if they are not
    -- confirmed even if they have no conflicts anymore.
    getBy (UniqueAccTx ai $ txHash tx3) >>=
        liftIO . (assertEqual "tx3 confidence is not dead") (Just TxDead)
            . fmap (walletTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx4) >>=
        liftIO . (assertEqual "tx4 confidence is not dead") (Just TxDead)
            . fmap (walletTxConfidence . entityVal)

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
    (accE@(Entity ai _), _) <- newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just (Deriv :| 0)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = []
        , newAccountReadOnly = False
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1PY7pWZ5FddWi747C6k5Y7okNHFUM2BKAm", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (tid1, 4) ]
            [ ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 20000000) ]

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
            . fmap (walletTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx2) >>=
        liftIO . (assertEqual "tx2 confidence is not dead") (Just TxDead)
            . fmap (walletTxConfidence . entityVal)

    getBy (UniqueAccTx ai $ txHash tx3) >>=
        liftIO . (assertEqual "tx3 confidence is not dead") (Just TxDead)
            . fmap (walletTxConfidence . entityVal)

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
    (accE@(Entity ai _), _) <- newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just (Deriv :| 0)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = []
        , newAccountReadOnly = False
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1PY7pWZ5FddWi747C6k5Y7okNHFUM2BKAm", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 2000000) -- external
            , ("1PY7pWZ5FddWi747C6k5Y7okNHFUM2BKAm", 3000000) -- change
            , ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 5000000) -- more change
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
        . walletTxConfidence . entityVal . fromJust

    getBy (UniqueAccTx ai (txHash tx3))
        >>= liftIO
        . (assertEqual "Confidence is not dead" TxDead)
        . walletTxConfidence . entityVal . fromJust

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
            [ ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1PY7pWZ5FddWi747C6k5Y7okNHFUM2BKAm", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (tid1, 4) ]
            [ ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 20000000) ]

    assertException (WalletException "Could not import offline transaction") $ do
        _ <- newAccount NewAccount
            { newAccountName = "acc1"
            , newAccountType = AccountRegular
            , newAccountDeriv = Just (Deriv :| 0)
            , newAccountMaster = Nothing
            , newAccountMnemonic = Just (cs ms)
            , newAccountKeys = []
            , newAccountReadOnly = False
            }
        Entity ai _ <- getAccount "acc1"
        importNetTx tx1
            >>= liftIO
            . (assertEqual "Confidence is not pending"
                ([(ai, TxPending)], 1))
            . testTx
        importTx tx4 ai

    assertException (WalletException "Could not import offline transaction") $ do
        _ <- newAccount NewAccount
            { newAccountName = "acc1"
            , newAccountType = AccountRegular
            , newAccountDeriv = Just (Deriv :| 0)
            , newAccountMaster = Nothing
            , newAccountMnemonic = Just (cs ms)
            , newAccountKeys = []
            , newAccountReadOnly = False
            }
        Entity ai _ <- getAccount "acc1"
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
        _ <- newAccount NewAccount
            { newAccountName = "acc1"
            , newAccountType = AccountRegular
            , newAccountDeriv = Just (Deriv :| 0)
            , newAccountMaster = Nothing
            , newAccountMnemonic = Just (cs ms)
            , newAccountKeys = []
            , newAccountReadOnly = False
            }
        Entity ai _ <- getAccount "acc1"
        importNetTx tx1
            >>= liftIO
            . (assertEqual "Confidence is not pending"
                ([(ai, TxPending)], 1))
            . testTx
        importTx tx1 ai

-- This test create a multisig account with the key of testImportMultisig2
testImportMultisig :: App ()
testImportMultisig = do
    (accE1@(Entity ai1 _), _) <- newAccount NewAccount
        { newAccountName = "ms1"
        , newAccountType = AccountMultisig 2 2
        , newAccountDeriv = Just (Deriv :| 0)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = ["xpub68kRFKHWxUt3oS8X5kVogwH5rvuAd4jrLkxVfHeudFC4MfwQ8oYV59F91uFnsLXANRB1MkN4Wa1PwymE4cRsU8PE755HNCb1EoBbSoAKXpW"]
        , newAccountReadOnly = False
        }
    (accE2@(Entity ai2 _), _) <- newAccount NewAccount
        { newAccountName = "ms2"
        , newAccountType = AccountMultisig 2 2
        , newAccountDeriv = Just (Deriv :| 1)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = ["xpub68kRFKHWxUt3mfJjcXdLeuDjCHnByqKSBVfMktJRXM6LSNNDR4ae6Nw1Kh621fzyKiBf6ssyZWPPTDUTQp1BhuZQuoVdtb8j2TRzqDLHmY7"]
        , newAccountReadOnly = False
        }
    let fundingTx =
            Tx 1 [ TxIn (OutPoint tid1 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayScriptHash $ fromJust $
                    base58ToAddr "32RexHZdsMoV8yzL1pQyFhYY6XeUNcWP78"
                 ] 0

    importNetTx fundingTx
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai1, TxPending), (ai2, TxPending)], 2))
        . testTx

    -- Create a transaction which has 0 signatures in ms1
    (tx1, _) <- createTx accE1 Nothing
        [ ( fromJust $ base58ToAddr "3BYWaQHz6AVXx7wXmCka4846tRfa1ccWvh"
          , 5000000
          )
        ] 10000 0 False True
    liftIO $ assertEqual "Confidence is not offline" TxOffline $
        walletTxConfidence tx1
    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins" [])
        . map (walletCoinHash . entityVal . inCoinDataCoin)
    txs ai1 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx1]))
        . sort . map walletTxHash . fst
    accountBalance ai1 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai1 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai1 0 True >>=
        liftIO . (assertEqual "Offline balance is not 9990000") 9990000

    -- Import the empty transaction in ms2
    (tx2:_, _) <- importTx (walletTxTx tx1) ai2
    -- This second import should be idempotent
    _ <- importTx (walletTxTx tx1) ai2
    liftIO $ assertEqual "Txid do not match"
        (walletTxHash tx1) (walletTxHash tx2)
    liftIO $ assertEqual "Confidence is not offline" TxOffline $
        walletTxConfidence tx2
    spendableCoins ai2 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins" [])
        . map (walletCoinHash . entityVal . inCoinDataCoin)
    txs ai2 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx2]))
        . sort . map walletTxHash . fst
    accountBalance ai2 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai2 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai2 0 True >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

    -- Sign the transaction in ms2
    (tx3:_, _) <- signAccountTx accE2 Nothing $ walletTxHash tx2
    liftIO $ assertEqual "Confidence is not pending" TxPending $
        walletTxConfidence tx3
    spendableCoins ai2 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins"
            [walletTxHash tx3, walletTxHash tx3])
        . map (walletCoinHash . entityVal . inCoinDataCoin)
    txs ai2 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx3]))
        . sort . map walletTxHash . fst
    accountBalance ai2 0 False >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance ai2 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai2 0 True  >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

    tx4 <- liftM (entityVal . fromJust) $
        getBy $ UniqueAccTx ai1 $ walletTxHash tx3
    liftIO $ assertEqual "Confidence is not pending" TxPending $
        walletTxConfidence tx4
    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins"
            [walletTxHash tx3, walletTxHash tx3])
        . map (walletCoinHash . entityVal . inCoinDataCoin)
    txs ai1 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx3]))
        . sort . map walletTxHash . fst
    accountBalance ai1 0 False >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance ai1 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai1 0 True  >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

    -- Importing the transaction should have no effect as it was globally
    -- imported already in the previous step.
    (tx5:_, _) <- importTx (walletTxTx tx3) ai1
    liftIO $ assertEqual "Confidence is not pending" TxPending $
        walletTxConfidence tx5
    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins"
            [walletTxHash tx5, walletTxHash tx5])
        . map (walletCoinHash . entityVal . inCoinDataCoin)
    txs ai1 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx5]))
        . sort . map walletTxHash . fst
    accountBalance ai1 0 False >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000
    accountBalance ai1 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai1 0 True  >>=
        liftIO . (assertEqual "Balance is not 9990000") 9990000

testDeleteTx :: App ()
testDeleteTx = do
    (accE@(Entity ai _), _) <- newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just (Deriv :| 0)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = []
        , newAccountReadOnly = False
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1PY7pWZ5FddWi747C6k5Y7okNHFUM2BKAm", 4000000) -- change
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

    tx2M <- getTx $ txHash tx2
    liftIO $ assertBool "Transaction 2 not found" $ isJust tx2M
    deleteTx $ txHash tx2

    tx1M <- getTx $ txHash tx1
    tx2M <- getTx $ txHash tx2
    tx3M <- getTx $ txHash tx3
    liftIO $ assertEqual "Transaction 1 removed" (Just tx1) tx1M
    liftIO $ assertEqual "Transaction 2 not removed" Nothing tx2M
    liftIO $ assertEqual "Transaction 3 not removed" Nothing tx3M

    accountBalance ai 0 True >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000

    addressBalances accE 0 0 AddressExternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (10000000, 0, 1, 0)")
            [(0, BalanceInfo 10000000 0 1 0)]

    addressBalances accE 0 0 AddressInternal 0 True >>=
        liftIO . (assertEqual "Address 0 balance is not (0, 0, 0, 0)")
            [(0, BalanceInfo 0 0 0 0)]

testDeleteUnsignedTx :: App ()
testDeleteUnsignedTx = do
    (accE1@(Entity ai1 _), _) <- newAccount NewAccount
        { newAccountName = "ms1"
        , newAccountType = AccountMultisig 2 2
        , newAccountDeriv = Just (Deriv :| 0)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = ["xpub68kRFKHWxUt3oS8X5kVogwH5rvuAd4jrLkxVfHeudFC4MfwQ8oYV59F91uFnsLXANRB1MkN4Wa1PwymE4cRsU8PE755HNCb1EoBbSoAKXpW"]
        , newAccountReadOnly = False
        }
    ((Entity ai2 _), _) <- newAccount NewAccount
        { newAccountName = "ms2"
        , newAccountType = AccountMultisig 2 2
        , newAccountDeriv = Just (Deriv :| 1)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = ["xpub68kRFKHWxUt3mfJjcXdLeuDjCHnByqKSBVfMktJRXM6LSNNDR4ae6Nw1Kh621fzyKiBf6ssyZWPPTDUTQp1BhuZQuoVdtb8j2TRzqDLHmY7"]
        , newAccountReadOnly = False
        }
    let fundingTx =
            Tx 1 [ TxIn (OutPoint tid1 0) (BS.pack [1]) maxBound ] -- dummy input
                 [ TxOut 10000000 $
                    encodeOutputBS $ PayScriptHash $ fromJust $
                    base58ToAddr "32RexHZdsMoV8yzL1pQyFhYY6XeUNcWP78"
                 ] 0

    importNetTx fundingTx
        >>= liftIO
        . (assertEqual "Confidence is not pending"
            ([(ai1, TxPending), (ai2, TxPending)], 2))
        . testTx

    -- Create a transaction which has 0 signatures in ms1
    (tx1, _) <- createTx accE1 Nothing
        [ ( fromJust $ base58ToAddr "3BYWaQHz6AVXx7wXmCka4846tRfa1ccWvh"
          , 5000000
          )
        ] 10000 0 False True
    liftIO $ assertEqual "Confidence is not offline" TxOffline $
        walletTxConfidence tx1
    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . (assertEqual "Wrong txhash in coins" [])
        . map (walletCoinHash . entityVal . inCoinDataCoin)
    txs ai1 (ListRequest 0 10 False)
        >>= liftIO
        . (assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx1]))
        . sort . map walletTxHash . fst
    accountBalance ai1 0 False >>=
        liftIO . (assertEqual "Balance is not 10000000") 10000000
    accountBalance ai1 1 False >>=
        liftIO . (assertEqual "Balance is not 0") 0
    accountBalance ai1 0 True >>=
        liftIO . (assertEqual "Offline balance is not 9990000") 9990000

    tx1EM <- getTx $ walletTxHash tx1
    liftIO $ assertBool "Transaction 1 not found" $ isJust tx1EM
    deleteTx $ walletTxHash tx1

    tx1M <- getTx $ walletTxHash tx1
    liftIO $ assertEqual "Transaction not removed" Nothing tx1M

testKillTx :: App ()
testKillTx = do
    (accE@(Entity ai _), _) <- newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just (Deriv :| 0)
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountKeys = []
        , newAccountReadOnly = False
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1DLW4wieCwUPMh6ThVwT2bKqSzkjeb8wUe", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1PY7pWZ5FddWi747C6k5Y7okNHFUM2BKAm", 4000000) -- change
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

testTx :: ([WalletTx], [WalletAddr])
       -> ([(AccountId, TxConfidence)], Int)
testTx (txls, addrs) = (map f txls, length addrs)
  where
    f tx = (walletTxAccount tx, walletTxConfidence tx)

