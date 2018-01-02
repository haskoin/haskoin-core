{-# LANGUAGE RecordWildCards #-}
module Network.Haskoin.Wallet.Units (tests) where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMChan
import           Control.Exception                (Exception, handleJust)
import           Control.Monad                    (guard)
import           Control.Monad.Logger             (NoLoggingT)
import           Control.Monad.Trans              (liftIO)
import           Control.Monad.Trans.Resource     (ResourceT)
import qualified Data.ByteString                  as BS
import           Data.List                        (sort)
import           Data.Maybe                       (fromJust, isJust)
import           Data.String.Conversions          (cs)
import           Data.Word                        (Word32, Word64)
import           Database.Persist                 (Entity (..), entityVal,
                                                   getBy)
import           Database.Persist.Sqlite          (SqlPersistT,
                                                   runMigrationSilent,
                                                   runSqlite)
import           Network.Haskoin.Block
import           Network.Haskoin.Crypto
import           Network.Haskoin.Node.HeaderTree
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Wallet.Internals
import           Test.Framework                   (Test, testGroup)
import           Test.Framework.Providers.HUnit   (testCase)
import           Test.HUnit                       (Assertion, assertBool,
                                                   assertEqual, assertFailure)

import           Network.Haskoin.Util

type App = SqlPersistT (NoLoggingT (ResourceT IO))

xPrv1 :: XPrvKey
xPrv1 = "xprv9s21ZrQH143K4a5123LatJaWPdyMvCG4Phpb79kLUXNF3Y9U537QUeKzUjkrdoZVVse747ZnNNUGryPZXEoMFjkuUKyWpEMcg7jbxYECE2b"

-- Bip44 account derivation 0 of xPrv1
xPub1 :: XPubKey
xPub1 = "xpub6CsAAZAfnTnNFdmJu6St12N1vHHCPpG8uunWsTwc5PPnB4tk4k99mrgGoRBbt48SarGnLJZf5uwqGBtaBnQzVBtoA6aqtZAsx1xYvFCXM6H"

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
                    , newAccountMaster = Just "xprv9s21ZrQH143K33Ezpb81k5upGyhrVcwgqNzHRHnQ2kGBPHkJ3sLPjGwj4LML1kr1bLfguJiY21XrYfVrL1CGurfVoMKSPwRdmzt1LwBtVyR"
                    , newAccountDeriv = Just 0
                    , newAccountKeys = [xPub1]
                    , newAccountMnemonic = Nothing
                    , newAccountPassword = Nothing
                    , newAccountReadOnly = False
                    , newAccountEntropy  = Nothing
                    }
                )

        , testCase "Creating two accounts with the same data should fail" $
            assertException
                (WalletException "Account already exists") $ do
                    _ <- newAccount NewAccount
                        { newAccountName = "main"
                        , newAccountType = AccountRegular
                        , newAccountMaster = Just xPrv1
                        , newAccountDeriv = Just 0
                        , newAccountKeys = [xPub1]
                        , newAccountMnemonic = Nothing
                        , newAccountPassword = Nothing
                        , newAccountReadOnly = False
                        , newAccountEntropy  = Nothing
                        }
                    newAccount NewAccount
                        { newAccountName = "main"
                        , newAccountType = AccountRegular
                        , newAccountMaster = Nothing
                        , newAccountDeriv = Just 0
                        , newAccountKeys = [xPub1]
                        , newAccountMnemonic = Nothing
                        , newAccountPassword = Nothing
                        , newAccountReadOnly = False
                        , newAccountEntropy  = Nothing
                        }
        , testCase "Invalid multisig parameters (0 of 1)" $
            assertException (WalletException "Invalid account type") $
                newAccount NewAccount
                    { newAccountName = "multisig-0-of-1"
                    , newAccountType = AccountMultisig 0 1
                    , newAccountMaster = Just xPrv1
                    , newAccountDeriv = Nothing
                    , newAccountKeys = [xPub1]
                    , newAccountMnemonic = Nothing
                    , newAccountPassword = Nothing
                    , newAccountReadOnly = False
                    , newAccountEntropy  = Nothing
                    }

        , testCase "Invalid multisig parameters (2 of 1)" $
            assertException (WalletException "Invalid account type") $
                newAccount NewAccount
                    { newAccountName = "multisig-2-of-1"
                    , newAccountType = AccountMultisig 2 1
                    , newAccountMaster = Just xPrv1
                    , newAccountDeriv = Nothing
                    , newAccountKeys = [xPub1]
                    , newAccountMnemonic = Nothing
                    , newAccountPassword = Nothing
                    , newAccountReadOnly = False
                    , newAccountEntropy  = Nothing
                    }

        , testCase "Invalid multisig parameters (15 of 16)" $
            assertException (WalletException "Invalid account type") $
                newAccount NewAccount
                    { newAccountName = "multisig-15-of-16"
                    , newAccountType = AccountMultisig 15 16
                    , newAccountMaster = Just xPrv1
                    , newAccountDeriv = Nothing
                    , newAccountKeys = [xPub1]
                    , newAccountMnemonic = Nothing
                    , newAccountPassword = Nothing
                    , newAccountReadOnly = False
                    , newAccountEntropy  = Nothing
                    }

        , testCase "To many multisig keys (3 keys for 1 of 2)" $
            assertException (WalletException "Invalid account keys") $
                newAccount NewAccount
                    { newAccountName = "multisig-1-of-2-with-3"
                    , newAccountType = AccountMultisig 1 2
                    , newAccountMaster = Just xPrv1
                    , newAccountDeriv = Nothing
                    , newAccountKeys =
                        [ xPub1
                        , "xpub661MyMwAqRbcFEPH5Aon6F7edspeu1v6a1Nw5qJgk1aX5XYg1ktBL9Azra2CKaAJ2bHXEXkeKHE3eFaCJktFiA5tSMDQDs6bi83maQtdYby"
                        , "xpub661MyMwAqRbcFtDszBWpawpg4KbNWL9qD4VdRwjd1L5cmcS8nXHWXpg9WL1Xc9Yh7HbQBwWDw37YJfc4AF3YEpvAHEBPBFQPFkUcFHnopw8"
                        ]
                    , newAccountMnemonic = Nothing
                    , newAccountPassword = Nothing
                    , newAccountReadOnly = False
                    , newAccountEntropy  = Nothing
                    }

        , testCase "Calling addAccountKeys with an empty key list should fail" $
            assertException
                (WalletException "Invalid account keys") $ do
                    res <- newAccount NewAccount
                        { newAccountName = "multisig-1-of-2-plus-empty"
                        , newAccountType = AccountMultisig 1 2
                        , newAccountMaster = Just xPrv1
                        , newAccountDeriv = Nothing
                        , newAccountKeys = [xPub1]
                        , newAccountMnemonic = Nothing
                        , newAccountPassword = Nothing
                        , newAccountReadOnly = False
                        , newAccountEntropy  = Nothing
                        }
                    addAccountKeys (fst res) []

        , testCase "Calling addAccountKeys on a non-multisig account should fail" $
            assertException
                (WalletException "The account is already complete") $ do
                    res <- newAccount NewAccount
                        { newAccountName = "regular-plus-more"
                        , newAccountType = AccountRegular
                        , newAccountMaster = Just xPrv1
                        , newAccountDeriv = Nothing
                        , newAccountKeys = [xPub1]
                        , newAccountMnemonic = Nothing
                        , newAccountPassword = Nothing
                        , newAccountReadOnly = False
                        , newAccountEntropy  = Nothing
                        }
                    addAccountKeys (fst res)
                        ["xpub661MyMwAqRbcFEPH5Aon6F7edspeu1v6a1Nw5qJgk1aX5XYg1ktBL9Azra2CKaAJ2bHXEXkeKHE3eFaCJktFiA5tSMDQDs6bi83maQtdYby"]

        , testCase "Adding keys to a complete multisig account should fail" $
            assertException
                (WalletException "The account is already complete") $ do
                    res <- newAccount NewAccount
                        { newAccountName = "regular-plus-more"
                        , newAccountType = AccountMultisig 1 2
                        , newAccountMaster = Just xPrv1
                        , newAccountDeriv = Nothing
                        , newAccountKeys =
                            [ xPub1
                            , "xpub661MyMwAqRbcFEPH5Aon6F7edspeu1v6a1Nw5qJgk1aX5XYg1ktBL9Azra2CKaAJ2bHXEXkeKHE3eFaCJktFiA5tSMDQDs6bi83maQtdYby"
                            ]
                        , newAccountMnemonic = Nothing
                        , newAccountPassword = Nothing
                        , newAccountReadOnly = False
                        , newAccountEntropy  = Nothing
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
                    , newAccountMaster = Just xPrv1
                    , newAccountDeriv = Nothing
                    , newAccountKeys = [xPub1]
                    , newAccountMnemonic = Nothing
                    , newAccountPassword = Nothing
                    , newAccountReadOnly = False
                    , newAccountEntropy  = Nothing
                    }
                accE' <- setAccountGap (fst res) 15
                setAccountGap accE' 14

        , testCase "Setting a label on a hidden address key should fail" $
            assertException (WalletException "Invalid address index 10") $ do
                res <- newAccount NewAccount
                    { newAccountName = "label-hidden"
                    , newAccountType = AccountRegular
                    , newAccountMaster = Just xPrv1
                    , newAccountDeriv = Nothing
                    , newAccountKeys = [xPub1]
                    , newAccountMnemonic = Nothing
                    , newAccountPassword = Nothing
                    , newAccountReadOnly = False
                    , newAccountEntropy  = Nothing
                    }
                setAddrLabel (fst res) 10 AddressExternal "Gym membership"

        , testCase "Setting a label on an invalid address key should fail" $
            assertException (WalletException "Invalid address index 20") $ do
                res <- newAccount NewAccount
                    { newAccountName = "label-invalid"
                    , newAccountType = AccountRegular
                    , newAccountMaster = Just xPrv1
                    , newAccountDeriv = Nothing
                    , newAccountKeys = [xPub1]
                    , newAccountMnemonic = Nothing
                    , newAccountPassword = Nothing
                    , newAccountReadOnly = False
                    , newAccountEntropy  = Nothing
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
                        , newAccountKeys = [xPub1]
                        , newAccountMnemonic = Nothing
                        , newAccountPassword = Nothing
                        , newAccountReadOnly = False
                        , newAccountEntropy  = Nothing
                        }
                    addressPrvKey (fst res) Nothing 2 AddressExternal
        , testCase "Invalid entropy (15)" $ assertException
            (WalletException "Entropy can only be 16, 20, 24, 28 or 32 bytes") $
                newAccount NewAccount
                    { newAccountName = "account"
                    , newAccountType = AccountRegular
                    , newAccountMaster = Nothing
                    , newAccountDeriv = Nothing
                    , newAccountKeys = []
                    , newAccountMnemonic = Nothing
                    , newAccountPassword = Nothing
                    , newAccountReadOnly = False
                    , newAccountEntropy  = Just 15
                    }
        , testCase "Invalid entropy (33)" $ assertException
            (WalletException "Entropy can only be 16, 20, 24, 28 or 32 bytes") $
                newAccount NewAccount
                    { newAccountName = "account"
                    , newAccountType = AccountRegular
                    , newAccountMaster = Nothing
                    , newAccountDeriv = Nothing
                    , newAccountKeys = []
                    , newAccountMnemonic = Nothing
                    , newAccountPassword = Nothing
                    , newAccountReadOnly = False
                    , newAccountEntropy  = Just 33
                    }
        ]
    , testGroup "Wallet tests"
        [ testCase "Verify address derivations" $ runUnit testDerivations
        , testCase "Verify balances" $ runUnit testBalances
        , testCase "Verify balances in conflict" $ runUnit testConflictBalances
        , testCase "Offline transactions" $ runUnit testOffline
        , testCase "Kill an offline tx by spending its coins" $ runUnit testKillOffline
        , testCase "Importing coinbase txs" $ runUnit testCoinbaseTxs
        , testCase "Offline transaction exceptions" testOfflineExceptions
        , testCase "Multisig test 1" $ runUnit testImportMultisig
        , testCase "Kill Tx" $ runUnit testKillTx
        , testCase "Delete Tx" $ runUnit testDeleteTx
        , testCase "Delete Unsigned Tx" $ runUnit testDeleteUnsignedTx
        , testCase "Notifications" $ runUnit testNotification
        , testCase "RootToAccKey" $ testRootToAccKey
        ]
    , testGroup "Dice conversion tests"
        [ testCase "Base 6 to Base 16" testDecodeBase6
        , testCase "diceToEntropy" testDiceToEntropy
        , testCase "diceToMnemonic" testDiceToMnemonic
        , testCase "Invalid dice rolls" testInvalidDiceToEntropy
        , testCase "mixEntropy" testMixEntropy
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

z :: Hash256
z = "0000000000000000000000000000000000000000000000000000000000000000"

fakeNode :: NodeBlock     -- ^ Parent
         -> [TxHash]      -- ^ Transactions
         -> Word32        -- ^ Chain
         -> Word32        -- ^ Nonce
         -> NodeBlock
fakeNode parent tids chain nonce =
    nodeBlock parent chain header
  where
    header = BlockHeader
        (blockVersion $ nodeHeader parent)
        (nodeHash parent)
        (if null tids then z else buildMerkleRoot tids)
        (nodeTimestamp parent + 600)
        (blockBits $ nodeHeader parent)
        nonce

-- -- Creates fake testing blocks
-- fakeNode :: Word32 -> BlockHash -> NodeBlock
-- fakeNode i h = BlockHeaderNode
--     { blockHeaderNodeHash = headerHash header
--     , blockHeaderNodeHeader = BlockHeader 1 z1 z2 0 0 0
--     , blockHeaderNodeHeight = i
--     , blockHeaderNodeWork = 0
--     , blockHeaderNodeMedianTimes = []
--     , blockHeaderNodeMinWork = 0
--     }
--   where
--     z1 = "0000000000000000000000000000000000000000000000000000000000000000"
--     z2 = "0000000000000000000000000000000000000000000000000000000000000000"

fakeTx :: [(TxHash, Word32)] -> [(BS.ByteString, Word64)] -> Tx
fakeTx xs ys =
    createTx 1 txi txo 0
  where
    txi = map (\(h,p) -> TxIn (OutPoint h p) (BS.pack [1]) maxBound) xs
    f = encodeOutputBS . PayPKHash . fromJust . base58ToAddr
    txo = map (\(a,v) -> TxOut v $ f a ) ys

testDerivations :: App ()
testDerivations = do
    accE <- fst <$> newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = []
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }

    unusedAddresses accE AddressExternal (ListRequest 0 3 False)
        >>= liftIO . assertEqual "Generated external addresses do not match"
            [ "1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in"
            , "1Cf66C6MVTYgMSuXrpn5W1x12RRtAa6v2x"
            , "1Guyp96E7ph4PQJoPpz1DLsash8pqjEdVN"
            ] . map (addrToBase58 . walletAddrAddress) . fst

    unusedAddresses accE AddressInternal (ListRequest 0 3 False)
        >>= liftIO . assertEqual "Generated internal addresses do not match"
            [ "1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1"
            , "1JMTxp3kbXtwHwFMNPCUc2QyjQNQ8ZZxc4"
            , "19mTjNo7m7eRVULZ47PzYvnbdXGHGnKXKi"
            ] . map (addrToBase58 . walletAddrAddress) . fst

assertBalance :: AccountId -> Word32 -> Word64 -> App ()
assertBalance ai conf b = do
    bal <- accountBalance ai conf False
    liftIO $ assertEqual ("Balance is not " ++ show b) b bal

assertBalanceOffline :: AccountId -> Word32 -> Word64 -> App ()
assertBalanceOffline ai conf b = do
    bal <- accountBalance ai conf True
    liftIO $ assertEqual ("Balance is not " ++ show b) b bal

assertAddress :: Entity Account
              -> Word32      -- Confirmations
              -> Word32      -- Address Index
              -> AddressType
              -> [(KeyIndex, BalanceInfo)]
              -> App ()
assertAddress acc conf addr addrtype b = do
    b' <- addressBalances acc addr addr addrtype conf False
    liftIO $ assertEqual "Address Balance incorrect" b b'

assertAddressOffline :: Entity Account
                     -> Word32      -- Confirmations
                     -> Word32      -- Address Index
                     -> AddressType
                     -> [(KeyIndex, BalanceInfo)]
                     -> App ()
assertAddressOffline acc conf addr addrtype b = do
    b' <- addressBalances acc addr addr addrtype conf True
    liftIO $ assertEqual "Address Balance incorrect" b b'

assertImportTx :: AccountId -> Int -> TxConfidence -> Tx -> App ()
assertImportTx ai as conf tx = do
    tx' <- testTx <$> importNetTx tx Nothing
    liftIO $ assertEqual "Transaction import failed" ([(ai, conf)], as) tx'

assertImportTxOffline :: AccountId -> Int -> TxConfidence -> Tx -> App ()
assertImportTxOffline ai as conf tx = do
    tx' <- testTx <$> importTx tx Nothing ai
    liftIO $ assertEqual "Transaction import failed" ([(ai, conf)], as) tx'

assertTxConfidence :: AccountId -> TxHash -> TxConfidence -> App ()
assertTxConfidence ai txh conf = do
    txM <- getBy $ UniqueAccTx ai txh
    case txM of
        Just tx -> do
            let conf' = walletTxConfidence $ entityVal tx
            liftIO $ assertEqual "Transaction confidence wrong" conf' conf
        Nothing -> liftIO $ assertFailure "Transaction not found"

testBalances :: App ()
testBalances = do
    accE@(Entity ai _) <- fst <$> newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = []
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    let fundingTx = fakeTx
            [ (tid1, 0) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 10000000)
            , ("1Cf66C6MVTYgMSuXrpn5W1x12RRtAa6v2x", 20000000)
            ]
    let tx1 = fakeTx
            [ (txHash fundingTx, 0)
            , (txHash fundingTx, 1)
            ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 30000000) ] -- external
        tx2 = fakeTx
            [ (txHash fundingTx, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 5000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 5000000) -- change
            ]

    assertBalance ai 0 0

    -- Import funding transaction twice. This operation should be idempotent
    assertImportTx ai 2 TxPending fundingTx
    assertImportTx ai 0 TxPending fundingTx

    spendableCoins ai 0 (const . const []) >>=
        liftIO . assertEqual "0-conf spendable coins is not 2" 2 . length
    spendableCoins ai 1 (const . const []) >>=
        liftIO . assertEqual "1-conf spendable coins is not 0" 0 . length

    assertBalance ai 0 30000000
    assertBalance ai 1 0

    assertAddress accE 0 0 AddressExternal [(0, BalanceInfo 10000000 0 1 0)]
    assertAddress accE 0 1 AddressExternal [(1, BalanceInfo 20000000 0 1 0)]
    assertAddress accE 1 0 AddressExternal [(0, BalanceInfo 0 0 0 0)]
    assertAddress accE 1 1 AddressExternal [(1, BalanceInfo 0 0 0 0)]

    assertImportTx ai 0 TxPending tx1

    assertBalance ai 0 0
    assertBalance ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 1 AddressExternal
        [(1, BalanceInfo 20000000 20000000 1 1)]
    assertAddress accE 1 0 AddressExternal
        [(0, BalanceInfo 0 0 0 0)]
    assertAddress accE 1 1 AddressExternal
        [(1, BalanceInfo 0 0 0 0)]

    -- We re-import tx1. This operation has to be idempotent with respect to
    -- balances.
    assertImportTx ai 0 TxPending tx1

    assertBalance ai 1 0
    assertBalance ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 1 AddressExternal
        [(1, BalanceInfo 20000000 20000000 1 1)]
    assertAddress accE 1 0 AddressExternal
        [(0, BalanceInfo 0 0 0 0)]
    assertAddress accE 1 1 AddressExternal
        [(1, BalanceInfo 0 0 0 0)]

    -- Importing tx2 twice. This operation has to be idempotent.
    assertImportTx ai 1 TxDead tx2
    assertImportTx ai 0 TxDead tx2

    assertBalance ai 0 0
    assertBalance ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 1 AddressExternal
        [(1, BalanceInfo 20000000 20000000 1 1)]

    -- Confirm the funding transaction at height 1
    let block1 = fakeNode genesisBlock [txHash fundingTx] 0 1
    importMerkles (BestChain [block1]) [[txHash fundingTx]] Nothing

    assertBalance ai 0 0
    assertBalance ai 1 0
    assertBalance ai 2 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 1 AddressExternal
        [(1, BalanceInfo 20000000 20000000 1 1)]
    assertAddress accE 1 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 1 1 AddressExternal
        [(1, BalanceInfo 20000000 20000000 1 1)]

    -- Confirm tx1 at height 2
    let block2 = fakeNode block1 [txHash tx1] 0 2
    importMerkles (BestChain [block2]) [[txHash tx1]] Nothing

    assertBalance ai 0 0
    assertBalance ai 1 0
    assertBalance ai 2 0
    assertBalance ai 3 0

    assertAddress accE 2 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 2 1 AddressExternal
        [(1, BalanceInfo 20000000 20000000 1 1)]
    assertAddress accE 3 0 AddressExternal
        [(0, BalanceInfo 0 0 0 0)]
    assertAddress accE 3 1 AddressExternal
        [(1, BalanceInfo 0 0 0 0)]

    -- Reorg on tx2
    let block2' = fakeNode block1 [] 1 22
        block3' = fakeNode block2 [txHash tx2] 1 33
    importMerkles (ChainReorg block1 [block2] [block2', block3']) [[], [txHash tx2]] Nothing

    getBy (UniqueAccTx ai (txHash tx1))
        >>= liftIO
        . assertEqual "Confidence is not dead" TxDead
        . walletTxConfidence . entityVal . fromJust

    getBy (UniqueAccTx ai (txHash tx2))
        >>= liftIO
        . assertEqual "Confidence is not building" TxBuilding
        . walletTxConfidence . entityVal . fromJust

    assertBalance ai 0 25000000
    assertBalance ai 1 25000000
    assertBalance ai 2 20000000
    assertBalance ai 3 20000000
    assertBalance ai 4 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 1 AddressExternal
        [(1, BalanceInfo 20000000 0 1 0)]

    assertAddress accE 3 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 3 1 AddressExternal
        [(1, BalanceInfo 20000000 0 1 0)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 5000000 0 1 0)]
    assertAddress accE 1 0 AddressInternal
        [(0, BalanceInfo 5000000 0 1 0)]
    assertAddress accE 2 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

    -- Reimporting tx2 should be idempotent and return TxBuilding
    assertImportTx ai 0 TxBuilding tx2

    accountBalance ai 0 False >>=
        liftIO . assertEqual "Balance is not 25000000" 25000000
    accountBalance ai 1 False >>=
        liftIO . assertEqual "Balance is not 25000000" 25000000
    accountBalance ai 2 False >>=
        liftIO . assertEqual "Balance is not 20000000" 20000000
    accountBalance ai 3 False >>=
        liftIO . assertEqual "Balance is not 20000000" 20000000
    accountBalance ai 4 False >>=
        liftIO . assertEqual "Balance is not 0" 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 1 AddressExternal
        [(1, BalanceInfo 20000000 0 1 0)]
    assertAddress accE 3 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 3 1 AddressExternal
        [(1, BalanceInfo 20000000 0 1 0)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 5000000 0 1 0)]
    assertAddress accE 1 0 AddressInternal
        [(0, BalanceInfo 5000000 0 1 0)]
    assertAddress accE 2 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

    -- Reorg back onto tx1
    let block3 = fakeNode block2 [] 0 3
        block4 = fakeNode block3 [] 0 4
    importMerkles (ChainReorg block1 [block2', block3'] [block2, block3, block4])
        [[txHash tx1], [], []] Nothing

    assertBalance ai 0 0
    assertBalance ai 1 0
    assertBalance ai 2 0
    assertBalance ai 3 0
    assertBalance ai 4 0
    assertBalance ai 5 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 1 AddressExternal
        [(1, BalanceInfo 20000000 20000000 1 1)]
    assertAddress accE 4 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 4 1 AddressExternal
        [(1, BalanceInfo 20000000 20000000 1 1)]
    assertAddress accE 5 0 AddressExternal
        [(0, BalanceInfo 0 0 0 0)]
    assertAddress accE 5 1 AddressExternal
        [(1, BalanceInfo 0 0 0 0)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

-- tx1, tx2 and tx3 form a chain, and tx4 is in conflict with tx1
testConflictBalances :: App ()
testConflictBalances = do
    accE@(Entity ai _) <- fst <$> newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = []
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (tid1, 4) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 20000000) ]

    -- Import first transaction
    assertImportTx ai 1 TxPending tx1

    assertBalance ai 0 10000000
    assertBalance ai 0 10000000
    assertBalance ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 0 1 0)]

    -- Import second transaction
    assertImportTx ai 1 TxPending tx2

    assertBalance ai 0 4000000
    assertBalance ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 0 1 0)]

    -- Let's confirm these two transactions
    let block1 = fakeNode genesisBlock [txHash tx1] 0 1
        block2 = fakeNode block1 [txHash tx2] 0 2
    importMerkles (BestChain [block1, block2])
        [[txHash tx1], [txHash tx2]] Nothing

    assertBalance ai 0 4000000
    assertBalance ai 1 4000000
    assertBalance ai 2 0
    assertBalance ai 3 0

    assertAddress accE 1 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 1 0 AddressInternal
        [(0, BalanceInfo 4000000 0 1 0)]
    assertAddress accE 2 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 2 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

    -- Import third transaction
    assertImportTx ai 0 TxPending tx3

    assertBalance ai 0 0
    assertBalance ai 1 0
    assertBalance ai 2 0
    assertBalance ai 3 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 4000000 1 1)]
    assertAddress accE 1 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 1 0 AddressInternal
        [(0, BalanceInfo 4000000 4000000 1 1)]
    assertAddress accE 2 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 2 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

    -- Now let's add tx4 which is in conflict with tx1
    assertImportTx ai 0 TxDead tx4

    assertBalance ai 0 0
    assertBalance ai 1 0
    assertBalance ai 2 0
    assertBalance ai 3 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 4000000 1 1)]
    assertAddress accE 1 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 1 0 AddressInternal
        [(0, BalanceInfo 4000000 4000000 1 1)]
    assertAddress accE 2 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 2 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

    -- Now we trigger a reorg that validates tx4. tx1, tx2 and tx3 should be dead
    let block1' = fakeNode genesisBlock [] 1 11
        block2' = fakeNode block1' [txHash tx4] 1 22
        block3' = fakeNode block2' [] 1 33
    importMerkles
        (ChainReorg genesisBlock [block1, block2] [block1', block2', block3'])
        [[], [txHash tx4], []] Nothing

    assertTxConfidence ai (txHash tx1) TxDead
    assertTxConfidence ai (txHash tx2) TxDead
    assertTxConfidence ai (txHash tx3) TxDead

    assertBalance ai 0 20000000
    assertBalance ai 1 20000000
    assertBalance ai 2 20000000
    assertBalance ai 3 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 20000000 0 1 0)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]
    assertAddress accE 2 0 AddressExternal
        [(0, BalanceInfo 20000000 0 1 0)]
    assertAddress accE 3 0 AddressExternal
        [(0, BalanceInfo 0 0 0 0)]

    -- Reorg back to tx1, tx2 and tx3
    let block3 = fakeNode block2 [] 0 3
        block4 = fakeNode block3 [] 0 4
    importMerkles
        (ChainReorg genesisBlock [block1', block2', block3']
         [block1, block2, block3, block4])
        [[txHash tx1], [txHash tx2], [], []] Nothing

    assertTxConfidence ai (txHash tx1) TxBuilding
    assertTxConfidence ai (txHash tx2) TxBuilding

    -- Tx3 remains dead until it is included into a block. Dead transaction are
    -- only revived upon confirmations. They are not revived if they are not
    -- confirmed even if they have no conflicts anymore.
    assertTxConfidence ai (txHash tx3) TxDead
    assertTxConfidence ai (txHash tx4) TxDead

    assertBalance ai 0 4000000
    assertBalance ai 1 4000000
    assertBalance ai 2 4000000
    assertBalance ai 3 4000000
    assertBalance ai 4 0
    assertBalance ai 5 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 0 1 0)]
    assertAddress accE 3 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 3 0 AddressInternal
        [(0, BalanceInfo 4000000 0 1 0)]
    assertAddress accE 4 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddress accE 4 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]
    assertAddress accE 5 0 AddressExternal
        [(0, BalanceInfo 0 0 0 0)]

testOffline :: App ()
testOffline = do
    accE@(Entity ai _) <- fst <$> newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = []
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (tid1, 4) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 20000000) ]

    -- Import first transaction
    assertImportTxOffline ai 1 TxOffline tx1

    assertBalance        ai 0 0
    assertBalanceOffline ai 0 10000000
    assertBalance        ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 0 0 0 0)]
    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 0 1 0)]

    -- Reimporting a transaction should me idempotent
    assertImportTxOffline ai 0 TxOffline tx1

    assertBalance        ai 0 0
    assertBalanceOffline ai 0 10000000
    assertBalance        ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 0 0 0 0)]
    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 0 1 0)]

    -- Import tx2
    assertImportTxOffline ai 1 TxOffline tx2

    assertBalanceOffline ai 0 4000000

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 0 1 0)]

    -- Import tx3
    assertImportTxOffline ai 0 TxOffline tx3

    assertBalanceOffline ai 0 0

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 4000000 1 1)]

    -- Import tx4
    assertImportTxOffline ai 0 TxOffline tx4

    assertTxConfidence ai (txHash tx1) TxDead
    assertTxConfidence ai (txHash tx2) TxDead
    assertTxConfidence ai (txHash tx3) TxDead

    assertBalanceOffline ai 0 20000000

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 20000000 0 1 0)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

    -- importTx should be idempotent
    assertImportTxOffline ai 0 TxOffline tx4

    assertBalanceOffline ai 0 20000000

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 20000000 0 1 0)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]


tid0 :: TxHash
tid0 = "0000000000000000000000000000000000000000000000000000000000000000"

testCoinbaseTxs :: App ()
testCoinbaseTxs = do
    (Entity ai _) <- fst <$> newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = []
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    let cb1 = fakeTx
            [ (tid0, 0) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 10000000) ]
        cb2 = fakeTx
            [ (tid0, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 4000000) -- change
            ]
        tx1 = fakeTx
            [ (tid1, 0) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 10000000) ]
        tx2 = fakeTx
            [ (tid1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 4000000) -- change
            ]
        block1 = fakeNode genesisBlock [txHash cb1, txHash tx1] 0 1
        block2 = fakeNode block1 [txHash cb2, txHash tx2] 0 1

    liftIO $ assertBool "Tx1 is not a coinbase tx" $ isCoinbaseTx cb1
    liftIO $ assertBool "Tx2 is not a coinbase tx" $ isCoinbaseTx cb2

    -- Here we are testing that coinbase transactions can be imported
    -- without double spending each other (they all spend the same coin 0x000...)

    assertImportTx ai 1 TxPending cb1
    assertImportTx ai 0 TxPending tx1
    importMerkles (BestChain [block1]) [[txHash cb1, txHash tx1]] Nothing
    assertTxConfidence ai (txHash cb1) TxBuilding
    assertTxConfidence ai (txHash tx1) TxBuilding

    assertImportTx ai 1 TxPending cb2 -- This is pending and not dead
    assertImportTx ai 0 TxDead tx2
    importMerkles (BestChain [block1, block2]) [[txHash cb2, txHash tx2]] Nothing
    assertTxConfidence ai (txHash cb1) TxBuilding -- This is pending and not dead
    assertTxConfidence ai (txHash tx1) TxDead
    assertTxConfidence ai (txHash cb2) TxBuilding
    assertTxConfidence ai (txHash tx2) TxBuilding

testKillOffline :: App ()
testKillOffline = do
    accE@(Entity ai _) <- fst <$> newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = []
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 2000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 3000000) -- change
            , ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 5000000) -- more change
            ]

    -- Import tx1 as a network transaction
    assertImportTx ai 1 TxPending tx1

    assertBalance        ai 0 10000000
    assertBalanceOffline ai 0 10000000
    assertBalance        ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 0 1 0)]

    -- Import tx2 as offline
    assertImportTxOffline ai 1 TxOffline tx2

    assertBalance        ai 0 10000000
    assertBalanceOffline ai 0 4000000
    assertBalance        ai 1 0
    assertBalanceOffline ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 0 1 0)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]
    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 0 1 0)]

    -- Import tx3 as offline
    assertImportTxOffline ai 0 TxOffline tx3

    assertBalance ai 0 10000000
    assertBalanceOffline ai 0 0
    assertBalance ai 1 0
    assertBalanceOffline ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 0 1 0)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]
    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 4000000 1 1)]

    -- Import tx4 as a network transaction. It should override tx2 and tx3.
    assertImportTx ai 0 TxPending tx4

    assertTxConfidence ai (txHash tx2) TxDead
    assertTxConfidence ai (txHash tx3) TxDead

    assertBalance        ai 0 8000000
    assertBalanceOffline ai 0 8000000
    assertBalance        ai 1 0
    assertBalanceOffline ai 1 0

    assertAddress accE 0 0 AddressExternal
        [(0, BalanceInfo 15000000 10000000 2 1)]
    assertAddress accE 0 0 AddressInternal
        [(0, BalanceInfo 3000000 0 1 0)]
    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 15000000 10000000 2 1)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 3000000 0 1 0)]

testOfflineExceptions :: Assertion
testOfflineExceptions = do
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external
        tx4 = fakeTx
            [ (tid1, 4) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 20000000) ]

    assertException (WalletException "Could not import offline transaction") $ do
        _ <- newAccount NewAccount
            { newAccountName = "acc1"
            , newAccountType = AccountRegular
            , newAccountDeriv = Just 0
            , newAccountMaster = Nothing
            , newAccountMnemonic = Just (cs ms)
            , newAccountPassword = Nothing
            , newAccountKeys = []
            , newAccountReadOnly = False
            , newAccountEntropy  = Nothing
            }
        Entity ai _ <- getAccount "acc1"
        assertImportTx ai 1 TxPending tx1
        importTx tx4 Nothing ai

    assertException (WalletException "Could not import offline transaction") $ do
        _ <- newAccount NewAccount
            { newAccountName = "acc1"
            , newAccountType = AccountRegular
            , newAccountDeriv = Just 0
            , newAccountMaster = Nothing
            , newAccountMnemonic = Just (cs ms)
            , newAccountPassword = Nothing
            , newAccountKeys = []
            , newAccountReadOnly = False
            , newAccountEntropy  = Nothing
            }
        Entity ai _ <- getAccount "acc1"
        assertImportTx ai 1 TxPending tx4
        assertImportTx ai 0 TxDead tx1
        assertImportTx ai 1 TxDead tx2
        importTx tx3 Nothing ai

    assertException (WalletException "Could not import offline transaction") $ do
        _ <- newAccount NewAccount
            { newAccountName = "acc1"
            , newAccountType = AccountRegular
            , newAccountDeriv = Just 0
            , newAccountMaster = Nothing
            , newAccountMnemonic = Just (cs ms)
            , newAccountPassword = Nothing
            , newAccountKeys = []
            , newAccountReadOnly = False
            , newAccountEntropy  = Nothing
            }
        Entity ai _ <- getAccount "acc1"
        assertImportTx ai 1 TxPending tx1
        importTx tx1 Nothing ai

-- This test create a multisig account with the key of testImportMultisig2
testImportMultisig :: App ()
testImportMultisig = do
    accE1@(Entity ai1 _) <- fst <$> newAccount NewAccount
        { newAccountName = "ms1"
        , newAccountType = AccountMultisig 2 2
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = ["xpub6C5bmQQw4h4DVMVydW4bhtuz4jZpUpsrvfMYdZXVVuXyePRcDhBzXufZ5sfSZqtcnXPtDCYyAAPPkuAKEtasfRo9RatgFNP4X58zM1QjjYK"]
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    accE2@(Entity ai2 _) <- fst <$> newAccount NewAccount
        { newAccountName = "ms2"
        , newAccountType = AccountMultisig 2 2
        , newAccountDeriv = Just 1
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = ["xpub6C5bmQQw4h4DSHbWsT7GDaU1CxcamCwTGRo81T2g9VewnEyb16eHwzojDPsZguGizLD3ttFynKPby7ABY4MQ3xAf5DNafj32uf84Gw48Phb"]
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    let fundingTx = createTx
            1
            [ TxIn (OutPoint tid1 0) (BS.pack [1]) maxBound ] -- dummy input
            [ TxOut 10000000 $
              encodeOutputBS $ PayScriptHash $ fromJust $
              base58ToAddr "32SupmLrYyZMSPfL4tgkuUCyvbzEyBfqxd"
            ]
            0

    importNetTx fundingTx Nothing
        >>= liftIO
        . assertEqual "Transaction import failed"
            ([(ai1, TxPending), (ai2, TxPending)], 2)
        . testTx

    -- Create a transaction which has 0 signatures in ms1
    tx1 <- fst <$> createWalletTx accE1 Nothing Nothing
        [ ( fromJust $ base58ToAddr "3AV9s2W9atAaChWdwTpRv8qvTHcV7L1zyj"
          , 5000000
          )
        ] 10000 0 False True

    liftIO $ assertEqual "Confidence is not offline" TxOffline $
        walletTxConfidence tx1

    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . assertEqual "Wrong txhash in coins" []
        . map (walletCoinHash . entityVal . inCoinDataCoin)

    txs Nothing ai1 (ListRequest 0 10 False)
        >>= liftIO
        . assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx1])
        . sort . map walletTxHash . fst

    assertBalance        ai1 0 10000000
    assertBalance        ai1 1 0
    assertBalanceOffline ai1 0 9990000

    -- Import the empty transaction in ms2
    tx2 <- head . fst <$> importTx (walletTxTx tx1) Nothing ai2

    -- This second import should be idempotent
    _ <- importTx (walletTxTx tx1) Nothing ai2

    liftIO $ assertEqual "Txid do not match"
        (walletTxHash tx1) (walletTxHash tx2)

    liftIO $ assertEqual "Confidence is not offline" TxOffline $
        walletTxConfidence tx2

    spendableCoins ai2 0 (const . const [])
        >>= liftIO
        . assertEqual "Wrong txhash in coins" []
        . map (walletCoinHash . entityVal . inCoinDataCoin)

    txs Nothing ai2 (ListRequest 0 10 False)
        >>= liftIO
        . assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx2])
        . sort . map walletTxHash . fst

    assertBalance        ai2 0 10000000
    assertBalance        ai2 1 0
    assertBalanceOffline ai2 0 9990000

    -- Sign the transaction in ms2
    tx3:_ <- fst <$> signAccountTx accE2 Nothing Nothing (walletTxHash tx2)

    liftIO $ assertEqual "Confidence is not pending" TxPending $
        walletTxConfidence tx3

    spendableCoins ai2 0 (const . const [])
        >>= liftIO
        . assertEqual "Wrong txhash in coins"
            [walletTxHash tx3, walletTxHash tx3]
        . map (walletCoinHash . entityVal . inCoinDataCoin)

    txs Nothing ai2 (ListRequest 0 10 False)
        >>= liftIO
        . assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx3])
        . sort . map walletTxHash . fst

    assertBalance        ai2 0 9990000
    assertBalance        ai2 1 0
    assertBalanceOffline ai2 0 9990000

    tx4 <- fmap (entityVal . fromJust) $
        getBy $ UniqueAccTx ai1 $ walletTxHash tx3

    liftIO $ assertEqual "Confidence is not pending" TxPending $
        walletTxConfidence tx4

    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . assertEqual "Wrong txhash in coins"
            [walletTxHash tx3, walletTxHash tx3]
        . map (walletCoinHash . entityVal . inCoinDataCoin)

    txs Nothing ai1 (ListRequest 0 10 False)
        >>= liftIO
        . assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx3])
        . sort . map walletTxHash . fst

    assertBalance        ai1 0 9990000
    assertBalance        ai1 1 0
    assertBalanceOffline ai1 0 9990000

    -- Importing the transaction should have no effect as it was globally
    -- imported already in the previous step.
    tx5 <- head . fst <$> importTx (walletTxTx tx3) Nothing ai1

    liftIO $ assertEqual "Confidence is not pending" TxPending $
        walletTxConfidence tx5

    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . assertEqual "Wrong txhash in coins"
            [walletTxHash tx5, walletTxHash tx5]
        . map (walletCoinHash . entityVal . inCoinDataCoin)

    txs Nothing ai1 (ListRequest 0 10 False)
        >>= liftIO
        . assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx5])
        . sort . map walletTxHash . fst

    assertBalance        ai1 0 9990000
    assertBalance        ai1 1 0
    assertBalanceOffline ai1 0 9990000

testDeleteTx :: App ()
testDeleteTx = do
    accE@(Entity ai _) <- fst <$> newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = []
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external

    assertImportTx ai 1 TxPending tx1
    assertImportTx ai 1 TxPending tx2
    assertImportTx ai 0 TxPending tx3

    assertBalance ai 0 0

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 4000000 1 1)]

    tx2M' <- getTx $ txHash tx2
    liftIO $ assertBool "Transaction 2 not found" $ isJust tx2M'
    deleteTx $ txHash tx2

    tx1M   <- getTx $ txHash tx1
    tx2M'' <- getTx $ txHash tx2
    tx3M   <- getTx $ txHash tx3
    liftIO $ assertEqual "Transaction 1 removed" (Just tx1) tx1M
    liftIO $ assertEqual "Transaction 2 not removed" Nothing tx2M''
    liftIO $ assertEqual "Transaction 3 not removed" Nothing tx3M

    assertBalance ai 0 10000000

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 0 1 0)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

testDeleteUnsignedTx :: App ()
testDeleteUnsignedTx = do
    accE1@(Entity ai1 _) <- fst <$> newAccount NewAccount
        { newAccountName = "ms1"
        , newAccountType = AccountMultisig 2 2
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = ["xpub6C5bmQQw4h4DVMVydW4bhtuz4jZpUpsrvfMYdZXVVuXyePRcDhBzXufZ5sfSZqtcnXPtDCYyAAPPkuAKEtasfRo9RatgFNP4X58zM1QjjYK"]
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    Entity ai2 _ <- fst <$> newAccount NewAccount
        { newAccountName = "ms2"
        , newAccountType = AccountMultisig 2 2
        , newAccountDeriv = Just 1
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = ["xpub6C5bmQQw4h4DSHbWsT7GDaU1CxcamCwTGRo81T2g9VewnEyb16eHwzojDPsZguGizLD3ttFynKPby7ABY4MQ3xAf5DNafj32uf84Gw48Phb"]
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    let fundingTx = createTx
            1
            [ TxIn (OutPoint tid1 0) (BS.pack [1]) maxBound ] -- dummy input
            [ TxOut 10000000 $
              encodeOutputBS $ PayScriptHash $ fromJust $
              base58ToAddr "32SupmLrYyZMSPfL4tgkuUCyvbzEyBfqxd"
            ]
            0

    importNetTx fundingTx Nothing
        >>= liftIO
        . assertEqual "Confidence is not pending"
            ([(ai1, TxPending), (ai2, TxPending)], 2)
        . testTx

    -- Create a transaction which has 0 signatures in ms1
    tx1 <- fst <$> createWalletTx accE1 Nothing Nothing
        [ ( fromJust $ base58ToAddr "3AV9s2W9atAaChWdwTpRv8qvTHcV7L1zyj"
          , 5000000
          )
        ] 10000 0 False True

    liftIO $ assertEqual "Confidence is not offline" TxOffline $
        walletTxConfidence tx1

    spendableCoins ai1 0 (const . const [])
        >>= liftIO
        . assertEqual "Wrong txhash in coins" []
        . map (walletCoinHash . entityVal . inCoinDataCoin)

    txs Nothing ai1 (ListRequest 0 10 False)
        >>= liftIO
        . assertEqual "Wrong txhash in tx list"
            (sort [txHash fundingTx, walletTxHash tx1])
        . sort . map walletTxHash . fst

    assertBalance        ai1 0 10000000
    assertBalance        ai1 1 0
    assertBalanceOffline ai1 0 9990000

    tx1EM <- getTx $ walletTxHash tx1
    liftIO $ assertBool "Transaction 1 not found" $ isJust tx1EM
    deleteTx $ walletTxHash tx1

    tx1M <- getTx $ walletTxHash tx1
    liftIO $ assertEqual "Transaction not removed" Nothing tx1M

testNotification :: App ()
testNotification = do
    _ <- newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = []
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external

    notifChan <- liftIO $ atomically $ newTBMChan 1000

    _ <- importNetTx tx1 (Just notifChan)
    tx1NM <- liftIO $ atomically $ readTBMChan notifChan
    liftIO $ case tx1NM of
        Just (NotifTx JsonTx{..}) ->
            assertEqual "Notif hash does not match" (txHash tx1) jsonTxHash
        _ -> assertFailure "Transaction notification is not the right type"

    _ <- importNetTx tx2 (Just notifChan)
    tx2NM <- liftIO $ atomically $ readTBMChan notifChan
    liftIO $ case tx2NM of
        Just (NotifTx JsonTx{..}) ->
            assertEqual "Notif hash does not match" (txHash tx2) jsonTxHash
        _ -> assertFailure "Transaction notification is not the right type"

    _ <- importNetTx tx3 Nothing

    let block1 = fakeNode genesisBlock [txHash tx1] 0 1
        block2 = fakeNode block1 [txHash tx2] 0 2
        best = BestChain [block1, block2]
        txs1 = [[txHash tx1], [txHash tx2]]
    importMerkles best txs1 (Just notifChan)
    b1NM <- liftIO $ atomically $ readTBMChan notifChan
    liftIO $ case b1NM of
        Just (NotifBlock JsonBlock{..}) ->
            assertEqual "Block hash does not match"
                (nodeHash block1) jsonBlockHash
        _ -> assertFailure "Block notification not the right type"
    tx1NM' <- liftIO $ atomically $ readTBMChan notifChan
    liftIO $ case tx1NM' of
        Just (NotifTx JsonTx{..}) ->
            assertEqual "Transaction list does not match" (txHash tx1)
                jsonTxHash
        _ -> assertFailure "Transaction notification not the right type"
    b2NM <- liftIO $ atomically $ readTBMChan notifChan
    liftIO $ case b2NM of
        Just (NotifBlock JsonBlock{..}) ->
            assertEqual "Block hash does not match"
                (nodeHash block2) jsonBlockHash
        _ -> assertFailure "Block notification not the right type"
    tx2NM' <- liftIO $ atomically $ readTBMChan notifChan
    liftIO $ case tx2NM' of
        Just (NotifTx JsonTx{..}) ->
            assertEqual "Transaction list does not match" (txHash tx2)
                jsonTxHash
        _ -> assertFailure "Transaction notification not the right type"


    let block2' = fakeNode block1 [txHash tx2, txHash tx3] 1 22
        txs2 = [[txHash tx2, txHash tx3]]
        reorg = ChainReorg block1 [block2] [block2']
    importMerkles reorg txs2 (Just notifChan)
    b3NM <- liftIO $ atomically $ readTBMChan notifChan
    liftIO $ case b3NM of
        Just (NotifBlock JsonBlock{..}) ->
            assertEqual "Block hash does not match"
                (nodeHash block2') jsonBlockHash
        _ -> assertFailure "Block notification not the right type"
    tx3NM2 <- liftIO $ atomically $ readTBMChan notifChan
    liftIO $ case tx3NM2 of
        Just (NotifTx JsonTx{..}) ->
            assertEqual "Transaction does not match"
                (txHash tx2)
                jsonTxHash
        _ -> assertFailure "Transaction notification not the right type"
    tx3NM3 <- liftIO $ atomically $ readTBMChan notifChan
    liftIO $ case tx3NM3 of
        Just (NotifTx JsonTx{..}) ->
            assertEqual "Transaction does not match"
                (txHash tx3)
                jsonTxHash
        _ -> assertFailure "Transaction notification not the right type"


testKillTx :: App ()
testKillTx = do
    accE@(Entity ai _) <- fst <$> newAccount NewAccount
        { newAccountName = "acc1"
        , newAccountType = AccountRegular
        , newAccountDeriv = Just 0
        , newAccountMaster = Nothing
        , newAccountMnemonic = Just (cs ms)
        , newAccountPassword = Nothing
        , newAccountKeys = []
        , newAccountReadOnly = False
        , newAccountEntropy  = Nothing
        }
    let tx1 = fakeTx
            [ (tid1, 4) ]
            [ ("1BThGRupK6Ah44sfCtsg2QkoEDJA58d8in", 10000000) ]
        tx2 = fakeTx
            [ (txHash tx1, 0) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 6000000) -- external
            , ("1JGvK2MYQ3wwxMdYeyf7Eg1HeVJuEq3AT1", 4000000) -- change
            ]
        tx3 = fakeTx
            [ (txHash tx2, 1) ]
            [ ("1MchgrtQEUgV1f7Nqe1vEzvdmBzJHz8zrY", 4000000) ] -- external

    assertImportTx ai 1 TxPending tx1
    assertImportTx ai 1 TxPending tx2
    assertImportTx ai 0 TxPending tx3

    assertBalanceOffline ai 0 0

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 4000000 1 1)]

    killTxs Nothing [txHash tx2]

    assertBalanceOffline ai 0 10000000

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 0 1 0)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

    -- Killing a transaction should be idempotent
    killTxs Nothing [txHash tx2]

    assertBalanceOffline ai 0 10000000

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 0 1 0)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

    killTxs Nothing [txHash tx3]

    assertBalanceOffline ai 0 10000000

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 0 1 0)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 0 0 0 0)]

    reviveTx Nothing tx2

    assertBalanceOffline ai 0 4000000

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 0 1 0)]

    -- Reviving a transaction should be idempotent
    reviveTx Nothing tx2

    assertBalanceOffline ai 0 4000000

    assertAddressOffline accE 0 0 AddressExternal
        [(0, BalanceInfo 10000000 10000000 1 1)]
    assertAddressOffline accE 0 0 AddressInternal
        [(0, BalanceInfo 4000000 0 1 0)]

testTx :: ([WalletTx], [WalletAddr])
       -> ([(AccountId, TxConfidence)], Int)
testTx (txls, addrs) = (map f txls, length addrs)
  where
    f tx = (walletTxAccount tx, walletTxConfidence tx)

testDecodeBase6 :: Assertion
testDecodeBase6 = do
    assertEqual "Unit 1" (decodeBase6 BS.empty) $ Just BS.empty
    assertEqual "Unit 2" (decodeBase6 "6") $ decodeHex "00"
    assertEqual "Unit 3" (decodeBase6 "666") $ decodeHex "00"
    assertEqual "Unit 4" (decodeBase6 "661") $ decodeHex "01"
    assertEqual "Unit 5" (decodeBase6 "6615") $ decodeHex "0B"
    assertEqual "Unit 6" (decodeBase6 "6645") $ decodeHex "1D"
    assertEqual "Unit 7" (decodeBase6 "66456666") $ decodeHex "92D0"
    assertEqual "Unit 8" (decodeBase6 "111111111111111111111111111111111") $ decodeHex "07E65FDC244B0133333333"
    assertEqual "Unit 9" (decodeBase6 "55555555555555555555555555555555") $ decodeHex "06954FE21E3E80FFFFFFFF"
    assertEqual "Unit 10" (decodeBase6 "161254362643213454433626115643626632163246612666332415423213664") $ decodeHex "0140F8D002341BDF377F1723C9EB6C7ACFF134581C"

testDiceToEntropy :: Assertion
testDiceToEntropy = do
    assertEqual "Unit 1" (diceToEntropy "666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666") $ Right $ BS.replicate 32 0x00
    assertEqual "Unit 2" (diceToEntropy "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111") $ Right $ fromJust $ decodeHex "302582058C61D13F1F9AA61CB6B5982DC3D9A42B333333333333333333333333"
    assertEqual "Unit 3" (diceToEntropy "666655555555555555555544444444444444444444444333333333333333333322222222222222222111111111111111111") $ Right $ fromJust $ decodeHex "002F8D57547E01B124FE849EE71CB96CA91478A542F7D4AA833EFAF5255F3333"
    assertEqual "Unit 4" (diceToEntropy "615243524162543244414631524314243526152432442413461523424314523615243251625434236413615423162365223") $ Right $ fromJust $ decodeHex "0CC66852D7580358E47819E37CDAF115E00364724346D83D49E59F094DB4972F"

-- These test cases have been generated with haskoin and are provided here
-- for regression
testDiceToMnemonic :: Assertion
testDiceToMnemonic = do
    assertEqual "Unit 1" (diceToMnemonic "666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666") $ Right "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art"
    assertEqual "Unit 2" (diceToMnemonic "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111") $ Right "coral clown lift boat brown panel lazy feel bronze remember gravity fortune diesel spirit proud grid creek office smoke grid creek office smoke interest"

testInvalidDiceToEntropy :: Assertion
testInvalidDiceToEntropy = do
    assertEqual "Invalid empty dice roll"
        (Left "99 dice rolls are required")
        (diceToEntropy "")
    assertEqual "Dice roll too short"
        (Left "99 dice rolls are required")
        (diceToEntropy "666")
    assertEqual "Dice roll too short (98 rolls)"
        (Left "99 dice rolls are required")
        (diceToEntropy "66666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666")
    assertEqual "Dice roll too short (100 rolls)"
        (Left "99 dice rolls are required")
        (diceToEntropy "6666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666")
    assertEqual "Invalid dice roll digit 7"
        (Left "Could not decode base6")
        (diceToEntropy "666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666667")
    assertEqual "Invalid dice roll digit 0"
        (Left "Could not decode base6")
        (diceToEntropy "666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666660")

testMixEntropy :: Assertion
testMixEntropy = do
    assertEqual "Unit 1"
        (BS.pack [0x00])
        (mixEntropy (BS.pack [0x00]) (BS.pack [0x00]))
    assertEqual "Unit 2"
        (BS.pack [0xff])
        (mixEntropy (BS.pack [0x00]) (BS.pack [0xff]))
    assertEqual "Unit 3"
        (BS.pack [0xff])
        (mixEntropy (BS.pack [0xff]) (BS.pack [0x00]))
    assertEqual "Unit 4"
        (BS.pack [0x00])
        (mixEntropy (BS.pack [0xff]) (BS.pack [0xff]))
    assertEqual "Unit 5"
        (BS.pack [0xff])
        (mixEntropy (BS.pack [0xaa]) (BS.pack [0x55]))
    assertEqual "Unit 6"
        (BS.pack [0xff, 0xff])
        (mixEntropy (BS.pack [0x55, 0xaa]) (BS.pack [0xaa, 0x55]))
    assertEqual "Unit 7"
        (BS.pack [0xa9, 0xda])
        (mixEntropy (BS.pack [0x7a, 0x54]) (BS.pack [0xd3, 0x8e]))

testRootToAccKey :: Assertion
testRootToAccKey = do
    let root = "xprv9s21ZrQH143K3rhWwd1RSvWM64Z2a5ZzT5RHZ7pPC4DikEGW9AWTLzuGfX8C117bfhargkgKSm3PTSyM74AnkLfJo8iHb4hoRmJWZ5AH1C6"
        prv0 = "xprv9y6FMtt3EKVvDoX3mRaFrSXGevn6MkDbuCsXD4d4bA7xuSeSTZL3QCVFN7u1dGf7af1utDqwGjXSKVsPbcy4s5dVRJ4iFpQseVf4ZgHZuMP"
        prv1 = "xprv9y6FMtt3EKVvGsRWXUXbLkyFWhjL5NA1ZSRwqB7swZzzmb6Tg9sjz7M5EavfWWBzLxSzA5AHxZswUCwV1KUxhyKefw8UBsWT34HuDisuDg7"
        prv2 = "xprv9y6FMtt3EKVvLtvKwuA8H4AFB2Vz4o7T15y4GDJ3FjY2vxafMdDLBuCkG65HDW8nMQs3FVRr3SqMQ1X5LHsdDd6VzxuwLmPcz7SP1TafMyN"
        acc0 = "xpub6C5bmQQw4h4DSHbWsT7GDaU1CxcamCwTGRo81T2g9VewnEyb16eHwzojDPsZguGizLD3ttFynKPby7ABY4MQ3xAf5DNafj32uf84Gw48Phb"
        acc1 = "xpub6C5bmQQw4h4DVMVydW4bhtuz4jZpUpsrvfMYdZXVVuXyePRcDhBzXufZ5sfSZqtcnXPtDCYyAAPPkuAKEtasfRo9RatgFNP4X58zM1QjjYK"
        acc2 = "xpub6C5bmQQw4h4DZNzo3vh8eC6yj4LUUFqJNJtf4bhep551okuouAXajhXE7QZqGNLJigrRWairbTKeKZ5LsLcSNbmzZGarY1JJcjFDi8JxFPC"
        custom = "xprvA1kxrV2ViHBViVSwasXMsWutXtNR8N3L6w88KT7Z6FodeCVtg2qsrgnqcvLkMpoNq4tSnEuNwFaKtSTaAH2U15qipAPWpT7g1PG6ecZfED8"

    assertEqual "Unit 1" prv0 $ rootToAccKey root 0
    assertEqual "Unit 2" prv0 $ rootToAccKey prv0 0
    assertEqual "Unit 3" custom $ rootToAccKey custom 0
    assertEqual "Unit 4" [prv0, prv1, prv2] $
        rootToAccKeys root [acc0, acc1, acc2]
    assertEqual "Unit 5" [prv0] $
        rootToAccKeys prv0 [acc0, acc1, acc2]
    assertEqual "Unit 6" [custom] $
        rootToAccKeys custom [acc0, acc1, acc2]

