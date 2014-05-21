{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Haskoin.Wallet.Model 
( DbWalletGeneric(..)
, DbAccountGeneric(..)
, DbAddressGeneric(..)
, DbCoinGeneric(..)
, DbTxGeneric(..)
, DbTxBlobGeneric(..)
, DbWalletId
, DbAccountId
, DbAddressId
, DbCoinId
, DbTxId
, DbTxBlobId
, Unique(..)
, EntityField(..)
, AccountName
, CoinStatus(..)
, WalletException(..)
, catStatus
, dbGetWallet
, isWalletInit
, checkInit
, dbGetTxBlob
, migrateAll
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Trans (liftIO)
import Control.Monad (unless)

import Data.Typeable (Typeable)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Data.Int (Int64)
import Data.Time (UTCTime)
import Data.Yaml
    ( ToJSON, toJSON
    , object, (.=)
    )
import qualified Data.Text as T (pack)
import qualified Data.ByteString as BS (ByteString)

import Database.Persist 
    ( PersistStore
    , PersistUnique
    , PersistMonadBackend
    , Entity
    , EntityField
    , Unique
    , getBy
    )
import Database.Persist.TH 
    ( mkPersist
    , sqlSettings
    , mkMigrate
    , persistLowerCase
    , share
    )

import Network.Haskoin.Wallet.CustomFields
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util

data WalletException 
    = InitializationException String
    | AccountSetupException String
    | InvalidAccountException String
    | InvalidPageException String
    | AddressGenerationException String
    | InvalidAddressException String
    | InvalidTransactionException String
    | DoubleSpendException String
    | CoinSelectionException String
    | TransactionBuildingException String
    | TransactionSigningException String
    | ParsingException String
    | InvalidCommandException String
    deriving (Eq, Read, Show, Typeable)

instance Exception WalletException

type AccountName = String

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbWallet 
    name String
    type String
    master MasterKey 
    accIndex Int
    created UTCTime default=CURRENT_TIME
    UniqueWalletName name
    deriving Show

DbAccount 
    name String
    index Int
    tree String
    key AccPubKey
    extIndex Int
    extGap Int
    intIndex Int
    intGap Int
    msRequired Int Maybe
    msTotal Int Maybe
    msKeys [XPubKey] 
    wallet DbWalletId
    created UTCTime default=CURRENT_TIME
    UniqueAccName name
    deriving Show

DbAddress 
    base58 String
    label String
    index Int
    tree String
    account DbAccountId
    internal Bool
    created UTCTime default=CURRENT_TIME
    UniqueAddress base58
    UniqueAddressKey account index internal
    deriving Show

DbCoin 
    txid Hash256
    pos Int
    value Word64
    script Script
    rdmScript Script Maybe
    address String 
    status CoinStatus
    account DbAccountId
    orphan Bool
    created UTCTime default=CURRENT_TIME
    CoinOutPoint txid pos
    deriving Show

DbTx 
    txid Hash256
    recipients [String]
    value Int64
    account DbAccountId
    orphan Bool
    partial Bool
    created UTCTime default=CURRENT_TIME
    UniqueTx txid account
    deriving Show

DbTxBlob
    txid Hash256
    value BS.ByteString 
    created UTCTime default=CURRENT_TIME
    UniqueTxBlob txid
    deriving Show

|]

isWalletInit :: PersistUnique m => String -> m Bool
isWalletInit name = do
    entM <- getBy $ UniqueWalletName name
    return $ isJust entM

checkInit :: PersistUnique m => m ()
checkInit = do
    isInit <- isWalletInit "main"
    unless isInit $ liftIO $ throwIO $ 
        InitializationException "Wallet main is not initialized"

dbGetWallet :: (PersistUnique m, PersistMonadBackend m ~ b)
            => String -> m (Entity (DbWalletGeneric b))
dbGetWallet name = do
    entM <- getBy $ UniqueWalletName name
    case entM of
        Just ent -> return ent
        Nothing  -> liftIO $ throwIO $ InitializationException $ 
            unwords ["Wallet", name, "is not initialized"]

dbGetTxBlob :: (PersistUnique m, PersistMonadBackend m ~ b)
            => Hash256 -> m (Entity (DbTxBlobGeneric b))
dbGetTxBlob tid = do
    entM <- getBy $ UniqueTxBlob tid
    case entM of
        Just ent -> return ent
        Nothing  -> liftIO $ throwIO $ InvalidTransactionException $
            unwords ["Transaction", encodeTxid tid, "not in database"]

{- YAML templates -}

instance ToJSON OutPoint where
    toJSON (OutPoint h i) = object
        [ (T.pack "TxID") .= encodeTxid h
        , (T.pack "Index") .= toJSON i
        ]

instance ToJSON TxOut where
    toJSON (TxOut v s) = object $
        [ (T.pack "Value") .= toJSON v
        , (T.pack "Raw Script") .= (bsToHex $ encodeScriptOps s)
        , (T.pack "Script") .= toJSON s
        ] ++ scptPair 
        where scptPair = 
                either (const [])
                       (\out -> [(T.pack "Decoded Script") .= toJSON out]) 
                       (decodeOutput s)

instance ToJSON TxIn where
    toJSON (TxIn o s i) = object $ concat
        [ [ (T.pack "OutPoint") .= toJSON o
          , (T.pack "Sequence") .= toJSON i
          , (T.pack "Raw Script") .= (bsToHex $ encodeScriptOps s)
          , (T.pack "Script") .= toJSON s
          ] 
        , decoded 
        ]
        where decoded = either (const $ either (const []) f $ decodeInput s) 
                               f $ decodeScriptHash s
              f inp = [(T.pack "Decoded Script") .= toJSON inp]
              
instance ToJSON Tx where
    toJSON tx@(Tx v is os i) = object
        [ (T.pack "TxID") .= encodeTxid (txid tx)
        , (T.pack "Version") .= toJSON v
        , (T.pack "Inputs") .= (toJSON $ map input $ zip is [0..])
        , (T.pack "Outputs") .= (toJSON $ map output $ zip os [0..])
        , (T.pack "LockTime") .= toJSON i
        ]
        where input (x,j) = object 
                [(T.pack $ unwords ["Input", show (j :: Int)]) .= toJSON x]
              output (x,j) = object 
                [(T.pack $ unwords ["Output", show (j :: Int)]) .= toJSON x]

instance ToJSON Script where
    toJSON (Script ops) = toJSON $ map show ops

instance ToJSON ScriptOutput where
    toJSON (PayPK p) = object 
        [ (T.pack "PayToPublicKey") .= object
            [ (T.pack "Public Key") .= (bsToHex $ encode' p)
            ]
        ]
    toJSON (PayPKHash a) = object 
        [ (T.pack "PayToPublicKeyHash") .= object
            [ (T.pack "Address Hash160") .= (bsToHex $ encode' $ getAddrHash a)
            , (T.pack "Address Base58") .= addrToBase58 a
            ]
        ]
    toJSON (PayMulSig ks r) = object 
        [ (T.pack "PayToMultiSig") .= object
            [ (T.pack "Required Keys (M)") .= toJSON r
            , (T.pack "Public Keys") .= (toJSON $ map (bsToHex . encode') ks)
            ]
        ]
    toJSON (PayScriptHash a) = object 
        [ (T.pack "PayToScriptHash") .= object
            [ (T.pack "Address Hash160") .= (bsToHex $ encode' $ getAddrHash a)
            , (T.pack "Address Base58") .= addrToBase58 a
            ]
        ]

instance ToJSON ScriptInput where
    toJSON (SpendPK s) = object 
        [ (T.pack "SpendPublicKey") .= object
            [ (T.pack "Signature") .= toJSON s
            ]
        ]
    toJSON (SpendPKHash s p) = object 
        [ (T.pack "SpendPublicKeyHash") .= object
            [ (T.pack "Signature") .= toJSON s
            , (T.pack "Public Key") .= (bsToHex $ encode' p)
            , (T.pack "Sender Addr") .= addrToBase58 (pubKeyAddr p)
            ]
        ]
    toJSON (SpendMulSig sigs r) = object 
        [ (T.pack "SpendMultiSig") .= object
            [ (T.pack "Required Keys (M)") .= toJSON r
            , (T.pack "Signatures") .= (toJSON $ map toJSON sigs)
            ]
        ]

instance ToJSON ScriptHashInput where
    toJSON (ScriptHashInput s r) = object
        [ (T.pack "SpendScriptHash") .= object
            [ (T.pack "ScriptInput") .= toJSON s
            , (T.pack "RedeemScript") .= toJSON r
            , (T.pack "Raw Redeem Script") .= 
                (bsToHex $ encodeScriptOps $ encodeOutput r)
            , (T.pack "Sender Addr") .= (addrToBase58 $ scriptAddr  r)
            ]
        ]

instance ToJSON TxSignature where
    toJSON ts@(TxSignature _ h) = object
        [ (T.pack "Raw Sig") .= (bsToHex $ encodeSig ts)
        , (T.pack "SigHash") .= toJSON h
        ]

instance ToJSON SigHash where
    toJSON sh = case sh of
        (SigAll acp) -> object
            [ (T.pack "Type") .= T.pack "SigAll"
            , (T.pack "AnyoneCanPay") .= acp
            ]
        (SigNone acp) -> object
            [ (T.pack "Type") .= T.pack "SigNone"
            , (T.pack "AnyoneCanPay") .= acp
            ]
        (SigSingle acp) -> object
            [ (T.pack "Type") .= T.pack "SigSingle"
            , (T.pack "AnyoneCanPay") .= acp
            ]
        (SigUnknown acp v) -> object
            [ (T.pack "Type") .= T.pack "SigUnknown"
            , (T.pack "AnyoneCanPay") .= acp
            , (T.pack "Value") .= v
            ]



