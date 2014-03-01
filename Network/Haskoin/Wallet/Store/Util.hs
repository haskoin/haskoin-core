{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Store.Util 
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
, catStatus
, dbGetWallet
, dbGetTxBlob
, migrateAll
) where

import Control.Monad.Trans.Either (EitherT)

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

import Network.Haskoin.Wallet.Store.CoinStatus
import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Util.Instances ()

type AccountName = String

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbWallet json
    name String
    type String
    master String 
    accIndex Int
    created UTCTime default=CURRENT_TIME
    UniqueWalletName name
    deriving Show

DbAccount json
    name String
    index Int
    tree String
    key String
    extIndex Int
    extGap Int
    intIndex Int
    intGap Int
    msRequired Int Maybe
    msTotal Int Maybe
    msKeys [String] 
    wallet DbWalletId
    created UTCTime default=CURRENT_TIME
    UniqueAccName name
    deriving Show

DbAddress json
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

DbCoin json
    txid String
    pos Int
    value Int
    script String
    rdmScript String Maybe
    address String 
    status CoinStatus
    account DbAccountId
    orphan Bool
    created UTCTime default=CURRENT_TIME
    CoinOutPoint txid pos
    deriving Show

DbTx json
    txid String
    recipients [String]
    value Int
    account DbAccountId
    orphan Bool
    partial Bool
    created UTCTime default=CURRENT_TIME
    UniqueTx txid account
    deriving Show

DbTxBlob json
    txid String
    value BS.ByteString 
    created UTCTime default=CURRENT_TIME
    UniqueTxBlob txid
    deriving Show

|]

dbGetWallet :: (PersistUnique m, PersistMonadBackend m ~ b)
            => String -> EitherT String m (Entity (DbWalletGeneric b))
dbGetWallet name = liftMaybe walletErr =<< (getBy $ UniqueWalletName name)
  where 
    walletErr = unwords ["dbGetWallet: Invalid wallet", name]

dbGetTxBlob :: (PersistUnique m, PersistMonadBackend m ~ b)
            => String -> EitherT String m (Entity (DbTxBlobGeneric b))
dbGetTxBlob tid = liftMaybe txErr =<< (getBy $ UniqueTxBlob tid)
  where
    txErr = unwords ["dbGetTxBlob: Invalid txid", tid]

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



