{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Haskoin.Wallet.Store.Util 
( DbWalletGeneric(..)
, DbAccountGeneric(..)
, DbAddressGeneric(..)
, DbCoinGeneric(..)
, Unique(..)
, EntityField(..)
, AccountName
, dbGetWallet
, liftEither
, liftMaybe
, migrateAll
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Data.Time
import Data.Yaml
import Data.Maybe
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Conduit as C

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Haskoin.Wallet.Keys
import Haskoin.Wallet.Manager
import Haskoin.Wallet.TxBuilder
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

type AccountName = String

liftEither :: Monad m => Either String a -> EitherT String m a
liftEither = hoistEither

liftMaybe :: Monad m => String -> Maybe a -> EitherT String m a
liftMaybe err = liftEither . (maybeToEither err)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbWallet json
    name String
    type String
    master String 
    accDerivation Int
    created UTCTime default=CURRENT_TIME
    UniqueWalletName name
    deriving Show

DbAccount json
    name String
    derivation Int
    tree String
    key String
    extDerivation Int
    intDerivation Int
    msRequired Int Maybe
    msTotal Int Maybe
    msKeys [String] 
    created UTCTime default=CURRENT_TIME
    UniqueAccName name
    deriving Show

DbAddress json
    base58 String
    label String
    derivation Int
    tree String
    account DbAccountId
    internal Bool
    created UTCTime default=CURRENT_TIME
    UniqueAddress base58
    UniqueAddressKey account derivation
    deriving Show

DbCoin json
    txid String
    index Int
    value Int
    script String
    spent Bool
    account DbAccountId
    created UTCTime default=CURRENT_TIME
    CoinOutPoint txid index
    deriving Show
|]

dbGetWallet :: PersistUnique m => String 
         -> EitherT String m (Entity (DbWalletGeneric (PersistMonadBackend m)))
dbGetWallet name = liftMaybe walletErr =<< (getBy $ UniqueWalletName name)
    where walletErr = unwords ["dbGetWallet: Invalid wallet", name]

instance PersistStore m => PersistStore (EitherT e m) where
    type PersistMonadBackend (EitherT e m) = PersistMonadBackend m
    get = lift . get
    insert = lift . insert
    insert_ = lift . insert_
    insertMany = lift . insertMany
    insertKey k = lift . (insertKey k)
    repsert k = lift . (repsert k)
    replace k = lift . (replace k)
    delete = lift . delete

instance PersistUnique m => PersistUnique (EitherT e m) where
    getBy = lift . getBy
    deleteBy = lift . deleteBy
    insertUnique = lift . insertUnique

instance PersistQuery m => PersistQuery (EitherT e m) where
    update k = lift . (update k)
    updateGet k = lift . (updateGet k)
    updateWhere f = lift . (updateWhere f)
    deleteWhere = lift . deleteWhere
    selectSource f = (C.transPipe lift) . (selectSource f)
    selectFirst f = lift . (selectFirst f)
    selectKeys f = (C.transPipe lift) . (selectKeys f)
    count = lift . count

{- YAML templates -}

instance ToJSON OutPoint where
    toJSON (OutPoint h i) = object
        [ (T.pack "TxID") .= (bsToHex $ BS.reverse $ encode' h)
        , (T.pack "index") .= toJSON i
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
    toJSON (TxIn o s i) = object $
        [ (T.pack "OutPoint") .= toJSON o
        , (T.pack "Sequence") .= toJSON i
        , (T.pack "Raw Script") .= (bsToHex $ encodeScriptOps s)
        , (T.pack "Script") .= toJSON s
        ] ++ decoded 
        where decoded = either (const $ either (const []) f $ decodeInput s) 
                               f $ decodeScriptHash s
              f inp = [(T.pack "Decoded Script") .= toJSON inp]
              
instance ToJSON Tx where
    toJSON tx@(Tx v is os i) = object
        [ (T.pack "TxID") .= (bsToHex $ BS.reverse $ encode' $ txid tx)
        , (T.pack "Version") .= toJSON v
        , (T.pack "Inputs") .= (toJSON $ map input $ zip is [0..])
        , (T.pack "Outputs") .= (toJSON $ map output $ zip os [0..])
        , (T.pack "LockTime") .= toJSON i
        ]
        where input (x,j) = object 
                [(T.pack $ unwords ["Input", show j]) .= toJSON x]
              output (x,j) = object 
                [(T.pack $ unwords ["Output", show j]) .= toJSON x]

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
            [ (T.pack "Address Hash160") .= (bsToHex $ encode' $ runAddress a)
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
            [ (T.pack "Address Hash160") .= (bsToHex $ encode' $ runAddress a)
            , (T.pack "Address Base58") .= addrToBase58 a
            ]
        ]

instance ToJSON ScriptInput where
    toJSON (SpendPK s) = object 
        [ (T.pack "SpendPublicKey") .= object
            [ (T.pack "Signature") .= (bsToHex $ encodeSig s)
            ]
        ]
    toJSON (SpendPKHash s p) = object 
        [ (T.pack "SpendPublicKeyHash") .= object
            [ (T.pack "Signature") .= (bsToHex $ encodeSig s)
            , (T.pack "Public Key") .= (bsToHex $ encode' p)
            ]
        ]
    toJSON (SpendMulSig sigs r) = object 
        [ (T.pack "SpendMultiSig") .= object
            [ (T.pack "Required Keys (M)") .= toJSON r
            , (T.pack "Signatures") .= (toJSON $ map (bsToHex . encodeSig) sigs)
            ]
        ]

instance ToJSON ScriptHashInput where
    toJSON (ScriptHashInput s r) = object
        [ (T.pack "SpendScriptHash") .= object
            [ (T.pack "ScriptInput") .= toJSON s
            , (T.pack "RedeemScript") .= toJSON r
            ]
        ]

