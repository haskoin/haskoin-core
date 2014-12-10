{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Types
( WalletName
, AccountName
, Wallet(..)
, Account(..)
, Balance(..)
, BalanceAddress(..)
, PaymentAddress(..)
, RecipientAddress(..)
, AccTx(..)
, TxConfidence(..)
, TxSource(..)
, WalletException(..)
, SigBlob(..)
, printWallet
, printAccount
, printPaymentAddress
, printBalanceAddress
, printAccTx
, printBalance
) where

import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Control.Exception (Exception)

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import Data.Maybe (maybeToList, isJust, fromJust)
import Data.Word (Word32, Word64)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Aeson
    ( Value (Object, String)
    , FromJSON
    , ToJSON
    , withText
    , (.=), (.:), (.:?)
    , object
    , parseJSON
    , toJSON
    , encode
    , decode
    )

import Database.Persist.Class
    ( PersistField
    , toPersistValue
    , fromPersistValue 
    )
import Database.Persist.Types (PersistValue(..))
import Database.Persist.Sql (PersistFieldSql, SqlType(..), sqlType)

import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Util

type WalletName  = String
type AccountName = String

data WalletException = WalletException String
    deriving (Eq, Read, Show, Typeable)

instance Exception WalletException

data SigBlob = SigBlob
    { sigBlobData :: ![(OutPoint, ScriptOutput, Bool, KeyIndex)]
    , sigBlobTx   :: !Tx
    } deriving (Eq, Show, Read)

instance ToJSON SigBlob where
    toJSON (SigBlob dat tx) = object
        [ "data" .= dat
        , "tx"   .= tx
        ]

instance FromJSON SigBlob where
    parseJSON (Object o) = do
        dat <- o .: "data"
        tx  <- o .: "tx"
        return $ SigBlob dat tx
    parseJSON _ = mzero

data TxConfidence
    = TxOffline
    | TxDead 
    | TxPending
    | TxBuilding
    deriving (Eq, Show, Read)

instance ToJSON TxConfidence where
    toJSON conf = case conf of
        TxOffline  -> "offline"
        TxDead     -> "dead"
        TxPending  -> "pending"
        TxBuilding -> "building"

instance FromJSON TxConfidence where
    parseJSON = withText "TxConfidence" $ \t -> case t of
        "offline"  -> return TxOffline
        "dead"     -> return TxDead
        "pending"  -> return TxPending
        "building" -> return TxBuilding
        _          -> mzero
        
data TxSource
    = NetworkSource
    | WalletSource
    | UnknownSource
    deriving (Eq, Show, Read)

instance ToJSON TxSource where
    toJSON s = case s of
        NetworkSource -> "network"
        WalletSource  -> "wallet"
        UnknownSource -> "unknown"

instance FromJSON TxSource where
    parseJSON = withText "TxSource" $ \t -> case t of
        "network" -> return NetworkSource
        "wallet"  -> return WalletSource
        "unknown" -> return UnknownSource
        _         -> mzero

-- TODO: Add NFData instances for all those types
data Wallet = Wallet
    { walletName      :: !String
    , walletMasterKey :: !MasterKey
    } deriving (Eq, Show, Read)

instance ToJSON Wallet where
    toJSON (Wallet n k) = object
        [ "name"   .= n
        , "master" .= (xPrvExport $ masterKey k)
        ]

instance FromJSON Wallet where
    parseJSON (Object o) = do
        n <- o .: "name"
        m <- o .: "master" 
        let masterM = loadMasterKey =<< xPrvImport m
        maybe mzero (return . (Wallet n)) masterM
    parseJSON _ = mzero

printWallet :: Wallet -> String
printWallet (Wallet n k) = unlines
    [ unwords [ "Wallet    :", n ]
    , unwords [ "Master key:", xPrvExport $ masterKey k ]
    ]

data Account
    = RegularAccount 
        { accountWallet :: !String
        , accountName   :: !String
        , accountIndex  :: !KeyIndex
        , accountKey    :: !AccPubKey
        }
    | MultisigAccount
        { accountWallet   :: !String
        , accountName     :: !String
        , accountIndex    :: !KeyIndex
        , accountRequired :: !Int
        , accountTotal    :: !Int
        , accountKeys     :: ![XPubKey]
        }
    | ReadAccount
        { accountWallet :: !String
        , accountName   :: !String
        , accountKey    :: !AccPubKey
        }
    | ReadMSAccount
        { accountWallet   :: !String
        , accountName     :: !String
        , accountRequired :: !Int
        , accountTotal    :: !Int
        , accountKeys     :: ![XPubKey]
        }
    deriving (Eq, Show, Read)

instance ToJSON Account where
    toJSON (RegularAccount w n i k) = object
        [ "type"   .= String "regular"
        , "wallet" .= w
        , "name"   .= n
        , "index"  .= i
        , "key"    .= (xPubExport $ getAccPubKey k)
        ]
    toJSON (MultisigAccount w n i r t ks) = object
        [ "type"     .= String "multisig"
        , "wallet"   .= w
        , "name"     .= n
        , "index"    .= i
        , "required" .= r
        , "total"    .= t
        , "keys"     .= map xPubExport ks
        ]
    toJSON (ReadAccount w n k) = object
        [ "type"   .= String "read"
        , "wallet" .= w
        , "name"   .= n
        , "key"    .= (xPubExport $ getAccPubKey k)
        ]
    toJSON (ReadMSAccount w n r t ks) = object
        [ "type"     .= String "readmultisig"
        , "wallet"   .= w
        , "name"     .= n
        , "required" .= r
        , "total"    .= t
        , "keys"     .= map xPubExport ks
        ]

instance FromJSON Account where
    parseJSON (Object o) = do
        x <- o .: "type"
        w <- o .: "wallet"
        n <- o .: "name"
        case x of
            String "regular" -> do
                k <- o .: "key"
                i <- o .: "index"
                let keyM = loadPubAcc =<< xPubImport k
                maybe mzero (return . (RegularAccount w n i)) keyM
            String "multisig" -> do
                i  <- o .: "index"
                r  <- o .: "required"
                t  <- o .: "total"
                ks <- o .: "keys"
                let keysM = mapM xPubImport ks
                maybe mzero (return . (MultisigAccount w n i r t)) keysM
            String "read" -> do
                k  <- o .: "key"
                let keyM       = loadPubAcc =<< xPubImport k
                maybe mzero (return . (ReadAccount w n)) keyM
            String "readmultisig" -> do
                r  <- o .: "required"
                t  <- o .: "total"
                ks <- o .: "keys"
                let keysM      = mapM xPubImport ks
                maybe mzero (return . (ReadMSAccount w n r t)) keysM
            _ -> mzero
    parseJSON _ = mzero

printAccount :: Account -> String
printAccount a = case a of
    RegularAccount w n i k -> unlines
        [ unwords [ "Wallet :", w ]
        , unwords [ "Account:", n ]
        , unwords [ "Type   :", "Regular" ]
        , unwords [ "Tree   :", concat [ "m/",show i,"'/" ] ]
        , unwords [ "Key    :", xPubExport $ getAccPubKey k ]
        ]
    MultisigAccount w n i r t ks -> unlines $
        [ unwords [ "Wallet :", w ]
        , unwords [ "Account:", n ]
        , unwords [ "Type   :", "Multisig", show r, "of", show t ]
        , unwords [ "Tree   :", concat [ "m/",show i,"'/" ] ]
        ] ++ if null ks then [] else 
            (unwords [ "Keys   :", xPubExport $ head ks ]) : 
                (map (\x -> unwords ["        ", xPubExport x]) $ tail ks)
    ReadAccount w n k -> unlines
        [ unwords [ "Wallet :", w ]
        , unwords [ "Account:", n ]
        , unwords [ "Type   :", "Read-only" ]
        , unwords [ "Key    :", xPubExport $ getAccPubKey k ]
        ]
    ReadMSAccount w n r t ks -> unlines $
        [ unwords [ "Wallet :", w ]
        , unwords [ "Account:", n ]
        , unwords [ "Type   :", "Read-only multisig", show r, "of", show t ]
        ] ++ if null ks then [] else 
            (unwords [ "Keys   :", xPubExport $ head ks ]) : 
                (map (\x -> unwords ["        ", xPubExport x]) $ tail ks)

data Balance = BalanceConflict | Balance !Word64
    deriving (Eq, Show, Read)

instance ToJSON Balance where
    toJSON BalanceConflict = object [ "status"  .= String "conflict" ]
    toJSON (Balance b)     = object [ "status"  .= String "valid" 
                                    , "balance" .= b
                                    ]

instance FromJSON Balance where
    parseJSON (Object o) = do
        String str <- o .: "status"
        case str of
            "conflict" -> return BalanceConflict
            "valid"    -> Balance <$> o .: "balance"
            _          -> mzero
    parseJSON _ = mzero

printBalance :: Balance -> String
printBalance b = case b of
    BalanceConflict -> "Conflict"
    Balance x       -> show x

data BalanceAddress = BalanceAddress
    { balanceAddress :: !PaymentAddress
    , finalBalance   :: !Balance
    , totalReceived  :: !Balance
    , fundingTxs     :: ![TxHash]
    , spendingTxs    :: ![TxHash]
    , conflictTxs    :: ![TxHash]
    } deriving (Eq, Show, Read)

instance ToJSON BalanceAddress where
    toJSON (BalanceAddress pa fb tr ft st ct) = object
        [ "address"       .= pa
        , "finalbalance"  .= fb
        , "totalreceived" .= tr
        , "fundingtxs"    .= ft
        , "spendingtxs"   .= st
        , "conflicttxs"   .= ct
        ]

instance FromJSON BalanceAddress where
    parseJSON (Object o) = do
        pa <- o .: "address"
        fb <- o .: "finalbalance"
        tr <- o .: "totalreceived"
        ft <- o .: "fundingtxs"
        st <- o .: "spendingtxs"
        ct <- o .: "conflicttxs"
        return $ BalanceAddress pa fb tr ft st ct
    parseJSON _ = mzero

printBalanceAddress :: BalanceAddress -> String
printBalanceAddress (BalanceAddress pa fb _ ft st ct) = unwords $ concat
    [ [ printPaymentAddress pa ]
    , if null ft && null st && null ct then [] else
        [ "["
        , "Balance:", printBalance fb ++ ","
        , "TxCount:", show $ (length ft) + (length st)
        , "]" 
        ]
    ]

data PaymentAddress = PaymentAddress 
    { paymentAddress :: !Address
    , addressLabel   :: !String
    , addressIndex   :: !KeyIndex
    } deriving (Eq, Show, Read)

instance ToJSON PaymentAddress where
    toJSON (PaymentAddress a l i) = object
        [ "address"       .= addrToBase58 a
        , "label"         .= l
        , "index"         .= i
        ]

instance FromJSON PaymentAddress where
    parseJSON (Object o) = do
        a <- o .: "address"
        l <- o .: "label"
        i <- o .: "index"
        let f add = return $ PaymentAddress add l i
        maybe mzero f $ base58ToAddr a
    parseJSON _ = mzero

printPaymentAddress :: PaymentAddress -> String
printPaymentAddress (PaymentAddress a l i) = 
    unwords $ concat
        [ [ show i, ":" , addrToBase58 a ] 
        , if null l then [] else [concat ["(",l,")"]]
        ]

data RecipientAddress = RecipientAddress
    { recipientAddress :: !Address
    , recipientLabel   :: !String
    , recipientIsLocal :: !Bool
    } deriving (Eq, Show, Read)

instance ToJSON RecipientAddress where
    toJSON (RecipientAddress a l lo) = object
        [ "address" .= addrToBase58 a
        , "label"   .= l
        , "islocal" .= lo
        ]

instance FromJSON RecipientAddress where
    parseJSON (Object o) = do
        a  <- o .: "address"
        l  <- o .: "label"
        lo <- o .: "islocal"
        let f add = return $ RecipientAddress add l lo
        maybe mzero f $ base58ToAddr a
    parseJSON _ = mzero

printRecipientAddress :: RecipientAddress -> String
printRecipientAddress (RecipientAddress a l lo) = unwords $ concat
    [ ["    " ++ if lo then "<-" else "->"]
    , [ addrToBase58 a ]
    , if null l then [] else [concat ["(",l,")"]]
    ] 

data AccTx = AccTx
    { accTxHash           :: !TxHash
    , accTxRecipients     :: ![RecipientAddress]
    , accTxValue          :: !Int64
    , accTxConfidence     :: !TxConfidence
    , accIsCoinbase       :: !Bool
    , accTxConfirmations  :: !Int
    , accTx               :: !Tx
    , accReceivedDate     :: !UTCTime
    , accConfirmationDate :: !(Maybe Word32)
    } deriving (Eq, Show, Read)

instance ToJSON AccTx where
    toJSON (AccTx h as v x cb c tx rd cd) = object $ concat
        [ [ "txid"          .= h
          , "recipients"    .= as
          , "value"         .= v
          , "confidence"    .= x
          , "iscoinbase"    .= cb
          , "confirmations" .= c
          , "tx"            .= tx
          , "receiveddate"  .= (round (utcTimeToPOSIXSeconds rd) :: Word32)
          ]
        , maybeToList $ ("confirmationdate" .=) <$> cd
        ]

instance FromJSON AccTx where
    parseJSON (Object o) = do
        h  <- o .: "txid"
        as <- o .: "recipients"
        v  <- o .: "value"
        x  <- o .: "confidence"
        cb <- o .: "iscoinbase"
        c  <- o .: "confirmations"
        tx <- o .: "tx"
        rd <- o .: "receiveddate"
        cd <- o .:? "confirmationdate"
        let rDate = posixSecondsToUTCTime $ realToFrac (rd :: Word32)
        return $ AccTx h as v x cb c tx rDate cd
    parseJSON _ = mzero

printAccTx :: AccTx -> String
printAccTx (AccTx h r v ci cb co _ rd cd) = unlines $ concat
    [ [ unwords [ "Value     :", show v ]
      , unwords [ "Recipients:" ]
      ]
    , map printRecipientAddress r
    , [ unwords [ "Confidence:"
                , printConfidence ci
                , concat ["(",show co," confirmations)"] 
                ]
      , unwords [ "TxHash    :", encodeTxHashLE h ]
      ] 
    , if cb then [unwords ["Coinbase  :", "Yes"]] else []
    , [ unwords [ "Received  :", show rd ] ]
    , if isJust cd 
        then [ unwords [ "Confirmed :"
                       , show $ posixSecondsToUTCTime $ realToFrac $ fromJust cd 
                       ] 
             ] 
        else []
    ]

printConfidence :: TxConfidence -> String
printConfidence c = case c of
    TxBuilding -> "Building"
    TxPending  -> "Pending"
    TxDead     -> "Dead"
    TxOffline  -> "Offline"

persistBSErrMsg :: T.Text 
persistBSErrMsg = "Has to be a PersistByteString" 

toPersistJson :: (ToJSON a) => a -> PersistValue
toPersistJson = PersistByteString . toStrict . encode

fromPersistJson :: (FromJSON a) => T.Text -> PersistValue -> Either T.Text a
fromPersistJson msg (PersistByteString w) = 
    maybeToEither msg (decode $ toLazyBS w)
fromPersistJson _ _ = Left persistBSErrMsg

instance PersistField Address where
    toPersistValue = PersistByteString . stringToBS . addrToBase58
    fromPersistValue (PersistByteString a) = 
        maybeToEither "Not a valid Address" . base58ToAddr $ bsToString a
    fromPersistValue _ = Left persistBSErrMsg

instance PersistFieldSql Address where
    sqlType _ = SqlString

instance PersistField [Address] where
    toPersistValue = toPersistJson
    fromPersistValue = fromPersistJson "Not a valid Address list"

instance PersistFieldSql [Address] where
    sqlType _ = SqlString

instance PersistField TxHash where
    toPersistValue = PersistByteString . stringToBS . encodeTxHashLE
    fromPersistValue (PersistByteString h) =
        maybeToEither "Not a valid TxHash" (decodeTxHashLE $ bsToString h)
    fromPersistValue _ = Left persistBSErrMsg

instance PersistFieldSql TxHash where
    sqlType _ = SqlString

instance PersistField BlockHash where
    toPersistValue = PersistByteString . stringToBS . encodeBlockHashLE
    fromPersistValue (PersistByteString h) = 
        maybeToEither "Not a valid BlockHash"
            (decodeBlockHashLE $ bsToString h)
    fromPersistValue _ = Left persistBSErrMsg

instance PersistFieldSql BlockHash where
    sqlType _ = SqlString

instance PersistField Wallet where
    toPersistValue = toPersistJson
    fromPersistValue = fromPersistJson "Not a valid Wallet"

instance PersistFieldSql Wallet where
    sqlType _ = SqlString

instance PersistField Account where
    toPersistValue = toPersistJson
    fromPersistValue = fromPersistJson "Not a valid Account"

instance PersistFieldSql Account where
    sqlType _ = SqlString

instance PersistField TxConfidence where
    toPersistValue tc = PersistByteString $ case tc of
        TxOffline  -> "offline"
        TxDead     -> "dead"
        TxPending  -> "pending"
        TxBuilding -> "building"

    fromPersistValue (PersistByteString t) = case t of
        "offline"  -> return TxOffline
        "dead"     -> return TxDead
        "pending"  -> return TxPending
        "building" -> return TxBuilding
        _          -> Left "Not a valid TxConfidence"
    fromPersistValue _ = Left "Not a valid TxConfidence"
        
instance PersistFieldSql TxConfidence where
    sqlType _ = SqlString

instance PersistField TxSource where
    toPersistValue ts = PersistByteString $ case ts of
        NetworkSource -> "network"
        WalletSource  -> "wallet"
        UnknownSource -> "unknown"

    fromPersistValue (PersistByteString t) = case t of
        "network" -> return NetworkSource
        "wallet"  -> return WalletSource
        "unknown" -> return UnknownSource
        _         -> Left "Not a valid TxSource"
    fromPersistValue _ = Left "Not a valid TxSource"

instance PersistFieldSql TxSource where
    sqlType _ = SqlString

instance PersistField Coin where
    toPersistValue = toPersistJson
    fromPersistValue = fromPersistJson "Not a valid Coin"

instance PersistFieldSql Coin where
    sqlType _ = SqlString

instance PersistField OutPoint where
    toPersistValue = PersistByteString . stringToBS . bsToHex . encode'
    fromPersistValue (PersistByteString t) = 
        maybeToEither "Not a valid OutPoint" $
            decodeToMaybe =<< (hexToBS $ bsToString t)
    fromPersistValue _ = Left "Not a valid OutPoint"

instance PersistFieldSql OutPoint where
    sqlType _ = SqlString

instance PersistField Tx where
    toPersistValue = PersistByteString . encode'
    fromPersistValue (PersistByteString bs) = case txE of
        Right tx -> Right tx
        Left str -> Left $ T.pack str
      where
        txE = decodeToEither bs
    fromPersistValue _ = Left persistBSErrMsg

instance PersistFieldSql Tx where
    sqlType _ = SqlBlob

