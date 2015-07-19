{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Types
( KeyRingName
, AccountName

-- JSON Types
, JsonKeyRing(..)
, JsonAccount(..)
, JsonAddr(..)
, JsonCoin(..)
, JsonTx(..)
, JsonWithKeyRing(..)
, JsonWithAccount(..)
, JsonWithAddr(..)

-- Request Types
, WalletRequest(..)
, PageRequest(..)
, validPageRequest
, NewKeyRing(..)
, NewAccount(..)
, SetAccountGap(..)
, OfflineTxData(..)
, CoinSignData(..)
, TxAction(..)
, AddressLabel(..)
, NodeAction(..)
, AccountType(..)
, AddressType(..)
, addrTypeIndex
, TxType(..)
, AddrTxType(..)
, TxConfidence(..)
, AddressInfo(..)
, BalanceInfo(..)

-- Response Types
, WalletResponse(..)
, TxCompleteRes(..)
, AddrTx(..)
, PageRes(..)
, RescanRes(..)

-- Helper Types
, WalletException(..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Exception (Exception)

import Text.Read (readMaybe)
import Data.Int (Int64)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Maybe (maybeToList, isJust, fromJust)
import Data.Char (toLower)
import Data.Word (Word32, Word64)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import Data.Aeson.Types 
    ( Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject
    )
import Data.Aeson.TH (deriveJSON)
import Data.Aeson
    ( Value (..)
    , FromJSON
    , ToJSON
    , withObject
    , (.=), (.:), (.:?), (.!=)
    , object
    , parseJSON
    , toJSON
    )

import Database.Persist.Class
    ( PersistField
    , toPersistValue
    , fromPersistValue 
    )
import Database.Persist.Types (PersistValue(..))
import Database.Persist.Sql (PersistFieldSql, SqlType(..), sqlType)

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Node
import Network.Haskoin.Util

import Network.Haskoin.Wallet.Types.DeriveJSON

type KeyRingName = Text
type AccountName = Text

-- TODO: Add NFData instances for all those types

{- Request Types -}

data TxType
    = TxIncoming
    | TxOutgoing
    | TxSelf
    deriving (Eq, Show, Read)

$(deriveJSON (dropSumLabels 2 0 "") ''TxType)

data AddrTxType
    = AddrTxIncoming
    | AddrTxOutgoing
    | AddrTxChange
    deriving (Eq, Show, Read)

$(deriveJSON (dropSumLabels 6 0 "") ''AddrTxType)

data TxConfidence
    = TxOffline
    | TxDead 
    | TxPending
    | TxBuilding
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 2) ''TxConfidence)

data AddressInfo = AddressInfo
    { addressInfoAddress :: !Address
    , addressInfoValue   :: !(Maybe Word64)
    , addressInfoIsLocal :: !Bool
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''AddressInfo)

data BalanceInfo = BalanceInfo
    { balanceInfoInBalance   :: !Word64
    , balanceInfoOutBalance  :: !Word64
    , balanceInfoCoins       :: !Int
    , balanceInfoSpentCoins  :: !Int
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''BalanceInfo)

data NewKeyRing = NewKeyRing
    { newKeyRingKeyRingName :: !KeyRingName
    , newKeyRingPassphrase  :: !(Maybe Text)
    , newKeyRingMnemonic    :: !(Maybe Text)
    } deriving (Eq, Read, Show)

$(deriveJSON (dropFieldLabel 10) ''NewKeyRing)

data AccountType
    = AccountRegular
        { accountTypeRead         :: !Bool }
    | AccountMultisig 
        { accountTypeRead         :: !Bool
        , accountTypeRequiredSigs :: !Int 
        , accountTypeTotalKeys    :: !Int
        }
    deriving (Eq, Show, Read)

instance ToJSON AccountType where
    toJSON accType = case accType of
        AccountRegular r -> object
            [ "type"         .= String "regular"
            , "readonly"     .= r
            ]
        AccountMultisig r m n -> object
            [ "type"         .= String "multisig"
            , "readonly"     .= r
            , "requiredsigs" .= m
            , "totalkeys"    .= n
            ]

instance FromJSON AccountType where
    parseJSON = withObject "AccountType" $ \o -> 
        o .: "type" >>= \t -> case (t :: Text) of
            "regular"  -> AccountRegular <$> o .: "readonly"
            "multisig" -> AccountMultisig <$> o .: "readonly"
                                          <*> o .: "requiredsigs"
                                          <*> o .: "totalkeys"
            _ -> mzero

data NewAccount = NewAccount
    { newAccountAccountName  :: !AccountName
    , newAccountType         :: !AccountType
    , newAccountKeys         :: ![XPubKey]
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''NewAccount)

data SetAccountGap = SetAccountGap { getAccountGap :: !Word32 }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''SetAccountGap)

data PageRequest = PageRequest
    { pageNum     :: !Word32
    , pageLen     :: !Word32
    , pageReverse :: !Bool
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 0) ''PageRequest)

validPageRequest :: PageRequest -> Bool
validPageRequest PageRequest{..} = pageNum >= 1 && pageLen >= 1

data CoinSignData = CoinSignData 
    { coinSignOutPoint     :: !OutPoint 
    , coinSignScriptOutput :: !ScriptOutput
    , coinSignDeriv        :: !SoftPath
    }
    deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 8) ''CoinSignData)

data OfflineTxData = OfflineTxData
    { offlineTxDataTx       :: !Tx
    , offlineTxDataCoinData :: ![CoinSignData]
    }

$(deriveJSON (dropFieldLabel 13) ''OfflineTxData)

data TxAction
    = CreateTx 
        { accTxActionRecipients :: ![(Address, Word64)] 
        , accTxActionFee        :: !Word64
        , accTxActionMinConf    :: !Word32
        , accTxActionRcptFee    :: !Bool
        , accTxActionSign       :: !Bool
        }
    | ImportTx 
        { accTxActionTx :: !Tx }
    | SignTx 
        { accTxActionHash :: !TxHash }
    deriving (Eq, Show)

instance ToJSON TxAction where
    toJSON (CreateTx recipients fee minConf rcptFee sign) = object $
        [ "type" .= ("createtx" :: Text)
        , "recipients" .= recipients
        , "fee"  .= fee
        , "minconf" .= minConf
        , "sign" .= sign
        ] ++ [ "rcptfee" .= True | rcptFee ]
    toJSON (ImportTx tx) = object
        [ "type" .= ("importtx" :: Text)
        , "tx" .= tx
        ]
    toJSON (SignTx txid) = object
        [ "type" .= ("signtx" :: Text)
        , "hash" .= txid
        ]

instance FromJSON TxAction where
    parseJSON = withObject "TxAction" $ \o -> do
        t <- o .: "type"
        case (t :: Text) of
            "createtx" -> do
                recipients <- o .: "recipients"
                fee <- o .: "fee"
                minConf <- o .: "minconf"
                sign <- o .: "sign"
                rcptFee <- o .:? "rcptfee" .!= False
                return (CreateTx recipients fee minConf rcptFee sign)
            "importtx" -> do
                tx <- o .: "tx"
                return (ImportTx tx)
            "signtx" -> do
                txid <- o .: "hash"
                return (SignTx txid)
            _ -> mzero

data AddressLabel = AddressLabel { addressLabelLabel :: !Text }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 12) ''AddressLabel)

data NodeAction 
    = NodeActionRescan { nodeActionTimestamp :: !(Maybe Word32) }
    | NodeActionStatus
    deriving (Eq, Show, Read)

instance ToJSON NodeAction where
    toJSON na = case na of
        NodeActionRescan tM -> object $
            ("type" .= String "rescan") : (("timestamp" .=) <$> maybeToList tM)
        NodeActionStatus -> object [ "type" .= String "status" ]

instance FromJSON NodeAction where
    parseJSON = withObject "NodeAction" $ \o -> do
        String t <- o .: "type"
        case t of
            "rescan" -> NodeActionRescan <$> o .:? "timestamp"
            "status" -> return NodeActionStatus
            _ -> mzero

data AddressType
    = AddressInternal
    | AddressExternal
    deriving (Eq, Show, Read)

$(deriveJSON (dropSumLabels 7 0 "") ''AddressType)

addrTypeIndex :: AddressType -> KeyIndex
addrTypeIndex AddressExternal = 0
addrTypeIndex AddressInternal = 1

data WalletRequest
    = GetKeyRingsR
    | GetKeyRingR !KeyRingName
    | PostKeyRingsR !NewKeyRing
    | GetAccountsR !KeyRingName
    | PostAccountsR !KeyRingName !NewAccount
    | GetAccountR !KeyRingName !AccountName
    | PostAccountKeysR !KeyRingName !AccountName ![XPubKey]
    | PostAccountGapR !KeyRingName !AccountName !SetAccountGap
    | GetAddressesR !KeyRingName !AccountName 
        !AddressType !Word32 !Bool !PageRequest
    | GetAddressesUnusedR !KeyRingName !AccountName !AddressType
    | GetAddressR !KeyRingName !AccountName !KeyIndex !AddressType
        !Word32 !Bool
    | PutAddressR !KeyRingName !AccountName !KeyIndex !AddressType !AddressLabel
    | GetTxsR !KeyRingName !AccountName !PageRequest
    | GetAddrTxsR !KeyRingName !AccountName !KeyIndex !AddressType !PageRequest
    | PostTxsR !KeyRingName !AccountName !TxAction
    | GetTxR !KeyRingName !AccountName !TxHash
    | GetOfflineTxR !KeyRingName !AccountName !TxHash
    | PostOfflineTxR !KeyRingName !AccountName !Tx ![CoinSignData]
    | GetBalanceR !KeyRingName !AccountName !Word32 !Bool
    | PostNodeR !NodeAction

-- TODO: Set omitEmptyContents on aeson-0.9
$(deriveJSON 
    defaultOptions 
        { constructorTagModifier = map toLower . init 
        , sumEncoding = defaultTaggedObject
            { tagFieldName      = "method"
            , contentsFieldName = "request"
            }
        }
    ''WalletRequest
 )

{- JSON Types -}

data JsonKeyRing = JsonKeyRing
    { jsonKeyRingName     :: !Text
    , jsonKeyRingMaster   :: !(Maybe XPrvKey) 
    , jsonKeyRingMnemonic :: !(Maybe Mnemonic) 
    , jsonKeyRingCreated  :: !UTCTime
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''JsonKeyRing)

data JsonWithKeyRing a = JsonWithKeyRing
    { withKeyRingKeyRing :: !JsonKeyRing
    , withKeyRingData    :: !a
    }

$(deriveJSON (dropFieldLabel 11) ''JsonWithKeyRing)

data JsonAccount = JsonAccount
    { jsonAccountName       :: !Text 
    , jsonAccountType       :: !AccountType 
    , jsonAccountDerivation :: !(Maybe HardPath)
    , jsonAccountKeys       :: ![XPubKey]
    , jsonAccountGap        :: !Word32
    , jsonAccountCreated    :: !UTCTime
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''JsonAccount)

data JsonWithAccount a = JsonWithAccount
    { withAccountKeyRing :: !JsonKeyRing
    , withAccountAccount :: !JsonAccount
    , withAccountData    :: !a
    }

$(deriveJSON (dropFieldLabel 11) ''JsonWithAccount)

data JsonAddr = JsonAddr
    { jsonAddrAddress        :: !Address
    , jsonAddrIndex          :: !KeyIndex
    , jsonAddrType           :: !AddressType
    , jsonAddrLabel          :: !Text
    , jsonAddrFullDerivation :: !(Maybe DerivPath)
    , jsonAddrDerivation     :: !SoftPath
    , jsonAddrRedeem         :: !(Maybe ScriptOutput)
    , jsonAddrKey            :: !(Maybe PubKeyC)
    , jsonAddrCreated        :: !UTCTime
    -- Optional Balance
    , jsonAddrBalance        :: !(Maybe BalanceInfo)
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 8) ''JsonAddr)

data JsonWithAddr a = JsonWithAddr
    { withAddrKeyRing :: !JsonKeyRing
    , withAddrAccount :: !JsonAccount
    , withAddrAddress :: !JsonAddr
    , withAddrData    :: !a
    }

$(deriveJSON (dropFieldLabel 8) ''JsonWithAddr)

data JsonTx = JsonTx
    { jsonTxHash            :: !TxHash 
    , jsonTxNosigHash       :: !TxHash 
    , jsonTxType            :: !TxType 
    , jsonTxInValue         :: !Word64
    , jsonTxOutValue        :: !Word64
    , jsonTxValue           :: !Int64
    , jsonTxInputs          :: ![AddressInfo]
    , jsonTxOutputs         :: ![AddressInfo]
    , jsonTxChange          :: ![AddressInfo]
    , jsonTxTx              :: !Tx
    , jsonTxIsCoinbase      :: !Bool
    , jsonTxConfidence      :: !TxConfidence 
    , jsonTxConfirmedBy     :: !(Maybe BlockHash)
    , jsonTxConfirmedHeight :: !(Maybe Word32)
    , jsonTxConfirmedDate   :: !(Maybe Timestamp)
    , jsonTxCreated         :: !UTCTime
    -- Optional confirmation
    , jsonTxConfirmations   :: !(Maybe Word32)
    } 
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 6) ''JsonTx)

data JsonCoin = JsonCoin
    { jsonCoinHash       :: !TxHash
    , jsonCoinPos        :: !Word32
    , jsonCoinValue      :: !Word64
    , jsonCoinScript     :: !ScriptOutput
    , jsonCoinCreated    :: !UTCTime
    -- Optional Tx
    , jsonCoinTx         :: !(Maybe JsonTx)
    -- Optional Address
    , jsonCoinAddress    :: !(Maybe JsonAddr)
    -- Optional spender
    , jsonCoinSpendingTx :: !(Maybe JsonTx)
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 8) ''JsonCoin)

{- Response Types -}

data AddrTx = AddrTx
    { addrTxTx      :: !JsonTx
    , addrTxBalance :: !BalanceInfo
    }

$(deriveJSON (dropFieldLabel 6) ''AddrTx)

data TxCompleteRes = TxCompleteRes
    { txCompleteTx       :: !Tx 
    , txCompleteComplete :: !Bool
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''TxCompleteRes)

data PageRes a = PageRes
    { pageResPage    :: ![a]
    , pageResMaxPage :: !Word32
    }

$(deriveJSON (dropFieldLabel 7) ''PageRes)

data RescanRes = RescanRes { rescanTimestamp :: !Word32 }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 6) ''RescanRes)

data WalletResponse a
    = ResponseError { responseError  :: !Text }
    | ResponseValid { responseResult :: !(Maybe a)  }
    deriving (Eq, Show)

$(deriveJSON (dropSumLabels 8 8 "status" ) ''WalletResponse)

{- Helper Types -}

data WalletException = WalletException String
    deriving (Eq, Read, Show, Typeable)

instance Exception WalletException

{- Persistent Instances -}

instance PersistField XPrvKey where
    toPersistValue = PersistByteString . C.pack . xPrvExport
    fromPersistValue (PersistByteString bs) = 
        maybeToEither "Invalid Persistent XPrvKey" $ xPrvImport $ C.unpack bs
    fromPersistValue _ = Left "Invalid Persistent XPrvKey"

instance PersistFieldSql XPrvKey where
    sqlType _ = SqlString

instance PersistField XPubKey where
    toPersistValue = PersistByteString . C.pack . xPubExport
    fromPersistValue (PersistByteString bs) = 
        maybeToEither "Invalid Persistent XPubKey" $ xPubImport $ C.unpack bs
    fromPersistValue _ = Left "Invalid Persistent XPubKey"

instance PersistFieldSql XPubKey where
    sqlType _ = SqlString

instance PersistField DerivPath where
    toPersistValue = PersistByteString . C.pack . show
    fromPersistValue (PersistByteString bs) = 
        maybeToEither "Invalid Persistent DerivPath" $ parsePath $ C.unpack bs
    fromPersistValue _ = Left "Invalid Persistent DerivPath"

instance PersistFieldSql DerivPath where
    sqlType _ = SqlString

instance PersistField HardPath where
    toPersistValue = PersistByteString . C.pack . show
    fromPersistValue (PersistByteString bs) = 
        maybeToEither "Invalid Persistent HardPath" $ parseHard $ C.unpack bs
    fromPersistValue _ = Left "Invalid Persistent HardPath"

instance PersistFieldSql HardPath where
    sqlType _ = SqlString

instance PersistField SoftPath where
    toPersistValue = PersistByteString . C.pack . show
    fromPersistValue (PersistByteString bs) = 
        maybeToEither "Invalid Persistent SoftPath" $ parseSoft $ C.unpack bs
    fromPersistValue _ = Left "Invalid Persistent SoftPath"

instance PersistFieldSql SoftPath where
    sqlType _ = SqlString

instance PersistField AccountType where
    toPersistValue ts = PersistByteString $ case ts of
        AccountRegular r -> C.pack $ unwords $ 
            "regular" : [ "read-only" | r ]
        AccountMultisig r m n -> C.pack $ unwords $ 
            "multisig" : show m : "of" : show n : [ "read-only" | r ]

    fromPersistValue (PersistByteString t) = case words $ C.unpack t of
        ("regular" : r) -> maybeToEither err $ 
            AccountRegular <$> checkRead r
        ("multisig" : m : "of" : n : r) -> maybeToEither err $
            AccountMultisig <$> checkRead r <*> readMaybe m <*> readMaybe n
        _ -> Left err
      where
        checkRead [ "read-only" ] = return True
        checkRead [] = return False
        checkRead _  = Nothing
        err = "Invalid Persistent AccountType"

    fromPersistValue _ = Left "Invalid Persistent AccountType"

instance PersistFieldSql AccountType where
    sqlType _ = SqlString

instance PersistField AddressType where
    toPersistValue ts = PersistInt64 $ case ts of
        AddressExternal -> 0
        AddressInternal -> 1

    fromPersistValue (PersistInt64 t) = case t of
        0 -> return AddressExternal
        1 -> return AddressInternal
        _ -> Left "Invalid Persistent AddressType"

    fromPersistValue _ = Left "Invalid Persistent AddressType"

instance PersistFieldSql AddressType where
    sqlType _ = SqlInt64

instance PersistField TxType where
    toPersistValue ts = PersistByteString $ case ts of
        TxIncoming -> "incoming"
        TxOutgoing -> "outgoing"
        TxSelf     -> "self"

    fromPersistValue (PersistByteString t) = case t of
        "incoming" -> return TxIncoming
        "outgoing" -> return TxOutgoing
        "self"     -> return TxSelf
        _ -> Left "Invalid Persistent TxType"

    fromPersistValue _ = Left "Invalid Persistent TxType"

instance PersistFieldSql TxType where
    sqlType _ = SqlString

instance PersistField AddrTxType where
    toPersistValue ts = PersistByteString $ case ts of
        AddrTxIncoming -> "incoming"
        AddrTxOutgoing -> "outgoing"
        AddrTxChange   -> "change"

    fromPersistValue (PersistByteString t) = case t of
        "incoming" -> return AddrTxIncoming
        "outgoing" -> return AddrTxOutgoing
        "change"   -> return AddrTxChange
        _ -> Left "Invalid Persistent AddrTxType"

    fromPersistValue _ = Left "Invalid Persistent AddrTxType"

instance PersistFieldSql AddrTxType where
    sqlType _ = SqlString

instance PersistField Address where
    toPersistValue = PersistByteString . C.pack . addrToBase58
    fromPersistValue (PersistByteString a) =
        maybeToEither "Invalid Persistent Address" . base58ToAddr $ C.unpack a
    fromPersistValue _ = Left "Invalid Persistent Address"

instance PersistFieldSql Address where
    sqlType _ = SqlString

-- TODO: Change the AddressInfo instance ?
instance PersistField AddressInfo where
    toPersistValue val = PersistByteString $ C.pack $ show val

    fromPersistValue (PersistByteString bs) = 
        maybeToEither "Invalid Persistent AddressInfo" $ readMaybe (C.unpack bs)
    fromPersistValue _ = Left "Invalid Persistent Address"

instance PersistFieldSql AddressInfo where
    sqlType _ = SqlString

instance PersistField BloomFilter where
    toPersistValue = PersistByteString . encode'
    fromPersistValue (PersistByteString bs) = 
        maybeToEither "Invalid Persistent BloomFilter" $ decodeToMaybe bs
    fromPersistValue _ = Left "Invalid Persistent BloomFilter"

instance PersistFieldSql BloomFilter where
    sqlType _ = SqlBlob

instance PersistField BlockHash where
    toPersistValue = PersistByteString . stringToBS . encodeBlockHashLE
    fromPersistValue (PersistByteString h) =
        maybeToEither "Invalid Persistent BlockHash" $
            decodeBlockHashLE $ bsToString h
    fromPersistValue _ = Left "Invalid Persistent BlockHash"

instance PersistFieldSql BlockHash where
    sqlType _ = SqlString

instance PersistField TxHash where
    toPersistValue = PersistByteString . stringToBS . encodeTxHashLE
    fromPersistValue (PersistByteString h) =
        maybeToEither "Invalid Persistent TxHash" $ 
            decodeTxHashLE $ bsToString h
    fromPersistValue _ = Left "Invalid Persistent TxHash"

instance PersistFieldSql TxHash where
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
        _ -> Left "Invalid Persistent TxConfidence"
    fromPersistValue _ = Left "Invalid Persistent TxConfidence"
        
instance PersistFieldSql TxConfidence where
    sqlType _ = SqlString

instance PersistField Tx where
    toPersistValue = PersistByteString . encode'
    fromPersistValue (PersistByteString bs) = 
        maybeToEither "Invalid Persistent Tx" $ decodeToMaybe bs
    fromPersistValue _ = Left "Invalid Persistent Tx"

instance PersistFieldSql Tx where
    sqlType _ = SqlBlob

instance PersistField PubKeyC where
    toPersistValue = PersistByteString . stringToBS . bsToHex . encode'
    fromPersistValue (PersistByteString t) =
        maybeToEither "Invalid Persistent PubKeyC" $
            decodeToMaybe =<< hexToBS (bsToString t)
    fromPersistValue _ = Left "Invalid Persistent PubKeyC"

instance PersistFieldSql PubKeyC where
    sqlType _ = SqlString

instance PersistField ScriptOutput where
    toPersistValue = 
        PersistByteString . stringToBS . bsToHex . encodeOutputBS
    fromPersistValue (PersistByteString t) =
        maybeToEither "Invalid Persistent ScriptOutput" $
            (eitherToMaybe . decodeOutputBS) =<< hexToBS (bsToString t)
    fromPersistValue _ = Left "Invalid Persistent ScriptOutput"

instance PersistFieldSql ScriptOutput where
    sqlType _ = SqlString

