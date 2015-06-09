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
, JsonAddrTx(..)

-- Request Types
, WalletRequest(..)
, PageRequest(..)
, validPageRequest
, BalanceType(..)
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
, TxConfidence(..)
, CoinStatus(..)

-- Response Types
, WalletResponse(..)
, MnemonicRes(..)
, TxHashConfidenceRes(..)
, TxConfidenceRes(..)
, TxCompleteRes(..)
, PageRes(..)
, RescanRes(..)
, TxRes(..)
, BalanceRes(..)

-- Helper Types
, WalletException(..)
) where

import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Control.Exception (Exception)

import Text.Read (readMaybe)
import Data.Int (Int64)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Maybe (maybeToList)
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

data CoinStatus
    = CoinUnspent
    | CoinLocked
    | CoinSpent
    deriving (Eq, Show, Read)

$(deriveJSON (dropSumLabels 4 0 "") ''CoinStatus)

data TxConfidence
    = TxOffline
    | TxDead 
    | TxPending
    | TxBuilding
    | TxExternal -- Used for input coins of transactions outside the wallet
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 2) ''TxConfidence)

data BalanceType
    = BalanceOffline
    | BalancePending
    | BalanceHeight Word32
    deriving (Eq, Show, Read)
 
data NewKeyRing = NewKeyRing
    { newKeyRingKeyRingName :: !KeyRingName
    , newKeyRingPassphrase  :: !(Maybe Text)
    , newKeyRingMnemonic    :: !(Maybe Text)
    } deriving (Eq, Read, Show)

$(deriveJSON (dropFieldLabel 10) ''NewKeyRing)

data AccountType
    = AccountRegular
    | AccountMultisig
    | AccountRead
    | AccountReadMultisig
    deriving (Eq, Show, Read)

$(deriveJSON (dropSumLabels 7 0 "") ''AccountType)

data NewAccount = NewAccount
    { newAccountAccountName  :: !AccountName
    , newAccountType         :: !AccountType
    , newAccountKeys         :: ![XPubKey]
    , newAccountRequiredSigs :: !(Maybe Int)
    , newAccountTotalKeys    :: !(Maybe Int)
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''NewAccount)

data SetAccountGap = SetAccountGap { getAccountGap :: !Int }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''SetAccountGap)

data PageRequest = PageRequest
    { pageNum     :: !Int
    , pageLen     :: !Int
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
        , accTxActionRcptFee    :: !Bool
        , accTxActionMinConf    :: !Word32
        , accTxActionSign       :: !Bool
        }
    | ImportTx 
        { accTxActionTx :: !Tx }
    | SignTx 
        { accTxActionHash :: !TxHash }
    | SignOfflineTx
        { accTxActionTx           :: !Tx
        , accTxActionCoinSignData :: ![CoinSignData]
        }
    deriving (Eq, Show)

instance ToJSON TxAction where
    toJSON (CreateTx recipients fee rcptFee minConf sign) = object $
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
    toJSON (SignOfflineTx tx signData) = object
        [ "type" .= ("signofflinetx" :: Text)
        , "tx" .= tx
        , "coinsigndata" .= signData
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
                return (CreateTx recipients fee rcptFee minConf sign)
            "importtx" -> do
                tx <- o .: "tx"
                return (ImportTx tx)
            "signtx" -> do
                txid <- o .: "hash"
                return (SignTx txid)
            "signofflinetx" -> do
                tx <- o .: "tx"
                signData <- o .: "coinsigndata"
                return (SignOfflineTx tx signData)
            _ -> mzero

data AddressLabel = AddressLabel { addressLabelLabel :: !Text }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 12) ''AddressLabel)

data NodeAction = Rescan { nodeActionTimestamp :: !(Maybe Word32) }
    deriving (Eq, Show, Read)

instance ToJSON NodeAction where
    toJSON (Rescan tM) = object $
        ("type" .= String "rescan") : (("timestamp" .=) <$> maybeToList tM)

instance FromJSON NodeAction where
    parseJSON = withObject "NodeAction" $ \o -> do
        String t <- o .: "type"
        case t of
            "rescan" -> Rescan <$> o .:? "timestamp"
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
    | GetAddressesR !KeyRingName !AccountName !AddressType !PageRequest
    | GetAddressesUnusedR !KeyRingName !AccountName !AddressType
    | GetAddressR !KeyRingName !AccountName !KeyIndex !AddressType
    | PutAddressR !KeyRingName !AccountName !KeyIndex !AddressType !AddressLabel
    | GetTxsR !KeyRingName !AccountName !PageRequest
    | GetAddrTxsR !KeyRingName !AccountName !KeyIndex !AddressType !PageRequest
    | PostTxsR !KeyRingName !AccountName !TxAction
    | GetTxR !KeyRingName !AccountName !TxHash
    | GetOfflineTxR !KeyRingName !AccountName !TxHash
    | GetBalanceR !KeyRingName !AccountName !Word32
    | GetOfflineBalanceR !KeyRingName !AccountName
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
    { jsonKeyRingName    :: !Text
    , jsonKeyRingMaster  :: !XPrvKey
    , jsonKeyRingCreated :: !UTCTime
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''JsonKeyRing)

data JsonAccount = JsonAccount
    { jsonAccountName         :: !Text 
    , jsonAccountKeyRingName  :: !Text 
    , jsonAccountType         :: !AccountType 
    , jsonAccountDerivation   :: !(Maybe HardPath)
    , jsonAccountKeys         :: ![XPubKey]
    , jsonAccountRequiredSigs :: !(Maybe Int) 
    , jsonAccountTotalKeys    :: !(Maybe Int)
    , jsonAccountGap          :: !Int
    , jsonAccountCreated      :: !UTCTime
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''JsonAccount)

data JsonAddr = JsonAddr
    { jsonAddrKeyRingName        :: !Text 
    , jsonAddrAccountName        :: !Text
    , jsonAddrAddress            :: !Address
    , jsonAddrIndex              :: !KeyIndex
    , jsonAddrType               :: !AddressType
    , jsonAddrLabel              :: !Text
    , jsonAddrRootDerivation     :: !(Maybe DerivPath)
    , jsonAddrDerivation         :: !SoftPath
    , jsonAddrRedeem             :: !(Maybe ScriptOutput)
    , jsonAddrKey                :: !(Maybe PubKeyC)
    , jsonAddrInBalance          :: !Word64
    , jsonAddrOutBalance         :: !Word64
    , jsonAddrInOfflineBalance   :: !Word64
    , jsonAddrOutOfflineBalance  :: !Word64
    , jsonAddrFundingTxs         :: !Int
    , jsonAddrSpendingTxs        :: !Int
    , jsonAddrFundingOfflineTxs  :: !Int
    , jsonAddrSpendingOfflineTxs :: !Int
    , jsonAddrCreated            :: !UTCTime
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 8) ''JsonAddr)

data JsonCoin = JsonCoin
    { jsonCoinKeyRingName     :: !Text
    , jsonCoinAccountName     :: !Text
    , jsonCoinHash            :: !TxHash
    , jsonCoinPos             :: !Word32
    , jsonCoinValue           :: !Word64
    , jsonCoinScript          :: !(Maybe ScriptOutput)
    , jsonCoinRedeem          :: !(Maybe ScriptOutput)
    , jsonCoinRootDerivation  :: !(Maybe DerivPath)
    , jsonCoinDerivation      :: !(Maybe SoftPath)
    , jsonCoinKey             :: !(Maybe PubKeyC)
    , jsonCoinAddress         :: !(Maybe Address)
    , jsonCoinAddressType     :: !(Maybe AddressType)
    , jsonCoinStatus          :: !CoinStatus 
    , jsonCoinSpentBy         :: !(Maybe TxHash)
    , jsonCoinIsCoinbase      :: !Bool
    , jsonCoinConfidence      :: !TxConfidence
    , jsonCoinConfirmations   :: !Int
    , jsonCoinConfirmedBy     :: !(Maybe BlockHash)
    , jsonCoinConfirmedHeight :: !(Maybe Word32)
    , jsonCoinConfirmedDate   :: !(Maybe Timestamp)
    , jsonCoinCreated         :: !UTCTime
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 8) ''JsonCoin)

data JsonTx = JsonTx
    { jsonTxKeyRingName     :: !Text 
    , jsonTxAccountName     :: !Text 
    , jsonTxHash            :: !TxHash 
    , jsonTxNosigHash       :: !TxHash 
    , jsonTxType            :: !TxType 
    , jsonTxInValue         :: !Word64
    , jsonTxOutValue        :: !Word64
    , jsonTxValue           :: !Int64
    , jsonTxFrom            :: ![Address]
    , jsonTxTo              :: ![Address]
    , jsonTxChange          :: ![Address]
    , jsonTxTx              :: !Tx
    , jsonTxIsCoinbase      :: !Bool
    , jsonTxConfidence      :: !TxConfidence 
    , jsonTxConfirmations   :: !Int
    , jsonTxConfirmedBy     :: !(Maybe BlockHash)
    , jsonTxConfirmedHeight :: !(Maybe Word32)
    , jsonTxConfirmedDate   :: !(Maybe Timestamp)
    , jsonTxCreated         :: !UTCTime
    } 
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 6) ''JsonTx)

data JsonAddrTx = JsonAddrTx
    { jsonAddrTxAddressIndex    :: !KeyIndex
    , jsonAddrTxAddressType     :: !AddressType
    , jsonAddrTxAddress         :: !Address
    , jsonAddrTxKeyRingName     :: !Text 
    , jsonAddrTxAccountName     :: !Text 
    , jsonAddrTxHash            :: !TxHash 
    , jsonAddrTxNosigHash       :: !TxHash 
    , jsonAddrTxTxType          :: !TxType 
    , jsonAddrTxInValue         :: !Word64
    , jsonAddrTxOutValue        :: !Word64
    , jsonAddrTxValue           :: !Int64
    , jsonAddrTxFrom            :: ![Address]
    , jsonAddrTxTo              :: ![Address]
    , jsonAddrTxChange          :: ![Address]
    , jsonAddrTxTx              :: !Tx
    , jsonAddrTxIsCoinbase      :: !Bool
    , jsonAddrTxConfidence      :: !TxConfidence 
    , jsonAddrTxConfirmations   :: !Int
    , jsonAddrTxConfirmedBy     :: !(Maybe BlockHash)
    , jsonAddrTxConfirmedHeight :: !(Maybe Word32)
    , jsonAddrTxConfirmedDate   :: !(Maybe Timestamp)
    , jsonAddrTxCreated         :: !UTCTime
    } 
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''JsonAddrTx)
 
{- Response Types -}

data MnemonicRes = MnemonicRes { resMnemonic :: !Mnemonic }
    deriving (Eq, Read, Show)

$(deriveJSON (dropFieldLabel 3) ''MnemonicRes)

data TxHashConfidenceRes = TxHashConfidenceRes 
    { txHashConfidenceTxHash   :: !TxHash 
    , txHashConfidenceComplete :: !TxConfidence
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 16) ''TxHashConfidenceRes)

data TxConfidenceRes = TxConfidenceRes 
    { txConfidenceTx       :: !Tx 
    , txConfidenceComplete :: !TxConfidence
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 12) ''TxConfidenceRes)

data TxCompleteRes = TxCompleteRes
    { txCompleteTx       :: !Tx 
    , txCompleteComplete :: !Bool
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''TxCompleteRes)

data PageRes a = PageRes
    { addrPageAddresses :: ![a]
    , addrPageMaxPage   :: !Int
    }

$(deriveJSON (dropFieldLabel 8) ''PageRes)

data TxRes = TxRes { resTx :: !Tx } 
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 3) ''TxRes)

data BalanceRes = BalanceRes { balanceBalance :: !Word64 } 
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 7) ''BalanceRes)

data RescanRes = RescanRes { rescanTimestamp :: !Word32 }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 6) ''RescanRes)

data WalletResponse a
    = ResponseError { responseError  :: !Text }
    | ResponseValid { responseResult :: !a  }
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
        AccountRegular      -> "regular"
        AccountMultisig     -> "multisig"
        AccountRead         -> "read"
        AccountReadMultisig -> "readmultisig"

    fromPersistValue (PersistByteString t) = case t of
        "regular"      -> return AccountRegular
        "multisig"     -> return AccountMultisig
        "read"         -> return AccountRead
        "readmultisig" -> return AccountReadMultisig
        _ -> Left "Invalid Persistent AccountType"

    fromPersistValue _ = Left "Invalid Persistent AccountType"

instance PersistFieldSql AccountType where
    sqlType _ = SqlString

instance PersistField AddressType where
    toPersistValue ts = PersistByteString $ case ts of
        AddressInternal -> "internal"
        AddressExternal -> "external"

    fromPersistValue (PersistByteString t) = case t of
        "internal" -> return AddressInternal
        "external" -> return AddressExternal
        _ -> Left "Invalid Persistent AddressType"

    fromPersistValue _ = Left "Invalid Persistent AddressType"

instance PersistFieldSql AddressType where
    sqlType _ = SqlString

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

instance PersistField Address where
    toPersistValue = PersistByteString . C.pack . addrToBase58
    fromPersistValue (PersistByteString a) =
        maybeToEither "Invalid Persistent Address" . base58ToAddr $ C.unpack a
    fromPersistValue _ = Left "Invalid Persistent Address"

instance PersistFieldSql Address where
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

instance PersistField CoinStatus where
    toPersistValue ts = PersistByteString $ case ts of
        CoinUnspent -> "unspent"
        CoinLocked  -> "locked"
        CoinSpent   -> "spent"

    fromPersistValue (PersistByteString t) = case t of
        "unspent" -> return CoinUnspent
        "locked"  -> return CoinLocked
        "spent"   -> return CoinSpent
        _         -> Left "Invalid Persistent CoinStatus"
    fromPersistValue _ = Left "Invalid Persistent CoinStatus"

instance PersistFieldSql CoinStatus where
    sqlType _ = SqlString

instance PersistField TxConfidence where
    toPersistValue tc = PersistByteString $ case tc of
        TxOffline  -> "offline"
        TxDead     -> "dead"
        TxPending  -> "pending"
        TxBuilding -> "building"
        TxExternal -> "external"

    fromPersistValue (PersistByteString t) = case t of
        "offline"  -> return TxOffline
        "dead"     -> return TxDead
        "pending"  -> return TxPending
        "building" -> return TxBuilding
        "external" -> return TxExternal
        _ -> Left "Invalid Persistent TxConfidence"
    fromPersistValue _ = Left "Invalid Persistent TxConfidence"
        
instance PersistFieldSql TxConfidence where
    sqlType _ = SqlString

instance PersistField BalanceType where
    toPersistValue bt = PersistByteString $ case bt of
        BalanceOffline  -> "offline"
        BalancePending  -> "pending"
        BalanceHeight h -> C.pack $ show h

    fromPersistValue (PersistByteString t) = case t of
        "offline"  -> return BalanceOffline
        "pending"  -> return BalancePending
        h -> maybe err (return . BalanceHeight) $ readMaybe $ C.unpack h
      where
        err = Left "Invalid Persistent BalanceType"

    fromPersistValue _ = Left "Invalid Persistent BalanceType"
        
instance PersistFieldSql BalanceType where
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

