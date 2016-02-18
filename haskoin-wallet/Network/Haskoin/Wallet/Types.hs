{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Types
( AccountName

-- JSON Types
, JsonAccount(..)
, JsonAddr(..)
, JsonCoin(..)
, JsonTx(..)

-- Request Types
, WalletRequest(..)
, ListRequest(..)
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
, AddressInfo(..)
, BalanceInfo(..)

-- Response Types
, WalletResponse(..)
, TxCompleteRes(..)
, ListResult(..)
, RescanRes(..)
, JsonBlock(..)
, Notif(..)

-- Helper Types
, WalletException(..)
, BTCNode(..)

-- *Helpers
, splitSelect
, splitUpdate
, splitDelete
, splitInsertMany_
, join2
, limitOffset
) where

import Control.Monad (mzero, forM, forM_, when)
import Control.Monad.Trans (MonadIO)
import Control.Exception (Exception)
import Control.DeepSeq (NFData(..))

import Data.Int (Int64)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Maybe (maybeToList)
import Data.Char (toLower)
import Data.Word (Word32, Word64)
import Data.Text (Text)
import Data.String.Conversions (cs)
import Data.List.Split (chunksOf)
import Data.Aeson.Types
    ( Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject
    )
import Data.Aeson.TH (deriveJSON)
import Data.Aeson
    ( Value (..), FromJSON, ToJSON
    , withObject, encode, decodeStrict'
    , (.=), (.:), (.:?), (.!=)
    , object, parseJSON, toJSON
    )

import Database.Persist.Types
    ( PersistValue(..)
    )
import Database.Persist.Class
    ( PersistField
    , toPersistValue
    , fromPersistValue
    )
import Database.Persist.Sql
    ( PersistFieldSql
    , SqlType(..)
    , sqlType
    )
import qualified Database.Persist as P
    ( insertMany_, PersistEntity
    , PersistEntityBackend
    )
import Database.Esqueleto
    ( SqlQuery, SqlExpr, SqlBackend
    , update, limit, offset
    , select, val
    , (||.)
    -- Reexports from Database.Persist
    , SqlPersistT, Entity(..)
    )
import qualified Database.Esqueleto as E (delete, Value)
import Database.Esqueleto.Internal.Sql (SqlSelect)

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Node
import Network.Haskoin.Node.HeaderTree
import Network.Haskoin.Util
import Network.Haskoin.Wallet.Database

type AccountName = Text

-- TODO: Add NFData instances for all those types

{- Request Types -}

data TxType
    = TxIncoming
    | TxOutgoing
    | TxSelf
    deriving (Eq, Show, Read)

instance NFData TxType where
    rnf x = x `seq` ()

$(deriveJSON (dropSumLabels 2 0 "") ''TxType)

data TxConfidence
    = TxOffline
    | TxDead
    | TxPending
    | TxBuilding
    deriving (Eq, Show, Read)

instance NFData TxConfidence where
    rnf x = x `seq` ()

$(deriveJSON (dropSumLabels 2 0 "") ''TxConfidence)

data AddressInfo = AddressInfo
    { addressInfoAddress :: !Address
    , addressInfoValue   :: !(Maybe Word64)
    , addressInfoIsLocal :: !Bool
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''AddressInfo)

instance NFData AddressInfo where
    rnf AddressInfo{..} =
        rnf addressInfoAddress `seq`
        rnf addressInfoValue `seq`
        rnf addressInfoIsLocal

data BalanceInfo = BalanceInfo
    { balanceInfoInBalance   :: !Word64
    , balanceInfoOutBalance  :: !Word64
    , balanceInfoCoins       :: !Int
    , balanceInfoSpentCoins  :: !Int
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''BalanceInfo)

instance NFData BalanceInfo where
    rnf BalanceInfo{..} =
        rnf balanceInfoInBalance `seq`
        rnf balanceInfoOutBalance `seq`
        rnf balanceInfoCoins `seq`
        rnf balanceInfoSpentCoins

data AccountType
    = AccountRegular
    | AccountMultisig
        { accountTypeRequiredSigs :: !Int
        , accountTypeTotalKeys    :: !Int
        }
    deriving (Eq, Show, Read)

instance NFData AccountType where
    rnf t = case t of
        AccountRegular -> ()
        AccountMultisig m n -> rnf m `seq` rnf n

instance ToJSON AccountType where
    toJSON accType = case accType of
        AccountRegular -> object
            [ "type"         .= String "regular" ]
        AccountMultisig m n -> object
            [ "type"         .= String "multisig"
            , "requiredsigs" .= m
            , "totalkeys"    .= n
            ]

instance FromJSON AccountType where
    parseJSON = withObject "AccountType" $ \o ->
        o .: "type" >>= \t -> case (t :: Text) of
            "regular"  -> return AccountRegular
            "multisig" -> AccountMultisig <$> o .: "requiredsigs"
                                          <*> o .: "totalkeys"
            _ -> mzero

data NewAccount = NewAccount
    { newAccountName     :: !AccountName
    , newAccountType     :: !AccountType
    , newAccountMnemonic :: !(Maybe Text)
    , newAccountMaster   :: !(Maybe XPrvKey)
    , newAccountDeriv    :: !(Maybe HardPath)
    , newAccountKeys     :: ![XPubKey]
    , newAccountReadOnly :: !Bool
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''NewAccount)

data SetAccountGap = SetAccountGap { getAccountGap :: !Word32 }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''SetAccountGap)

data ListRequest = ListRequest
    { listOffset  :: !Word32
    , listLimit   :: !Word32
    , listReverse :: !Bool
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 4) ''ListRequest)

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

instance NFData AddressType where
    rnf x = x `seq` ()

addrTypeIndex :: AddressType -> KeyIndex
addrTypeIndex AddressExternal = 0
addrTypeIndex AddressInternal = 1

data WalletRequest
    = GetAccountsR !ListRequest
    | PostAccountsR !NewAccount
    | PostAccountRenameR !AccountName !AccountName
    | GetAccountR !AccountName
    | PostAccountKeysR !AccountName ![XPubKey]
    | PostAccountGapR !AccountName !SetAccountGap
    | GetAddressesR !AccountName
        !AddressType !Word32 !Bool !ListRequest
    | GetAddressesUnusedR !AccountName !AddressType !ListRequest
    | GetAddressR !AccountName !KeyIndex !AddressType
        !Word32 !Bool
    | PutAddressR !AccountName !KeyIndex !AddressType !AddressLabel
    | PostAddressesR !AccountName !KeyIndex !AddressType
    | GetTxsR !AccountName !ListRequest
    | GetAddrTxsR !AccountName !KeyIndex !AddressType !ListRequest
    | PostTxsR !AccountName !(Maybe XPrvKey) !TxAction
    | GetTxR !AccountName !TxHash
    | GetOfflineTxR !AccountName !TxHash
    | PostOfflineTxR !AccountName !(Maybe XPrvKey) !Tx ![CoinSignData]
    | GetBalanceR !AccountName !Word32 !Bool
    | PostNodeR !NodeAction
    | DeleteTxIdR !TxHash

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

data JsonAccount = JsonAccount
    { jsonAccountName       :: !Text
    , jsonAccountType       :: !AccountType
    , jsonAccountMaster     :: !(Maybe XPrvKey)
    , jsonAccountMnemonic   :: !(Maybe Text)
    , jsonAccountDerivation :: !(Maybe HardPath)
    , jsonAccountKeys       :: ![XPubKey]
    , jsonAccountGap        :: !Word32
    , jsonAccountCreated    :: !UTCTime
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''JsonAccount)

data JsonAddr = JsonAddr
    { jsonAddrAddress        :: !Address
    , jsonAddrIndex          :: !KeyIndex
    , jsonAddrType           :: !AddressType
    , jsonAddrLabel          :: !Text
    , jsonAddrRedeem         :: !(Maybe ScriptOutput)
    , jsonAddrKey            :: !(Maybe PubKeyC)
    , jsonAddrCreated        :: !UTCTime
    -- Optional Balance
    , jsonAddrBalance        :: !(Maybe BalanceInfo)
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 8) ''JsonAddr)

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
    , jsonTxConfirmedDate   :: !(Maybe Word32)
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

data TxCompleteRes = TxCompleteRes
    { txCompleteTx       :: !Tx
    , txCompleteComplete :: !Bool
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''TxCompleteRes)

data ListResult a = ListResult
    { listResultItems :: ![a]
    , listResultTotal :: !Word32
    }

$(deriveJSON (dropFieldLabel 10) ''ListResult)

data RescanRes = RescanRes { rescanTimestamp :: !Word32 }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 6) ''RescanRes)

data WalletResponse a
    = ResponseError { responseError  :: !Text }
    | ResponseValid { responseResult :: !(Maybe a)  }
    deriving (Eq, Show)

$(deriveJSON (dropSumLabels 8 8 "status") ''WalletResponse)

data JsonBlock = JsonBlock
    { jsonBlockHash   :: BlockHash
    , jsonBlockHeight :: BlockHeight
    , jsonBlockPrev   :: BlockHash
    , jsonBlockTxs    :: [JsonTx]
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 9) ''JsonBlock)

data Notif
    = NotifBlock JsonBlock
    | NotifTx    JsonTx
    deriving (Eq, Show, Read)

$(deriveJSON (dropSumLabels 5 5 "type") ''Notif)

{- Helper Types -}

data WalletException = WalletException String
    deriving (Eq, Read, Show, Typeable)

instance Exception WalletException

data BTCNode = BTCNode { btcNodeHost :: String, btcNodePort :: Int }
    deriving (Eq, Read, Show)

$(deriveJSON (dropFieldLabel 7) ''BTCNode)

{- Persistent Instances -}

instance PersistField XPrvKey where
    toPersistValue = PersistText . cs . xPrvExport
    fromPersistValue (PersistText txt) =
        maybeToEither "Invalid Persistent XPrvKey" $ xPrvImport $ cs txt
    fromPersistValue (PersistByteString bs) =
        maybeToEither "Invalid Persistent XPrvKey" $ xPrvImport bs
    fromPersistValue _ = Left "Invalid Persistent XPrvKey"

instance PersistFieldSql XPrvKey where
    sqlType _ = SqlString

instance PersistField [XPubKey] where
    toPersistValue = PersistText . cs . encode
    fromPersistValue (PersistText txt) =
        maybeToEither "Invalid Persistent XPubKey" $ decodeStrict' $ cs txt
    fromPersistValue (PersistByteString bs) =
        maybeToEither "Invalid Persistent XPubKey" $ decodeStrict' bs
    fromPersistValue _ = Left "Invalid Persistent XPubKey"

instance PersistFieldSql [XPubKey] where
    sqlType _ = SqlString

instance PersistField DerivPath where
    toPersistValue = PersistText . cs . pathToStr
    fromPersistValue (PersistText txt) = maybeToEither
        "Invalid Persistent DerivPath" . fmap getParsedPath .
        parsePath . cs $ txt
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Invalid Persistent DerivPath" . fmap getParsedPath .
        parsePath . cs $ bs
    fromPersistValue _ = Left "Invalid Persistent DerivPath"

instance PersistFieldSql DerivPath where
    sqlType _ = SqlString

instance PersistField HardPath where
    toPersistValue = PersistText . cs . pathToStr
    fromPersistValue (PersistText txt) = maybeToEither
        "Invalid Persistent HardPath" $ parseHard $ cs txt
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Invalid Persistent HardPath" $ parseHard $ cs bs
    fromPersistValue _ = Left "Invalid Persistent HardPath"

instance PersistFieldSql HardPath where
    sqlType _ = SqlString

instance PersistField SoftPath where
    toPersistValue = PersistText . cs . pathToStr
    fromPersistValue (PersistText txt) = maybeToEither
        "Invalid Persistent SoftPath" $ parseSoft $ cs txt
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Invalid Persistent SoftPath" $ parseSoft $ cs bs
    fromPersistValue _ = Left "Invalid Persistent SoftPath"

instance PersistFieldSql SoftPath where
    sqlType _ = SqlString

instance PersistField AccountType where
    toPersistValue = PersistText . cs . encode
    fromPersistValue (PersistText txt) = maybeToEither
        "Invalid Persistent AccountType" $ decodeStrict' $ cs txt
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Invalid Persistent AccountType" $ decodeStrict' bs
    fromPersistValue _ = Left "Invalid Persistent AccountType"

instance PersistFieldSql AccountType where
    sqlType _ = SqlString

instance PersistField AddressType where
    toPersistValue ts = PersistBool $ case ts of
        AddressExternal -> True
        AddressInternal -> False

    fromPersistValue (PersistBool b) = return $
        if b then AddressExternal else AddressInternal

    fromPersistValue (PersistInt64 i) = return $ case i of
        0 -> AddressInternal
        _ -> AddressExternal

    fromPersistValue _ = Left "Invalid Persistent AddressType"

instance PersistFieldSql AddressType where
    sqlType _ = SqlBool

instance PersistField TxType where
    toPersistValue ts = PersistText $ case ts of
        TxIncoming -> "incoming"
        TxOutgoing -> "outgoing"
        TxSelf     -> "self"

    fromPersistValue (PersistText txt) = case txt of
        "incoming" -> return TxIncoming
        "outgoing" -> return TxOutgoing
        "self"     -> return TxSelf
        _ -> Left "Invalid Persistent TxType"

    fromPersistValue (PersistByteString bs) = case bs of
        "incoming" -> return TxIncoming
        "outgoing" -> return TxOutgoing
        "self"     -> return TxSelf
        _ -> Left "Invalid Persistent TxType"

    fromPersistValue _ = Left "Invalid Persistent TxType"

instance PersistFieldSql TxType where
    sqlType _ = SqlString

instance PersistField Address where
    toPersistValue = PersistText . cs . addrToBase58
    fromPersistValue (PersistText a) =
        maybeToEither "Invalid Persistent Address" $ base58ToAddr $ cs a
    fromPersistValue (PersistByteString a) =
        maybeToEither "Invalid Persistent Address" $ base58ToAddr a
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
    toPersistValue = PersistText . cs . blockHashToHex
    fromPersistValue (PersistText h) =
        maybeToEither "Could not decode BlockHash" $ hexToBlockHash $ cs h
    fromPersistValue (PersistByteString h) =
        maybeToEither "Could not decode BlockHash" $ hexToBlockHash h
    fromPersistValue _ = Left "Invalid Persistent BlockHash"

instance PersistFieldSql BlockHash where
    sqlType _ = SqlString

instance PersistField TxHash where
    toPersistValue = PersistText . cs . txHashToHex
    fromPersistValue (PersistText h) =
        maybeToEither "Invalid Persistent TxHash" $ hexToTxHash $ cs h
    fromPersistValue (PersistByteString h) =
        maybeToEither "Invalid Persistent TxHash" $ hexToTxHash h
    fromPersistValue _ = Left "Invalid Persistent TxHash"

instance PersistFieldSql TxHash where
    sqlType _ = SqlString

instance PersistField TxConfidence where
    toPersistValue tc = PersistText $ case tc of
        TxOffline  -> "offline"
        TxDead     -> "dead"
        TxPending  -> "pending"
        TxBuilding -> "building"

    fromPersistValue (PersistText txt) = case txt of
        "offline"  -> return TxOffline
        "dead"     -> return TxDead
        "pending"  -> return TxPending
        "building" -> return TxBuilding
        _ -> Left "Invalid Persistent TxConfidence"

    fromPersistValue (PersistByteString bs) = case bs of
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
    sqlType _ = SqlOther "MEDIUMBLOB"

instance PersistField PubKeyC where
    toPersistValue = PersistText . cs . encodeHex . encode'
    fromPersistValue (PersistText txt) =
        maybeToEither "Invalid Persistent PubKeyC" $
            decodeToMaybe =<< decodeHex (cs txt)
    fromPersistValue (PersistByteString bs) =
        maybeToEither "Invalid Persistent PubKeyC" $
            decodeToMaybe =<< decodeHex bs
    fromPersistValue _ = Left "Invalid Persistent PubKeyC"

instance PersistFieldSql PubKeyC where
    sqlType _ = SqlString

instance PersistField ScriptOutput where
    toPersistValue = PersistByteString . encodeOutputBS
    fromPersistValue (PersistByteString bs) =
        maybeToEither "Invalid Persistent ScriptOutput" $
            eitherToMaybe $ decodeOutputBS bs
    fromPersistValue _ = Left "Invalid Persistent ScriptOutput"

instance PersistFieldSql ScriptOutput where
    sqlType _ = SqlBlob

instance PersistField [AddressInfo] where
    toPersistValue = PersistText . cs . encode
    fromPersistValue (PersistText txt) =
        maybeToEither "Invalid Persistent AddressInfo" $ decodeStrict' $ cs txt
    fromPersistValue (PersistByteString bs) =
        maybeToEither "Invalid Persistent AddressInfo" $ decodeStrict' bs
    fromPersistValue _ = Left "Invalid Persistent AddressInfo"

instance PersistFieldSql [AddressInfo] where
    sqlType _ = SqlString

{- Helpers -}

-- Join AND expressions with OR conditions in a binary way
join2 :: [SqlExpr (E.Value Bool)] -> SqlExpr (E.Value Bool)
join2 xs = case xs of
    [] -> val False
    [x] -> x
    _ -> let (ls,rs) = splitAt (length xs `div` 2) xs
         in  join2 ls ||. join2 rs

splitSelect :: (SqlSelect a r, MonadIO m)
            => [t]
            -> ([t] -> SqlQuery a)
            -> SqlPersistT m [r]
splitSelect ts queryF =
    fmap concat $ forM vals $ select . queryF
  where
    vals = chunksOf paramLimit ts

splitUpdate :: ( MonadIO m
               , P.PersistEntity val
               , P.PersistEntityBackend val ~ SqlBackend
               )
            => [t]
            -> ([t] -> SqlExpr (Entity val) -> SqlQuery ())
            -> SqlPersistT m ()
splitUpdate ts updateF =
    forM_ vals $ update . updateF
  where
    vals = chunksOf paramLimit ts

splitDelete :: MonadIO m => [t] -> ([t] -> SqlQuery ()) -> SqlPersistT m ()
splitDelete ts deleteF =
    forM_ vals $ E.delete . deleteF
  where
    vals = chunksOf paramLimit ts

splitInsertMany_ :: ( MonadIO m
                    , P.PersistEntity val
                    , P.PersistEntityBackend val ~ SqlBackend
                    )
                 => [val] -> SqlPersistT m ()
splitInsertMany_ = mapM_ P.insertMany_ . chunksOf paramLimit

limitOffset :: Word32 -> Word32 -> SqlQuery ()
limitOffset l o = do
    when (l > 0) $ limit  $ fromIntegral l
    when (o > 0) $ offset $ fromIntegral o

