{-# LANGUAGE DeriveGeneric #-}
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
, OfflineTxData(..)
, CoinSignData(..)
, CreateTx(..)
, SignTx(..)
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
, JsonSyncBlock(..)
, JsonBlock(..)
, Notif(..)
, BlockInfo(..)

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

import           Control.DeepSeq                        (NFData (..))
import           Control.Exception                      (Exception)
import           Control.Monad
import           Control.Monad.Trans                    (MonadIO)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString                        as BS
import           Data.Char                              (toLower)
import           Data.Int                               (Int64)
import           Data.List.Split                        (chunksOf)
import           Data.Maybe                             (maybeToList)
import qualified Data.Serialize                         as S
import           Data.String                            (fromString)
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
import           Data.Time                              (UTCTime)
import           Data.Typeable                          (Typeable)
import           Data.Word                              (Word32, Word64, Word8)
import qualified Database.Esqueleto                     as E
import           Database.Esqueleto.Internal.Sql        (SqlSelect)
import qualified Database.Persist                       as P
import           Database.Persist.Class
import           Database.Persist.Sql
import           GHC.Generics
import           Network.Haskoin.Block
import           Network.Haskoin.Crypto
import           Network.Haskoin.Network
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.Database
import           Network.Haskoin.Wallet.Types.BlockInfo

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
    deriving (Eq, Show, Read, Generic)

instance S.Serialize AddressInfo

$(deriveJSON (dropFieldLabel 11) ''AddressInfo)

instance NFData AddressInfo where
    rnf AddressInfo{..} =
        rnf addressInfoAddress `seq`
        rnf addressInfoValue `seq`
        rnf addressInfoIsLocal

data BalanceInfo = BalanceInfo
    { balanceInfoInBalance  :: !Word64
    , balanceInfoOutBalance :: !Word64
    , balanceInfoCoins      :: !Int
    , balanceInfoSpentCoins :: !Int
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
    , newAccountPassword :: !(Maybe Text)
    , newAccountEntropy  :: !(Maybe Word8)
    , newAccountMaster   :: !(Maybe XPrvKey)
    , newAccountDeriv    :: !(Maybe KeyIndex)
    , newAccountKeys     :: ![XPubKey]
    , newAccountReadOnly :: !Bool
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''NewAccount)

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

instance S.Serialize CoinSignData where
    get = do
        p <- S.get
        s <- readBS >>= \bs -> case decodeOutputBS bs of
            Right s -> return s
            _ -> error "Invalid ScriptOutput in CoinSignData"
        S.get >>= \dM -> case toSoft (dM :: DerivPath) of
            Just d -> return $ CoinSignData p s d
            _ -> error "Invalid derivation in CoinSignData"
      where
        readBS = S.get >>= \(VarInt l) -> S.getByteString $ fromIntegral l

    put (CoinSignData p s d) = do
        S.put p
        writeBS $ encodeOutputBS s
        S.put $ toGeneric d
      where
        writeBS bs = do
            S.put $ VarInt $ fromIntegral $ BS.length bs
            S.putByteString bs

data OfflineTxData = OfflineTxData
    { offlineTxDataTx       :: !Tx
    , offlineTxDataCoinData :: ![CoinSignData]
    }

$(deriveJSON (dropFieldLabel 13) ''OfflineTxData)

instance S.Serialize OfflineTxData where
    get = OfflineTxData <$> S.get <*> (replicateList =<< S.get)
      where
        replicateList (VarInt c) = replicateM (fromIntegral c) S.get

    put (OfflineTxData t ds) = do
        S.put t
        S.put $ VarInt $ fromIntegral $ length ds
        forM_ ds S.put

data CreateTx = CreateTx
    { createTxRecipients :: ![(Address, Word64)]
    , createTxFee        :: !Word64
    , createTxMinConf    :: !Word32
    , createTxRcptFee    :: !Bool
    , createTxSign       :: !Bool
    , createTxSignKey    :: !(Maybe XPrvKey)
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 8) ''CreateTx)

data SignTx = SignTx
    { signTxTxHash  :: !TxHash
    , signTxSignKey :: !(Maybe XPrvKey)
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 6) ''SignTx)

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
    = AccountReq !AccountName
    | AccountsReq !ListRequest
    | NewAccountReq !NewAccount
    | RenameAccountReq !AccountName !AccountName
    | AddPubKeysReq !AccountName ![XPubKey]
    | SetAccountGapReq !AccountName !Word32
    | AddrsReq !AccountName !AddressType !Word32 !Bool !ListRequest
    | UnusedAddrsReq !AccountName !AddressType !ListRequest
    | AddressReq !AccountName !KeyIndex !AddressType !Word32 !Bool
    | PubKeyIndexReq !AccountName !PubKeyC !AddressType
    | SetAddrLabelReq !AccountName !KeyIndex !AddressType !Text
    | GenerateAddrsReq !AccountName !KeyIndex !AddressType
    | TxsReq !AccountName !ListRequest
    | PendingTxsReq !AccountName !ListRequest
    | DeadTxsReq !AccountName !ListRequest
    | AddrTxsReq !AccountName !KeyIndex !AddressType !ListRequest
    | CreateTxReq !AccountName !CreateTx
    | ImportTxReq !AccountName !Tx
    | SignTxReq !AccountName !SignTx
    | TxReq !AccountName !TxHash
    | DeleteTxReq !TxHash
    | OfflineTxReq !AccountName !TxHash
    | SignOfflineTxReq !AccountName !(Maybe XPrvKey) !Tx ![CoinSignData]
    | BalanceReq !AccountName !Word32 !Bool
    | NodeActionReq !NodeAction
    | SyncReq !AccountName !BlockHash !ListRequest
    | SyncHeightReq !AccountName !BlockHeight !ListRequest
    | BlockInfoReq ![BlockHash]
    | StopServerReq
        deriving (Show, Eq)

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
    , jsonAccountKeys       :: ![XPubKey]
    , jsonAccountGap        :: !Word32
    , jsonAccountCreated    :: !UTCTime
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''JsonAccount)

data JsonAddr = JsonAddr
    { jsonAddrAddress :: !Address
    , jsonAddrIndex   :: !KeyIndex
    , jsonAddrType    :: !AddressType
    , jsonAddrLabel   :: !Text
    , jsonAddrRedeem  :: !(Maybe ScriptOutput)
    , jsonAddrKey     :: !(Maybe PubKeyC)
    , jsonAddrCreated :: !UTCTime
    -- Optional Balance
    , jsonAddrBalance :: !(Maybe BalanceInfo)
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
    , jsonTxAccount         :: !AccountName
    -- Optional confirmation
    , jsonTxConfirmations   :: !(Maybe Word32)
    , jsonTxBestBlock       :: !(Maybe BlockHash)
    , jsonTxBestBlockHeight :: !(Maybe BlockHeight)
    }
    deriving (Eq, Show)

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
    deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 8) ''JsonCoin)

{- Response Types -}

data TxCompleteRes = TxCompleteRes
    { txCompleteTx       :: !Tx
    , txCompleteComplete :: !Bool
    } deriving (Eq, Show)

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
    = ResponseError { responseError :: !Text }
    | ResponseValid { responseResult :: !(Maybe a)  }
    deriving (Eq, Show)

$(deriveJSON (dropSumLabels 8 8 "status") ''WalletResponse)

data JsonSyncBlock = JsonSyncBlock
    { jsonSyncBlockHash   :: !BlockHash
    , jsonSyncBlockHeight :: !BlockHeight
    , jsonSyncBlockPrev   :: !BlockHash
    , jsonSyncBlockTxs    :: ![JsonTx]
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 13) ''JsonSyncBlock)

data JsonBlock = JsonBlock
    { jsonBlockHash   :: !BlockHash
    , jsonBlockHeight :: !BlockHeight
    , jsonBlockPrev   :: !BlockHash
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 9) ''JsonBlock)

data Notif
    = NotifBlock !JsonBlock
    | NotifTx    !JsonTx
    deriving (Eq, Show)

$(deriveJSON (dropSumLabels 5 5 "type") ''Notif)

{- Helper Types -}

data WalletException = WalletException !String
    deriving (Eq, Read, Show, Typeable)

instance Exception WalletException

data BTCNode = BTCNode { btcNodeHost :: !String, btcNodePort :: !Int }
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
    toPersistValue = PersistByteString . S.encode
    fromPersistValue (PersistByteString bs) =
        case S.decode bs of
            Right x -> Right x
            Left  e -> Left  (fromString e)
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
    toPersistValue = PersistByteString . S.encode
    fromPersistValue (PersistByteString bs) =
        case S.decode bs of
            Right x -> Right x
            Left  e -> Left (fromString e)
    fromPersistValue _ = Left "Invalid Persistent Tx"

instance PersistFieldSql Tx where
    sqlType _ = SqlOther "MEDIUMBLOB"

instance PersistField PubKeyC where
    toPersistValue = PersistText . cs . encodeHex . S.encode
    fromPersistValue (PersistText txt) =
        case hex >>= S.decode of
            Right x -> Right x
            Left  e -> Left (fromString e)
      where
        hex = maybeToEither "Could not decode hex" (decodeHex (cs txt))
    fromPersistValue (PersistByteString bs) =
        case hex >>= S.decode of
            Right x -> Right x
            Left  e -> Left (fromString e)
      where
        hex = maybeToEither "Could not decode hex" (decodeHex bs)
    fromPersistValue _ = Left "Invalid Persistent PubKeyC"

instance PersistFieldSql PubKeyC where
    sqlType _ = SqlString

instance PersistField ScriptOutput where
    toPersistValue = PersistByteString . encodeOutputBS
    fromPersistValue (PersistByteString bs) =
        case decodeOutputBS bs of
            Right x -> Right x
            Left  e -> Left (fromString e)
    fromPersistValue _ = Left "Invalid Persistent ScriptOutput"

instance PersistFieldSql ScriptOutput where
    sqlType _ = SqlBlob

instance PersistField [AddressInfo] where
    toPersistValue = PersistByteString . S.encode
    fromPersistValue (PersistByteString bs) =
        case S.decode bs of
            Right x -> Right x
            Left  e -> Left (fromString e)
    fromPersistValue _ = Left "Invalid Persistent AddressInfo"

instance PersistFieldSql [AddressInfo] where
    sqlType _ = SqlOther "MEDIUMBLOB"

{- Helpers -}

-- Join AND expressions with OR conditions in a binary way
join2 :: [E.SqlExpr (E.Value Bool)] -> E.SqlExpr (E.Value Bool)
join2 xs = case xs of
    [] -> E.val False
    [x] -> x
    _ -> let (ls,rs) = splitAt (length xs `div` 2) xs
         in  join2 ls E.||. join2 rs

splitSelect :: (SqlSelect a r, MonadIO m)
            => [t]
            -> ([t] -> E.SqlQuery a)
            -> E.SqlPersistT m [r]
splitSelect ts queryF =
    fmap concat $ forM vals $ E.select . queryF
  where
    vals = chunksOf paramLimit ts

splitUpdate :: ( MonadIO m
               , P.PersistEntity val
               , P.PersistEntityBackend val ~ E.SqlBackend
               )
            => [t]
            -> ([t] -> E.SqlExpr (E.Entity val) -> E.SqlQuery ())
            -> E.SqlPersistT m ()
splitUpdate ts updateF =
    forM_ vals $ E.update . updateF
  where
    vals = chunksOf paramLimit ts

splitDelete :: MonadIO m => [t] -> ([t] -> E.SqlQuery ()) -> E.SqlPersistT m ()
splitDelete ts deleteF =
    forM_ vals $ E.delete . deleteF
  where
    vals = chunksOf paramLimit ts

splitInsertMany_ :: ( MonadIO m
                    , P.PersistEntity val
                    , P.PersistEntityBackend val ~ E.SqlBackend
                    )
                 => [val] -> E.SqlPersistT m ()
splitInsertMany_ = mapM_ P.insertMany_ . chunksOf paramLimit

limitOffset :: Word32 -> Word32 -> E.SqlQuery ()
limitOffset l o = do
    when (l > 0) $ E.limit  $ fromIntegral l
    when (o > 0) $ E.offset $ fromIntegral o

