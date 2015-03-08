{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Types
( -- Wallet Types
  WalletName
, AccountName
, Wallet(..)
, Account(..)
, Balance(..)
, BalanceAddress(..)
, LabeledAddress(..)
, RecipientAddress(..)
, AccTx(..)
, TxConfidence(..)
, TxSource(..)
, WalletException(..)
, OfflineTxData(..)

-- Request Types
, WalletRequest(..)
, PagedResult(..)
, NewWallet(..)
, NewAccount(..)
, AccTxAction(..)
, AddressData(..)
, NodeAction(..)

-- Response Types
, WalletResponse(..)
, MnemonicRes(..)
, AddressPageRes(..)
, TxPageRes(..)
, TxHashStatusRes(..)
, TxStatusRes(..)
, TxRes(..)
, BalanceRes(..)
, SpendableRes(..)
, RescanRes(..)

, printWallet
, printAccount
, printLabeledAddress
, printBalanceAddress
, printAccTx
, printBalance
) where

import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Control.Exception (Exception)

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import Data.Maybe (maybeToList, isJust, fromJust)
import Data.Char (toLower)
import Data.Word (Word32, Word64)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T (Text, pack, unpack)
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

import Network.Haskoin.Wallet.Types.DeriveJSON
import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Node
import Network.Haskoin.Util

type WalletName  = T.Text
type AccountName = T.Text

data WalletException = WalletException String
    deriving (Eq, Read, Show, Typeable)

instance Exception WalletException

{- Wallet Types -}

-- TODO: Add NFData instances for all those types
data Wallet = Wallet
    { walletName   :: !T.Text
    , walletMaster :: !MasterKey
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 6) ''Wallet)

data Account
    = AccountRegular
        { accountWallet :: !T.Text
        , accountName   :: !T.Text
        , accountIndex  :: !KeyIndex
        , accountKey    :: !AccPubKey
        }
    | AccountMultisig
        { accountWallet   :: !T.Text
        , accountName     :: !T.Text
        , accountIndex    :: !KeyIndex
        , accountRequired :: !Int
        , accountTotal    :: !Int
        , accountKeys     :: ![XPubKey]
        }
    | AccountRead
        { accountWallet :: !T.Text
        , accountName   :: !T.Text
        , accountKey    :: !AccPubKey
        }
    | AccountReadMultisig
        { accountWallet   :: !T.Text
        , accountName     :: !T.Text
        , accountRequired :: !Int
        , accountTotal    :: !Int
        , accountKeys     :: ![XPubKey]
        }
    deriving (Eq, Show, Read)

$(deriveJSON (dropSumLabels 7 7 "type") ''Account)

data OfflineTxData = OfflineTxData
    { offlineData :: ![(OutPoint, ScriptOutput, Bool, KeyIndex)]
    , offlineTx   :: !Tx
    } deriving (Eq, Read, Show)

$(deriveJSON (dropFieldLabel 7) ''OfflineTxData)

data TxConfidence
    = TxOffline
    | TxDead 
    | TxPending
    | TxBuilding
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 2) ''TxConfidence)

data TxSource
    = SourceNetwork
    | SourceWallet
    | SourceUnknown
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 6) ''TxSource)

data Balance = BalanceConflict | Balance !Word64
    deriving (Eq, Show, Read)

-- TODO: Remove the balance JSON instance with aeson 0.9
instance ToJSON Balance where
    toJSON BalanceConflict = object [ "status"  .= String "conflict" ]
    toJSON (Balance b)     = object [ "status"  .= String "valid" 
                                    , "balance" .= b
                                    ]

instance FromJSON Balance where
    parseJSON = withObject "Balance" $ \o -> do
        String str <- o .: "status"
        case str of
            "conflict" -> return BalanceConflict
            "valid"    -> Balance <$> o .: "balance"
            _          -> mzero

data LabeledAddress = LabeledAddress 
    { laAddress :: !Address
    , laLabel   :: !T.Text
    , laIndex   :: !KeyIndex
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 2) ''LabeledAddress)

data BalanceAddress = BalanceAddress
    { baAddress        :: !LabeledAddress
    , baFinalBalance   :: !Balance
    , baTotalReceived  :: !Balance
    , baFundingTxs     :: ![TxHash]
    , baSpendingTxs    :: ![TxHash]
    , baConflictTxs    :: ![TxHash]
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 2) ''BalanceAddress)

data RecipientAddress = RecipientAddress
    { recipientAddress :: !Address
    , recipientLabel   :: !T.Text
    , recipientIsLocal :: !Bool
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 9) ''RecipientAddress)

data AccTx = AccTx
    { accTxTxId             :: !TxHash
    , accTxRecipients       :: ![RecipientAddress]
    , accTxValue            :: !Int64
    , accTxConfidence       :: !TxConfidence
    , accTxIsCoinbase       :: !Bool
    , accTxConfirmations    :: !Int
    , accTxTx               :: !Tx
    , accTxReceivedDate     :: !Word32
    , accTxConfirmationDate :: !(Maybe Word32)
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 5) ''AccTx)

{- Helper Types -}

data PagedResult = PagedResult 
    { pageNum     :: !Int
    , elemPerPage :: !Int
    } deriving (Eq, Read, Show)

$(deriveJSON (dropFieldLabel 0) ''PagedResult)

{- Request Types -}

data NewWallet = NewWallet 
    { newWalletWalletName :: !WalletName 
    , newWalletPassphrase :: !(Maybe T.Text)
    , newWalletMnemonic   :: !(Maybe T.Text)
    } deriving (Eq, Read, Show)

$(deriveJSON (dropFieldLabel 9) ''NewWallet)

data NewAccount
    = NewAccountRegular 
        { newAccountAccountName :: !AccountName }
    | NewAccountMultisig 
        { newAccountAccountName :: !AccountName 
        , newAccountRequired    :: !Int 
        , newAccountTotal       :: !Int 
        , newAccountKeys        :: ![XPubKey]
        }
    | NewAccountRead 
        { newAccountAccountName :: !AccountName 
        , newAccountKey         :: !XPubKey
        }
    | NewAccountReadMultisig 
        { newAccountAccountName :: !AccountName 
        , newAccountRequired    :: !Int 
        , newAccountTotal       :: !Int 
        , newAccountKeys        :: ![XPubKey]
        }
    deriving (Eq, Read, Show)

$(deriveJSON (dropSumLabels 10 10 "type" ) ''NewAccount)

data AccTxAction
    = CreateTx 
        { accTxActionRecipients :: ![(Address, Word64)] 
        , accTxActionFee        :: !Word64
        , accTxActionMinConf    :: !Word32
        , accTxActionSign       :: !Bool
        }
    | SignTx 
        { accTxActionTx       :: !Tx 
        , accTxActionFinalize :: !Bool
        }
    | SignOfflineTxData
        { accTxActionOfflineTxData :: !OfflineTxData 
        , accTxActionFinalize      :: !Bool
        }
    | ImportTx 
        { accTxActionTx :: !Tx }
    deriving (Eq, Read, Show)

$(deriveJSON (dropSumLabels 0 11 "type" ) ''AccTxAction)

data AddressData = AddressData { addressDataLabel :: !T.Text }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''AddressData)

data NodeAction = Rescan { nodeActionTimestamp :: !(Maybe Word32) }
    deriving (Eq, Show, Read)

instance ToJSON NodeAction where
    toJSON (Rescan tM) = object $
        [ "type" .= String "rescan" ] ++ 
        ( ("timestamp" .=) <$> maybeToList tM )

instance FromJSON NodeAction where
    parseJSON = withObject "NodeAction" $ \o -> do
        String t <- o .: "type"
        case t of
            "rescan" -> Rescan <$> o .:? "timestamp"
            _ -> mzero

data WalletRequest
    = GetWalletsR 
    | GetWalletR !WalletName
    | PostWalletsR !NewWallet
    | GetAccountsR !WalletName
    | PostAccountsR !WalletName !NewAccount
    | GetAccountR !WalletName !AccountName
    | PostAccountKeysR !WalletName !AccountName ![XPubKey]
    | GetAddressesR !WalletName !AccountName 
        !(Maybe PagedResult) !Word32 !Bool !Bool !Bool
    | PostAddressesR !WalletName !AccountName !AddressData
    | GetAddressR !WalletName !AccountName !KeyIndex !Word32 !Bool
    | PutAddressR !WalletName !AccountName !KeyIndex !AddressData
    | GetTxsR !WalletName !AccountName !(Maybe PagedResult)
    | PostTxsR !WalletName !AccountName !AccTxAction
    | GetTxR !WalletName !AccountName !TxHash !Bool
    | GetOfflineTxDataR !WalletName !AccountName !TxHash
    | GetBalanceR !WalletName !AccountName !Word32
    | GetSpendableR !WalletName !AccountName !Word32
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


{- Response Types -}

data MnemonicRes = MnemonicRes { resMnemonic :: !Mnemonic }
    deriving (Eq, Read, Show)

$(deriveJSON (dropFieldLabel 3) ''MnemonicRes)

data AddressPageRes = AddressPageRes 
    { addressPageAddressPage :: ![BalanceAddress] 
    , addressPageMaxPage     :: !Int
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 11) ''AddressPageRes)

data TxPageRes = TxPageRes 
    { txPageTxPage  :: ![AccTx] 
    , txPageMaxPage :: !Int
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 6) ''TxPageRes)

data TxHashStatusRes = TxHashStatusRes 
    { txHashStatusTxHash   :: !TxHash 
    , txHashStatusComplete :: !Bool 
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 12) ''TxHashStatusRes)

data TxStatusRes = TxStatusRes 
    { txStatusTx       :: !Tx 
    , txStatusComplete :: !Bool 
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 8) ''TxStatusRes)

data TxRes = TxRes { resTx :: !Tx } 
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 3) ''TxRes)

data BalanceRes = BalanceRes 
    { balanceBalance   :: !Balance 
    , balanceConflicts :: ![TxHash]
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 7) ''BalanceRes)

data SpendableRes = SpendableRes { spendableBalance :: !Word64 }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 9) ''SpendableRes)

data RescanRes = RescanRes { rescanTimestamp :: !Word32 }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 6) ''RescanRes)

data WalletResponse a
    = ResponseError { responseError  :: !T.Text }
    | ResponseValid { responseResult :: !a  }
    deriving (Eq, Show)

$(deriveJSON (dropSumLabels 8 8 "status" ) ''WalletResponse)

{- Utilities -}

printWallet :: Wallet -> String
printWallet Wallet{..} = unlines
    [ unwords [ "Wallet    :", T.unpack walletName ]
    , unwords [ "Master key:", xPrvExport $ masterKey walletMaster ]
    ]

printAccount :: Account -> String
printAccount a = case a of
    AccountRegular{..} -> unlines
        [ unwords [ "Wallet :", T.unpack accountWallet ]
        , unwords [ "Account:", T.unpack accountName ]
        , unwords [ "Type   :", "Regular" ]
        , unwords [ "Tree   :", concat [ "m/",show accountIndex,"'/" ] ]
        , unwords [ "Key    :", xPubExport $ getAccPubKey accountKey ]
        ]
    AccountMultisig{..} -> unlines $
        [ unwords [ "Wallet :", T.unpack accountWallet ]
        , unwords [ "Account:", T.unpack accountName ]
        , unwords [ "Type   :", "Multisig"
                  , show accountRequired, "of", show accountTotal 
                  ]
        , unwords [ "Tree   :", concat [ "m/",show accountIndex,"'/" ] ]
        ] ++ if null accountKeys then [] else 
        ( unwords [ "Keys   :", xPubExport $ head accountKeys ] ) : 
        ( map (\x -> 
          unwords [ "        ", xPubExport x]) $ tail accountKeys )
    AccountRead{..} -> unlines
        [ unwords [ "Wallet :", T.unpack accountWallet ]
        , unwords [ "Account:", T.unpack accountName ]
        , unwords [ "Type   :", "Read-only" ]
        , unwords [ "Key    :", xPubExport $ getAccPubKey accountKey ]
        ]
    AccountReadMultisig{..} -> unlines $
        [ unwords [ "Wallet :", T.unpack accountWallet ]
        , unwords [ "Account:", T.unpack accountName ]
        , unwords [ "Type   :", "Read-only Multisig"
                  , show accountRequired, "of", show accountTotal 
                  ]
        ] ++ if null accountKeys then [] else 
        ( unwords [ "Keys   :", xPubExport $ head accountKeys ] ) : 
        ( map (\x -> 
          unwords [ "        ", xPubExport x]) $ tail accountKeys )

printBalance :: Balance -> String
printBalance b = case b of
    BalanceConflict -> "Conflict"
    Balance x       -> show x

printBalanceAddress :: BalanceAddress -> String
printBalanceAddress BalanceAddress{..} = unwords $
    ( printLabeledAddress baAddress ) : 
        if null baFundingTxs && null baSpendingTxs && null baConflictTxs 
           then [] 
           else [ "["
                , "Balance:", printBalance baFinalBalance ++ ","
                , "TxCount:", show $ (length baFundingTxs) 
                                   + (length baSpendingTxs)
                , "]" 
                ]

printLabeledAddress :: LabeledAddress -> String
printLabeledAddress LabeledAddress{..} = unwords $
    [ show laIndex, ":" , addrToBase58 laAddress ]
    ++ if null (T.unpack laLabel)
          then [] else [ concat [ "(", T.unpack laLabel ,")" ] ]

printRecipientAddress :: RecipientAddress -> String
printRecipientAddress RecipientAddress{..} = unwords $
    ( "    " ++ if recipientIsLocal then "<-" else "->" )
    : ( addrToBase58 recipientAddress )
    : if null (T.unpack recipientLabel)
         then [] 
         else [ concat [ "(", T.unpack recipientLabel ,")" ] ]
    

printAccTx :: AccTx -> String
printAccTx AccTx{..} = unlines $ 
    [ unwords [ "Value     :", show accTxValue ]
    , unwords [ "Recipients:" ]
    ] 
    ++ ( map printRecipientAddress accTxRecipients )
    ++ [ unwords [ "Confidence:"
                 , printConfidence accTxConfidence
                 , concat [ "(",show accTxConfirmations," confirmations)" ] 
                 ]
       , unwords [ "TxHash    :", encodeTxHashLE accTxTxId ]
       ] 
    ++ if accTxIsCoinbase then [ unwords [ "Coinbase  :", "Yes" ] ] else [] 
    ++ [ unwords [ "Received  :", show $ f accTxReceivedDate ] ] 
    ++ if isJust accTxConfirmationDate 
          then [ unwords [ "Confirmed :" 
                         , show $ f $ fromJust accTxConfirmationDate 
                         ] 
               ] 
          else []
  where
    f = posixSecondsToUTCTime . realToFrac 
    
printConfidence :: TxConfidence -> String
printConfidence c = case c of
    TxBuilding -> "Building"
    TxPending  -> "Pending"
    TxDead     -> "Dead"
    TxOffline  -> "Offline"

{- Persistent Instances -}

toPersistJson :: (ToJSON a) => a -> PersistValue
toPersistJson = PersistByteString . toStrict . encode

fromPersistJson :: (FromJSON a) => T.Text -> PersistValue -> Either T.Text a
fromPersistJson msg pb = case pb of
    PersistByteString w -> maybeToEither msg (decode $ toLazyBS w)
    _ -> Left "Hash to be a PersistByteString"

instance PersistField Address where
    toPersistValue = PersistByteString . stringToBS . addrToBase58
    fromPersistValue (PersistByteString a) =
        maybeToEither "Not a valid Address" . base58ToAddr $ bsToString a
    fromPersistValue _ = Left "Has to be a PersistByteString"

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
    fromPersistValue _ = Left "Has to be a PersistByteString"

instance PersistFieldSql TxHash where
    sqlType _ = SqlString

instance PersistField BlockHash where
    toPersistValue = PersistByteString . stringToBS . encodeBlockHashLE
    fromPersistValue (PersistByteString h) =
        maybeToEither "Not a valid BlockHash" (decodeBlockHashLE $ bsToString h)
    fromPersistValue _ = Left "Has to be a PersistByteString"

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
        _ -> Left "Not a valid TxConfidence"
    fromPersistValue _ = Left "Not a valid TxConfidence"
        
instance PersistFieldSql TxConfidence where
    sqlType _ = SqlString

instance PersistField TxSource where
    toPersistValue ts = PersistByteString $ case ts of
        SourceNetwork -> "network"
        SourceWallet  -> "wallet"
        SourceUnknown -> "unknown"

    fromPersistValue (PersistByteString t) = case t of
        "network" -> return SourceNetwork
        "wallet"  -> return SourceWallet
        "unknown" -> return SourceUnknown
        _ -> Left "Not a valid TxSource"
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
    fromPersistValue (PersistByteString bs) = case decodeToEither bs of
        Right tx -> Right tx
        Left str -> Left $ T.pack str
    fromPersistValue _ = Left "Has to be a PersistByteString"

instance PersistFieldSql Tx where
    sqlType _ = SqlBlob

instance PersistField BloomFilter where
    toPersistValue = PersistByteString . encode'
    fromPersistValue (PersistByteString bs) = case decodeToEither bs of
        Right bloom -> Right bloom
        Left str -> Left $ T.pack str
    fromPersistValue _ = Left "Has to be a PersistByteString"

instance PersistFieldSql BloomFilter where
    sqlType _ = SqlBlob

