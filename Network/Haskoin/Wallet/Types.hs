{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Types
( AccountName
, CoinStatus(..)
, RawPrvKey(..)
, RawSigInput(..)
, RawTxDests(..)
, RawTxOutPoints(..)
, WalletException(..)
) where

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception)

import Data.Aeson.Types
    ( Value (Object, String)
    , FromJSON
    , ToJSON
    , Parser
    , (.=)
    , (.:)
    , (.:?)
    , object
    , parseJSON
    , toJSON
    , withArray
    , withObject
    )
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust, isJust, fromMaybe)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)

import Database.Persist.Class
    ( PersistField
    , toPersistValue
    , fromPersistValue )
import Database.Persist.Types (PersistValue (PersistByteString))
import Database.Persist.TH (derivePersistField)
import Database.Persist.Sql (PersistFieldSql, SqlType (SqlBlob), sqlType)

import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Script
import Network.Haskoin.Transaction
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

-- | Spent if a complete transaction spends this coin
-- Reserved if a partial transaction is spending these coins
-- Unspent if the coins are still available
-- The purpose of the Reserved status is to block this coin from being used in
-- subsequent coin selection algorithms. However, Reserved coins can always be
-- spent (set status to Spent) by complete transactions.
data CoinStatus = Spent Hash256 | Reserved Hash256 | Unspent
    deriving (Show, Read, Eq)

data RawTxOutPoints = RawTxOutPoints [OutPoint] 
    deriving (Eq, Show)

data RawTxDests = RawTxDests [(String,Word64)]
    deriving (Eq, Show)

data RawSigInput = RawSigInput [(SigHash -> SigInput)]

data RawPrvKey = RawPrvKey [PrvKey]
    deriving (Eq, Show)

type AccountName = String

instance Exception WalletException

{- Instances for PersistField and PersistFieldSql -}

derivePersistField "CoinStatus"
derivePersistField "MasterKey"
derivePersistField "AccPubKey"
derivePersistField "XPubKey"
derivePersistField "Hash256"
derivePersistField "Script"

instance PersistField Tx where
    toPersistValue = PersistByteString . encode'
    fromPersistValue (PersistByteString bs) = case txE of
        Right tx -> Right tx
        Left str -> Left $ T.pack str
      where
        txE = decodeToEither bs
    fromPersistValue _ = Left "Has to be a PersistByteString"

instance PersistFieldSql Tx where
    sqlType _ = SqlBlob

{- Instances for JSON -}

instance FromJSON RawTxOutPoints where
    parseJSON = withArray "Expected: Array" $ \arr -> do
        RawTxOutPoints <$> (mapM f $ V.toList arr)
      where
        f = withObject "Expected: Object" $ \obj -> do
            tid  <- obj .: "txid" :: Parser String
            vout <- obj .: "vout" :: Parser Word32
            let i = maybeToEither ("Failed to decode txid" :: String)
                                  (decodeTxid tid)
                o = OutPoint <$> i <*> (return vout)
            either (const mzero) return o

instance FromJSON RawTxDests where
    parseJSON = withObject "Expected: Object" $ \obj ->
        RawTxDests <$> (mapM f $ H.toList obj)
      where
        f (add,v) = do
            amnt <- parseJSON v :: Parser Word64
            return (T.unpack add, amnt)

instance FromJSON RawSigInput where
    parseJSON = withArray "Expected: Array" $ \arr -> do
        RawSigInput <$> (mapM f $ V.toList arr)
      where
        f = withObject "Expected: Object" $ \obj -> do
            tid  <- obj .: "txid" :: Parser String
            vout <- obj .: "vout" :: Parser Word32
            scp  <- obj .: "scriptPubKey" :: Parser String
            rdm  <- obj .:? "redeemScript" :: Parser (Maybe String)
            let s = decodeToEither =<< maybeToEither "Hex parsing failed" 
                        (hexToBS scp)
                i = maybeToEither "Failed to decode txid" (decodeTxid tid)
                o = OutPoint <$> i <*> (return vout)
                r = decodeToEither =<< maybeToEither "Hex parsing failed" 
                        (hexToBS $ fromJust rdm)
                res | isJust rdm = SigInputSH <$> s <*> o <*> r
                    | otherwise  = SigInput <$> s <*> o
            either (const mzero) return res

instance FromJSON RawPrvKey where
    parseJSON = withArray "Expected: Array" $ \arr ->
        RawPrvKey <$> (mapM f $ V.toList arr)
      where
        f v = do
            str <- parseJSON v :: Parser String  
            maybe mzero return $ fromWIF str

instance ToJSON CoinStatus where
    toJSON (Spent tid) = 
        object [ "Status".= String "Spent"
               , "Txid"  .= encodeTxid tid
               ]
    toJSON (Reserved tid) = 
        object [ "Status".= String "Reserved"
               , "Txid"  .= encodeTxid tid
               ]
    toJSON Unspent = object [ "Status".= String "Unspent" ]

instance FromJSON CoinStatus where
    parseJSON (Object obj) = obj .: "Status" >>= \status -> case status of
        (String "Spent")    -> 
            (Spent . fromJust . decodeTxid)    <$> obj .: "Txid"
        (String "Reserved") -> 
            (Reserved . fromJust . decodeTxid) <$> obj .: "Txid"
        (String "Unspent")  -> return Unspent
        _                   -> mzero
    parseJSON _ = mzero

{- YAML templates -}

instance ToJSON OutPoint where
    toJSON (OutPoint h i) = object
        [ "TxID" .= encodeTxid h
        , "Index" .= toJSON i
        ]

instance ToJSON TxOut where
    toJSON (TxOut v s) = object $
        [ "Value" .= v
        , "Raw Script" .= bsToHex (encode' s)
        , "Script" .= (fromMaybe (Script []) $ decodeToMaybe s)
        ] ++ scptPair 
      where scptPair = 
              either (const [])
                     (\out -> ["Decoded Script" .= out]) 
                     (decodeOutputBS s)

instance ToJSON TxIn where
    toJSON (TxIn o s i) = object $ concat
        [ [ "OutPoint" .= o
          , "Sequence" .= i
          , "Raw Script" .= bsToHex (encode' s)
          , "Script" .= (fromMaybe (Script []) $ decodeToMaybe s)
          ] 
        , decoded 
        ]
      where decoded = either (const $ either (const []) f $ decodeInputBS s) 
                             f $ decodeScriptHashBS s
            f inp = ["Decoded Script" .= inp]
              
instance ToJSON Tx where
    toJSON tx@(Tx v is os i) = object
        [ "TxID" .= encodeTxid (txid tx)
        , "Version" .= v
        , "Inputs" .= map input (zip is [0..])
        , "Outputs" .= map output (zip os [0..])
        , "LockTime" .= i
        ]
      where input (x,j) = object 
              [T.pack ("Input " ++ show (j :: Int)) .= x]
            output (x,j) = object 
              [T.pack ("Output " ++ show (j :: Int)) .= x]

instance ToJSON Script where
    toJSON (Script ops) = toJSON $ map show ops

instance ToJSON ScriptOutput where
    toJSON (PayPK p) = object 
        [ "PayToPublicKey" .= object [ "Public Key" .= bsToHex (encode' p) ] ]
    toJSON (PayPKHash a) = object 
        [ "PayToPublicKeyHash" .= object
            [ "Address Hash160" .= bsToHex (encode' $ getAddrHash a)
            , "Address Base58" .= addrToBase58 a
            ]
        ]
    toJSON (PayMulSig ks r) = object 
        [ "PayToMultiSig" .= object
            [ "Required Keys (M)" .= toJSON r
            , "Public Keys" .= map (bsToHex . encode') ks
            ]
        ]
    toJSON (PayScriptHash a) = object 
        [ "PayToScriptHash" .= object
            [ "Address Hash160" .= bsToHex (encode' $ getAddrHash a)
            , "Address Base58" .= addrToBase58 a
            ]
        ]

instance ToJSON ScriptInput where
    toJSON (SpendPK s) = object 
        [ "SpendPublicKey" .= object [ "Signature" .= s ] ]
    toJSON (SpendPKHash s p) = object 
        [ "SpendPublicKeyHash" .= object
            [ "Signature" .= s
            , "Public Key" .= bsToHex (encode' p)
            , "Sender Addr" .= addrToBase58 (pubKeyAddr p)
            ]
        ]
    toJSON (SpendMulSig sigs r) = object 
        [ "SpendMultiSig" .= object
            [ "Required Keys (M)" .= r
            , "Signatures" .= sigs
            ]
        ]

instance ToJSON ScriptHashInput where
    toJSON (ScriptHashInput s r) = object
        [ "SpendScriptHash" .= object
            [ "ScriptInput" .= s
            , "RedeemScript" .= r
            , "Raw Redeem Script" .= bsToHex (encodeOutputBS r)
            , "Sender Addr" .= addrToBase58 (scriptAddr  r)
            ]
        ]

instance ToJSON TxSignature where
    toJSON ts@(TxSignature _ h) = object
        [ "Raw Sig" .= bsToHex (encodeSig ts)
        , "SigHash" .= h
        ]

instance ToJSON SigHash where
    toJSON sh = case sh of
        (SigAll acp) -> object
            [ "Type" .= String "SigAll"
            , "AnyoneCanPay" .= acp
            ]
        (SigNone acp) -> object
            [ "Type" .= String "SigNone"
            , "AnyoneCanPay" .= acp
            ]
        (SigSingle acp) -> object
            [ "Type" .= String "SigSingle"
            , "AnyoneCanPay" .= acp
            ]
        (SigUnknown acp v) -> object
            [ "Type" .= String "SigUnknown"
            , "AnyoneCanPay" .= acp
            , "Value" .= v
            ]

