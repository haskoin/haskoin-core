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
import Data.Maybe (fromJust, isJust)
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
    ( Hash256
    , PrvKey
    , MasterKey
    , AccPubKey
    , XPubKey
    , addrToBase58
    , getAddrHash
    , fromWIF
    , pubKeyAddr
    )
import Network.Haskoin.Protocol
    ( OutPoint (OutPoint)
    , Tx (Tx)
    , TxIn (TxIn)
    , TxOut (TxOut)
    , Script (Script)
    , decodeTxid
    , encodeTxid
    , decodeScriptOps
    , encodeScriptOps
    , txid
    )
import Network.Haskoin.Script
    ( ScriptHashInput (ScriptHashInput)
    , ScriptInput (SpendPK, SpendPKHash, SpendMulSig)
    , ScriptOutput (PayPK, PayPKHash, PayMulSig, PayScriptHash)
    , SigHash (SigAll, SigNone, SigSingle, SigUnknown)
    , TxSignature (TxSignature)
    , decodeScriptHash
    , decodeInput
    , decodeOutput
    , encodeOutput
    , encodeSig
    , scriptAddr
    )
import Network.Haskoin.Transaction (SigInput (SigInput, SigInputSH))
import Network.Haskoin.Util
    ( bsToHex
    , hexToBS
    , encode'
    , decodeToEither
    , maybeToEither
    )

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
            tid  <- obj .: T.pack "txid" :: Parser String
            vout <- obj .: T.pack "vout" :: Parser Word32
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
            tid  <- obj .: T.pack "txid" :: Parser String
            vout <- obj .: T.pack "vout" :: Parser Word32
            scp  <- obj .: T.pack "scriptPubKey" :: Parser String
            rdm  <- obj .:? T.pack "redeemScript" :: Parser (Maybe String)
            let s = decodeScriptOps =<< maybeToEither "Hex parsing failed" 
                        (hexToBS scp)
                i = maybeToEither "Failed to decode txid" (decodeTxid tid)
                o = OutPoint <$> i <*> (return vout)
                r = decodeScriptOps =<< maybeToEither "Hex parsing failed" 
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
        object [ "Status".= T.pack "Spent"
               , "Txid"  .= (encodeTxid tid)
               ]
    toJSON (Reserved tid) = 
        object [ "Status".= T.pack "Reserved"
               , "Txid"  .= (encodeTxid tid)
               ]
    toJSON Unspent = object [ "Status".= T.pack "Unspent" ]

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
            [ (T.pack "Address Hash160") .=
              (bsToHex $ encode' $ getAddrHash a)
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
            [ (T.pack "Address Hash160") .=
              (bsToHex $ encode' $ getAddrHash a)
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

