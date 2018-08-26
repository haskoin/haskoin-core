{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.Address where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson                      as A
import qualified Data.Array                      as Arr
import           Data.Bits
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as C
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Serialize                  as S
import           Data.String
import           Data.String.Conversions
import           Data.Word
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Base58
import           Network.Haskoin.Crypto.Bech32
import           Network.Haskoin.Crypto.CashAddr
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Util
import           Text.Read                       as R

-- | Data type representing a Bitcoin address
data Address
    -- | Public Key Hash Address
    = PubKeyAddress { getAddrHash :: !Hash160 }
    -- | Script Hash Address
    | ScriptAddress { getAddrHash :: !Hash160 }
    -- | SegWit Public Key Hash Address
    | WitnessPubKeyAddress { getAddrHash :: !Hash160 }
    -- | SegWit Script Hash Address
    | WitnessScriptAddress { getScriptHash :: !Hash256 }
    deriving (Eq, Ord)

base58get :: Get Address
base58get = do
        pfx <- getWord8
        addr <- S.get
        f pfx addr
      where
        f x a | x == addrPrefix   = return (PubKeyAddress a)
              | x == scriptPrefix = return (ScriptAddress a)
              | otherwise = fail "Does not recognize address prefix"

base58put :: Putter Address
base58put (PubKeyAddress h) = do
        putWord8 addrPrefix
        put h
base58put (ScriptAddress h) = do
        putWord8 scriptPrefix
        put h

instance Show Address where
    showsPrec d a =
        case addrToString a of
            Just s -> showParen (d > 10) $ showString "Address " . shows s
            Nothing -> showString "InvalidAddress"

instance Read Address where
    readPrec = j <|> n
      where
        j =
            parens $ do
                R.Ident "Address" <- lexP
                R.String str <- lexP
                maybe pfail return $ stringToAddr $ cs str
        n =
            parens $ do
                R.Ident "InvalidAddress" <- lexP
                pfail

instance IsString Address where
    fromString =
        fromMaybe e . stringToAddr . cs
      where
        e = error "Could not decode bitcoin address"

instance NFData Address where
    rnf (PubKeyAddress h) = rnf h
    rnf (ScriptAddress h) = rnf h

instance FromJSON Address where
    parseJSON = withText "address" $ maybe mzero return . stringToAddr . cs

instance ToJSON Address where
    toJSON =
        A.String .
        cs . fromMaybe (error "Could not encode address") . addrToString

-- | Transforms an Address into an encoded String
addrToString :: Address -> Maybe ByteString
addrToString a@PubKeyAddress {getAddrHash = h}
    | isNothing cashAddrPrefix =
        return $ encodeBase58Check $ runPut $ base58put a
    | otherwise = cashAddrEncode 0 (S.encode h)
addrToString a@ScriptAddress {getAddrHash = h}
    | isNothing cashAddrPrefix =
        return $ encodeBase58Check $ runPut $ base58put a
    | otherwise = cashAddrEncode 1 (S.encode h)
addrToString WitnessPubKeyAddress {getAddrHash = h} = do
    hrp <- bech32Prefix
    segwitEncode hrp 0 (B.unpack (S.encode h))
addrToString WitnessScriptAddress {getScriptHash = h} = do
    hrp <- bech32Prefix
    segwitEncode hrp 0 (B.unpack (S.encode h))

-- | Decodes an Address from an encoded String. This function can fail
-- if the String is not properly encoded or its checksum fails.
stringToAddr :: ByteString -> Maybe Address
stringToAddr bs = b58 <|> cash <|> segwit
  where
    b58 = eitherToMaybe . runGet base58get =<< decodeBase58Check bs
    cash = cashAddrDecode bs >>= \(ver, bs') -> case ver of
        0 -> do
            h <- eitherToMaybe (S.decode bs')
            return $ PubKeyAddress h
        1 -> do
            h <- eitherToMaybe (S.decode bs')
            return $ ScriptAddress h
    segwit = do
        hrp <- bech32Prefix
        (ver, bs') <- segwitDecode hrp bs
        guard (ver == 0)
        let bs'' = B.pack bs'
        case B.length bs'' of
            20 -> do
                h <- eitherToMaybe (S.decode bs'')
                return $ WitnessPubKeyAddress h
            32 -> do
                h <- eitherToMaybe (S.decode bs'')
                return $ WitnessScriptAddress h
            _ -> Nothing
