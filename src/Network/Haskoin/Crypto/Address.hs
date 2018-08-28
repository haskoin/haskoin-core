{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.Address where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson                      as A
import           Data.Aeson.Types
import qualified Data.Array                      as Arr
import           Data.Bits
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as C
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Serialize                  as S
import           Data.String
import           Data.String.Conversions
import           Data.Word
import           GHC.Generics                    (Generic)
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
    = PubKeyAddress { getAddrHash :: !Hash160
                    , getAddrNet  :: !Network }
    -- | Script Hash Address
    | ScriptAddress { getAddrHash :: !Hash160
                    , getAddrNet  :: !Network }
    -- | SegWit Public Key Hash Address
    | WitnessPubKeyAddress { getAddrHash :: !Hash160
                           , getAddrNet  :: !Network }
    -- | SegWit Script Hash Address
    | WitnessScriptAddress { getScriptHash :: !Hash256
                           , getAddrNet    :: !Network }
    deriving (Eq, Generic)

instance Ord Address where
    compare = compare `on` f
      where
        f (PubKeyAddress h _)        = S.encode h
        f (ScriptAddress h _)        = S.encode h
        f (WitnessPubKeyAddress h _) = S.encode h
        f (WitnessScriptAddress h _) = S.encode h

instance NFData Address

base58get :: Network -> Get Address
base58get net = do
    pfx <- getWord8
    addr <- S.get
    f pfx addr
  where
    f x a
        | x == getAddrPrefix net = return (PubKeyAddress a net)
        | x == getScriptPrefix net = return (ScriptAddress a net)
        | otherwise = fail "Does not recognize address prefix"

base58put :: Putter Address
base58put (PubKeyAddress h net) = do
        putWord8 (getAddrPrefix net)
        put h
base58put (ScriptAddress h net) = do
        putWord8 (getScriptPrefix net)
        put h

instance Show Address where
    showsPrec d a =
        case addrToString a of
            Just s  -> shows s
            Nothing -> showString "InvalidAddress"

instance ToJSON Address where
    toJSON =
        A.String .
        cs . fromMaybe (error "Could not encode address") . addrToString

addrFromJSON :: Network -> Value -> Parser Address
addrFromJSON net =
    withText "address" $ \t ->
        case stringToAddr net (cs t) of
            Nothing -> fail "could not decode address"
            Just x  -> return x

-- | Transforms an Address into an encoded String
addrToString :: Address -> Maybe ByteString
addrToString a@PubKeyAddress {getAddrHash = h, getAddrNet = net}
    | isNothing (getCashAddrPrefix net) =
        return $ encodeBase58Check $ runPut $ base58put a
    | otherwise = cashAddrEncode net 0 (S.encode h)
addrToString a@ScriptAddress {getAddrHash = h, getAddrNet = net}
    | isNothing (getCashAddrPrefix net) =
        return $ encodeBase58Check $ runPut $ base58put a
    | otherwise = cashAddrEncode net 1 (S.encode h)
addrToString WitnessPubKeyAddress {getAddrHash = h, getAddrNet = net} = do
    hrp <- (getBech32Prefix net)
    segwitEncode hrp 0 (B.unpack (S.encode h))
addrToString WitnessScriptAddress {getScriptHash = h, getAddrNet = net} = do
    hrp <- (getBech32Prefix net)
    segwitEncode hrp 0 (B.unpack (S.encode h))

-- | Decodes an Address from an encoded String. This function can fail
-- if the String is not properly encoded or its checksum fails.
stringToAddr :: Network -> ByteString -> Maybe Address
stringToAddr net bs = cash <|> segwit <|> b58
  where
    b58 = eitherToMaybe . runGet (base58get net) =<< decodeBase58Check bs
    cash = cashAddrDecode net bs >>= \(ver, bs') -> case ver of
        0 -> do
            h <- eitherToMaybe (S.decode bs')
            return $ PubKeyAddress h net
        1 -> do
            h <- eitherToMaybe (S.decode bs')
            return $ ScriptAddress h net
    segwit = do
        hrp <- getBech32Prefix net
        (ver, bs') <- segwitDecode hrp bs
        guard (ver == 0)
        let bs'' = B.pack bs'
        case B.length bs'' of
            20 -> do
                h <- eitherToMaybe (S.decode bs'')
                return $ WitnessPubKeyAddress h net
            32 -> do
                h <- eitherToMaybe (S.decode bs'')
                return $ WitnessScriptAddress h net
            _ -> Nothing
