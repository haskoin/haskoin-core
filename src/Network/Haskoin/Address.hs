{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Haskoin.Address
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX

Base58, CashAddr, Bech32 address and WIF private key serialization support.
-}
module Network.Haskoin.Address
    ( Address
    , getAddrHash160
    , getAddrHash256
    , getAddrNet
    , isPubKeyAddress
    , isScriptAddress
    , isWitnessPubKeyAddress
    , isWitnessScriptAddress
    , addrToString
    , stringToAddr
    , addrFromJSON
    , pubKeyAddr
    , pubKeyWitnessAddr
    , p2pkhAddr
    , p2wpkhAddr
    , p2shAddr
    , p2wshAddr
    , inputAddress
    , outputAddress
    , addressToScript
    , addressToScriptBS
    , addressToOutput
    , payToScriptAddress
    , payToWitnessScriptAddress
    , scriptToAddress
    , scriptToAddressBS
      -- * Private Key Wallet Import Format (WIF)
    , fromWif
    , toWif
    ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson                       as A
import           Data.Aeson.Types
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           Data.Function
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Serialize                   as S
import           Data.String.Conversions
import           Data.Text                        (Text)
import           GHC.Generics                     as G (Generic)
import           Network.Haskoin.Address.Base58
import           Network.Haskoin.Address.Bech32
import           Network.Haskoin.Address.CashAddr
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Keys.Common
import           Network.Haskoin.Script
import           Network.Haskoin.Util
import           Text.Read                        as R

-- | Address format for Bitcoin and Bitcoin Cash.
data Address
    -- | pay to public key hash (regular)
    = PubKeyAddress { getAddrHash160 :: !Hash160
                        -- ^ RIPEMD160 hash of public key's SHA256 hash
                    , getAddrNet     :: !Network
                        -- ^ address network
                     }
    -- | pay to script hash
    | ScriptAddress { getAddrHash160 :: !Hash160
                        -- ^ RIPEMD160 hash of script's SHA256 hash
                    , getAddrNet     :: !Network
                        -- ^ address network
                     }
    -- | pay to witness public key hash
    | WitnessPubKeyAddress { getAddrHash160 :: !Hash160
                               -- ^ RIPEMD160 hash of public key's SHA256 hash
                           , getAddrNet     :: !Network
                               -- ^ address network
                           }
    -- | pay to witness script hash
    | WitnessScriptAddress { getAddrHash256 :: !Hash256
                               -- ^ HASH256 hash of script
                           , getAddrNet     :: !Network
                               -- ^ address network
                           }
    deriving (Eq, G.Generic)

instance Serialize Address where
    put a = do
        put $ getAddrNet a
        let bs = addressToScriptBS a
        put $ B.length bs
        putByteString bs
    get = do
        net <- S.get
        bs <- getByteString =<< S.get
        case scriptToAddressBS net bs of
            Nothing -> fail "Could not decode address"
            Just a -> return a

instance Hashable Address where
    hashWithSalt i (PubKeyAddress h _)        = i `hashWithSalt` h
    hashWithSalt i (ScriptAddress h _)        = i `hashWithSalt` h
    hashWithSalt i (WitnessPubKeyAddress h _) = i `hashWithSalt` h
    hashWithSalt i (WitnessScriptAddress h _) = i `hashWithSalt` h
    hash (PubKeyAddress h _)        = hash h
    hash (ScriptAddress h _)        = hash h
    hash (WitnessPubKeyAddress h _) = hash h
    hash (WitnessScriptAddress h _) = hash h

instance Ord Address where
    compare = compare `on` f
      where
        f (PubKeyAddress h _)        = S.encode h
        f (ScriptAddress h _)        = S.encode h
        f (WitnessPubKeyAddress h _) = S.encode h
        f (WitnessScriptAddress h _) = S.encode h

instance NFData Address

-- | 'Address' pays to a public key hash.
isPubKeyAddress :: Address -> Bool
isPubKeyAddress PubKeyAddress {} = True
isPubKeyAddress _                = False

-- | 'Address' pays to a script hash.
isScriptAddress :: Address -> Bool
isScriptAddress ScriptAddress {} = True
isScriptAddress _                = False

-- | 'Address' pays to a witness public key hash. Only valid for SegWit
-- networks.
isWitnessPubKeyAddress :: Address -> Bool
isWitnessPubKeyAddress WitnessPubKeyAddress {} = True
isWitnessPubKeyAddress _                       = False

-- | 'Address' pays to a witness script hash. Only valid for SegWit networks.
isWitnessScriptAddress :: Address -> Bool
isWitnessScriptAddress WitnessScriptAddress {} = True
isWitnessScriptAddress _                       = False

-- | Deserializer for binary 'Base58' addresses.
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

-- | Binary serializer for 'Base58' addresses.
base58put :: Putter Address
base58put (PubKeyAddress h net) = do
        putWord8 (getAddrPrefix net)
        put h
base58put (ScriptAddress h net) = do
        putWord8 (getScriptPrefix net)
        put h
base58put _ = error "Cannot serialize this address as Base58"

instance Show Address where
    showsPrec _ a = shows (addrToString a)

instance Read Address where
    readPrec = do
        R.String str <- lexP
        let bs = cs str
        maybe pfail return $
            foldl' (\a n -> a <|> stringToAddr n bs) Nothing allNets

instance ToJSON Address where
    toJSON = A.String . addrToString

-- | JSON parsing for Bitcoin addresses. Works with 'Base58', 'CashAddr' and
-- 'Bech32'.
addrFromJSON :: Network -> Value -> Parser Address
addrFromJSON net =
    withText "address" $ \t ->
        case stringToAddr net t of
            Nothing -> fail "could not decode address"
            Just x  -> return x

-- | Convert address to human-readable string. Uses 'Base58', 'Bech32', or
-- 'CashAddr' depending on network.
addrToString :: Address -> Text
addrToString a@PubKeyAddress {getAddrHash160 = h, getAddrNet = net}
    | isNothing (getCashAddrPrefix net) =
        encodeBase58Check $ runPut $ base58put a
    | otherwise =
        fromMaybe (error "Colud not encode a CashAddr") $
        cashAddrEncode net 0 (S.encode h)

addrToString a@ScriptAddress {getAddrHash160 = h, getAddrNet = net}
    | isNothing (getCashAddrPrefix net) =
        encodeBase58Check $ runPut $ base58put a
    | otherwise =
        fromMaybe (error "Could not encode a CashAddr") $
        cashAddrEncode net 1 (S.encode h)

addrToString WitnessPubKeyAddress {getAddrHash160 = h, getAddrNet = net} =
    let mt = do
            hrp <- getBech32Prefix net
            segwitEncode hrp 0 (B.unpack (S.encode h))
     in fromMaybe (error "Could not encode a Bech32 address") mt

addrToString WitnessScriptAddress {getAddrHash256 = h, getAddrNet = net} =
    let mt = do
            hrp <- getBech32Prefix net
            segwitEncode hrp 0 (B.unpack (S.encode h))
     in fromMaybe (error "Could not encode a Bech32 address") mt

-- | Parse 'Base58', 'Bech32' or 'CashAddr' address, depending on network.
stringToAddr :: Network -> Text -> Maybe Address
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
        _ -> Nothing
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

-- | Obtain a P2PKH address from a public key.
pubKeyAddr :: Network -> PubKeyI -> Address
pubKeyAddr net k = PubKeyAddress (addressHash (S.encode k)) net

-- | Obtain a P2PKH address from a 'Hash160'.
p2pkhAddr :: Network -> Hash160 -> Address
p2pkhAddr net h = PubKeyAddress h net

-- | Obtain a P2WPKH address from a public key. Only on SegWit networks.
pubKeyWitnessAddr :: Network -> PubKeyI -> Maybe Address
pubKeyWitnessAddr net k
    | getSegWit net = Just $ WitnessPubKeyAddress (addressHash (S.encode k)) net
    | otherwise = Nothing

-- | Obtain a P2WPKH address from a 'Hash160'.
p2wpkhAddr :: Network -> Hash160 -> Maybe Address
p2wpkhAddr net h
    | getSegWit net = Just $ WitnessPubKeyAddress h net
    | otherwise = Nothing

-- | Obtain a P2SH address from a 'Hash160'.
p2shAddr :: Network -> Hash160 -> Address
p2shAddr net h = ScriptAddress h net

-- | Obtain a P2WSH address from a 'Hash256'
p2wshAddr :: Network -> Hash256 -> Maybe Address
p2wshAddr net h
    | getSegWit net = Just $ WitnessScriptAddress h net
    | otherwise = Nothing

-- | Compute a pay-to-script-hash address for an output script.
payToScriptAddress :: Network -> ScriptOutput -> Address
payToScriptAddress net out = p2shAddr net (addressHash (encodeOutputBS out))

-- | Compute a pay-to-witness-script-hash address for an output script. Only on
-- SegWit networks.
payToWitnessScriptAddress :: Network -> ScriptOutput -> Maybe Address
payToWitnessScriptAddress net out = p2wshAddr net (sha256 (encodeOutputBS out))

-- | Encode an output script from an address. Will fail if using a
-- pay-to-witness address on a non-SegWit network.
addressToOutput :: Address -> ScriptOutput
addressToOutput a
    | isPubKeyAddress a = PayPKHash (getAddrHash160 a)
    | isScriptAddress a = PayScriptHash (getAddrHash160 a)
    | isWitnessPubKeyAddress a = PayWitnessPKHash (getAddrHash160 a)
    | isWitnessScriptAddress a = PayWitnessScriptHash (getAddrHash256 a)
    | otherwise = undefined

-- | Get output script AST for an 'Address'.
addressToScript :: Address -> Script
addressToScript = encodeOutput . addressToOutput

-- | Encode address as output script in 'ByteString' form.
addressToScriptBS :: Address -> ByteString
addressToScriptBS = S.encode . addressToScript

-- | Decode an output script into an 'Address' if it has such representation.
scriptToAddress :: Network -> Script -> Maybe Address
scriptToAddress net = eitherToMaybe . (outputAddress net <=< decodeOutput)

-- | Decode a serialized script into an 'Address'.
scriptToAddressBS :: Network -> ByteString -> Maybe Address
scriptToAddressBS net = eitherToMaybe . (outputAddress net <=< decodeOutputBS)

-- | Get the 'Address' of a 'ScriptOutput'.
outputAddress :: Network -> ScriptOutput -> Either String Address
outputAddress net s =
    case s of
        PayPKHash h -> Right $ p2pkhAddr net h
        PayScriptHash h -> Right $ p2shAddr net h
        PayPK k -> Right $ pubKeyAddr net k
        PayWitnessPKHash h ->
            maybeToEither "outputAddress: segwit not supported in this network" $
            p2wpkhAddr net h
        PayWitnessScriptHash h ->
            maybeToEither "outputAddress: segwit not supported in this network" $
            p2wshAddr net h
        _ -> Left "outputAddress: bad output script type"

-- | Infer the address of a 'ScriptInput'
inputAddress :: Network -> ScriptInput -> Either String Address
inputAddress net s = case s of
    RegularInput (SpendPKHash _ key) -> return $ pubKeyAddr net key
    ScriptHashInput _ rdm -> return $ payToScriptAddress net rdm
    _ -> Left "inputAddress: bad input script type"

-- | Decode private key from WIF (wallet import format) string.
fromWif :: Network -> Base58 -> Maybe SecKeyI
fromWif net wif = do
    bs <- decodeBase58Check wif
    -- Check that this is a private key
    guard (B.head bs == getSecretPrefix net)
    case B.length bs of
        -- Uncompressed format
        33 -> wrapSecKey False <$> secKey (B.tail bs)
        -- Compressed format
        34 -> do
            guard $ B.last bs == 0x01
            wrapSecKey True <$> secKey (B.tail $ B.init bs)
        -- Bad length
        _  -> Nothing

-- | Encode private key into a WIF string.
toWif :: Network -> SecKeyI -> Base58
toWif net (SecKeyI k c) =
    encodeBase58Check . B.cons (getSecretPrefix net) $
    if c
        then getSecKey k `B.snoc` 0x01
        else getSecKey k
