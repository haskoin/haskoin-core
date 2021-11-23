{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskoin.Address
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Base58, CashAddr, Bech32 address and WIF private key serialization support.
-}
module Haskoin.Address (
    -- * Addresses
    Address (..),
    isPubKeyAddress,
    isScriptAddress,
    isWitnessAddress,
    isWitnessPubKeyAddress,
    isWitnessScriptAddress,
    addrToText,
    textToAddr,
    bech32ToAddr,
    cashToAddr,
    base58ToAddr,
    addrToJSON,
    addrToEncoding,
    addrFromJSON,
    pubKeyAddr,
    pubKeyWitnessAddr,
    pubKeyCompatWitnessAddr,
    p2pkhAddr,
    p2wpkhAddr,
    p2shAddr,
    p2wshAddr,
    inputAddress,
    outputAddress,
    addressToScript,
    addressToScriptBS,
    addressToOutput,
    payToScriptAddress,
    payToWitnessScriptAddress,
    payToNestedScriptAddress,
    scriptToAddress,
    scriptToAddressBS,
    module Haskoin.Address.Base58,
    module Haskoin.Address.Bech32,
    module Haskoin.Address.CashAddr,
) where

import Control.Applicative
import Control.Arrow (second)
import Control.DeepSeq
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Encoding as A
import Data.Aeson.Types
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Hashable
import Data.Maybe
import Data.Serialize (Serialize (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Generics (Generic)
import Haskoin.Address.Base58
import Haskoin.Address.Bech32
import Haskoin.Address.CashAddr
import Haskoin.Crypto
import Haskoin.Data
import Haskoin.Keys.Common
import Haskoin.Script
import Haskoin.Util

-- | Address format for Bitcoin and Bitcoin Cash.
data Address
    = -- | pay to public key hash (regular)
      PubKeyAddress
        { -- | RIPEMD160 hash of public key's SHA256 hash
          getAddrHash160 :: !Hash160
        }
    | -- | pay to script hash
      ScriptAddress
        { -- | RIPEMD160 hash of script's SHA256 hash
          getAddrHash160 :: !Hash160
        }
    | -- | pay to witness public key hash
      WitnessPubKeyAddress
        { -- | RIPEMD160 hash of public key's SHA256 hash
          getAddrHash160 :: !Hash160
        }
    | -- | pay to witness script hash
      WitnessScriptAddress
        { -- | HASH256 hash of script
          getAddrHash256 :: !Hash256
        }
    | -- | other witness address
      WitnessAddress
        { getAddrVersion :: !Word8
        , getAddrData :: !ByteString
        }
    deriving
        (Eq, Ord, Generic, Show, Read, Hashable, NFData)

instance Serial Address where
    serialize (PubKeyAddress k) = do
        putWord8 0x00
        serialize k
    serialize (ScriptAddress s) = do
        putWord8 0x01
        serialize s
    serialize (WitnessPubKeyAddress h) = do
        putWord8 0x02
        serialize h
    serialize (WitnessScriptAddress s) = do
        putWord8 0x03
        serialize s
    serialize (WitnessAddress v d) = do
        putWord8 0x04
        putWord8 v
        putWord64be (fromIntegral (B.length d))
        putByteString d

    deserialize =
        getWord8 >>= \case
            0x00 -> PubKeyAddress <$> deserialize
            0x01 -> ScriptAddress <$> deserialize
            0x02 -> WitnessPubKeyAddress <$> deserialize
            0x03 -> WitnessScriptAddress <$> deserialize
            0x04 ->
                WitnessAddress <$> getWord8
                    <*> (getByteString . fromIntegral =<< getWord64be)
            b ->
                fail . T.unpack $
                    "Could not decode address type byte: "
                        <> encodeHex (B.singleton b)

instance Serialize Address where
    put = serialize
    get = deserialize

instance Binary Address where
    put = serialize
    get = deserialize

-- | 'Address' pays to a public key hash.
isPubKeyAddress :: Address -> Bool
isPubKeyAddress PubKeyAddress{} = True
isPubKeyAddress _ = False

-- | 'Address' pays to a script hash.
isScriptAddress :: Address -> Bool
isScriptAddress ScriptAddress{} = True
isScriptAddress _ = False

{- | 'Address' pays to a witness public key hash. Only valid for SegWit
 networks.
-}
isWitnessPubKeyAddress :: Address -> Bool
isWitnessPubKeyAddress WitnessPubKeyAddress{} = True
isWitnessPubKeyAddress _ = False

isWitnessScriptAddress :: Address -> Bool
isWitnessScriptAddress WitnessScriptAddress{} = True
isWitnessScriptAddress _ = False

isWitnessAddress :: Address -> Bool
isWitnessAddress WitnessAddress{} = True
isWitnessAddress _ = False

addrToJSON :: Network -> Address -> Value
addrToJSON net a = toJSON (addrToText net a)

addrToEncoding :: Network -> Address -> Encoding
addrToEncoding net = maybe null_ text . addrToText net

{- | JSON parsing for Bitcoin addresses. Works with 'Base58', 'CashAddr' and
 'Bech32'.
-}
addrFromJSON :: Network -> Value -> Parser Address
addrFromJSON net =
    withText "address" $ \t ->
        case textToAddr net t of
            Nothing -> fail "could not decode address"
            Just x -> return x

{- | Convert address to human-readable string. Uses 'Base58', 'Bech32', or
 'CashAddr' depending on network.
-}
addrToText :: Network -> Address -> Maybe Text
addrToText net a@PubKeyAddress{getAddrHash160 = h}
    | isNothing (getCashAddrPrefix net) =
        Just . encodeBase58Check . runPutS $ base58put net a
    | otherwise = cashAddrEncode net 0 (runPutS $ serialize h)
addrToText net a@ScriptAddress{getAddrHash160 = h}
    | isNothing (getCashAddrPrefix net) =
        Just . encodeBase58Check . runPutS $ base58put net a
    | otherwise =
        cashAddrEncode net 1 (runPutS $ serialize h)
addrToText net WitnessPubKeyAddress{getAddrHash160 = h} = do
    hrp <- getBech32Prefix net
    segwitEncode hrp 0 (B.unpack (runPutS $ serialize h))
addrToText net WitnessScriptAddress{getAddrHash256 = h} = do
    hrp <- getBech32Prefix net
    segwitEncode hrp 0 (B.unpack (runPutS $ serialize h))
addrToText net WitnessAddress{getAddrVersion = v, getAddrData = d} = do
    hrp <- getBech32Prefix net
    segwitEncode hrp v (B.unpack d)

-- | Parse 'Base58', 'Bech32' or 'CashAddr' address, depending on network.
textToAddr :: Network -> Text -> Maybe Address
textToAddr net txt =
    cashToAddr net txt <|> bech32ToAddr net txt <|> base58ToAddr net txt

cashToAddr :: Network -> Text -> Maybe Address
cashToAddr net txt = do
    (ver, bs) <- cashAddrDecode net txt
    case ver of
        0 -> PubKeyAddress <$> eitherToMaybe (runGetS deserialize bs)
        1 -> ScriptAddress <$> eitherToMaybe (runGetS deserialize bs)
        _ -> Nothing

bech32ToAddr :: Network -> Text -> Maybe Address
bech32ToAddr net txt = do
    hrp <- getBech32Prefix net
    (ver, bs) <- second B.pack <$> segwitDecode hrp txt
    case ver of
        0 -> case B.length bs of
            20 -> WitnessPubKeyAddress <$> eitherToMaybe (runGetS deserialize bs)
            32 -> WitnessScriptAddress <$> eitherToMaybe (runGetS deserialize bs)
            _ -> Nothing
        _ -> Just $ WitnessAddress ver bs

base58ToAddr :: Network -> Text -> Maybe Address
base58ToAddr net txt =
    eitherToMaybe . runGetS (base58get net) =<< decodeBase58Check txt

base58get :: MonadGet m => Network -> m Address
base58get net = do
    pfx <- getWord8
    addr <- deserialize
    f pfx addr
  where
    f x a
        | x == getAddrPrefix net = return $ PubKeyAddress a
        | x == getScriptPrefix net = return $ ScriptAddress a
        | otherwise = fail "Does not recognize address prefix"

base58put :: MonadPut m => Network -> Address -> m ()
base58put net (PubKeyAddress h) = do
    putWord8 (getAddrPrefix net)
    serialize h
base58put net (ScriptAddress h) = do
    putWord8 (getScriptPrefix net)
    serialize h
base58put _ _ = error "Cannot serialize this address as Base58"

-- | Obtain a standard pay-to-public-key-hash address from a public key.
pubKeyAddr :: PubKeyI -> Address
pubKeyAddr = PubKeyAddress . addressHash . runPutS . serialize

-- | Obtain a standard pay-to-public-key-hash (P2PKH) address from a 'Hash160'.
p2pkhAddr :: Hash160 -> Address
p2pkhAddr = PubKeyAddress

{- | Obtain a SegWit pay-to-witness-public-key-hash (P2WPKH) address from a
 public key.
-}
pubKeyWitnessAddr :: PubKeyI -> Address
pubKeyWitnessAddr = WitnessPubKeyAddress . addressHash . runPutS . serialize

-- | Obtain a backwards-compatible SegWit P2SH-P2WPKH address from a public key.
pubKeyCompatWitnessAddr :: PubKeyI -> Address
pubKeyCompatWitnessAddr =
    p2shAddr
        . addressHash
        . encodeOutputBS
        . PayWitnessPKHash
        . addressHash
        . runPutS
        . serialize

{- | Obtain a SegWit pay-to-witness-public-key-hash (P2WPKH) address from a
 'Hash160'.
-}
p2wpkhAddr :: Hash160 -> Address
p2wpkhAddr = WitnessPubKeyAddress

-- | Obtain a standard pay-to-script-hash (P2SH) address from a 'Hash160'.
p2shAddr :: Hash160 -> Address
p2shAddr = ScriptAddress

-- | Obtain a SegWit pay-to-witness-script-hash (P2WSH) address from a 'Hash256'
p2wshAddr :: Hash256 -> Address
p2wshAddr = WitnessScriptAddress

-- | Compute a standard pay-to-script-hash (P2SH) address for an output script.
payToScriptAddress :: ScriptOutput -> Address
payToScriptAddress = p2shAddr . addressHash . encodeOutputBS

{- | Compute a SegWit pay-to-witness-script-hash (P2WSH) address for an output
 script.
-}
payToWitnessScriptAddress :: ScriptOutput -> Address
payToWitnessScriptAddress = p2wshAddr . sha256 . encodeOutputBS

-- | Compute a backwards-compatible SegWit P2SH-P2WSH address.
payToNestedScriptAddress :: ScriptOutput -> Address
payToNestedScriptAddress =
    p2shAddr . addressHash . encodeOutputBS . toP2WSH . encodeOutput

{- | Encode an output script from an address. Will fail if using a
 pay-to-witness address on a non-SegWit network.
-}
addressToOutput :: Address -> ScriptOutput
addressToOutput =
    \case
        PubKeyAddress h -> PayPKHash h
        ScriptAddress h -> PayScriptHash h
        WitnessPubKeyAddress h -> PayWitnessPKHash h
        WitnessScriptAddress h -> PayWitnessScriptHash h
        WitnessAddress v d -> PayWitness v d

-- | Get output script AST for an 'Address'.
addressToScript :: Address -> Script
addressToScript = encodeOutput . addressToOutput

-- | Encode address as output script in 'ByteString' form.
addressToScriptBS :: Address -> ByteString
addressToScriptBS = runPutS . serialize . addressToScript

-- | Decode an output script into an 'Address' if it has such representation.
scriptToAddress :: Script -> Either String Address
scriptToAddress =
    maybeToEither "Could not decode address" . outputAddress <=< decodeOutput

-- | Decode a serialized script into an 'Address'.
scriptToAddressBS :: ByteString -> Either String Address
scriptToAddressBS =
    maybeToEither "Could not decode address" . outputAddress <=< decodeOutputBS

-- | Get the 'Address' of a 'ScriptOutput'.
outputAddress :: ScriptOutput -> Maybe Address
outputAddress =
    \case
        PayPKHash h -> Just $ PubKeyAddress h
        PayScriptHash h -> Just $ ScriptAddress h
        PayPK k -> Just $ pubKeyAddr k
        PayWitnessPKHash h -> Just $ WitnessPubKeyAddress h
        PayWitnessScriptHash h -> Just $ WitnessScriptAddress h
        PayWitness v d -> Just $ WitnessAddress v d
        _ -> Nothing

-- | Infer the 'Address' of a 'ScriptInput'.
inputAddress :: ScriptInput -> Maybe Address
inputAddress =
    \case
        (RegularInput (SpendPKHash _ key)) -> Just $ pubKeyAddr key
        (ScriptHashInput _ rdm) -> Just $ payToScriptAddress rdm
        _ -> Nothing
