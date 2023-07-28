{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module      : Haskoin.Address
-- Copyright   : No rights reserved
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Base58, CashAddr, Bech32 address and WIF private key serialization support.
module Haskoin.Address
  ( -- * Addresses
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
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (second)
import Control.DeepSeq (NFData)
import Control.Monad ((<=<))
import Crypto.Secp256k1
import Data.Aeson (ToJSON (toJSON), Value, withText)
import Data.Aeson.Encoding (Encoding, null_, text)
import Data.Aeson.Types (Encoding, Parser, ToJSON (toJSON), Value, withText)
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Bytes.Get (MonadGet (getByteString, getWord64be, getWord8), runGetS)
import Data.Bytes.Put (MonadPut (putByteString, putWord64be, putWord8), runPutS)
import Data.Bytes.Serial (Serial (..))
import Data.Hashable (Hashable)
import Data.Maybe (isNothing)
import Data.Serialize (Serialize (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import GHC.Generics (Generic)
import Haskoin.Address.Base58
import Haskoin.Address.Bech32
import Haskoin.Address.CashAddr
import Haskoin.Crypto.Hash
import Haskoin.Crypto.Keys.Common
import Haskoin.Network.Data
import Haskoin.Script.Common
import Haskoin.Script.Standard
import Haskoin.Util

-- | Address format for Bitcoin and Bitcoin Cash.
data Address
  = -- | pay to public key hash (regular)
    PubKeyAddress
      { -- | RIPEMD160 hash of public key's SHA256 hash
        hash160 :: !Hash160
      }
  | -- | pay to script hash
    ScriptAddress
      { -- | RIPEMD160 hash of script's SHA256 hash
        hash160 :: !Hash160
      }
  | -- | pay to witness public key hash
    WitnessPubKeyAddress
      { -- | RIPEMD160 hash of public key's SHA256 hash
        hash160 :: !Hash160
      }
  | -- | pay to witness script hash
    WitnessScriptAddress
      { -- | HASH256 hash of script
        hash256 :: !Hash256
      }
  | -- | other witness address
    WitnessAddress
      { version :: !Word8,
        bytes :: !ByteString
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
        WitnessAddress
          <$> getWord8
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
isPubKeyAddress PubKeyAddress {} = True
isPubKeyAddress _ = False

-- | 'Address' pays to a script hash.
isScriptAddress :: Address -> Bool
isScriptAddress ScriptAddress {} = True
isScriptAddress _ = False

-- | 'Address' pays to a witness public key hash. Only valid for SegWit
-- networks.
isWitnessPubKeyAddress :: Address -> Bool
isWitnessPubKeyAddress WitnessPubKeyAddress {} = True
isWitnessPubKeyAddress _ = False

isWitnessScriptAddress :: Address -> Bool
isWitnessScriptAddress WitnessScriptAddress {} = True
isWitnessScriptAddress _ = False

isWitnessAddress :: Address -> Bool
isWitnessAddress WitnessAddress {} = True
isWitnessAddress _ = False

instance MarshalJSON Network Address where
  marshalValue net a = toJSON (addrToText net a)
  marshalEncoding net = maybe null_ text . addrToText net
  unmarshalValue net =
    withText "address" $ \t ->
      case textToAddr net t of
        Nothing -> fail "could not decode address"
        Just x -> return x

-- | Convert address to human-readable string. Uses 'Base58', 'Bech32', or
-- 'CashAddr' depending on network.
addrToText :: Network -> Address -> Maybe Text
addrToText net a@PubKeyAddress {hash160 = h}
  | isNothing net.cashAddrPrefix =
      Just . encodeBase58Check . runPutS $ base58put net a
  | otherwise = cashAddrEncode net 0 (runPutS $ serialize h)
addrToText net a@ScriptAddress {hash160 = h}
  | isNothing net.cashAddrPrefix =
      Just . encodeBase58Check . runPutS $ base58put net a
  | otherwise =
      cashAddrEncode net 1 (runPutS $ serialize h)
addrToText net WitnessPubKeyAddress {hash160 = h} = do
  hrp <- net.bech32Prefix
  segwitEncode hrp 0 (B.unpack (runPutS $ serialize h))
addrToText net WitnessScriptAddress {hash256 = h} = do
  hrp <- net.bech32Prefix
  segwitEncode hrp 0 (B.unpack (runPutS $ serialize h))
addrToText net WitnessAddress {version = v, bytes = d} = do
  hrp <- net.bech32Prefix
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
  hrp <- net.bech32Prefix
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

base58get :: (MonadGet m) => Network -> m Address
base58get net = do
  pfx <- getWord8
  addr <- deserialize
  f pfx addr
  where
    f x a
      | x == net.addrPrefix = return $ PubKeyAddress a
      | x == net.scriptPrefix = return $ ScriptAddress a
      | otherwise = fail "Does not recognize address prefix"

base58put :: (MonadPut m) => Network -> Address -> m ()
base58put net (PubKeyAddress h) = do
  putWord8 net.addrPrefix
  serialize h
base58put net (ScriptAddress h) = do
  putWord8 net.scriptPrefix
  serialize h
base58put _ _ = error "Cannot serialize this address as Base58"

-- | Obtain a standard pay-to-public-key-hash address from a public key.
pubKeyAddr :: Ctx -> PublicKey -> Address
pubKeyAddr ctx = PubKeyAddress . addressHash . marshal ctx

-- | Obtain a standard pay-to-public-key-hash (P2PKH) address from a 'Hash160'.
p2pkhAddr :: Hash160 -> Address
p2pkhAddr = PubKeyAddress

-- | Obtain a SegWit pay-to-witness-public-key-hash (P2WPKH) address from a
-- public key.
pubKeyWitnessAddr :: Ctx -> PublicKey -> Address
pubKeyWitnessAddr ctx =
  WitnessPubKeyAddress . addressHash . marshal ctx

-- | Obtain a backwards-compatible SegWit P2SH-P2WPKH address from a public key.
pubKeyCompatWitnessAddr :: Ctx -> PublicKey -> Address
pubKeyCompatWitnessAddr ctx =
  p2shAddr
    . addressHash
    . marshal ctx
    . PayWitnessPKHash
    . addressHash
    . marshal ctx

-- | Obtain a SegWit pay-to-witness-public-key-hash (P2WPKH) address from a
-- 'Hash160'.
p2wpkhAddr :: Hash160 -> Address
p2wpkhAddr = WitnessPubKeyAddress

-- | Obtain a standard pay-to-script-hash (P2SH) address from a 'Hash160'.
p2shAddr :: Hash160 -> Address
p2shAddr = ScriptAddress

-- | Obtain a SegWit pay-to-witness-script-hash (P2WSH) address from a 'Hash256'
p2wshAddr :: Hash256 -> Address
p2wshAddr = WitnessScriptAddress

-- | Compute a standard pay-to-script-hash (P2SH) address for an output script.
payToScriptAddress :: Ctx -> ScriptOutput -> Address
payToScriptAddress ctx = p2shAddr . addressHash . marshal ctx

-- | Compute a SegWit pay-to-witness-script-hash (P2WSH) address for an output
-- script.
payToWitnessScriptAddress :: Ctx -> ScriptOutput -> Address
payToWitnessScriptAddress ctx = p2wshAddr . sha256 . marshal ctx

-- | Compute a backwards-compatible SegWit P2SH-P2WSH address.
payToNestedScriptAddress :: Ctx -> ScriptOutput -> Address
payToNestedScriptAddress ctx =
  p2shAddr . addressHash . marshal ctx . toP2WSH . encodeOutput ctx

-- | Encode an output script from an address. Will fail if using a
-- pay-to-witness address on a non-SegWit network.
addressToOutput :: Address -> ScriptOutput
addressToOutput =
  \case
    PubKeyAddress h -> PayPKHash h
    ScriptAddress h -> PayScriptHash h
    WitnessPubKeyAddress h -> PayWitnessPKHash h
    WitnessScriptAddress h -> PayWitnessScriptHash h
    WitnessAddress v d -> PayWitness v d

-- | Get output script AST for an 'Address'.
addressToScript :: Ctx -> Address -> Script
addressToScript ctx = encodeOutput ctx . addressToOutput

-- | Encode address as output script in 'ByteString' form.
addressToScriptBS :: Ctx -> Address -> ByteString
addressToScriptBS ctx = runPutS . serialize . addressToScript ctx

-- | Decode an output script into an 'Address' if it has such representation.
scriptToAddress :: Ctx -> Script -> Either String Address
scriptToAddress ctx =
  maybeToEither e . outputAddress ctx <=< decodeOutput ctx
  where
    e = "Could not decode address"

-- | Decode a serialized script into an 'Address'.
scriptToAddressBS :: Ctx -> ByteString -> Either String Address
scriptToAddressBS ctx =
  maybeToEither e . outputAddress ctx <=< unmarshal ctx
  where
    e = "Could not decode address"

-- | Get the 'Address' of a 'ScriptOutput'.
outputAddress :: Ctx -> ScriptOutput -> Maybe Address
outputAddress ctx =
  \case
    PayPKHash h -> Just $ PubKeyAddress h
    PayScriptHash h -> Just $ ScriptAddress h
    PayPK k -> Just $ pubKeyAddr ctx k
    PayWitnessPKHash h -> Just $ WitnessPubKeyAddress h
    PayWitnessScriptHash h -> Just $ WitnessScriptAddress h
    PayWitness v d -> Just $ WitnessAddress v d
    _ -> Nothing

-- | Infer the 'Address' of a 'ScriptInput'.
inputAddress :: Ctx -> ScriptInput -> Maybe Address
inputAddress ctx =
  \case
    (RegularInput (SpendPKHash _ key)) -> Just $ pubKeyAddr ctx key
    (ScriptHashInput _ rdm) -> Just $ payToScriptAddress ctx rdm
    _ -> Nothing
