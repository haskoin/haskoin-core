{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- Base58, Bech32 address and WIF private key serialization support.
module Bitcoin.Address (
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
    module Bitcoin.Address.Base58,
    module Bitcoin.Address.Bech32,
) where

import Bitcoin.Address.Base58
import Bitcoin.Address.Bech32
import Bitcoin.Crypto (Hash160, Hash256, addressHash, addressHashL, sha256)
import Bitcoin.Data (Network (..))
import Bitcoin.Keys.Common (PubKeyI)
import Bitcoin.Script (
    Script,
    ScriptInput (..),
    ScriptOutput (..),
    SimpleInput (SpendPKHash),
    decodeOutput,
    decodeOutputBS,
    encodeOutput,
    encodeOutputBS,
    toP2WSH,
 )
import Bitcoin.Util (eitherToMaybe, encodeHex, maybeToEither)
import qualified Bitcoin.Util as U
import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.DeepSeq (NFData)
import Control.Monad ((<=<))
import Data.Binary (Binary (..), Get, Put)
import qualified Data.Binary as Bin
import Data.Binary.Get (runGet)
import qualified Data.Binary.Get as Get
import Data.Binary.Put (runPut)
import qualified Data.Binary.Put as Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Generics (Generic)


-- | Address format for Bitcoin
data Address
    = -- | pay to public key hash (regular)
      PubKeyAddress
        { getAddrHash160 :: !Hash160
        -- ^ RIPEMD160 hash of public key's SHA256 hash
        }
    | -- | pay to script hash
      ScriptAddress
        { getAddrHash160 :: !Hash160
        -- ^ RIPEMD160 hash of script's SHA256 hash
        }
    | -- | pay to witness public key hash
      WitnessPubKeyAddress
        { getAddrHash160 :: !Hash160
        -- ^ RIPEMD160 hash of public key's SHA256 hash
        }
    | -- | pay to witness script hash
      WitnessScriptAddress
        { getAddrHash256 :: !Hash256
        -- ^ HASH256 hash of script
        }
    | -- | other witness address
      WitnessAddress
        { getAddrVersion :: !Word8
        , getAddrData :: !ByteString
        }
    deriving
        (Eq, Ord, Generic, Show, Read, Hashable, NFData)


instance Binary Address where
    put = \case
        PubKeyAddress k -> do
            Put.putWord8 0x00
            put k
        ScriptAddress s -> do
            Put.putWord8 0x01
            put s
        WitnessPubKeyAddress h -> do
            Put.putWord8 0x02
            put h
        WitnessScriptAddress s -> do
            Put.putWord8 0x03
            put s
        WitnessAddress v d -> do
            Put.putWord8 0x04
            Put.putWord8 v
            Put.putWord64be (fromIntegral (BS.length d))
            Put.putByteString d


    get =
        Get.getWord8 >>= \case
            0x00 -> PubKeyAddress <$> get
            0x01 -> ScriptAddress <$> get
            0x02 -> WitnessPubKeyAddress <$> get
            0x03 -> WitnessScriptAddress <$> get
            0x04 ->
                WitnessAddress
                    <$> Get.getWord8
                    <*> (Get.getByteString . fromIntegral =<< Get.getWord64be)
            b ->
                fail . T.unpack $
                    "Could not decode address type byte: "
                        <> encodeHex (BS.singleton b)


-- | 'Address' pays to a public key hash.
isPubKeyAddress :: Address -> Bool
isPubKeyAddress PubKeyAddress{} = True
isPubKeyAddress _ = False


-- | 'Address' pays to a script hash.
isScriptAddress :: Address -> Bool
isScriptAddress ScriptAddress{} = True
isScriptAddress _ = False


-- | 'Address' pays to a witness public key hash. Only valid for SegWit
-- networks.
isWitnessPubKeyAddress :: Address -> Bool
isWitnessPubKeyAddress WitnessPubKeyAddress{} = True
isWitnessPubKeyAddress _ = False


isWitnessScriptAddress :: Address -> Bool
isWitnessScriptAddress WitnessScriptAddress{} = True
isWitnessScriptAddress _ = False


isWitnessAddress :: Address -> Bool
isWitnessAddress WitnessAddress{} = True
isWitnessAddress _ = False


-- | Convert address to human-readable string. Uses 'Base58', or 'Bech32'
-- depending on network.
addrToText :: Network -> Address -> Maybe Text
addrToText net a@PubKeyAddress{} = Just . encodeBase58Check . Put.runPut $ base58put net a
addrToText net a@ScriptAddress{} = Just . encodeBase58Check . Put.runPut $ base58put net a
addrToText net WitnessPubKeyAddress{getAddrHash160 = h} = do
    hrp <- getBech32Prefix net
    segwitEncode hrp 0 . BSL.unpack $ Bin.encode h
addrToText net WitnessScriptAddress{getAddrHash256 = h} = do
    hrp <- getBech32Prefix net
    segwitEncode hrp 0 . BSL.unpack $ Bin.encode h
addrToText net WitnessAddress{getAddrVersion = v, getAddrData = d} = do
    hrp <- getBech32Prefix net
    segwitEncode hrp v (BS.unpack d)


-- | Parse 'Base58', or 'Bech32' address, depending on network.
textToAddr :: Network -> Text -> Maybe Address
textToAddr net txt =
    bech32ToAddr net txt <|> base58ToAddr net txt


bech32ToAddr :: Network -> Text -> Maybe Address
bech32ToAddr net txt = do
    hrp <- getBech32Prefix net
    (ver, bs) <- second BS.pack <$> segwitDecode hrp txt
    case ver of
        0 -> case BS.length bs of
            20 -> WitnessPubKeyAddress <$> (eitherToMaybe . U.decode . BSL.fromStrict) bs
            32 -> WitnessScriptAddress <$> (eitherToMaybe . U.decode . BSL.fromStrict) bs
            _ -> Nothing
        _ -> Just $ WitnessAddress ver bs


base58ToAddr :: Network -> Text -> Maybe Address
base58ToAddr net txt =
    eitherToMaybe . U.runGet (base58get net) =<< decodeBase58Check txt


base58get :: Network -> Get Address
base58get net = do
    pfx <- Get.getWord8
    addr <- get
    f pfx addr
  where
    f x a
        | x == getAddrPrefix net = return $ PubKeyAddress a
        | x == getScriptPrefix net = return $ ScriptAddress a
        | otherwise = fail "Does not recognize address prefix"


base58put :: Network -> Address -> Put
base58put net (PubKeyAddress h) = do
    Put.putWord8 (getAddrPrefix net)
    put h
base58put net (ScriptAddress h) = do
    Put.putWord8 (getScriptPrefix net)
    put h
base58put _ _ = error "Cannot serialize this address as Base58"


-- | Obtain a standard pay-to-public-key-hash address from a public key.
pubKeyAddr :: PubKeyI -> Address
pubKeyAddr = PubKeyAddress . addressHashL . Bin.encode


-- | Obtain a standard pay-to-public-key-hash (P2PKH) address from a 'Hash160'.
p2pkhAddr :: Hash160 -> Address
p2pkhAddr = PubKeyAddress


-- | Obtain a SegWit pay-to-witness-public-key-hash (P2WPKH) address from a
-- public key.
pubKeyWitnessAddr :: PubKeyI -> Address
pubKeyWitnessAddr = WitnessPubKeyAddress . addressHashL . Bin.encode


-- | Obtain a backwards-compatible SegWit P2SH-P2WPKH address from a public key.
pubKeyCompatWitnessAddr :: PubKeyI -> Address
pubKeyCompatWitnessAddr =
    p2shAddr
        . addressHash
        . encodeOutputBS
        . PayWitnessPKHash
        . addressHashL
        . Bin.encode


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
payToScriptAddress :: ScriptOutput -> Address
payToScriptAddress = p2shAddr . addressHash . encodeOutputBS


-- | Compute a SegWit pay-to-witness-script-hash (P2WSH) address for an output
-- script.
payToWitnessScriptAddress :: ScriptOutput -> Address
payToWitnessScriptAddress = p2wshAddr . sha256 . encodeOutputBS


-- | Compute a backwards-compatible SegWit P2SH-P2WSH address.
payToNestedScriptAddress :: ScriptOutput -> Address
payToNestedScriptAddress =
    p2shAddr . addressHash . encodeOutputBS . toP2WSH . encodeOutput


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
addressToScript :: Address -> Script
addressToScript = encodeOutput . addressToOutput


-- | Encode address as output script in 'ByteString' form.
addressToScriptBS :: Address -> ByteString
addressToScriptBS = U.encodeS . addressToScript


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
