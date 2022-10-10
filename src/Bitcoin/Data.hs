{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bitcoin.Data (
    Network (..),
) where

import Bitcoin.Block.Common (BlockHash, BlockHeader, BlockHeight)
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32, Word64, Word8)
import GHC.Generics (Generic)


-- | Network definition.
data Network = Network
    { getNetworkName :: !String
    -- ^ lowercase alphanumeric and dashes
    , getAddrPrefix :: !Word8
    -- ^ prefix for 'Base58' P2PKH addresses
    , getScriptPrefix :: !Word8
    -- ^ prefix for 'Base58' P2SH addresses
    , getSecretPrefix :: !Word8
    -- ^ prefix for WIF private key
    , getExtPubKeyPrefix :: !Word32
    -- ^ prefix for extended public key
    , getExtSecretPrefix :: !Word32
    -- ^ prefix for extended private key
    , getNetworkMagic :: !Word32
    -- ^ network magic
    , getGenesisHeader :: !BlockHeader
    -- ^ genesis block header
    , getMaxBlockSize :: !Int
    -- ^ maximum block size in bytes
    , getMaxSatoshi :: !Word64
    -- ^ maximum amount of satoshi
    , getBitcoinUserAgent :: !ByteString
    -- ^ user agent string
    , getDefaultPort :: !Int
    -- ^ default port for P2P connections
    , getAllowMinDifficultyBlocks :: !Bool
    -- ^ allow min difficulty blocks (testnet)
    , getPowNoRetargetting :: !Bool
    -- ^ do not retarget difficulty (regtest)
    , getPowLimit :: !Integer
    -- ^ proof-of-work target higest possible value
    , getBip34Block :: !(BlockHeight, BlockHash)
    -- ^ block at which BIP34 activates
    , getBip65Height :: !BlockHeight
    -- ^ block at which BIP65 activates
    , getBip66Height :: !BlockHeight
    -- ^ block at which BIP66 activates
    , getTargetTimespan :: !Word32
    -- ^ time between difficulty retargets
    , getTargetSpacing :: !Word32
    -- ^ time between blocks
    , getCheckpoints :: ![(BlockHeight, BlockHash)]
    -- ^ checkpoints
    , getBip44Coin :: !Word32
    -- ^ BIP44 derivation path root
    , getSeeds :: ![String]
    -- ^ peer-to-peer network seeds
    , getEdaBlockHeight :: !(Maybe Word32)
    -- ^ EDA start block height
    , getDaaBlockHeight :: !(Maybe Word32)
    -- ^ DAA start block height
    , getSegWit :: !Bool
    -- ^ segregated witness active
    , getBech32Prefix :: !(Maybe Text)
    -- ^ 'Bech32' prefix (for SegWit network)
    , getReplaceByFee :: !Bool
    -- ^ Replace-By-Fee (BIP-125)
    , getHalvingInterval :: !Word32
    -- ^ Subsidy halving interval
    }
    deriving (Eq, Show, Read, Generic, NFData)
