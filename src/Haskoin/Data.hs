{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Haskoin.Data (
    Network(..),
) where

import Control.DeepSeq
import Data.Binary (Binary (..))
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.ByteString (ByteString)
import Data.List
import Data.Serialize (Serialize (..))
import Data.String
import Data.Text (Text)
import Data.Word (Word8, Word32, Word64)
import GHC.Generics (Generic)
import Haskoin.Block.Common
import Text.Read

-- | Network definition.
data Network = Network
    { -- | lowercase alphanumeric and dashes
      getNetworkName :: !String
    , -- | prefix for 'Base58' P2PKH addresses
      getAddrPrefix :: !Word8
    , -- | prefix for 'Base58' P2SH addresses
      getScriptPrefix :: !Word8
    , -- | prefix for WIF private key
      getSecretPrefix :: !Word8
    , -- | prefix for extended public key
      getExtPubKeyPrefix :: !Word32
    , -- | prefix for extended private key
      getExtSecretPrefix :: !Word32
    , -- | network magic
      getNetworkMagic :: !Word32
    , -- | genesis block header
      getGenesisHeader :: !BlockHeader
    , -- | maximum block size in bytes
      getMaxBlockSize :: !Int
    , -- | maximum amount of satoshi
      getMaxSatoshi :: !Word64
    , -- | user agent string
      getHaskoinUserAgent :: !ByteString
    , -- | default port for P2P connections
      getDefaultPort :: !Int
    , -- | allow min difficulty blocks (testnet)
      getAllowMinDifficultyBlocks :: !Bool
    , -- | do not retarget difficulty (regtest)
      getPowNoRetargetting :: !Bool
    , -- | proof-of-work target higest possible value
      getPowLimit :: !Integer
    , -- | block at which BIP34 activates
      getBip34Block :: !(BlockHeight, BlockHash)
    , -- | block at which BIP65 activates
      getBip65Height :: !BlockHeight
    , -- | block at which BIP66 activates
      getBip66Height :: !BlockHeight
    , -- | time between difficulty retargets
      getTargetTimespan :: !Word32
    , -- | time between blocks
      getTargetSpacing :: !Word32
    , -- | checkpoints
      getCheckpoints :: ![(BlockHeight, BlockHash)]
    , -- | BIP44 derivation path root
      getBip44Coin :: !Word32
    , -- | peer-to-peer network seeds
      getSeeds :: ![String]
    , -- | fork id for replay protection
      getSigHashForkId :: !(Maybe Word32)
    , -- | EDA start block height
      getEdaBlockHeight :: !(Maybe Word32)
    , -- | DAA start block height
      getDaaBlockHeight :: !(Maybe Word32)
    , -- | asert3-2d algorithm activation time
      -- TODO: Replace with block height after fork
      getAsertActivationTime :: !(Maybe Word32)
    , -- | asert3-2d algorithm halflife (not used for non-BCH networks)
      getAsertHalfLife :: !Integer
    , -- | segregated witness active
      getSegWit :: !Bool
    , -- | 'CashAddr' prefix (for Bitcoin Cash)
      getCashAddrPrefix :: !(Maybe Text)
    , -- | 'Bech32' prefix (for SegWit network)
      getBech32Prefix :: !(Maybe Text)
    , -- | Replace-By-Fee (BIP-125)
      getReplaceByFee :: !Bool
    , -- | Subsidy halving interval
      getHalvingInterval :: !Word32
    }
    deriving (Eq, Show, Read, Generic, NFData)
