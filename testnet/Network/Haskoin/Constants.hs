{-|
  Network specific constants
-}
module Network.Haskoin.Constants where

import Data.Word (Word8,Word32)
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Protocol.Types

-- | Name of the bitcoin network
networkName :: String
networkName = "testnet"

-- | Prefix for base58 PubKey hash address
addrPrefix :: Word8
addrPrefix = 111

-- | Prefix for base58 script hash address
scriptPrefix :: Word8
scriptPrefix = 196

-- | Prefix for private key WIF format
secretPrefix :: Word8
secretPrefix = 239

-- | Prefix for extended public keys (BIP32)
extPubKeyPrefix :: Word32
extPubKeyPrefix = 0x043587cf

-- | Prefix for extended private keys (BIP32)
extSecretPrefix :: Word32
extSecretPrefix = 0x04358394

-- | Network magic bytes
networkMagic :: Word32
networkMagic = 0x0b110907 

-- | Genesis block header information
genesisHeader :: BlockHeader
genesisHeader = BlockHeader
    -- Hash = 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
    { blockVersion   = 0x01
    , prevBlock      = 0x00
    , merkleRoot     = 
        0x3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a
    , blockTimestamp = 1296688602
    , blockBits      = 486604799
    , bhNonce        = 414098458
    }

-- | Maximum size of a block in bytes
maxBlockSize :: Int
maxBlockSize = 1000000

-- | User agent of this haskoin package
haskoinUserAgent :: String
haskoinUserAgent = "/haskoin-testnet:0.1.0.2/"

-- | Default port
defaultPort :: Int
defaultPort = 18333

-- | Should we allow relaxed difficulty transition rules. This is useful
-- for testnet.
allowMinDifficultyBlocks :: Bool
allowMinDifficultyBlocks = True

-- | Checkpoints to enfore
checkpoints :: [(Int, BlockHash)]
checkpoints =
    -- These are in little endian notation!
    [ (546, 0x000000002a936ca763904c3c35fce2f3556c559c0214345d31b1bcebf76acb70) ]

