{-|
  Network specific constants
-}
module Network.Haskoin.Constants where

import Data.Word (Word8,Word32)
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Protocol.Types

-- | Name of the bitcoin network
networkName :: String
networkName = "prodnet"

-- | Prefix for base58 PubKey hash address
addrPrefix :: Word8
addrPrefix = 0

-- | Prefix for base58 script hash address
scriptPrefix :: Word8
scriptPrefix = 5

-- | Prefix for private key WIF format
secretPrefix :: Word8
secretPrefix = 128

-- | Prefix for extended public keys (BIP32)
extPubKeyPrefix :: Word32
extPubKeyPrefix = 0x0488b21e

-- | Prefix for extended private keys (BIP32)
extSecretPrefix :: Word32
extSecretPrefix = 0x0488ade4

-- | Network magic bytes
networkMagic :: Word32
networkMagic = 0xf9beb4d9 

-- | Genesis block header information
genesisHeader :: BlockHeader
genesisHeader = BlockHeader
    -- Hash = 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    { blockVersion   = 0x01
    , prevBlock      = 0x00
    , merkleRoot     = 
        0x3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a
    , blockTimestamp = 1231006505
    , blockBits      = 486604799
    , bhNonce        = 2083236893
    }

-- | Maximum size of a block in bytes
maxBlockSize :: Int
maxBlockSize = 1000000

-- | User agent of this haskoin package
haskoinUserAgent :: String
haskoinUserAgent = "/haskoin:0.1.0/"

-- | Default port
defaultPort :: Int
defaultPort = 8333

-- | Should we allow relaxed difficulty transition rules. This is useful
-- for testnet.
allowMinDifficultyBlocks :: Bool
allowMinDifficultyBlocks = False

-- | Checkpoints to enfore
checkpoints :: [(Int, BlockHash)]
checkpoints =
    -- These are in little endian notation!
    [ ( 11111, 0x0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d)
    , ( 33333, 0x000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6)
    , ( 74000, 0x0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20)
    , (105000, 0x00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97)
    , (134444, 0x00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe)
    , (168000, 0x000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763)
    , (193000, 0x000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317)
    , (210000, 0x000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e)
    , (216116, 0x00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e)
    , (225430, 0x00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932)
    , (250000, 0x000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214)
    , (279000, 0x0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40)
    ]

