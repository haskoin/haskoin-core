{-|
  Network specific constants
-}
module Network.Haskoin.Network where

import Data.Bits (shiftR)
import Data.Word (Word8, Word32, Word64)
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Block.Types

data Prodnet
data Testnet

class Network a where
    -- | Name of the bitcoin network
    networkName :: a -> String
    -- | Prefix for base58 PubKey hash address
    addrPrefix :: a -> Word8
    -- | Prefix for base58 script hash address
    scriptPrefix :: a -> Word8
    -- | Prefix for private key WIF format
    secretPrefix :: a -> Word8
    -- | Prefix for extended public keys (BIP32)
    extPubKeyPrefix :: a -> Word32
    -- | Prefix for extended private keys (BIP32)
    extSecretPrefix :: a -> Word32
    -- | Network magic bytes
    networkMagic :: a -> Word32
    -- | Genesis block header information
    genesisHeader :: a -> BlockHeader
    -- | Maximum size of a block in bytes
    maxBlockSize :: a -> Int
    -- | Maximum number of satoshis
    maxSatoshi :: a -> Word64
    -- | User agent of this haskoin package
    haskoinUserAgent :: a -> String
    -- | Default port
    defaultPort :: a -> Int
    -- | Should we allow relaxed difficulty transition rules. This is useful
    -- for testnet.
    allowMinDifficultyBlocks :: a -> Bool
    -- | Lower bound for the proof of work difficulty
    powLimit :: a -> Integer
    -- | Time between difficulty cycles (2 weeks on average)
    targetTimespan :: a -> Word32
    -- | Time between blocks (10 minutes per block)
    targetSpacing :: a -> Word32
    -- | Checkpoints to enfore
    checkpoints :: a -> [(Int, BlockHash)]

instance Network Prodnet where
    networkName _ = "prodnet"
    addrPrefix _ = 0
    scriptPrefix _ = 5
    secretPrefix _ = 128
    extPubKeyPrefix _ = 0x0488b21e
    extSecretPrefix _ = 0x0488ade4
    networkMagic _ = 0xf9beb4d9 
    genesisHeader _ = BlockHeader
        -- Hash :
        -- 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
        { blockVersion   = 0x01
        , prevBlock      = 0x00
        , merkleRoot     = 
            0x3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a
        , blockTimestamp = 1231006505
        , blockBits      = 486604799
        , bhNonce        = 2083236893
        }
    maxBlockSize _ = 1000000
    maxSatoshi _ = 2100000000000000
    haskoinUserAgent _ = "/haskoin:0.2.0/"
    defaultPort _ = 8333
    allowMinDifficultyBlocks _ = False
    powLimit _ = fromIntegral (maxBound `shiftR` 32 :: Word256)
    targetTimespan _ = 14 * 24 * 60 * 60
    targetSpacing _ = 10 * 60
    checkpoints _ =
        -- These are in little endian notation!
        [ ( 11111
          , 0x0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d
          )
        , ( 33333
          , 0x000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6
          )
        , ( 74000
          , 0x0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20
          )
        , ( 105000
          , 0x00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97
          )
        , ( 134444
          , 0x00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe
          )
        , ( 168000
          , 0x000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763
          )
        , ( 193000
          , 0x000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317
          )
        , ( 210000
          , 0x000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e
          )
        , ( 216116
          , 0x00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e
          )
        , ( 225430
          , 0x00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932
          )
        , ( 250000
          , 0x000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214
          )
        , ( 279000
          , 0x0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40
          )
        ]

instance Network Testnet where
    networkName _ = "testnet"
    addrPrefix _ = 111
    scriptPrefix _ = 196
    secretPrefix _ = 239
    extPubKeyPrefix _ = 0x043587cf
    extSecretPrefix _ = 0x04358394
    networkMagic _ = 0x0b110907 
    genesisHeader _ = BlockHeader
        -- Hash:
        -- 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
        { blockVersion   = 0x01
        , prevBlock      = 0x00
        , merkleRoot     = 
            0x3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a
        , blockTimestamp = 1296688602
        , blockBits      = 486604799
        , bhNonce        = 414098458
        }
    maxBlockSize _ = 1000000
    maxSatoshi _ = 2100000000000000
    haskoinUserAgent _ = "/haskoin-testnet:0.2.0/"
    defaultPort _ = 18333
    allowMinDifficultyBlocks _ = True
    powLimit _ = fromIntegral (maxBound `shiftR` 32 :: Word256)
    targetTimespan _ = 14 * 24 * 60 * 60
    targetSpacing _ = 10 * 60
    checkpoints _ =
        -- These are in little endian notation!
        [ ( 546
          , 0x000000002a936ca763904c3c35fce2f3556c559c0214345d31b1bcebf76acb70
          )
        ]

