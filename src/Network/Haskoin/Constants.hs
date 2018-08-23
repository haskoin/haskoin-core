{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Network specific constants
-}
module Network.Haskoin.Constants
( -- ** Data
  Network(..)
, btc
, btcTest
, btcRegTest
, bch
, bchTest
, bchRegTest
  -- ** Functions
, setBTC
, setBTCtest
, setBTCregTest
, setBCH
, setBCHtest
, setBCHregTest
, setNetwork
, getNetwork
  -- ** Network parameters
, networkName
, addrPrefix
, scriptPrefix
, secretPrefix
, extPubKeyPrefix
, extSecretPrefix
, networkMagic
, genesisHeader
, maxBlockSize
, maxSatoshi
, haskoinUserAgent
, defaultPort
, allowMinDifficultyBlocks
, powNoRetargetting
, powLimit
, bip34Block
, bip65Height
, bip66Height
, targetTimespan
, targetSpacing
, checkpoints
, bip44Coin
, seeds
, sigHashForkId
, edaBlockHeight
, daaBlockHeight
, segWit
) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as C8 (concat, pack)
import           Data.Maybe
import           Data.Version                (showVersion)
import           Data.Word                   (Word32, Word64, Word8)
import           Network.Haskoin.Block.Types
import           Paths_haskoin_core          (version)
import           System.IO.Unsafe            (unsafePerformIO)

data Network = Network
    { getNetworkName              :: !String
    , getAddrPrefix               :: !Word8
    , getScriptPrefix             :: !Word8
    , getSecretPrefix             :: !Word8
    , getExtPubKeyPrefix          :: !Word32
    , getExtSecretPrefix          :: !Word32
    , getNetworkMagic             :: !Word32
    , getGenesisHeader            :: !BlockHeader
    , getMaxBlockSize             :: !Int
    , getMaxSatoshi               :: !Word64
    , getHaskoinUserAgent         :: !ByteString
    , getDefaultPort              :: !Int
    , getAllowMinDifficultyBlocks :: !Bool
    , getPowNoRetargetting        :: !Bool
    , getPowLimit                 :: !Integer
    , getBip34Block               :: !(BlockHeight, BlockHash)
    , getBip65Height              :: !BlockHeight
    , getBip66Height              :: !BlockHeight
    , getTargetTimespan           :: !Word32
    , getTargetSpacing            :: !Word32
    , getCheckpoints              :: ![(BlockHeight, BlockHash)]
    , getBip44Coin                :: !Word32
    , getSeeds                    :: ![String]
    , getSigHashForkId            :: !(Maybe Word32)
    , getEDABlockHeight           :: !(Maybe Word32)
    , getDAABlockHeight           :: !(Maybe Word32)
    , getSegWit               :: !Bool
    } deriving (Eq)

setBTC :: IO ()
setBTC = setNetwork btc

setBTCtest :: IO ()
setBTCtest = setNetwork btcTest

setBTCregTest :: IO ()
setBTCregTest = setNetwork btcRegTest

setBCH :: IO ()
setBCH = setNetwork bch

setBCHtest :: IO ()
setBCHtest = setNetwork bchTest

setBCHregTest :: IO ()
setBCHregTest = setNetwork bchRegTest

setNetwork :: Network -> IO ()
setNetwork net = do
    success <- tryPutMVar networkMVar net
    unless success $ error "The network has already been set"

{-# NOINLINE networkMVar #-}
networkMVar :: MVar Network
networkMVar = unsafePerformIO newEmptyMVar

{-# NOINLINE getNetwork #-}
getNetwork :: Network
getNetwork =
    fromMaybe err $ unsafePerformIO $ tryTakeMVar networkMVar
  where
    err = error "Use Network.Haskoin.Constants.setNetwork"

networkName :: String
networkName = getNetworkName getNetwork

addrPrefix :: Word8
addrPrefix = getAddrPrefix getNetwork

scriptPrefix :: Word8
scriptPrefix = getScriptPrefix getNetwork

secretPrefix :: Word8
secretPrefix = getSecretPrefix getNetwork

extPubKeyPrefix :: Word32
extPubKeyPrefix = getExtPubKeyPrefix getNetwork

extSecretPrefix :: Word32
extSecretPrefix = getExtSecretPrefix getNetwork

networkMagic :: Word32
networkMagic = getNetworkMagic getNetwork

genesisHeader :: BlockHeader
genesisHeader = getGenesisHeader getNetwork

maxBlockSize :: Int
maxBlockSize = getMaxBlockSize getNetwork

maxSatoshi :: Word64
maxSatoshi = getMaxSatoshi getNetwork

haskoinUserAgent :: ByteString
haskoinUserAgent = getHaskoinUserAgent getNetwork

defaultPort :: Int
defaultPort = getDefaultPort getNetwork

allowMinDifficultyBlocks :: Bool
allowMinDifficultyBlocks = getAllowMinDifficultyBlocks getNetwork

powNoRetargetting :: Bool
powNoRetargetting = getPowNoRetargetting getNetwork

powLimit :: Integer
powLimit = getPowLimit getNetwork

-- | Version 2 blocks start here.
bip34Block :: (BlockHeight, BlockHash)
bip34Block = getBip34Block getNetwork

-- | Version 3 blocks start here.
bip65Height :: BlockHeight
bip65Height = getBip65Height getNetwork

-- | Version 4 blocks start here.
bip66Height :: BlockHeight
bip66Height = getBip66Height getNetwork

-- | Time between difficulty cycles (2 weeks on average).
targetTimespan :: Word32
targetTimespan = getTargetTimespan getNetwork

-- | Time between blocks (10 minutes per block).
targetSpacing :: Word32
targetSpacing = getTargetSpacing getNetwork

-- | Checkpoints to enfore.
checkpoints :: [(Word32, BlockHash)]
checkpoints = getCheckpoints getNetwork

-- | Bip44 coin derivation (m/44'/coin'/account'/internal/address/)
bip44Coin :: Word32
bip44Coin = getBip44Coin getNetwork

-- | DNS seeds.
seeds :: [String]
seeds = getSeeds getNetwork

-- | The Fork ID used for producing signatures on different networks.
sigHashForkId :: Maybe Word32
sigHashForkId = getSigHashForkId getNetwork

-- | EDA Block height. Used by Bitcoin Cash network.
edaBlockHeight :: Maybe Word32
edaBlockHeight = getEDABlockHeight getNetwork

-- | DAA Block height. Used by Bitcoin Cash network.
daaBlockHeight :: Maybe Word32
daaBlockHeight = getDAABlockHeight getNetwork

-- | Only connect to nodes advertising SegWit support.
segWit :: Bool
segWit = getSegWit getNetwork

btc :: Network
btc =
    Network
    { getNetworkName = "btc"
    , getAddrPrefix = 0
    , getScriptPrefix = 5
    , getSecretPrefix = 128
    , getExtPubKeyPrefix = 0x0488b21e
    , getExtSecretPrefix = 0x0488ade4
    , getNetworkMagic = 0xf9beb4d9
    , getGenesisHeader =
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1231006505
              0x1d00ffff
              2083236893
            -- Hash 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent =
          C8.concat ["/haskoin-btc:", C8.pack $ showVersion version, "/"]
    , getDefaultPort = 8333
    , getAllowMinDifficultyBlocks = False
    , getPowNoRetargetting = False
    , getPowLimit =
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 227931
          , "000000000000024b89b42a942fe0d9fea3bb44ab7bd1b19115dd6a759c0808b8")
    , getBip65Height = 388381
    , getBip66Height = 363725
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
          [ ( 11111
            , "0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d")
          , ( 33333
            , "000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6")
          , ( 74000
            , "0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20")
          , ( 105000
            , "00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97")
          , ( 134444
            , "00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe")
          , ( 168000
            , "000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763")
          , ( 193000
            , "000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317")
          , ( 210000
            , "000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e")
          , ( 216116
            , "00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e")
          , ( 225430
            , "00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932")
          , ( 250000
            , "000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214")
          , ( 279000
            , "0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40")
          , ( 295000
            , "00000000000000004d9b4ef50f0f9d686fd69db2e03af35a100370c64632a983")
          ]
    , getSeeds =
          [ "seed.bitcoin.sipa.be" -- Pieter Wuille
          , "dnsseed.bluematt.me" -- Matt Corallo
          , "dnsseed.bitcoin.dashjr.org" -- Luke Dashjr
          , "seed.bitcoinstats.com" -- Chris Decker
          , "seed.bitcoin.jonasschnelli.ch" -- Jonas Schnelli
          , "seed.btc.petertodd.org" -- Peter Todd
          , "seed.bitcoin.sprovoost.nl" -- Sjors Provoost
          ]
    , getBip44Coin = 0
    , getSigHashForkId = Nothing
    , getEDABlockHeight = Nothing
    , getDAABlockHeight = Nothing
    , getSegWit = True
    }

btcTest :: Network
btcTest =
    Network
    { getNetworkName = "btc-test"
    , getAddrPrefix = 111
    , getScriptPrefix = 196
    , getSecretPrefix = 239
    , getExtPubKeyPrefix = 0x043587cf
    , getExtSecretPrefix = 0x04358394
    , getNetworkMagic = 0x0b110907
    , getGenesisHeader =
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1296688602
              486604799
              414098458
            -- Hash 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent =
          C8.concat ["/haskoin-btc-test:", C8.pack $ showVersion version, "/"]
    , getDefaultPort = 18333
    , getAllowMinDifficultyBlocks = True
    , getPowNoRetargetting = False
    , getPowLimit =
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 21111
          , "0000000023b3a96d3484e5abb3755c413e7d41500f8e2a5c3f0dd01299cd8ef8")
    , getBip65Height = 581885
    , getBip66Height = 330776
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
          [ ( 546
            , "000000002a936ca763904c3c35fce2f3556c559c0214345d31b1bcebf76acb70")
          ]
    , getSeeds =
          [ "testnet-seed.bitcoin.jonasschnelli.ch"
          , "seed.tbtc.petertodd.org"
          , "seed.testnet.bitcoin.sprovoost.nl"
          , "testnet-seed.bluematt.me"
          ]
    , getBip44Coin = 1
    , getSigHashForkId = Nothing
    , getEDABlockHeight = Nothing
    , getDAABlockHeight = Nothing
    , getSegWit = True
    }

btcRegTest :: Network
btcRegTest =
    Network
    { getNetworkName = "btc-regtest"
    , getAddrPrefix = 111
    , getScriptPrefix = 196
    , getSecretPrefix = 239
    , getExtPubKeyPrefix = 0x043587cf
    , getExtSecretPrefix = 0x04358394
    , getNetworkMagic = 0xfabfb5da
    , getGenesisHeader =
          BlockHeader
           -- 0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1296688602
              0x207fffff
              2
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent =
          C8.concat ["/haskoin-btc-regtest:", C8.pack $ showVersion version, "/"]
    , getDefaultPort = 18444
    , getAllowMinDifficultyBlocks = True
    , getPowNoRetargetting = True
    , getPowLimit =
          0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 100000000
          , "0000000000000000000000000000000000000000000000000000000000000000")
    , getBip65Height = 1351
    , getBip66Height = 1251
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints = []
    , getSeeds = ["localhost"]
    , getBip44Coin = 1
    , getSigHashForkId = Nothing
    , getEDABlockHeight = Nothing
    , getDAABlockHeight = Nothing
    , getSegWit = True
    }

bch :: Network
bch =
    Network
    { getNetworkName = "bch"
    , getAddrPrefix = 0
    , getScriptPrefix = 5
    , getSecretPrefix = 128
    , getExtPubKeyPrefix = 0x0488b21e
    , getExtSecretPrefix = 0x0488ade4
    , getNetworkMagic = 0xe3e1f3e8
    , getGenesisHeader =
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1231006505
              0x1d00ffff
              2083236893
            -- Hash 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    , getMaxBlockSize = 32000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent =
          C8.concat
              ["/haskoin-bch:", C8.pack $ showVersion version, "/"]
    , getDefaultPort = 8333
    , getAllowMinDifficultyBlocks = False
    , getPowNoRetargetting = False
    , getPowLimit =
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 227931
          , "000000000000024b89b42a942fe0d9fea3bb44ab7bd1b19115dd6a759c0808b8")
    , getBip65Height = 388381
    , getBip66Height = 363725
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
          [ ( 11111
            , "0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d")
          , ( 33333
            , "000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6")
          , ( 74000
            , "0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20")
          , ( 105000
            , "00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97")
          , ( 134444
            , "00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe")
          , ( 168000
            , "000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763")
          , ( 193000
            , "000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317")
          , ( 210000
            , "000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e")
          , ( 216116
            , "00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e")
          , ( 225430
            , "00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932")
          , ( 250000
            , "000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214")
          , ( 279000
            , "0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40")
          , ( 295000
            , "00000000000000004d9b4ef50f0f9d686fd69db2e03af35a100370c64632a983")
            -- UAHF fork block.
          , ( 478559
            , "000000000000000000651ef99cb9fcbe0dadde1d424bd9f15ff20136191a5eec")
            -- Nov, 13 DAA activation block.
          , ( 504031
            , "0000000000000000011ebf65b60d0a3de80b8175be709d653b4c1a1beeb6ab9c")
          ]
    , getSeeds =
          [ "seed.bitcoinabc.org"
          , "seed-abc.bitcoinforks.org"
          , "btccash-seeder.bitcoinunlimited.info"
          , "seed.bitprim.org"
          , "seed.deadalnix.me"
          , "seeder.criptolayer.net"
          ]
    , getBip44Coin = 145
    , getSigHashForkId = Just 0
    , getEDABlockHeight = Just 478559
    , getDAABlockHeight = Just 404031
    , getSegWit = False
    }

bchTest :: Network
bchTest =
    Network
    { getNetworkName = "bch-test"
    , getAddrPrefix = 111
    , getScriptPrefix = 196
    , getSecretPrefix = 239
    , getExtPubKeyPrefix = 0x043587cf
    , getExtSecretPrefix = 0x04358394
    , getNetworkMagic = 0xf4e5f3f4
    , getGenesisHeader =
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1296688602
              486604799
              414098458
            -- Hash 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
    , getMaxBlockSize = 32000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent =
          C8.concat
              [ "/haskoin-bch-test:"
              , C8.pack $ showVersion version
              , "/"
              ]
    , getDefaultPort = 18333
    , getAllowMinDifficultyBlocks = True
    , getPowNoRetargetting = False
    , getPowLimit =
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 21111
          , "0000000023b3a96d3484e5abb3755c413e7d41500f8e2a5c3f0dd01299cd8ef8")
    , getBip65Height = 581885
    , getBip66Height = 330776
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
          [ ( 546
            , "000000002a936ca763904c3c35fce2f3556c559c0214345d31b1bcebf76acb70")
            -- UAHF fork block.
          , ( 1155876
            , "00000000000e38fef93ed9582a7df43815d5c2ba9fd37ef70c9a0ea4a285b8f5")
            -- Nov, 13. DAA activation block.
          , ( 1188697
            , "0000000000170ed0918077bde7b4d36cc4c91be69fa09211f748240dabe047fb")
          ]
    , getSeeds =
          [ "testnet-seed.bitcoinabc.org"
          , "testnet-seed-abc.bitcoinforks.org"
          , "testnet-seed.bitprim.org"
          , "testnet-seed.deadalnix.me"
          , "testnet-seeder.criptolayer.net"
          ]
    , getBip44Coin = 1
    , getSigHashForkId = Just 0
    , getEDABlockHeight = Just 1155876
    , getDAABlockHeight = Just 1188697
    , getSegWit = False
    }

bchRegTest :: Network
bchRegTest =
    Network
    { getNetworkName = "bch-regtest"
    , getAddrPrefix = 111
    , getScriptPrefix = 196
    , getSecretPrefix = 239
    , getExtPubKeyPrefix = 0x043587cf
    , getExtSecretPrefix = 0x04358394
    , getNetworkMagic = 0xdab5bffa
    , getGenesisHeader =
          BlockHeader
           -- 0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1296688602
              0x207fffff
              2
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent =
          C8.concat ["/haskoin-bch-regtest:", C8.pack $ showVersion version, "/"]
    , getDefaultPort = 18444
    , getAllowMinDifficultyBlocks = True
    , getPowNoRetargetting = True
    , getPowLimit =
          0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 100000000
          , "0000000000000000000000000000000000000000000000000000000000000000")
    , getBip65Height = 1351
    , getBip66Height = 1251
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
          [ ( 0
            , "0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206")
          ]
    , getSeeds = ["localhost"]
    , getBip44Coin = 1
    , getSigHashForkId = Just 0
    , getEDABlockHeight = Nothing
    , getDAABlockHeight = Just 0
    , getSegWit = False
    }
