{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskoin.Constants
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Network constants for various networks, including Bitcoin SegWit (BTC), Bitcoin
Cash (BCH), and corresponding public test and private regression test networks.
-}
module Haskoin.Constants (
    Network (..),

    -- * Constants
    btc,
    btcTest,
    btcRegTest,
    bch,
    bchTest,
    bchTest4,
    bchRegTest,
    allNets,
    netByName,
) where

import Control.DeepSeq
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.List
import Data.Maybe
import Data.Serialize (Serialize (..))
import Data.String
import Data.Text (Text)
import Data.Word (Word32, Word64, Word8)
import GHC.Generics (Generic)
import Haskoin.Block
import Haskoin.Data
import Haskoin.Network.Common
import Haskoin.Transaction
import Text.Read

-- | Version of Haskoin Core package.
versionString :: IsString a => a

#ifdef CURRENT_PACKAGE_VERSION
versionString = CURRENT_PACKAGE_VERSION
#else
versionString = "Unavailable"
#endif

-- | Query known networks by name.
netByName :: String -> Maybe Network
netByName str = find ((== str) . getNetworkName) allNets

-- | Bitcoin SegWit network. Symbol: BTC.
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
                (buildMerkleRoot [txHash genesisTx])
                1231006505
                0x1d00ffff
                2083236893
        , -- Hash 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
          getMaxBlockSize = 1000000
        , getMaxSatoshi = 2100000000000000
        , getHaskoinUserAgent =
            "/haskoin-btc:" <> versionString <> "/"
        , getDefaultPort = 8333
        , getAllowMinDifficultyBlocks = False
        , getPowNoRetargetting = False
        , getPowLimit =
            0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        , getBip34Block =
            ( 227931
            , "000000000000024b89b42a942fe0d9fea3bb44ab7bd1b19115dd6a759c0808b8"
            )
        , getBip65Height = 388381
        , getBip66Height = 363725
        , getTargetTimespan = 14 * 24 * 60 * 60
        , getTargetSpacing = 10 * 60
        , getCheckpoints =
            [
                ( 11111
                , "0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d"
                )
            ,
                ( 33333
                , "000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6"
                )
            ,
                ( 74000
                , "0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20"
                )
            ,
                ( 105000
                , "00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97"
                )
            ,
                ( 134444
                , "00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe"
                )
            ,
                ( 168000
                , "000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763"
                )
            ,
                ( 193000
                , "000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317"
                )
            ,
                ( 210000
                , "000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e"
                )
            ,
                ( 216116
                , "00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e"
                )
            ,
                ( 225430
                , "00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932"
                )
            ,
                ( 250000
                , "000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214"
                )
            ,
                ( 279000
                , "0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40"
                )
            ,
                ( 295000
                , "00000000000000004d9b4ef50f0f9d686fd69db2e03af35a100370c64632a983"
                )
            ]
        , getSeeds =
            [ "seed.bitcoin.sipa.be" -- Pieter Wuille
            , "dnsseed.bluematt.me" -- Matt Corallo
            , "dnsseed.bitcoin.dashjr.org" -- Luke Dashjr
            , "seed.bitcoinstats.com" -- Chris Decker
            , "seed.bitcoin.jonasschnelli.ch" -- Jonas Schnelli
            , "seed.btc.petertodd.org" -- Peter Todd
            , "seed.bitcoin.sprovoost.nl" -- Sjors Provoost
            , "dnsseed.emzy.de" -- Stephan Oeste
            , "seed.bitcoin.wiz.biz" -- Jason Maurice
            ]
        , getBip44Coin = 0
        , getSigHashForkId = Nothing
        , getEdaBlockHeight = Nothing
        , getDaaBlockHeight = Nothing
        , getAsertActivationTime = Nothing
        , getAsertHalfLife = 0
        , getSegWit = True
        , getCashAddrPrefix = Nothing
        , getBech32Prefix = Just "bc"
        , getReplaceByFee = True
        , getHalvingInterval = 210000
        }

-- | Testnet for Bitcoin SegWit network.
btcTest :: Network
btcTest =
    Network
        { getNetworkName = "btctest"
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
                (buildMerkleRoot [txHash genesisTx])
                1296688602
                486604799
                414098458
        , -- Hash 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
          getMaxBlockSize = 1000000
        , getMaxSatoshi = 2100000000000000
        , getHaskoinUserAgent = "/haskoin-btc-test:" <> versionString <> "/"
        , getDefaultPort = 18333
        , getAllowMinDifficultyBlocks = True
        , getPowNoRetargetting = False
        , getPowLimit =
            0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        , getBip34Block =
            ( 21111
            , "0000000023b3a96d3484e5abb3755c413e7d41500f8e2a5c3f0dd01299cd8ef8"
            )
        , getBip65Height = 581885
        , getBip66Height = 330776
        , getTargetTimespan = 14 * 24 * 60 * 60
        , getTargetSpacing = 10 * 60
        , getCheckpoints =
            [
                ( 546
                , "000000002a936ca763904c3c35fce2f3556c559c0214345d31b1bcebf76acb70"
                )
            ]
        , getSeeds =
            [ "testnet-seed.bitcoin.jonasschnelli.ch"
            , "seed.tbtc.petertodd.org"
            , "seed.testnet.bitcoin.sprovoost.nl"
            , "testnet-seed.bluematt.me"
            ]
        , getBip44Coin = 1
        , getSigHashForkId = Nothing
        , getEdaBlockHeight = Nothing
        , getDaaBlockHeight = Nothing
        , getAsertActivationTime = Nothing
        , getAsertHalfLife = 0
        , getSegWit = True
        , getCashAddrPrefix = Nothing
        , getBech32Prefix = Just "tb"
        , getReplaceByFee = True
        , getHalvingInterval = 210000
        }

-- | RegTest for Bitcoin SegWit network.
btcRegTest :: Network
btcRegTest =
    Network
        { getNetworkName = "btcreg"
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
                (buildMerkleRoot [txHash genesisTx])
                1296688602
                0x207fffff
                2
        , getMaxBlockSize = 1000000
        , getMaxSatoshi = 2100000000000000
        , getHaskoinUserAgent = "/haskoin-btc-regtest:" <> versionString <> "/"
        , getDefaultPort = 18444
        , getAllowMinDifficultyBlocks = True
        , getPowNoRetargetting = True
        , getPowLimit =
            0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        , getBip34Block =
            ( 100000000
            , "0000000000000000000000000000000000000000000000000000000000000000"
            )
        , getBip65Height = 1351
        , getBip66Height = 1251
        , getTargetTimespan = 14 * 24 * 60 * 60
        , getTargetSpacing = 10 * 60
        , getCheckpoints = []
        , getSeeds = ["localhost"]
        , getBip44Coin = 1
        , getSigHashForkId = Nothing
        , getEdaBlockHeight = Nothing
        , getDaaBlockHeight = Nothing
        , getAsertActivationTime = Nothing
        , getAsertHalfLife = 0
        , getSegWit = True
        , getCashAddrPrefix = Nothing
        , getBech32Prefix = Just "bcrt"
        , getReplaceByFee = True
        , getHalvingInterval = 150
        }

-- | Bitcoin Cash network. Symbol: BCH.
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
                (buildMerkleRoot [txHash genesisTx])
                1231006505
                0x1d00ffff
                2083236893
        , -- Hash 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
          getMaxBlockSize = 32000000
        , getMaxSatoshi = 2100000000000000
        , getHaskoinUserAgent = "/haskoin-bch:" <> versionString <> "/"
        , getDefaultPort = 8333
        , getAllowMinDifficultyBlocks = False
        , getPowNoRetargetting = False
        , getPowLimit =
            0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        , getBip34Block =
            ( 227931
            , "000000000000024b89b42a942fe0d9fea3bb44ab7bd1b19115dd6a759c0808b8"
            )
        , getBip65Height = 388381
        , getBip66Height = 363725
        , getTargetTimespan = 14 * 24 * 60 * 60
        , getTargetSpacing = 10 * 60
        , getCheckpoints =
            [
                ( 11111
                , "0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d"
                )
            ,
                ( 33333
                , "000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6"
                )
            ,
                ( 74000
                , "0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20"
                )
            ,
                ( 105000
                , "00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97"
                )
            ,
                ( 134444
                , "00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe"
                )
            ,
                ( 168000
                , "000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763"
                )
            ,
                ( 193000
                , "000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317"
                )
            ,
                ( 210000
                , "000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e"
                )
            ,
                ( 216116
                , "00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e"
                )
            ,
                ( 225430
                , "00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932"
                )
            ,
                ( 250000
                , "000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214"
                )
            ,
                ( 279000
                , "0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40"
                )
            ,
                ( 295000
                , "00000000000000004d9b4ef50f0f9d686fd69db2e03af35a100370c64632a983"
                )
            , -- UAHF fork block.

                ( 478559
                , "000000000000000000651ef99cb9fcbe0dadde1d424bd9f15ff20136191a5eec"
                )
            , -- Nov, 13 DAA activation block.

                ( 504031
                , "0000000000000000011ebf65b60d0a3de80b8175be709d653b4c1a1beeb6ab9c"
                )
            ]
        , getSeeds =
            [ "seed.bitcoinabc.org"
            , "seed-bch.bitcoinforks.org"
            , "btccash-seeder.bitcoinunlimited.info"
            , "seed.bchd.cash"
            , "seed.bch.loping.net"
            , "dnsseed.electroncash.de"
            ]
        , getBip44Coin = 145
        , getSigHashForkId = Just 0
        , getEdaBlockHeight = Just 478559
        , getDaaBlockHeight = Just 404031
        , getAsertActivationTime = Just 1605441600
        , getAsertHalfLife = 60 * 60 * 10
        , getSegWit = False
        , getCashAddrPrefix = Just "bitcoincash"
        , getBech32Prefix = Nothing
        , getReplaceByFee = False
        , getHalvingInterval = 210000
        }

-- | Testnet for Bitcoin Cash network.
bchTest4 :: Network
bchTest4 =
    Network
        { getNetworkName = "bchtest4"
        , getAddrPrefix = 111
        , getScriptPrefix = 196
        , getSecretPrefix = 239
        , getExtPubKeyPrefix = 0x043587cf
        , getExtSecretPrefix = 0x04358394
        , getNetworkMagic = 0xe2b7daaf
        , getGenesisHeader =
            BlockHeader
                0x01
                "0000000000000000000000000000000000000000000000000000000000000000"
                (buildMerkleRoot [txHash genesisTx])
                1597811185
                0x1d00ffff
                114152193
        , -- Hash 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
          getMaxBlockSize = 2000000
        , getMaxSatoshi = 2100000000000000
        , getHaskoinUserAgent = "/haskoin-bch-test4:" <> versionString <> "/"
        , getDefaultPort = 28333
        , getAllowMinDifficultyBlocks = True
        , getPowNoRetargetting = False
        , getPowLimit =
            0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        , getBip34Block =
            ( 2
            , "00000000b0c65b1e03baace7d5c093db0d6aac224df01484985ffd5e86a1a20c"
            )
        , getBip65Height = 3
        , getBip66Height = 4
        , getTargetTimespan = 14 * 24 * 60 * 60
        , getTargetSpacing = 10 * 60
        , getCheckpoints =
            [
                ( 5000
                , "000000009f092d074574a216faec682040a853c4f079c33dfd2c3ef1fd8108c4"
                )
            , -- Axion activation

                ( 16845
                , "00000000fb325b8f34fe80c96a5f708a08699a68bbab82dba4474d86bd743077"
                )
            ,
                ( 38000
                , "000000000015197537e59f339e3b1bbf81a66f691bd3d7aa08560fc7bf5113fb"
                )
            ,
                ( 54700
                , "00000000009af4379d87f17d0f172ee4769b48839a5a3a3e81d69da4322518b8"
                )
            ]
        , getSeeds =
            [ "testnet4-seed-bch.bitcoinforks.org"
            , "testnet4-seed-bch.toom.im"
            , "seed.tbch4.loping.net"
            , "testnet4-seed.flowee.cash"
            ]
        , getBip44Coin = 1
        , getSigHashForkId = Just 0
        , getEdaBlockHeight = Just 7
        , getDaaBlockHeight = Just 3000
        , getAsertActivationTime = Just 1605441600
        , getAsertHalfLife = 60 * 60
        , getSegWit = False
        , getCashAddrPrefix = Just "bchtest"
        , getBech32Prefix = Nothing
        , getReplaceByFee = False
        , getHalvingInterval = 210000
        }

-- | Testnet for Bitcoin Cash network.
bchTest :: Network
bchTest =
    Network
        { getNetworkName = "bchtest"
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
                (buildMerkleRoot [txHash genesisTx])
                1296688602
                486604799
                414098458
        , -- Hash 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
          getMaxBlockSize = 32000000
        , getMaxSatoshi = 2100000000000000
        , getHaskoinUserAgent = "/haskoin-bch-test:" <> versionString <> "/"
        , getDefaultPort = 18333
        , getAllowMinDifficultyBlocks = True
        , getPowNoRetargetting = False
        , getPowLimit =
            0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        , getBip34Block =
            ( 21111
            , "0000000023b3a96d3484e5abb3755c413e7d41500f8e2a5c3f0dd01299cd8ef8"
            )
        , getBip65Height = 581885
        , getBip66Height = 330776
        , getTargetTimespan = 14 * 24 * 60 * 60
        , getTargetSpacing = 10 * 60
        , getCheckpoints =
            [
                ( 546
                , "000000002a936ca763904c3c35fce2f3556c559c0214345d31b1bcebf76acb70"
                )
            , -- UAHF fork block.

                ( 1155876
                , "00000000000e38fef93ed9582a7df43815d5c2ba9fd37ef70c9a0ea4a285b8f5"
                )
            , -- Nov, 13. DAA activation block.

                ( 1188697
                , "0000000000170ed0918077bde7b4d36cc4c91be69fa09211f748240dabe047fb"
                )
            ]
        , getSeeds =
            [ "testnet-seed.bitcoinabc.org"
            , "testnet-seed-bch.bitcoinforks.org"
            , "testnet-seed.bchd.cash"
            , "seed.tbch.loping.net"
            ]
        , getBip44Coin = 1
        , getSigHashForkId = Just 0
        , getEdaBlockHeight = Just 1155876
        , getDaaBlockHeight = Just 1188697
        , getAsertActivationTime = Just 1605441600
        , getAsertHalfLife = 60 * 60
        , getSegWit = False
        , getCashAddrPrefix = Just "bchtest"
        , getBech32Prefix = Nothing
        , getReplaceByFee = False
        , getHalvingInterval = 210000
        }

-- | RegTest for Bitcoin Cash network.
bchRegTest :: Network
bchRegTest =
    Network
        { getNetworkName = "bchreg"
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
                (buildMerkleRoot [txHash genesisTx])
                1296688602
                0x207fffff
                2
        , getMaxBlockSize = 1000000
        , getMaxSatoshi = 2100000000000000
        , getHaskoinUserAgent = "/haskoin-bch-regtest:" <> versionString <> "/"
        , getDefaultPort = 18444
        , getAllowMinDifficultyBlocks = True
        , getPowNoRetargetting = True
        , getPowLimit =
            0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        , getBip34Block =
            ( 100000000
            , "0000000000000000000000000000000000000000000000000000000000000000"
            )
        , getBip65Height = 1351
        , getBip66Height = 1251
        , getTargetTimespan = 14 * 24 * 60 * 60
        , getTargetSpacing = 10 * 60
        , getCheckpoints =
            [
                ( 0
                , "0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206"
                )
            ]
        , getSeeds = ["localhost"]
        , getBip44Coin = 1
        , getSigHashForkId = Just 0
        , getEdaBlockHeight = Nothing
        , getDaaBlockHeight = Just 0
        , getAsertActivationTime = Just 1605441600
        , getAsertHalfLife = 2 * 24 * 60 * 60
        , getSegWit = False
        , getCashAddrPrefix = Just "bchreg"
        , getBech32Prefix = Nothing
        , getReplaceByFee = False
        , getHalvingInterval = 150
        }

-- | List of all networks supported by this library.
allNets :: [Network]
allNets = [btc, bch, btcTest, bchTest4, bchTest, btcRegTest, bchRegTest]
