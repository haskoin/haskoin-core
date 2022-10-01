{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- Network constants for main, test and private regression test networks.
module Bitcoin.Constants (
    Network (..),

    -- * Constants
    btc,
    btcTest,
    btcRegTest,
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
import Bitcoin.Block
import Bitcoin.Data
import Bitcoin.Network.Common
import Bitcoin.Transaction
import Text.Read


-- | Version of Bitcoin package.
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
        , getBitcoinUserAgent =
            "/haskell-bitcoin:" <> versionString <> "/"
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
        , getEdaBlockHeight = Nothing
        , getDaaBlockHeight = Nothing
        , getSegWit = True
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
        , getBitcoinUserAgent = "/haskell-bitcoin-test:" <> versionString <> "/"
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
        , getEdaBlockHeight = Nothing
        , getDaaBlockHeight = Nothing
        , getSegWit = True
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
        , getBitcoinUserAgent = "/haskell-bitcoin-regtest:" <> versionString <> "/"
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
        , getEdaBlockHeight = Nothing
        , getDaaBlockHeight = Nothing
        , getSegWit = True
        , getBech32Prefix = Just "bcrt"
        , getReplaceByFee = True
        , getHalvingInterval = 150
        }


-- | List of all networks supported by this library.
allNets :: [Network]
allNets = [btc, btcTest, btcRegTest]
