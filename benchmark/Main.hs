{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Bitcoin (Block, Tx, decodeHex)
import Control.DeepSeq (NFData)
import Criterion (Benchmark, bench, bgroup, nf)
import Criterion.Main (defaultMain)
import Data.Bifunctor (bimap)
import Data.Binary (Binary)
import qualified Data.Binary as Bin
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (..))
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
    -- BLOCKS
    blockA <- TIO.readFile "data/block-758000.dat"
    blockB <- TIO.readFile "data/block-758100.dat"
    blockC <- TIO.readFile "data/block-758200.dat"
    let benchBlocks =
            bgroup
                "blocks"
                [ roundTrip (Proxy @Block) "block 758000" blockA
                , roundTrip (Proxy @Block) "block 758100" blockB
                , roundTrip (Proxy @Block) "block 758200" blockC
                ]

    -- TRANSACTIONS
    txA <- TIO.readFile "data/tx-A.dat"
    txB <- TIO.readFile "data/tx-B.dat"
    txC <- TIO.readFile "data/tx-C.dat"
    let benchTxs =
            bgroup
                "transactions"
                [ roundTrip (Proxy @Tx) "tx A" txA
                , roundTrip (Proxy @Tx) "tx B" txB
                , roundTrip (Proxy @Tx) "tx C" txC
                ]

    defaultMain
        [ bgroup
            "serialization"
            [ benchBlocks
            , benchTxs
            ]
        ]


roundTrip ::
    forall a.
    (NFData a, Binary a, Serialize a) =>
    Proxy a ->
    String ->
    Text ->
    Benchmark
roundTrip _ label xHex =
    bgroup
        label
        [ bgroup
            "binary"
            [ bench "encode" $ nf Bin.encode x
            , bench "decode" $ nf binDecode xBytes
            ]
        , bgroup
            "cereal"
            [ bench "encode" $ nf S.encode x
            , bench "decode" $ nf (S.decode @a) xBytes
            ]
        ]
  where
    Just !xBytes = decodeHex $ Text.filter (/= '\n') xHex
    Right !x = binDecode xBytes

    binDecode :: Binary a => ByteString -> Either String a
    binDecode = bimap pr3 pr3 . Bin.decodeOrFail . BSL.fromStrict
    pr3 (_, _, z) = z
