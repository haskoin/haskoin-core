module Network.Haskoin.Block.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe
import Data.Binary (Binary)

import Network.Haskoin.Block.Arbitrary()
import Network.Haskoin.Block.Types
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize block types"
        [ testProperty "Block" (metaBinary :: Block -> Bool)
        , testProperty "BlockHeader" (metaBinary :: BlockHeader -> Bool)
        , testProperty "GetBlocks" (metaBinary :: GetBlocks -> Bool)
        , testProperty "GetHeaders" (metaBinary :: GetHeaders -> Bool)
        , testProperty "Headers" (metaBinary :: Headers -> Bool)
        ]
    , testGroup "Block tests"
        [ testProperty "decode . encode BlockHash id" decEncBlockHashid ]
    ]

metaBinary :: (Binary a, Eq a) => a -> Bool
metaBinary x = (decode' $ encode' x) == x

decEncBlockHashid :: BlockHash -> Bool
decEncBlockHashid h = (fromJust $ decodeBlockHashLE $ encodeBlockHashLE h) == h

