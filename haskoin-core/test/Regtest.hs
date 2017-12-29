{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.State.Strict
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Test.Framework                 as F
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     hiding (State)

myTime :: Timestamp
myTime = 1499083075

withChain :: State HeaderMemory a -> a
withChain f = evalState f initialChain

chain :: BlockHeaders m => BlockHeader -> Int -> m ()
chain bh i = do
    bnsE <- connectBlocks myTime bhs
    either error (const $ return ()) bnsE
  where
    bhs = appendBlocks 6 bh i

main :: IO ()
main = do
    setRegtest
    defaultMain [ testGroup "Regtest network" tests ]

tests :: [F.Test]
tests =
    [ testCase "Get best block" bestBlock
    , testCase "Build a block locator" buildLocator
    , testCase "Split chain best block" splitBest
    ]

bestBlock :: Assertion
bestBlock =
    100 @=? nodeHeight bb
  where
    bb = withChain $ do
        chain genesisHeader 100
        getBestBlockHeader

buildLocator :: Assertion
buildLocator =
    [100,99..90] ++ [88,84,76,60,28,0] @=? heights
  where
    heights = map nodeHeight loc
    loc = withChain $ do
        chain genesisHeader 100
        bb <- getBestBlockHeader
        blockLocatorNodes bb

splitBest :: Assertion
splitBest =
    4035 @=? nodeHeight bb
  where
    bb = withChain $ splitChain >> getBestBlockHeader

-- 0 → → 2015 → → → → → → → 4031
--       ↓
--       → → 2035 → → → → → → 4035*
--           ↓
--           → → 2185
splitChain :: State HeaderMemory ()
splitChain = do
    start <- go 1 genesisHeader 2015
    e 2015 (head start)
    tail1 <- go 2 (nodeHeader $ head start) 2016
    e 4031 (head tail1)
    tail2 <- go 3 (nodeHeader $ head start) 20
    e 2035 (head tail2)
    tail3 <- go 4 (nodeHeader $ head tail2) 2000
    e 4035 (head tail3)
    tail4 <- go 5 (nodeHeader $ head tail2) 150
    e 2185 (head tail4)
    sp1 <- splitPoint (head tail1) (head tail3)
    unless (sp1 == head start) $
        error $
        "Split point wrong between blocks 4031 and 4035: " ++
        show (nodeHeight sp1)
    sp2 <- splitPoint (head tail4) (head tail3)
    unless (sp2 == head tail2) $
        error $
        "Split point wrong between blocks 2185 and 4035: " ++
        show (nodeHeight sp2)
  where
    e n bn =
        unless (nodeHeight bn == n) $
        error $
        "Node height " ++
        show (nodeHeight bn) ++ " of first chunk should be " ++ show n
    go seed start n = do
        let bhs = appendBlocks seed start n
        bnE <- connectBlocks myTime bhs
        case bnE of
            Right bn -> return bn
            Left ex -> error ex
