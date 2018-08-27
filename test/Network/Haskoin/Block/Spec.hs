{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Block.Spec
    ( spec
    ) where
import           Control.Monad.State.Strict
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Test.Hspec

myTime :: Timestamp
myTime = 1499083075

withChain :: Network -> State HeaderMemory a -> a
withChain net f = evalState f (initialChain net)

chain :: BlockHeaders m => Network -> BlockHeader -> Int -> m ()
chain net bh i = do
    bnsE <- connectBlocks net myTime bhs
    either error (const $ return ()) bnsE
  where
    bhs = appendBlocks net 6 bh i

spec :: Network -> Spec
spec net = do
    describe "blockchain headers" $ do
        it "gets best block" $
            let bb =
                    withChain net $ do
                        chain net (getGenesisHeader net) 100
                        getBestBlockHeader
             in nodeHeight bb `shouldBe` 100
        it "builds a block locator" $
            let net = bchRegTest
                loc =
                    withChain net $ do
                        chain net (getGenesisHeader net) 100
                        bb <- getBestBlockHeader
                        blockLocatorNodes net bb
                heights = map nodeHeight loc
             in heights `shouldBe` [100,99 .. 90] <> [88, 84, 76, 60, 28, 0]
        it "follows split chains" $
            let bb = withChain net $ splitChain net >> getBestBlockHeader
             in nodeHeight bb `shouldBe` 4035

-- 0 → → 2015 → → → → → → → 4031
--       ↓
--       → → 2035 → → → → → → 4035*
--           ↓
--           → → 2185
splitChain :: Network -> State HeaderMemory ()
splitChain net = do
    start <- go 1 (getGenesisHeader net) 2015
    e 2015 (head start)
    tail1 <- go 2 (nodeHeader $ head start) 2016
    e 4031 (head tail1)
    tail2 <- go 3 (nodeHeader $ head start) 20
    e 2035 (head tail2)
    tail3 <- go 4 (nodeHeader $ head tail2) 2000
    e 4035 (head tail3)
    tail4 <- go 5 (nodeHeader $ head tail2) 150
    e 2185 (head tail4)
    sp1 <- splitPoint net (head tail1) (head tail3)
    unless (sp1 == head start) $
        error $
        "Split point wrong between blocks 4031 and 4035: " ++
        show (nodeHeight sp1)
    sp2 <- splitPoint net (head tail4) (head tail3)
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
        let bhs = appendBlocks net seed start n
        bnE <- connectBlocks net myTime bhs
        case bnE of
            Right bn -> return bn
            Left ex -> error ex
