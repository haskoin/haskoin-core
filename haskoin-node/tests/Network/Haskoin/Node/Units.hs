{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Node.Units where
import           Control.Monad                   (forM_, when)
import           Control.Monad.Logger            (NoLoggingT)
import           Control.Monad.Trans             (MonadIO, liftIO)
import           Control.Monad.Trans.Resource    (ResourceT)
import           Data.Maybe                      (fromJust, isNothing,
                                                  maybeToList)
import           Data.Word                       (Word32)
import           Database.Persist.Sqlite         (SqlPersistT,
                                                  runMigrationSilent, runSqlite)
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Node.HeaderTree
import           Test.Framework                  (Test, testGroup)
import           Test.Framework.Providers.HUnit  (testCase)
import           Test.HUnit                      (Assertion, assertBool,
                                                  assertEqual, assertFailure)

-- TODO: Make sure that evalNewChain for a partially overlapping best chain
-- properly evaluates to BestChain.

type App = SqlPersistT (NoLoggingT (ResourceT IO))

tests :: [Test]
tests =
    [ testGroup "Header Tree"
        [ testCase "Initalization successful"    $ runUnit initialize
        , testCase "Add second block"            $ runUnit addSecondBlock
        , testCase "Blockchain head correct"     $ runUnit blockChainHead
        , testCase "Find fork node"              $ runUnit forkNode
        , testCase "Find fork node (non-head)"   $ runUnit forkNodeNonHead
        , testCase "Find fork node (same chain)" $ runUnit forkNodeSameChain
        , testCase "Get best chain"              $ runUnit getBestChain
        , testCase "Get side chain"              $ runUnit getSideChain
        , testCase "Nodes at height"             $ runUnit getNodesHeight
        , testCase "Block locator to head"       $ runUnit blockLocatorToHead
        , testCase "Block locator to non-head"   $ runUnit blockLocatorToNode
        , testCase "Find split node"             $ runUnit splitNode
        ]
    ]

initialize :: App ()
initialize = do
    initHeaderTree
    bM <- getBlockByHash (headerHash genesisHeader)
    liftIO $ assertEqual "Genesis node in header tree" (Just genesisBlock) bM
    hs <- getHeads
    liftIO $ assertEqual "Genesis node is only head" [genesisBlock] hs
    bh <- getBestBlock
    liftIO $ assertEqual "Genesis node matches best header" genesisBlock bh

addSecondBlock :: App ()
addSecondBlock = do
    initHeaderTree
    let block = head chain0
    liftIO $ assertEqual "Block builds on genesis block"
        (headerHash genesisHeader)
        (nodePrev block)
    putBlock block
    block' <- getBlockByHash $ nodeHash block
    liftIO $ assertEqual "Block can be retrieved" (Just block) block'

blockChainHead :: App ()
blockChainHead = mockBlockChain >> do
    heads <- getHeads
    liftIO $ assertEqual "Heads match"
        [last chain0, last chain1, last chain2, last chain3]
        heads
    bh <- getBestBlock
    liftIO $ assertEqual "Best block has correct hash"
        (nodeHash $ last chain3) (nodeHash bh)
    liftIO $ assertEqual "Best block height is right"
        (nodeBlockHeight $ last chain3) (nodeBlockHeight bh)

forkNode :: App ()
forkNode = mockBlockChain >> do
    let l = last chain2
        r = last chain3
    bn <- splitBlock l r

    liftIO $ assertEqual "Split block are correct"
        (chain0 !! 1) bn

    commonLM <- getBlockByHeight l $ nodeBlockHeight bn
    when (isNothing commonLM) $ liftIO $
        assertFailure "Could not find fork on left side"
    let commonL = fromJust commonLM

    commonRM <- getBlockByHeight r $ nodeBlockHeight bn
    when (isNothing commonRM) $ liftIO $
        assertFailure "Could not find fork on right side"
    let commonR = fromJust commonRM

    firstLM  <- getBlockByHeight l (nodeBlockHeight bn + 1)
    when (isNothing firstLM) $ liftIO $
        assertFailure "Could not find fork child on left side"
    let firstL = fromJust firstLM

    firstRM  <- getBlockByHeight r (nodeBlockHeight bn + 1)
    when (isNothing firstLM) $ liftIO $
        assertFailure "Could not find fork child on right side"
    let firstR = fromJust firstRM

    liftIO $ assertEqual "Fork node is same in both sides" commonL commonR
    liftIO $ assertEqual "Fork node connect with left side"
        (nodeHash commonL)
        (nodePrev firstL)
    liftIO $ assertEqual "Fork node connect with right side"
        (nodeHash commonR)
        (nodePrev firstR)
    liftIO $ assertBool "After-fork chains diverge" $ firstL /= firstR
    liftIO $ assertEqual "Fork node matches hardcoded one"
        (chain0 !! 1) commonL

forkNodeNonHead :: App ()
forkNodeNonHead = mockBlockChain >> do
    let l = chain2 !! 1
        r = chain1 !! 1
    height <- nodeBlockHeight <$> splitBlock l r
    splitM <- getBlockByHeight l height
    liftIO $ assertEqual "Fork node is correct" (Just $ chain1 !! 1) splitM

forkNodeSameChain :: App ()
forkNodeSameChain = mockBlockChain >> do
    let l = chain3 !! 5
        r = chain3 !! 3
    height <- nodeBlockHeight <$> splitBlock l r
    splitM <- getBlockByHeight r height
    liftIO $ assertEqual "Fork node is correct" (Just $ chain3 !! 3) splitM

getBestChain :: App ()
getBestChain = mockBlockChain >> do
    h <- getBestBlock
    ch <- getBlocksFromHeight h 0 0
    liftIO $ assertEqual "Best chain correct" bch ch
  where
    bch = genesisBlock : take 2 chain0 ++ chain3

getSideChain :: App ()
getSideChain = mockBlockChain >> do
    ch <- getBlocksFromHeight (chain2 !! 1) 0 0
    liftIO $ assertEqual "Side chain correct" sch ch
  where
    sch = genesisBlock :
        take 3 chain0 ++ take 2 chain1 ++ take 2 chain2

getNodesHeight :: App ()
getNodesHeight = mockBlockChain >> do
    ns <- getBlocksAtHeight 3
    liftIO $ assertEqual "Nodes at height match" hns ns
  where
    hns = [chain0 !! 2, head chain3]

blockLocatorToHead :: App ()
blockLocatorToHead = do
    mockBlockChain
    putBlocks bs
    h <- getBestBlock
    liftIO $ assertEqual "Head matches" (last bs) h
    ls <- blockLocator h
    liftIO $ assertEqual "Last is genesis"
        (last ls)
        (headerHash genesisHeader)
    liftIO $ assertEqual "First is current head"
        (head ls)
        (nodeHash h)
    last10 <- map nodeHash . reverse <$>
        getBlocksFromHeight h 0 (nodeBlockHeight h - 9)
    liftIO $ assertEqual "Last ten blocks contiguous"
        last10
        (take 10 ls)
    let h10 = nodeBlockHeight h - 10
    bhs <- map (nodeHash . fromJust) <$>
        mapM (getBlockByHeight h)
        [h10, h10 - 2, h10 - 6, h10 - 14, h10 - 30, h10 - 62]
    liftIO $ assertEqual "All block hashes correct"
        (last10 ++ bhs ++ [headerHash genesisHeader])
        ls
  where
    bs = manyBlocks $ last chain1

blockLocatorToNode :: App ()
blockLocatorToNode = do
    mockBlockChain
    putBlocks bs
    n <- fromJust <$> getBlockByHash (nodeHash $ chain3 !! 4)
    ls <- blockLocator n
    xs <- map nodeHash . reverse <$>
        getBlocksFromHeight n 0 0
    liftIO $ assertEqual "Block locator for non-head node is correct" xs ls
  where
    bs = manyBlocks $ last chain1

splitNode :: App ()
splitNode = do
    mockBlockChain
    (split, ls, rs) <- splitChains (last chain2, 0) (last chain3, 0)
    liftIO $ assertEqual "Split node correct" (chain0 !! 1) split
    liftIO $ assertEqual "Left correct"
        ([chain0 !! 2] ++ take 2 chain1 ++ chain2)
        ls
    liftIO $ assertEqual "Right correct" chain3 rs

runUnit :: App () -> Assertion
runUnit action = runSqlite ":memory:" $ do
    _ <- runMigrationSilent migrateHeaderTree
    action

mockBlockChain :: MonadIO m => SqlPersistT m ()
mockBlockChain = do
    initHeaderTree
    forM_ (concat [chain0, chain1, chain2, chain3]) putBlock

manyBlocks :: NodeBlock -> [NodeBlock]
manyBlocks b =
    tail $ reverse $ foldBlock (Just b) $ zip [18..117] (repeat 4)


chain0 :: [NodeBlock]
chain0 =
    tail $ reverse $ foldBlock Nothing $ zip [1..4] (repeat 0)

chain1 :: [NodeBlock]
chain1 =
    tail $ reverse $ foldBlock (Just $ chain0 !! 2) $ zip [5..7] (repeat 1)

chain2 :: [NodeBlock]
chain2 =
    tail $ reverse $ foldBlock (Just $ chain1 !! 1) $ zip [8..10] (repeat 2)

chain3 :: [NodeBlock]
chain3 =
    tail $ reverse $ foldBlock (Just $ chain0 !! 1) $ zip [11..17] (repeat 3)

foldBlock :: Maybe NodeBlock -> [(Word32, Word32)] -> [NodeBlock]
foldBlock nM =
    foldl f (maybeToList nM)
  where
    f [] _ = [genesisBlock]
    f ls@(l:_) (n, chain) = mockBlock l chain n : ls

mockBlock :: NodeBlock -> Word32 -> Word32 -> NodeBlock
mockBlock parent chain n = nodeBlock parent chain bh
  where
    bh = BlockHeader
        { blockVersion   = blockVersion $ nodeHeader parent
        , prevBlock      = nodeHash parent
        , merkleRoot     = z
        , blockTimestamp = nodeTimestamp parent + 600
        , blockBits      = blockBits $ nodeHeader parent
        , bhNonce        = n
        }
    z = "0000000000000000000000000000000000000000000000000000000000000000"
