{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-|
Module      : Haskoin.Block.Headers
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Block chain header synchronization and proof-of-work consensus functions.
-}
module Haskoin.Block.Headers
    ( -- * Block Headers
      BlockNode(..)
    , BlockHeaders(..)
    , BlockWork
    , genesisNode
    , genesisBlock
    , isGenesis
    , chooseBest
      -- ** Header Store
    , parentBlock
    , getParents
    , getAncestor
    , splitPoint
    , connectBlocks
    , connectBlock
    , blockLocator
      -- ** Header Memory Store
    , HeaderMemory(..)
    , ShortBlockHash
    , BlockMap
    , shortBlockHash
    , initialChain
    , genesisMap
      -- ** Helper Functions
    , appendBlocks
    , validBlock
    , validCP
    , afterLastCP
    , bip34
    , validVersion
    , lastNoMinDiff
    , nextWorkRequired
    , nextEdaWorkRequired
    , nextDaaWorkRequired
    , nextAsertWorkRequired
    , computeAsertBits
    , computeTarget
    , getSuitableBlock
    , nextPowWorkRequired
    , calcNextWork
    , isValidPOW
    , blockPOW
    , headerWork
    , diffInterval
    , blockLocatorNodes
    , mineBlock
    , computeSubsidy
    , mtp
    , firstGreaterOrEqual
    , lastSmallerOrEqual
    , ) where

import           Control.Applicative         ((<|>))
import           Control.DeepSeq
import           Control.Monad               (guard, unless, when)
import           Control.Monad.Except        (ExceptT (..), runExceptT,
                                              throwError)
import           Control.Monad.State.Strict  as State (StateT, get, gets, lift,
                                                       modify)
import           Control.Monad.Trans.Maybe
import           Data.Bits                   (shiftL, shiftR, (.&.))
import qualified Data.ByteString             as B
import           Data.ByteString.Short       (ShortByteString, fromShort,
                                              toShort)
import           Data.Function               (on)
import           Data.Hashable
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.List                   (sort, sortBy)
import           Data.Maybe                  (fromMaybe, listToMaybe)
import           Data.Serialize              as S (Serialize (..), decode,
                                                   encode, get, put)
import           Data.Serialize.Get          as S
import           Data.Serialize.Put          as S
import           Data.Typeable               (Typeable)
import           Data.Word                   (Word32, Word64)
import           GHC.Generics                (Generic)
import           Haskoin.Block.Common
import           Haskoin.Constants
import           Haskoin.Crypto
import           Haskoin.Transaction.Genesis
import           Haskoin.Util

-- | Short version of the block hash. Uses the good end of the hash (the part
-- that doesn't have a long string of zeroes).
type ShortBlockHash = Word64

-- | Memory-based map to a serialized 'BlockNode' data structure.
-- 'ShortByteString' is used to avoid memory fragmentation and make the data
-- structure compact.
type BlockMap = HashMap ShortBlockHash ShortByteString

-- | Represents accumulated work in the block chain so far.
type BlockWork = Integer

-- | Data structure representing a block header and its position in the
-- block chain.
data BlockNode
    = BlockNode
          { nodeHeader :: !BlockHeader
          , nodeHeight :: !BlockHeight
          -- | accumulated work so far
          , nodeWork   :: !BlockWork
          -- | skip magic block hash
          , nodeSkip   :: !BlockHash
          }
    deriving (Show, Read, Generic, Hashable, NFData)

instance Serialize BlockNode where
    get = do
        nodeHeader <- S.get
        nodeHeight <- getWord32le
        nodeWork <- S.get
        if nodeHeight == 0
            then do
                let nodeSkip = headerHash nodeHeader
                return BlockNode {..}
            else do
                nodeSkip <- S.get
                return BlockNode {..}
    put bn = do
        put $ nodeHeader bn
        putWord32le $ nodeHeight bn
        put $ nodeWork bn
        case nodeHeight bn of
            0 -> return ()
            _ -> put $ nodeSkip bn

instance Eq BlockNode where
    (==) = (==) `on` nodeHeader

instance Ord BlockNode where
    compare = compare `on` nodeHeight

-- | Memory-based header tree.
data HeaderMemory = HeaderMemory
    { memoryHeaderMap  :: !BlockMap
    , memoryBestHeader :: !BlockNode
    } deriving (Eq, Typeable, Show, Read, Generic, Hashable, NFData)

-- | Typeclass for block header chain storage monad.
class Monad m => BlockHeaders m where
    -- | Add a new 'BlockNode' to the chain. Does not validate.
    addBlockHeader :: BlockNode -> m ()
    -- | Get a 'BlockNode' associated with a 'BlockHash'.
    getBlockHeader :: BlockHash -> m (Maybe BlockNode)
    -- | Locate the 'BlockNode' for the highest block in the chain
    getBestBlockHeader :: m BlockNode
    -- | Set the highest block in the chain.
    setBestBlockHeader :: BlockNode -> m ()
    -- | Add a continuous bunch of block headers the chain. Does not validate.
    addBlockHeaders :: [BlockNode] -> m ()
    addBlockHeaders = mapM_ addBlockHeader

instance Monad m => BlockHeaders (StateT HeaderMemory m) where
    addBlockHeader = modify . addBlockHeaderMemory
    getBlockHeader bh = getBlockHeaderMemory bh <$> State.get
    getBestBlockHeader = gets memoryBestHeader
    setBestBlockHeader bn = modify $ \s -> s { memoryBestHeader = bn }

-- | Initialize memory-based chain.
initialChain :: Network -> HeaderMemory
initialChain net = HeaderMemory
    { memoryHeaderMap = genesisMap net
    , memoryBestHeader = genesisNode net
    }

-- | Initialize map for memory-based chain.
genesisMap :: Network -> BlockMap
genesisMap net =
    HashMap.singleton
        (shortBlockHash (headerHash (getGenesisHeader net)))
        (toShort (encode (genesisNode net)))

-- | Add block header to memory block map.
addBlockHeaderMemory :: BlockNode -> HeaderMemory -> HeaderMemory
addBlockHeaderMemory bn s@HeaderMemory{..} =
    let bm' = addBlockToMap bn memoryHeaderMap
    in s { memoryHeaderMap = bm' }

-- | Get block header from memory block map.
getBlockHeaderMemory :: BlockHash -> HeaderMemory -> Maybe BlockNode
getBlockHeaderMemory bh HeaderMemory {..} = do
    bs <- shortBlockHash bh `HashMap.lookup` memoryHeaderMap
    eitherToMaybe . decode $ fromShort bs

-- | Calculate short block hash taking eight non-zero bytes from the 16-byte
-- hash. This function will take the bytes that are not on the zero-side of the
-- hash, making colissions between short block hashes difficult.
shortBlockHash :: BlockHash -> ShortBlockHash
shortBlockHash = either error id . decode . B.take 8 . encode

-- | Add a block to memory-based block map.
addBlockToMap :: BlockNode -> BlockMap -> BlockMap
addBlockToMap node =
    HashMap.insert
    (shortBlockHash $ headerHash $ nodeHeader node)
    (toShort $ encode node)

-- | Get the ancestor of the provided 'BlockNode' at the specified
-- 'BlockHeight'.
getAncestor :: BlockHeaders m
            => BlockHeight
            -> BlockNode
            -> m (Maybe BlockNode)
getAncestor height node
    | height > nodeHeight node = return Nothing
    | otherwise = go node
  where
    e1 = error "Could not get skip header"
    e2 = error "Could not get previous block header"
    go walk
        | nodeHeight walk > height =
            let heightSkip = skipHeight (nodeHeight walk)
                heightSkipPrev = skipHeight (nodeHeight walk - 1)
             in if not (isGenesis walk) &&
                   (heightSkip == height ||
                    (heightSkip > height &&
                     not
                         (heightSkipPrev < heightSkip - 2 &&
                          heightSkipPrev >= height)))
                    then do
                        walk' <- fromMaybe e1 <$> getBlockHeader (nodeSkip walk)
                        go walk'
                    else do
                        walk' <-
                            fromMaybe e2 <$>
                            getBlockHeader (prevBlock (nodeHeader walk))
                        go walk'
        | otherwise = return $ Just walk

-- | Is the provided 'BlockNode' the Genesis block?
isGenesis :: BlockNode -> Bool
isGenesis BlockNode {nodeHeight = 0} = True
isGenesis _                          = False

-- | Build the genesis 'BlockNode' for the supplied 'Network'.
genesisNode :: Network -> BlockNode
genesisNode net =
    BlockNode
        { nodeHeader = getGenesisHeader net
        , nodeHeight = 0
        , nodeWork = headerWork (getGenesisHeader net)
        , nodeSkip = headerHash (getGenesisHeader net)
        }

-- | Validate a list of continuous block headers and import them to the
-- block chain. Return 'Left' on failure with error information.
connectBlocks :: BlockHeaders m
              => Network
              -> Timestamp       -- ^ current time
              -> [BlockHeader]
              -> m (Either String [BlockNode])
connectBlocks _ _ [] = return $ Right []
connectBlocks net t bhs@(bh:_) =
    runExceptT $ do
        unless (chained bhs) $
            throwError "Blocks to connect do not form a chain"
        par <-
            maybeToExceptT
                "Could not get parent block"
                (MaybeT (parentBlock bh))
        pars <- lift $ getParents 10 par
        bb <- lift getBestBlockHeader
        go par [] bb par pars bhs >>= \case
            bns@(bn:_) -> do
                lift $ addBlockHeaders bns
                let bb' = chooseBest bn bb
                when (bb' /= bb) $ lift $ setBestBlockHeader bb'
                return bns
            _ -> undefined
  where
    chained (h1:h2:hs) = headerHash h1 == prevBlock h2 && chained (h2 : hs)
    chained _          = True
    skipit lbh ls par
        | sh == nodeHeight lbh = return lbh
        | sh < nodeHeight lbh = do
            skM <- lift $ getAncestor sh lbh
            case skM of
                Just sk -> return sk
                Nothing ->
                    throwError $
                    "BUG: Could not get skip for block " ++
                    show (headerHash $ nodeHeader par)
        | otherwise = do
            let sn = ls !! fromIntegral (nodeHeight par - sh)
            when (nodeHeight sn /= sh) $
                throwError "BUG: Node height not right in skip"
            return sn
      where
        sh = skipHeight (nodeHeight par + 1)
    go _ acc _ _ _ [] = return acc
    go lbh acc bb par pars (h:hs) = do
        sk <- skipit lbh acc par
        bn <- ExceptT . return $ validBlock net t bb par pars h sk
        go lbh (bn : acc) (chooseBest bn bb) bn (take 10 $ par : pars) hs

-- | Block's parent. If the block header is in the store, its parent must also
-- be there. No block header get deleted or pruned from the store.
parentBlock :: BlockHeaders m
            => BlockHeader
            -> m (Maybe BlockNode)
parentBlock bh = getBlockHeader (prevBlock bh)

-- | Validate and connect single block header to the block chain. Return 'Left'
-- if fails to be validated.
connectBlock ::
       BlockHeaders m
    => Network
    -> Timestamp -- ^ current time
    -> BlockHeader
    -> m (Either String BlockNode)
connectBlock net t bh =
    runExceptT $ do
        par <-
            maybeToExceptT
                "Could not get parent block"
                (MaybeT (parentBlock bh))
        pars <- lift $ getParents 10 par
        skM <- lift $ getAncestor (skipHeight (nodeHeight par + 1)) par
        sk <-
            case skM of
                Just sk -> return sk
                Nothing ->
                    throwError $
                    "BUG: Could not get skip for block " ++
                    show (headerHash $ nodeHeader par)
        bb <- lift getBestBlockHeader
        bn <- ExceptT . return $ validBlock net t bb par pars bh sk
        let bb' = chooseBest bb bn
        lift $ addBlockHeader bn
        when (bb /= bb') . lift $ setBestBlockHeader bb'
        return bn

-- | Validate this block header. Build a 'BlockNode' if successful.
validBlock :: Network
           -> Timestamp     -- ^ current time
           -> BlockNode     -- ^ best block
           -> BlockNode     -- ^ immediate parent
           -> [BlockNode]   -- ^ 10 parents above
           -> BlockHeader   -- ^ header to validate
           -> BlockNode     -- ^ skip node (black magic)
           -> Either String BlockNode
validBlock net t bb par pars bh sk = do
    let mt = medianTime . map (blockTimestamp . nodeHeader) $ par : pars
        nt = blockTimestamp bh
        hh = headerHash bh
        nv = blockVersion bh
        ng = nodeHeight par + 1
        aw = nodeWork par + headerWork bh
    unless (isValidPOW net bh) $
        Left $ "Proof of work failed: " ++ show (headerHash bh)
    unless (nt <= t + 2 * 60 * 60) $
        Left $ "Invalid header timestamp: " ++ show nt
    unless (nt >= mt) $
        Left $ "Block timestamp too early: " ++ show nt
    unless (afterLastCP net (nodeHeight bb) ng) $
        Left $ "Rewriting pre-checkpoint chain: " ++ show ng
    unless (validCP net ng hh) $
        Left $ "Rejected checkpoint: " ++ show ng
    unless (bip34 net ng hh) $
        Left $ "Rejected BIP-34 block: " ++ show hh
    unless (validVersion net ng nv) $
        Left $ "Invalid block version: " ++ show nv
    return BlockNode { nodeHeader = bh
                     , nodeHeight = ng
                     , nodeWork = aw
                     , nodeSkip = headerHash $ nodeHeader sk
                     }

-- | Return the median of all provided timestamps. Can be unsorted. Error on
-- empty list.
medianTime :: [Timestamp] -> Timestamp
medianTime ts
    | null ts = error "Cannot compute median time of empty header list"
    | otherwise = sort ts !! (length ts `div` 2)

-- | Calculate the height of the skip (magic) block that corresponds to the
-- given height. The block hash of the ancestor at that height will be placed on
-- the 'BlockNode' structure to help locate ancestors at any height quickly.
skipHeight :: BlockHeight -> BlockHeight
skipHeight height
    | height < 2 = 0
    | height .&. 1 /= 0 = invertLowestOne (invertLowestOne $ height - 1) + 1
    | otherwise = invertLowestOne height

-- | Part of the skip black magic calculation.
invertLowestOne :: BlockHeight -> BlockHeight
invertLowestOne height = height .&. (height - 1)

-- | Get a number of parents for the provided block.
getParents :: BlockHeaders m
           => Int
           -> BlockNode
           -> m [BlockNode]   -- ^ starts from immediate parent
getParents = getpars []
  where
    getpars acc 0 _ = return $ reverse acc
    getpars acc n BlockNode{..}
      | nodeHeight == 0 = return $ reverse acc
      | otherwise = do
        parM <- getBlockHeader $ prevBlock nodeHeader
        case parM of
            Just bn -> getpars (bn : acc) (n - 1) bn
            Nothing -> error "BUG: All non-genesis blocks should have a parent"

-- | Verify that checkpoint location is valid.
validCP :: Network
        -> BlockHeight  -- ^ new child height
        -> BlockHash    -- ^ new child hash
        -> Bool
validCP net height newChildHash =
    case lookup height (getCheckpoints net) of
        Just cpHash -> cpHash == newChildHash
        Nothing     -> True

-- | New block height above the last checkpoint imported. Used to prevent a
-- reorg below the highest checkpoint that was already imported.
afterLastCP :: Network
            -> BlockHeight  -- ^ best height
            -> BlockHeight  -- ^ new imported block height
            -> Bool
afterLastCP net bestHeight newChildHeight =
    case lM of
        Just l  -> l < newChildHeight
        Nothing -> True
  where
    lM =
        listToMaybe . reverse $
        [c | (c, _) <- getCheckpoints net, c <= bestHeight]

-- | This block should be at least version 2 (BIP34). Block height must be
-- included in the coinbase transaction to prevent non-unique transaction
-- hashes.
bip34 :: Network
      -> BlockHeight  -- ^ new child height
      -> BlockHash    -- ^ new child hash
      -> Bool
bip34 net height hsh
    | fst (getBip34Block net) == 0 = True
    | fst (getBip34Block net) == height = snd (getBip34Block net) == hsh
    | otherwise = True

-- | Check if the provided block height and version are valid.
validVersion :: Network
             -> BlockHeight  -- ^ new child height
             -> Word32       -- ^ new child version
             -> Bool
validVersion net height version
    | version < 2 = height < fst (getBip34Block net)
    | version < 3 = height < getBip66Height net
    | version < 4 = height < getBip65Height net
    | otherwise = True

-- | Find last block with normal, as opposed to minimum difficulty (for test
-- networks).
lastNoMinDiff :: BlockHeaders m => Network -> BlockNode -> m BlockNode
lastNoMinDiff _ bn@BlockNode {nodeHeight = 0} = return bn
lastNoMinDiff net bn@BlockNode {..} = do
    let i = nodeHeight `mod` diffInterval net /= 0
        c = encodeCompact (getPowLimit net)
        l = blockBits nodeHeader == c
        e1 =
            error $
            "Could not get block header for parent of " ++
            show (headerHash nodeHeader)
    if i && l
        then do
            bn' <- fromMaybe e1 <$> getBlockHeader (prevBlock nodeHeader)
            lastNoMinDiff net bn'
        else return bn

-- | Returns the work required on a block header given the previous block. This
-- coresponds to @bitcoind@ function @GetNextWorkRequired@ in @main.cpp@.
nextWorkRequired :: BlockHeaders m
                 => Network
                 -> BlockNode
                 -> BlockHeader
                 -> m Word32
nextWorkRequired net par bh = do
    ma <- getAsertAnchor net
    case asert ma <|> daa <|> eda <|> pow of
        Just f  -> f par bh
        Nothing -> error "Could not determine difficulty algorithm"
  where
    asert ma = do
        anchor <- ma
        guard (nodeHeight par > nodeHeight anchor)
        return $ nextAsertWorkRequired net anchor
    daa = do
        daa_height <- getDaaBlockHeight net
        guard (nodeHeight par + 1 >= daa_height)
        return $ nextDaaWorkRequired net
    eda = do
        eda_height <- getEdaBlockHeight net
        guard (nodeHeight par + 1 >= eda_height)
        return $ nextEdaWorkRequired net
    pow = return $ nextPowWorkRequired net

-- | Find out the next amount of work required according to the Emergency
-- Difficulty Adjustment (EDA) algorithm from Bitcoin Cash.
nextEdaWorkRequired ::
       BlockHeaders m => Network -> BlockNode -> BlockHeader -> m Word32
nextEdaWorkRequired net par bh
    | nodeHeight par + 1 `mod` diffInterval net == 0 =
        nextWorkRequired net par bh
    | minDifficulty = return (encodeCompact (getPowLimit net))
    | blockBits (nodeHeader par) == encodeCompact (getPowLimit net) =
        return (encodeCompact (getPowLimit net))
    | otherwise = do
        par6 <- fromMaybe e1 <$> getAncestor (nodeHeight par - 6) par
        pars <- getParents 10 par
        pars6 <- getParents 10 par6
        let par6med =
                medianTime $ map (blockTimestamp . nodeHeader) (par6 : pars6)
            parmed = medianTime $ map (blockTimestamp . nodeHeader) (par : pars)
            mtp6 = parmed - par6med
        if mtp6 < 12 * 3600
            then return $ blockBits (nodeHeader par)
            else return $
                 let (diff, _) = decodeCompact (blockBits (nodeHeader par))
                     ndiff = diff + (diff `shiftR` 2)
                  in if getPowLimit net > ndiff
                         then encodeCompact (getPowLimit net)
                         else encodeCompact ndiff
  where
    minDifficulty =
        blockTimestamp bh >
        blockTimestamp (nodeHeader par) + getTargetSpacing net * 2
    e1 = error "Could not get seventh ancestor of block"

-- | Find the next amount of work required according to the Difficulty
-- Adjustment Algorithm (DAA) from Bitcoin Cash.
nextDaaWorkRequired ::
       BlockHeaders m => Network -> BlockNode -> BlockHeader -> m Word32
nextDaaWorkRequired net par bh
    | minDifficulty = return (encodeCompact (getPowLimit net))
    | otherwise = do
        unless (height >= diffInterval net) $
            error "Block height below difficulty interval"
        l <- getSuitableBlock par
        par144 <- fromMaybe e1 <$> getAncestor (height - 144) par
        f <- getSuitableBlock par144
        let nextTarget = computeTarget net f l
        if nextTarget > getPowLimit net
            then return $ encodeCompact (getPowLimit net)
            else return $ encodeCompact nextTarget
  where
    height = nodeHeight par
    e1 = error "Cannot get ancestor at parent - 144 height"
    minDifficulty =
        blockTimestamp bh >
        blockTimestamp (nodeHeader par) + getTargetSpacing net * 2

mtp :: BlockHeaders m => BlockNode -> m Timestamp
mtp bn = do
    pars <- getParents 10 bn
    return $ medianTime . map (blockTimestamp . nodeHeader) $ bn : pars

-- TODO: Test this
firstGreaterOrEqual :: BlockHeaders m
                    => Network
                    -> (BlockNode -> m Ordering)
                    -> m (Maybe BlockNode)
firstGreaterOrEqual net f = runMaybeT $ do
    (a, b) <- lift $ extremes net
    go a b
  where
    go a b = do
        a' <- lift $ f a
        b' <- lift $ f b
        guard $ b' /= LT
        case a' of
            EQ -> return a
            GT -> return a
            LT -> do
                m <- MaybeT $ middleBlock a b
                m' <- lift $ f m
                if m' == LT
                    then go m b
                    else go a m

-- TODO: Test this
lastSmallerOrEqual :: BlockHeaders m
                   => Network
                   -> (BlockNode -> m Ordering)
                   -> m (Maybe BlockNode)
lastSmallerOrEqual net f = runMaybeT $ do
    (a, b) <- lift $ extremes net
    go a b
  where
    go a b = do
        a' <- lift $ f a
        b' <- lift $ f b
        guard $ a' /= GT
        case b' of
            EQ -> return b
            LT -> return b
            GT -> do
                  m <- MaybeT $ middleBlock a b
                  m' <- lift $ f m
                  if m' == GT
                      then go a m
                      else go m b

extremes :: BlockHeaders m => Network -> m (BlockNode, BlockNode)
extremes net = do
    b <- getBestBlockHeader
    return (genesisNode net, b)

middleBlock :: BlockHeaders m => BlockNode -> BlockNode -> m (Maybe BlockNode)
middleBlock a b = runMaybeT $ do
    h <- MaybeT $ return (middleOf (nodeHeight a) (nodeHeight b))
    MaybeT $ getAncestor h b

middleOf :: Integral a => a -> a -> Maybe a
middleOf a b
    | b - a <= 1 = Nothing
    | otherwise = Just m
  where
    m = a + ((b - a) `div` 2)

-- TODO: Use known anchor after fork
getAsertAnchor :: BlockHeaders m => Network -> m (Maybe BlockNode)
getAsertAnchor net =
    case getAsertActivationTime net of
        Nothing  -> return Nothing
        Just act -> firstGreaterOrEqual net (f act)
  where
    f act bn = do
        m <- mtp bn
        return $ compare m act

-- | Find the next amount of work required according to the aserti3-2d algorithm.
nextAsertWorkRequired :: BlockHeaders m
                      => Network
                      -> BlockNode
                      -> BlockNode
                      -> BlockHeader
                      -> m Word32
nextAsertWorkRequired net anchor par bh = do
    anchor_parent <- fromMaybe e_fork <$>
                      getBlockHeader (prevBlock (nodeHeader anchor))
    let anchor_parent_time = toInteger $ blockTimestamp $ nodeHeader anchor_parent
        time_diff = current_time - anchor_parent_time
    return $ computeAsertBits halflife anchor_bits time_diff height_diff
  where
    halflife = getAsertHalfLife net
    anchor_height = toInteger $ nodeHeight anchor
    anchor_bits = blockBits $ nodeHeader anchor
    current_height = toInteger (nodeHeight par) + 1
    height_diff = current_height - anchor_height
    current_time = toInteger $ blockTimestamp bh
    e_fork = error "Could not get fork block header"

idealBlockTime :: Integer
idealBlockTime = 10 * 60

rBits :: Int
rBits = 16

radix :: Integer
radix = 1 `shiftL` rBits

maxBits :: Word32
maxBits = 0x1d00ffff

maxTarget :: Integer
maxTarget = fst $ decodeCompact maxBits

computeAsertBits
    :: Integer
    -> Word32
    -> Integer
    -> Integer
    -> Word32
computeAsertBits halflife anchor_bits time_diff height_diff =
    if e2 >= 0 && e2 < 65536
    then if g4 == 0
         then encodeCompact 1
         else if g4 > maxTarget
              then maxBits
              else encodeCompact g4
    else error $ "Exponent not in range: " ++ show e2
  where
    g1 = fst (decodeCompact anchor_bits)
    e1 = ((time_diff - idealBlockTime * (height_diff + 1)) * radix)
         `quot`
         halflife
    s = e1 `shiftR` rBits
    e2 = e1 - s * radix
    g2 = g1 * (radix +
               ((195766423245049*e2 + 971821376*e2^2 + 5127*e2^3 + 2^47)
                `shiftR`
                (rBits*3)))
    g3 = if s < 0
         then g2 `shiftR` negate (fromIntegral s)
         else g2 `shiftL` fromIntegral s
    g4 = g3 `shiftR` rBits


-- | Compute Bitcoin Cash DAA target for a new block.
computeTarget :: Network -> BlockNode -> BlockNode -> Integer
computeTarget net f l =
    let work = (nodeWork l - nodeWork f) * fromIntegral (getTargetSpacing net)
        actualTimespan =
            blockTimestamp (nodeHeader l) - blockTimestamp (nodeHeader f)
        actualTimespan'
            | actualTimespan > 288 * getTargetSpacing net =
                288 * getTargetSpacing net
            | actualTimespan < 72 * getTargetSpacing net =
                72 * getTargetSpacing net
            | otherwise = actualTimespan
        work' = work `div` fromIntegral actualTimespan'
     in 2 ^ (256 :: Integer) `div` work'

-- | Get suitable block for Bitcoin Cash DAA computation.
getSuitableBlock :: BlockHeaders m => BlockNode -> m BlockNode
getSuitableBlock par = do
    unless (nodeHeight par >= 3) $ error "Block height is less than three"
    blocks <- (par :) <$> getParents 2 par
    return $ sortBy (compare `on` blockTimestamp . nodeHeader) blocks !! 1

-- | Returns the work required on a block header given the previous block. This
-- coresponds to bitcoind function GetNextWorkRequired in main.cpp.
nextPowWorkRequired ::
       BlockHeaders m => Network -> BlockNode -> BlockHeader -> m Word32
nextPowWorkRequired net par bh
    | nodeHeight par + 1 `mod` diffInterval net /= 0 =
        if getAllowMinDifficultyBlocks net
            then if ht > pt + delta
                     then return $ encodeCompact (getPowLimit net)
                     else do
                         d <- lastNoMinDiff net par
                         return $ blockBits $ nodeHeader d
            else return $ blockBits $ nodeHeader par
    | otherwise = do
        let rh = nodeHeight par - (diffInterval net - 1)
        a <- fromMaybe e1 <$> getAncestor rh par
        let t = blockTimestamp $ nodeHeader a
        return $ calcNextWork net (nodeHeader par) t
  where
    e1 = error "Could not get ancestor for block header"
    pt = blockTimestamp $ nodeHeader par
    ht = blockTimestamp bh
    delta = getTargetSpacing net * 2

-- | Computes the work required for the first block in a new retarget period.
calcNextWork :: Network
             -> BlockHeader  -- ^ last block in previous retarget (parent)
             -> Timestamp    -- ^ timestamp of first block in previous retarget
             -> Word32
calcNextWork net header time
    | getPowNoRetargetting net = blockBits header
    | new > getPowLimit net = encodeCompact (getPowLimit net)
    | otherwise = encodeCompact new
  where
    s = blockTimestamp header - time
    n | s < getTargetTimespan net `div` 4 = getTargetTimespan net `div` 4
      | s > getTargetTimespan net * 4 = getTargetTimespan net * 4
      | otherwise = s
    l = fst $ decodeCompact $ blockBits header
    new = l * fromIntegral n `div` fromIntegral (getTargetTimespan net)

-- | Returns True if the difficulty target (bits) of the header is valid and the
-- proof of work of the header matches the advertised difficulty target. This
-- function corresponds to the function @CheckProofOfWork@ from @bitcoind@ in
-- @main.cpp@.
isValidPOW :: Network -> BlockHeader -> Bool
isValidPOW net h
    | target <= 0 || over || target > getPowLimit net = False
    | otherwise = blockPOW (headerHash h) <= fromIntegral target
  where
    (target, over) = decodeCompact $ blockBits h

-- | Returns the proof of work of a block header hash as an 'Integer' number.
blockPOW :: BlockHash -> Integer
blockPOW =  bsToInteger . B.reverse . encode

-- | Returns the work represented by this block. Work is defined as the number
-- of tries needed to solve a block in the average case with respect to the
-- target.
headerWork :: BlockHeader -> Integer
headerWork bh = largestHash `div` (target + 1)
  where
    target      = fst $ decodeCompact $ blockBits bh
    largestHash = 1 `shiftL` 256

-- | Number of blocks on average between difficulty cycles (2016 blocks).
diffInterval :: Network -> Word32
diffInterval net = getTargetTimespan net `div` getTargetSpacing net

-- | Compare two blocks to get the best.
chooseBest :: BlockNode -> BlockNode -> BlockNode
chooseBest b1 b2 | nodeWork b1 == nodeWork b2 =
                       if nodeHeight b1 >= nodeHeight b2
                       then b1
                       else b2
                 | nodeWork b1 > nodeWork b2 = b1
                 | otherwise = b2

-- | Get list of blocks for a block locator.
blockLocatorNodes :: BlockHeaders m => BlockNode -> m [BlockNode]
blockLocatorNodes best =
    reverse <$> go [] best 1
  where
    e1 = error "Could not get ancestor"
    go loc bn n =
        let loc' = bn : loc
            n' = if length loc' > 10
                 then n * 2
                 else 1
        in if nodeHeight bn < n'
           then do a <- fromMaybe e1 <$> getAncestor 0 bn
                   return $ a : loc'
           else do let h = nodeHeight bn - n'
                   bn' <- fromMaybe e1 <$> getAncestor h bn
                   go loc' bn' n'

-- | Get block locator.
blockLocator :: BlockHeaders m => BlockNode -> m BlockLocator
blockLocator bn = map (headerHash . nodeHeader) <$> blockLocatorNodes bn

-- | Become rich beyond your wildest dreams.
mineBlock :: Network -> Word32 -> BlockHeader -> BlockHeader
mineBlock net seed h =
    head
        [ j
        | i <- (+ seed) <$> [0 .. maxBound]
        , let j = h {bhNonce = i}
        , isValidPOW net j
        ]

-- | Generate and append new blocks (mining). Only practical in regtest network.
appendBlocks ::
       Network
    -> Word32 -- ^ random seed
    -> BlockHeader
    -> Int
    -> [BlockHeader]
appendBlocks _ _ _ 0 = []
appendBlocks net seed bh i =
    bh' : appendBlocks net seed bh' (i - 1)
  where
    bh' = mineBlock net seed bh
        { prevBlock = headerHash bh
          -- Just to make it different in every header
        , merkleRoot = sha256 $ encode seed
        }

-- | Find the last common block ancestor between provided block headers.
splitPoint :: BlockHeaders m => BlockNode -> BlockNode -> m BlockNode
splitPoint l r = do
    let h = min (nodeHeight l) (nodeHeight r)
    ll <- fromMaybe e <$> getAncestor h l
    lr <- fromMaybe e <$> getAncestor h r
    f ll lr
  where
    e = error "BUG: Could not get ancestor at lowest height"
    f ll lr =
        if ll == lr
            then return lr
            else do
                let h = nodeHeight ll - 1
                pl <- fromMaybe e <$> getAncestor h ll
                pr <- fromMaybe e <$> getAncestor h lr
                f pl pr

-- | Generate the entire Genesis block for 'Network'.
genesisBlock :: Network -> Block
genesisBlock net = Block (getGenesisHeader net) [genesisTx]

-- | Compute block subsidy at particular height.
computeSubsidy :: Network -> BlockHeight -> Word64
computeSubsidy net height =
    let halvings = height `div` getHalvingInterval net
        ini = 50 * 100 * 1000 * 1000
     in if halvings >= 64
            then 0
            else ini `shiftR` fromIntegral halvings
