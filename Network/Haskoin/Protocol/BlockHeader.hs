module Network.Haskoin.Protocol.BlockHeader 
( BlockHeader(..) 
, blockid
) where

import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Network.Haskoin.Crypto (Hash256, doubleHash256)
import Network.Haskoin.Util (encode')

-- | Data type recording information on a 'Block'. The hash of a block is
-- defined as the hash of this data structure. The block mining process
-- involves finding a partial hash collision by varying the nonce in the
-- 'BlockHeader' and/or additional randomness in the 'CoinbaseTx' of this
-- 'Block'. Variations in the 'CoinbaseTx' will result in different merkle 
-- roots in the 'BlockHeader'.
data BlockHeader = 
    BlockHeader {
                  -- | Block version information, based on the version of the
                  -- software creating this block.
                  blockVersion   :: !Word32
                  -- | Hash of the previous block (parent) referenced by this
                  -- block.
                , prevBlock      :: !Hash256
                  -- | Root of the merkle tree of all transactions pertaining
                  -- to this block.
                , merkleRoot     :: !Hash256
                  -- | Unix timestamp recording when this block was created
                , blockTimestamp :: !Word32
                  -- | The difficulty target being used for this block
                , blockBits      :: !Word32
                  -- | A random nonce used to generate this block. Additional
                  -- randomness is included in the coinbase transaction of
                  -- this block.
                , bhNonce        :: !Word32
                } deriving (Eq, Show)

instance Binary BlockHeader where

    get = BlockHeader <$> getWord32le
                      <*> get
                      <*> get
                      <*> getWord32le
                      <*> getWord32le
                      <*> getWord32le

    put (BlockHeader v p m bt bb n) = do
        putWord32le v
        put         p
        put         m
        putWord32le bt
        putWord32le bb
        putWord32le n 

-- | Compute the hash of a block header
blockid :: BlockHeader -> Hash256
blockid = doubleHash256 . encode'

