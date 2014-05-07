module Network.Haskoin.Protocol.InvVector 
( InvVector(..) 
, InvType(..)
) where

import Control.Applicative ((<$>),(<*>))

import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

import Network.Haskoin.Crypto (Hash256)

-- | Data type identifying the type of an inventory vector. 
data InvType 
    = InvError -- ^ Error. Data containing this type can be ignored.
    | InvTx    -- ^ InvVector hash is related to a transaction 
    | InvBlock -- ^ InvVector hash is related to a block
    | InvMerkleBlock -- ^ InvVector has is related to a merkle block
    deriving (Eq, Show)

instance Binary InvType where

    get = go =<< getWord32le
      where 
        go x = case x of
            0 -> return InvError
            1 -> return InvTx
            2 -> return InvBlock
            3 -> return InvMerkleBlock
            _ -> fail "bitcoinGet InvType: Invalid Type"

    put x = putWord32le $ case x of
                InvError       -> 0
                InvTx          -> 1
                InvBlock       -> 2
                InvMerkleBlock -> 3

-- | Invectory vectors represent hashes identifying objects such as a 'Block'
-- or a 'Tx'. They are sent inside messages to notify other peers about 
-- new data or data they have requested.
data InvVector = 
    InvVector {
                -- | Type of the object referenced by this inventory vector
                invType :: !InvType
                -- | Hash of the object referenced by this inventory vector
              , invHash :: !Hash256
              } deriving (Eq, Show)

instance Binary InvVector where
    get = InvVector <$> get <*> get
    put (InvVector t h) = put t >> put h


