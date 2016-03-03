module Network.Haskoin.Node.HeaderTree.Types where

import           Data.LargeWord              (Word256)
import           Data.String.Conversions     (cs)
import           Data.Word                   (Word32)
import           Database.Persist            (PersistField (..),
                                              PersistValue (..), SqlType (..))
import           Database.Persist.Sql        (PersistFieldSql (..))
import           Network.Haskoin.Block
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util

type BlockHeight = Word32
type Timestamp = Word32

newtype MerkleHash = MerkleHash { getMerkleHash :: TxHash }
    deriving (Show, Eq)

newtype NodeHash = NodeHash { getNodeHash :: BlockHash }
    deriving (Show, Eq)

newtype NodeHeader = NodeHeader { getNodeHeader :: BlockHeader }
    deriving (Show, Eq)

newtype Work = Work { getWork :: Word256 }
    deriving (Show, Eq, Ord)

{- SQL database backend for HeaderTree -}

instance PersistField MerkleHash where
    toPersistValue = PersistText . cs . txHashToHex . getMerkleHash
    fromPersistValue (PersistText h) = maybeToEither "Could not decode block hash" $
        MerkleHash <$> hexToTxHash (cs h)
    fromPersistValue (PersistByteString h) = maybeToEither "Could not decode block hash" $
        MerkleHash <$> hexToTxHash h
    fromPersistValue _ = Left "Invalid persistent block hash"

instance PersistFieldSql MerkleHash where
    sqlType _ = SqlString

instance PersistField NodeHash where
    toPersistValue = PersistText . cs . blockHashToHex . getNodeHash
    fromPersistValue (PersistText h) = maybeToEither "Could not decode block hash" $
        NodeHash <$> hexToBlockHash (cs h)
    fromPersistValue (PersistByteString h) = maybeToEither "Could not decode block hash" $
        NodeHash <$> hexToBlockHash h
    fromPersistValue _ = Left "Invalid persistent block hash"

instance PersistFieldSql NodeHash where
    sqlType _ = SqlString

instance PersistField Work where
    toPersistValue = PersistText . cs . encodeHex . encode' . getWork
    fromPersistValue (PersistText h) = maybeToEither "Could not decode work" $
        fmap Work $ decodeToMaybe =<< decodeHex (cs h)
    fromPersistValue (PersistByteString h) = maybeToEither "Could not decode work" $
        fmap Work $ decodeToMaybe =<< decodeHex h
    fromPersistValue _ = Left "Invalid persistent work"

instance PersistFieldSql Work where
    sqlType _ = SqlString
