{-# LANGUAGE DeriveGeneric #-}
module Network.Haskoin.Node.HeaderTree.Types where

import           Data.Binary           (Binary)
import           Data.LargeWord        (Word256)
import           Data.Word             (Word32, Word64)
import           Database.Persist      (PersistField (..), PersistValue (..),
                                        SqlType (..))
import           Database.Persist.Sql  (PersistFieldSql (..))
import           GHC.Generics          (Generic)
import           Network.Haskoin.Block
import           Network.Haskoin.Util

type BlockHeight = Word32
type ShortHash = Word64
type Timestamp = Word32

newtype NodeHeader = NodeHeader { getNodeHeader :: BlockHeader }
    deriving (Show, Eq)

newtype Work = Work { getWork :: Word256 }
    deriving (Show, Eq, Ord)

newtype Pivots = Pivots { getPivots :: [(BlockHeight, Word32)] }
    deriving (Generic, Show, Eq, Ord)

instance Binary Pivots

{- SQL database backend for HeaderTree -}

instance PersistField Pivots where
    toPersistValue = PersistByteString . encode'
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Could not decode pivots" $ decodeToMaybe bs
    fromPersistValue _ = Left "Invalid persistent block header"

instance PersistFieldSql Pivots where
    sqlType _ = SqlBlob

instance PersistField NodeHeader where
    toPersistValue = PersistByteString . encode' . getNodeHeader
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Could not decode block header" $ NodeHeader <$> decodeToMaybe bs
    fromPersistValue _ = Left "Invalid persistent block header"

instance PersistFieldSql NodeHeader where
    sqlType _ = SqlBlob

instance PersistField Work where
    toPersistValue = PersistByteString . encode' . getWork
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Could not decode work" $ Work <$> decodeToMaybe bs
    fromPersistValue _ = Left "Invalid persistent work"

instance PersistFieldSql Work where
    sqlType _ = SqlBlob
