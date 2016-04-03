module Network.Haskoin.Node.HeaderTree.Types where

import           Data.Word             (Word32, Word64)
import           Database.Persist      (PersistField (..), PersistValue (..),
                                        SqlType (..))
import           Database.Persist.Sql  (PersistFieldSql (..))
import           Network.Haskoin.Block
import           Network.Haskoin.Util

type BlockHeight = Word32
type ShortHash = Word64
type Timestamp = Word32
type Work = Double

newtype NodeHeader = NodeHeader { getNodeHeader :: BlockHeader }
    deriving (Show, Eq)

{- SQL database backend for HeaderTree -}

instance PersistField NodeHeader where
    toPersistValue = PersistByteString . encode' . getNodeHeader
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Could not decode block header" $ NodeHeader <$> decodeToMaybe bs
    fromPersistValue _ = Left "Invalid persistent block header"

instance PersistFieldSql NodeHeader where
    sqlType _ = SqlBlob
