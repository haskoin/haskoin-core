module Network.Haskoin.Node.HeaderTree.Types where

import           Data.Serialize        (decode, encode)
import           Data.String           (fromString)
import           Data.Word             (Word64)
import           Database.Persist      (PersistField (..), PersistValue (..),
                                        SqlType (..))
import           Database.Persist.Sql  (PersistFieldSql (..))
import           Network.Haskoin.Block

type ShortHash = Word64
type Work = Double

newtype NodeHeader = NodeHeader { getNodeHeader :: BlockHeader }
    deriving (Show, Eq)

{- SQL database backend for HeaderTree -}

instance PersistField NodeHeader where
    toPersistValue = PersistByteString . encode . getNodeHeader
    fromPersistValue (PersistByteString bs) =
        case decode bs of
            Right x -> Right (NodeHeader x)
            Left  e -> Left  (fromString e)
    fromPersistValue _ = Left "Invalid persistent block header"

instance PersistFieldSql NodeHeader where
    sqlType _ = SqlBlob
