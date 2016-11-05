{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Wallet.Types.BlockInfo
(
  BlockInfo(..)
, JsonHash256
, fromNodeBlock
)
where

import Data.String.Conversions  (cs, ConvertibleStrings(..))
import Data.Maybe               (fromMaybe)
import Data.Aeson.TH            (deriveJSON)
import Data.Word                (Word32)
import Data.Time                (UTCTime)
import Data.ByteString          (ByteString)
import Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import Data.Aeson               (FromJSON, ToJSON, Value (String),
                                 parseJSON, toJSON, withText)

import Network.Haskoin.Block
import Network.Haskoin.Node.HeaderTree
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Constants


newtype JsonHash256 = JsonHash256 { jsonGetHash256 :: Hash256 }
    deriving (Eq, Show)

jsonHashToHex :: JsonHash256 -> ByteString
jsonHashToHex = blockHashToHex . BlockHash . jsonGetHash256

instance ToJSON JsonHash256 where
    toJSON = String . cs . jsonHashToHex

instance FromJSON JsonHash256 where
    parseJSON = withText "JsonHash256" $
        return . JsonHash256 . getBlockHash <$>
            fromMaybe (error "Invalid 256 bit hash") .
                hexToBlockHash . cs

instance ConvertibleStrings JsonHash256 String where
    convertString = cs . jsonHashToHex

data BlockInfo = BlockInfo
   { blockInfoHash          :: !BlockHash
   , blockInfoHeight        :: !BlockHeight
   , blockInfoVersion       :: !Word32
   , blockInfoTimestamp     :: !UTCTime
   , blockInfoPrevBlock     :: !BlockHash
   , blockInfoMerkleRoot    :: !JsonHash256
   , blockInfoBits          :: !Word32
   , blockInfoNonce         :: !Word32
   , blockInfoChain         :: !Word32
   , blockInfoChainWork     :: !Double
   } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 9) ''BlockInfo)

fromNodeBlock :: NodeBlock -> BlockInfo
fromNodeBlock nb =
    BlockInfo
       { blockInfoHash          =   headerHash header
       , blockInfoVersion       = blockVersion header
       , blockInfoPrevBlock     =    prevBlock header
       , blockInfoNonce         =      bhNonce header
       , blockInfoBits          =    blockBits header
       , blockInfoMerkleRoot    = JsonHash256 $ merkleRoot header
       , blockInfoTimestamp     = utcTimestamp
       , blockInfoChainWork     = nodeWork   nb
       , blockInfoHeight        = nodeHeight nb
       , blockInfoChain         = nodeChain  nb
       }
  where
    header = nodeHeader nb
    utcTimestamp = posixSecondsToUTCTime . realToFrac .
        blockTimestamp $ header

