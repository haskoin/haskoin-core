module Bitcoin.LevelDB
( BlockIndex(..)
, getHandle
, buildBlockIndex
, writeBlock
, readBlock
, BlockChainIO
) where

import Data.Default

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.BlockHeader
import Bitcoin.Util

import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Database.LevelDB as DB

type BlockChainIO a = StateT DB.DB (ResourceT IO) a

data BlockIndex = BlockIndex 
    { biHash      :: Word256
    , biPrev      :: Word256
    , biHeight    :: Word32
    , biTx        :: Word32
    , biChainWork :: Word256
    , biChainTx   :: Word32
    , biFile      :: Word32
    , biDataPos   :: Word32
    , biUndoPos   :: Word32
    , biStatus    :: Word32
    } deriving (Eq, Read, Show)

instance BitcoinProtocol BlockIndex where

    bitcoinGet = BlockIndex <$> getWord256be
                            <*> getWord256be
                            <*> getWord32le
                            <*> getWord32le
                            <*> getWord256be
                            <*> getWord32le
                            <*> getWord32le
                            <*> getWord32le
                            <*> getWord32le
                            <*> getWord32le

    bitcoinPut (BlockIndex h p he tx cw ct f d u s) = do
        putWord256be h
        putWord256be p
        putWord32le  he
        putWord32le  tx
        putWord256be cw
        putWord32le  ct
        putWord32le  f
        putWord32le  d
        putWord32le  u
        putWord32le  s

getHandle :: ResourceT IO DB.DB
getHandle = do
    db <- DB.open "blockindex"
        DB.defaultOptions
            { DB.createIfMissing = True
            , DB.cacheSize = 2048
            }
    return db

readBlock :: String -> BlockChainIO (Maybe String)
readBlock s = do
    db <- get
    val <- lift $ DB.get db def (BSC.pack s)
    return $ val >>= return . BSC.unpack

writeBlock :: String -> BlockChainIO ()
writeBlock s = do
    db <- get
    lift $ DB.put db def (BSC.pack s) (BSC.pack s)
    return ()

buildBlockIndex :: Block -> BS.ByteString
buildBlockIndex b = toStrictBS . runPut . bitcoinPut $ bi
    where h  = blockHeader b
          bi = BlockIndex (fromIntegral 1234)
                          (prevBlock h)
                          (fromIntegral 1)
                          (fromIntegral 2)
                          (fromIntegral 3)
                          (fromIntegral 4)
                          (fromIntegral 5)
                          (fromIntegral 6)
                          (fromIntegral 7)
                          (fromIntegral 8)

