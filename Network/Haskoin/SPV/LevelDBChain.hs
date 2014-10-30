{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Haskoin.SPV.LevelDBChain
( LevelSession(..)
, DBHandle
)
where

import Control.Monad (when, unless, foldM)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (left, runEitherT)
import qualified Control.Monad.State as S (StateT, gets)

import Data.Word (Word32)
import Data.List (sort, nub)
import Data.Bits (shiftR, shiftL)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.Default (def)
import qualified Data.ByteString as BS (ByteString, append, reverse)

import qualified Database.LevelDB.Base as DB 
    ( DB
    , get
    , put
    , write
    , BatchOp( Put )
    )

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Util
import Network.Haskoin.Constants

data LevelSession = LevelSession { handle :: DB.DB }

type DBHandle = S.StateT LevelSession IO

indexKey :: BlockHash -> BS.ByteString
indexKey h = "index_" `BS.append` encode' h

bestHeaderKey :: BS.ByteString
bestHeaderKey = "bestheader"

bestBlockKey :: BS.ByteString
bestBlockKey = "bestblock"

fastCatchupKey :: BS.ByteString
fastCatchupKey = "starttime"

lastDownloadKey :: BS.ByteString
lastDownloadKey = "lastdownload"

instance BlockHeaderStore DBHandle where

    getBlockHeaderNode h = do
        db  <- S.gets handle
        res <- DB.get db def $ indexKey h
        when (isNothing res) $ error "getBlockHeaderNode: Key does not exist"
        return $ fromJust $ decodeToMaybe =<< res
        
    putBlockHeaderNode bhn = do
        db <- S.gets handle
        DB.put db def (indexKey $ nodeBlockHash bhn) $ encode' bhn

    existsBlockHeaderNode h = do
        db <- S.gets handle
        res <- DB.get db def $ indexKey h
        return $ isJust res
        
    getBestBlockHeader = do
        db  <- S.gets handle
        key <- (decodeToMaybe =<<) <$> DB.get db def bestHeaderKey
        getBlockHeaderNode $ fromJust key

    setBestBlockHeader bhn = do
        db <- S.gets handle
        DB.put db def bestHeaderKey $ encode' $ nodeBlockHash bhn

    getBestBlock = do
        db <- S.gets handle
        key <- (decodeToMaybe =<<) <$> DB.get db def bestBlockKey
        getBlockHeaderNode $ fromJust key

    setBestBlock bhn = do
        db <- S.gets handle
        DB.put db def bestBlockKey $ encode' $ nodeBlockHash bhn

    getLastDownload = do
        db  <- S.gets handle
        key <- (decodeToMaybe =<<) <$> DB.get db def lastDownloadKey
        getBlockHeaderNode $ fromJust key

    setLastDownload bhn = do
        db <- S.gets handle
        DB.put db def lastDownloadKey $ encode' $ nodeBlockHash bhn

