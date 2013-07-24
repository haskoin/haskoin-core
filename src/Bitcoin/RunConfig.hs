module Bitcoin.RunConfig
( RunConfig(..)
, RunMode(..)
, BitcoinConfig
, Flag
, BitcoinApp
, AppState(..)
, MemState(..)
, STMState(..)
, MemMap
, buildRunConfig
, flagOptions
, usageHeader
, getRunMode
, getHost
, getPort
, getDBName
, maxBlockSize
, currentBlockVersion
, haskoinUserAgent
, protocolVersion
, genesisBlock
, genesisBlockHash
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent.STM

import Bitcoin.Protocol
import Bitcoin.Protocol.Block
import Bitcoin.Protocol.Tx
import Bitcoin.Protocol.BlockHeader
import Bitcoin.Protocol.Script
import Bitcoin.BlockChain.BlockIndex
import Bitcoin.Util

import qualified Data.ByteString as BS

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) 

import System.Console.GetOpt
import Data.Word
import Network

{- Command line arguments -}

data Flag = FTestnet
    deriving Show

flagOptions :: [OptDescr Flag] 
flagOptions = 
    [ Option ['T'] ["testnet"] (NoArg FTestnet) "Run haskoin in testnet mode" 
    ]
 
usageHeader :: String
usageHeader = "Usage: haskoin [OPTION...]"

{- Bitcoin application monads -}

type BitcoinApp m = ReaderT AppState m

data AppState = AppState 
    { memState  :: MemState 
    , runConfig :: RunConfig
    } 

{- Bitcoin Mem -}

data MemState = MemState
    { stateBlockIndex   :: TVar (STMState BlockIndex)
    , stateOrphanBlocks :: TVar (STMState Block)
    } 

type MemMap v = Map Word256 v

data STMState v = STMState
    { stmMap  :: MemMap v
    , stmBest :: Maybe v  
    }

{- Runtime Configuration -}

data RunMode = Prodnet | Testnet deriving Show

data RunConfig = RunConfig
    { runMode  :: RunMode
    , peerHost :: String
    , peerPort :: PortID
    , dbName   :: String
    } deriving Show

type BitcoinConfig = Reader RunConfig

buildRunConfig :: [Flag] -> RunConfig
buildRunConfig fs = loop fs $ RunConfig 
                                Prodnet 
                                "127.0.0.1" 
                                (PortNumber 8333)
                                "blockindex"
    where loop (FTestnet:fs) conf = loop fs conf
                                             { runMode = Testnet
                                             , peerPort = PortNumber 18333 
                                             , dbName = "blockindex-testnet"
                                             }
          loop []            conf = conf

getRunMode :: BitcoinConfig RunMode
getRunMode = runMode <$> ask

getHost :: BitcoinConfig String
getHost = peerHost <$> ask

getPort :: BitcoinConfig PortID
getPort = peerPort <$> ask

getDBName :: BitcoinConfig String
getDBName = dbName <$> ask

-- Various constants

maxBlockSize :: Int
maxBlockSize = 1000000 

currentBlockVersion :: Word32
currentBlockVersion = fromIntegral 1

haskoinUserAgent :: String
haskoinUserAgent = "haskoin:0.0.1"

protocolVersion :: Word32
protocolVersion = 70001

genesisBlock :: BitcoinConfig Block
genesisBlock = go =<< getRunMode
    where go Testnet = return testnetGenesisBlock
          go Prodnet = return prodnetGenesisBlock

genesisBlockHash :: BitcoinConfig Word256
genesisBlockHash = blockHash <$> genesisBlock

genesisMessage :: String
genesisMessage = 
    "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks";

genesisMerkle :: Word256
genesisMerkle = 
    fromIntegral 
        0x3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a

prodnetGenesisBlock :: Block
prodnetGenesisBlock = 
    Block
        (BlockHeader
            (fromIntegral 0x01)        -- version
            (fromIntegral 0x00)        -- previous block
            genesisMerkle              -- merkle root
            (fromIntegral 1231006505)  -- timestamp
            (fromIntegral 0x1d00ffff)  -- bits 
            (fromIntegral 2083236893)) -- nonce
        (CoinbaseTx
            txCurrentVersion
            (BS.append
                (BS.pack [0x04,0xff,0xff,0x00,0x1d,0x01,0x04,0x45])
                (stringToBS genesisMessage))
            [ TxOut
                (fromIntegral 5000000000)
                (Script
                    [ OP_PUSHDATA $ BS.pack [0x67,0x8a,0xfd,0xb0]
                    , OP_PUBKEY $ 
                        BS.pack [0x55,0x48,0x27,0x19,0x67,0xf1,0xa6,0x71,0x30
                                ,0xb7,0x10,0x5c,0xd6,0xa8,0x28,0xe0,0x39,0x09
                                ,0xa6,0x79,0x62,0xe0,0xea,0x1f,0x61,0xde,0xb6
                                ,0x49,0xf6,0xbc,0x3f,0x4c,0xef,0x38,0xc4,0xf3
                                ,0x55,0x04,0xe5,0x1e,0xc1,0x12,0xde,0x5c,0x38
                                ,0x4d,0xf7,0xba,0x0b,0x8d,0x57,0x8a,0x4c,0x70
                                ,0x2b,0x6b,0xf1,0x1d,0x5f
                                ]
                    , OP_CHECKSIG
                    ])
            ]
            0) --txLockTime
        [] --txns

testnetGenesisBlock :: Block
testnetGenesisBlock = 
    Block
        (BlockHeader
            (fromIntegral 0x01)        -- version
            (fromIntegral 0x00)        -- previous block
            genesisMerkle          -- merkle root
            (fromIntegral 1296688602)  -- timestamp
            (fromIntegral 0x1d00ffff)  -- bits 
            (fromIntegral 414098458))  -- nonce
        (CoinbaseTx
            txCurrentVersion
            (BS.append
                (BS.pack [0x04,0xff,0xff,0x00,0x1d,0x01,0x04,0x45])
                (stringToBS genesisMessage))
            [ TxOut
                (fromIntegral 5000000000)
                (Script
                    [ OP_PUSHDATA $ BS.pack [0x67,0x8a,0xfd,0xb0]
                    , OP_PUBKEY $ 
                        BS.pack [0x55,0x48,0x27,0x19,0x67,0xf1,0xa6,0x71,0x30
                                ,0xb7,0x10,0x5c,0xd6,0xa8,0x28,0xe0,0x39,0x09
                                ,0xa6,0x79,0x62,0xe0,0xea,0x1f,0x61,0xde,0xb6
                                ,0x49,0xf6,0xbc,0x3f,0x4c,0xef,0x38,0xc4,0xf3
                                ,0x55,0x04,0xe5,0x1e,0xc1,0x12,0xde,0x5c,0x38
                                ,0x4d,0xf7,0xba,0x0b,0x8d,0x57,0x8a,0x4c,0x70
                                ,0x2b,0x6b,0xf1,0x1d,0x5f
                                ]
                    , OP_CHECKSIG
                    ])
            ]
            0) --txLockTime
        [] --txns
