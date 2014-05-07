module Network.Haskoin.Protocol.MessageHeader 
    ( MessageHeader(..) 
    , MessageCommand(..)
    ) where

import Control.Applicative ((<$>),(<*>))

import Data.Word (Word32)
import qualified Data.ByteString as BS 
    ( ByteString
    , takeWhile
    )
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( getWord32le
    , getWord32be
    , getByteString
    )
import Data.Binary.Put 
    ( putWord32le
    , putWord32be
    , putByteString
    )

import Network.Haskoin.Util (stringToBS, bsToString)
import Network.Haskoin.Crypto (CheckSum32)

-- | A 'MessageCommand' is included in a 'MessageHeader' in order to identify
-- the type of message present in the payload. This allows the message 
-- de-serialization code to know how to decode a particular message payload.
-- Every valid 'Message' constructor has a corresponding 'MessageCommand'
-- constructor.
data MessageCommand 
    = MCVersion 
    | MCVerAck 
    | MCAddr 
    | MCInv 
    | MCGetData 
    | MCNotFound 
    | MCGetBlocks 
    | MCGetHeaders 
    | MCTx 
    | MCBlock 
    | MCHeaders 
    | MCGetAddr 
    | MCFilterLoad
    | MCFilterAdd
    | MCFilterClear
    | MCPing 
    | MCPong 
    | MCAlert
    deriving (Eq, Show)

instance Binary MessageCommand where
    
    get = go =<< getByteString 12
      where 
        go bs = case unpackCommand bs of
            "version"     -> return MCVersion
            "verack"      -> return MCVerAck
            "addr"        -> return MCAddr
            "inv"         -> return MCInv
            "getdata"     -> return MCGetData
            "notfound"    -> return MCNotFound
            "getblocks"   -> return MCGetBlocks
            "getheaders"  -> return MCGetHeaders
            "tx"          -> return MCTx
            "block"       -> return MCBlock
            "headers"     -> return MCHeaders
            "getaddr"     -> return MCGetAddr
            "filterload"  -> return MCFilterLoad
            "filteradd"   -> return MCFilterAdd
            "filterclear" -> return MCFilterClear
            "ping"        -> return MCPing
            "pong"        -> return MCPong
            "alert"       -> return MCAlert
            _             -> fail "get MessageCommand : Invalid command"

    put mc = putByteString $ packCommand $ case mc of
        MCVersion     -> "version"
        MCVerAck      -> "verack"
        MCAddr        -> "addr"
        MCInv         -> "inv"
        MCGetData     -> "getdata"
        MCNotFound    -> "notfound"
        MCGetBlocks   -> "getblocks"
        MCGetHeaders  -> "getheaders"
        MCTx          -> "tx"
        MCBlock       -> "block"
        MCHeaders     -> "headers"
        MCGetAddr     -> "getaddr"
        MCFilterLoad  -> "filterload"
        MCFilterAdd   -> "filteradd"
        MCFilterClear -> "filterclear"
        MCPing        -> "ping"
        MCPong        -> "pong"
        MCAlert       -> "alert"

packCommand :: String -> BS.ByteString
packCommand s = stringToBS $ take 12 $ s ++ repeat '\NUL'

unpackCommand :: BS.ByteString -> String
unpackCommand bs = bsToString $ BS.takeWhile (/= 0) bs

-- | Data type representing the header of a 'Message'. All messages sent between
-- nodes contain a message header.
data MessageHeader = 
    MessageHeader {
                  -- | Network magic bytes. It is used to differentiate 
                  -- messages meant for different bitcoin networks, such as
                  -- prodnet and testnet.
                    headMagic       :: !Word32
                  -- | Message command identifying the type of message.
                  -- included in the payload.
                  , headCmd         :: !MessageCommand
                  -- | Byte length of the payload.
                  , headPayloadSize :: !Word32
                  -- | Checksum of the payload. 
                  , headChecksum    :: !CheckSum32
                  } deriving (Eq, Show)

instance Binary MessageHeader where

    get = MessageHeader <$> getWord32be
                        <*> get
                        <*> getWord32le
                        <*> get

    put (MessageHeader m c l chk) = do
        putWord32be m
        put         c
        putWord32le l
        put         chk

