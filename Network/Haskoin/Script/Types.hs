module Network.Haskoin.Script.Types
( ScriptOp(..)
, Script(..)
, PushDataType(..)
, opPushData
) where

import Control.Monad (liftM2, unless, when, forM_)
import Control.Applicative ((<$>))

import Data.Word (Word8)
import Data.Binary (Binary, get, put)
import Data.Binary.Get 
    ( Get
    , isEmpty
    , getWord8
    , getWord16le
    , getWord32le
    , getByteString
    )
import Data.Binary.Put 
    ( Put 
    , putWord8
    , putWord16le
    , putWord32le
    , putByteString
    )
import qualified Data.ByteString as BS
    ( ByteString
    , length
    )

import Network.Haskoin.Protocol.VarInt
import Network.Haskoin.Crypto.Keys 
import Network.Haskoin.Util 

-- | Data type representing a transaction script. Scripts are defined as lists
-- of script operators 'ScriptOp'. Scripts are used to:
--
-- * Define the spending conditions in the output of a transaction
--
-- * Provide the spending signatures in the input of a transaction
data Script = 
    Script { 
             -- | List of script operators defining this script
             scriptOps :: [ScriptOp] 
           }
    deriving (Eq, Show, Read)

instance Binary Script where
    get = 
        Script <$> getScriptOps
      where
        getScriptOps = do
            empty <- isEmpty
            if empty 
                then return [] 
                else liftM2 (:) get getScriptOps

    put (Script ops) = forM_ ops put

-- | Data type representing the type of an OP_PUSHDATA opcode.
data PushDataType
    = 
      -- | The next opcode bytes is data to be pushed onto the stack
      OPCODE 
      -- | The next byte contains the number of bytes to be pushed onto
      -- the stack
    | OPDATA1 
      -- | The next two bytes contains the number of bytes to be pushed onto
      -- the stack
    | OPDATA2
      -- | The next four bytes contains the number of bytes to be pushed onto
      -- the stack
    | OPDATA4
    deriving (Show, Read, Eq)

-- | Data type representing all of the operators allowed inside a 'Script'.
data ScriptOp 
      -- Pushing Data
    = OP_PUSHDATA BS.ByteString PushDataType 
    | OP_0 
    | OP_1NEGATE 
    | OP_1  | OP_2  | OP_3  | OP_4  
    | OP_5  | OP_6  | OP_7  | OP_8  
    | OP_9  | OP_10 | OP_11 | OP_12 
    | OP_13 | OP_14 | OP_15 | OP_16 

      -- Flow control
    | OP_VERIFY 

      -- Stack operations
    | OP_DUP 

      -- Bitwise logic
    | OP_EQUAL 
    | OP_EQUALVERIFY 

      -- Crypto
    | OP_HASH160 
    | OP_CHECKSIG 
    | OP_CHECKMULTISIG 

      -- Other
    | OP_INVALIDOPCODE Word8
        deriving (Show, Read, Eq)

instance Binary ScriptOp where

    get = go =<< (fromIntegral <$> getWord8) 
      where 
        go op 
            | op == 0x00 = return $ OP_0
            | op <= 0x4b = do
                payload <- getByteString (fromIntegral op)
                return $ OP_PUSHDATA payload OPCODE
            | op == 0x4c = do
                len  <- getWord8
                payload <- getByteString (fromIntegral len)
                return $ OP_PUSHDATA payload OPDATA1
            | op == 0x4d = do
                len  <- getWord16le
                payload <- getByteString (fromIntegral len)
                return $ OP_PUSHDATA payload OPDATA2
            | op == 0x4e = do
                len  <- getWord32le
                payload <- getByteString (fromIntegral len)
                return $ OP_PUSHDATA payload OPDATA4
            | op == 0x4f = return $ OP_1NEGATE
            | op == 0x51 = return $ OP_1
            | op == 0x52 = return $ OP_2
            | op == 0x53 = return $ OP_3
            | op == 0x54 = return $ OP_4
            | op == 0x55 = return $ OP_5
            | op == 0x56 = return $ OP_6
            | op == 0x57 = return $ OP_7
            | op == 0x58 = return $ OP_8
            | op == 0x59 = return $ OP_9
            | op == 0x5a = return $ OP_10
            | op == 0x5b = return $ OP_11
            | op == 0x5c = return $ OP_12
            | op == 0x5d = return $ OP_13
            | op == 0x5e = return $ OP_14
            | op == 0x5f = return $ OP_15
            | op == 0x60 = return $ OP_16
            | op == 0x69 = return $ OP_VERIFY
            | op == 0x76 = return $ OP_DUP
            | op == 0x87 = return $ OP_EQUAL
            | op == 0x88 = return $ OP_EQUALVERIFY
            | op == 0xa9 = return $ OP_HASH160
            | op == 0xac = return $ OP_CHECKSIG
            | op == 0xae = return $ OP_CHECKMULTISIG
            | otherwise = return $ OP_INVALIDOPCODE op

    put op = case op of

        (OP_PUSHDATA payload optype)-> do
            let len = BS.length payload
            when (len == 0) $ fail "OP_PUSHDATA: Payload size must be > 0"
            case optype of
                OPCODE -> do
                    unless (len <= 0x4b) $ fail 
                        "OP_PUSHDATA OPCODE: Payload size too big"
                    putWord8 $ fromIntegral len
                OPDATA1 -> do
                    unless (len <= 0xff) $ fail 
                        "OP_PUSHDATA OPDATA1: Payload size too big"
                    putWord8 0x4c
                    putWord8 $ fromIntegral len
                OPDATA2 -> do
                    unless (len <= 0xffff) $ fail 
                        "OP_PUSHDATA OPDATA2: Payload size too big"
                    putWord8 0x4d
                    putWord16le $ fromIntegral len
                OPDATA4 -> do
                    unless (len <= 0xffffffff) $ fail 
                        "OP_PUSHDATA OPDATA4: Payload size too big"
                    putWord8 0x4e
                    putWord32le $ fromIntegral len
            putByteString payload

        OP_0                 -> putWord8 0x00
        OP_1NEGATE           -> putWord8 0x4f
        OP_1                 -> putWord8 0x51
        OP_2                 -> putWord8 0x52
        OP_3                 -> putWord8 0x53
        OP_4                 -> putWord8 0x54
        OP_5                 -> putWord8 0x55
        OP_6                 -> putWord8 0x56
        OP_7                 -> putWord8 0x57
        OP_8                 -> putWord8 0x58
        OP_9                 -> putWord8 0x59
        OP_10                -> putWord8 0x5a
        OP_11                -> putWord8 0x5b
        OP_12                -> putWord8 0x5c
        OP_13                -> putWord8 0x5d
        OP_14                -> putWord8 0x5e
        OP_15                -> putWord8 0x5f
        OP_16                -> putWord8 0x60
        OP_VERIFY            -> putWord8 0x69
        OP_DUP               -> putWord8 0x76
        OP_EQUAL             -> putWord8 0x87
        OP_EQUALVERIFY       -> putWord8 0x88
        OP_HASH160           -> putWord8 0xa9
        OP_CHECKSIG          -> putWord8 0xac
        OP_CHECKMULTISIG     -> putWord8 0xae
        (OP_INVALIDOPCODE _) -> putWord8 0xff

-- | Optimally encode data using one of the 4 types of data pushing opcodes
opPushData :: BS.ByteString -> ScriptOp
opPushData bs
    | len <= 0          = error "opPushData: data length must be > 0"
    | len <= 0x4b       = OP_PUSHDATA bs OPCODE
    | len <= 0xff       = OP_PUSHDATA bs OPDATA1
    | len <= 0xffff     = OP_PUSHDATA bs OPDATA2
    | len <= 0xffffffff = OP_PUSHDATA bs OPDATA4
    | otherwise         = error "opPushData: payload size too big"
  where
    len = BS.length bs

