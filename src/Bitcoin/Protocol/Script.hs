module Bitcoin.Protocol.Script 
( ScriptOp(..)
, Script(..)
) where

import Data.Word
import Control.Monad
import Control.Applicative

import Bitcoin.Util
import Bitcoin.Protocol
import Bitcoin.Protocol.VarInt

import qualified Data.ByteString as BS

newtype Script = Script [ScriptOp]
    deriving (Eq, Read, Show)

instance BitcoinProtocol Script where

    bitcoinGet = do
        (VarInt len) <- bitcoinGet
        bs           <- getByteString (fromIntegral len)
        return $ Script $ runGet getScriptOps (toLazyBS bs)

    bitcoinPut (Script xs) = do
        let bs = toStrictBS $ runPut (putScriptOps xs)
        bitcoinPut $ lengthFromBS bs
        putByteString bs

getScriptOps :: BitcoinGet [ScriptOp]
getScriptOps = hasMore 
                (liftM2 (:) bitcoinGet getScriptOps) 
                (return [])

putScriptOps :: [ScriptOp] -> BitcoinPut 
putScriptOps (x:xs) = bitcoinPut x >> putScriptOps xs
putScriptOps _       = return ()

data ScriptOp =

    -- Pushing Data
    OP_PUSHDATA BS.ByteString |
    OP_FALSE | 
    OP_1NEGATE | 
    OP_TRUE |
    OP_2 | OP_3  | OP_4  | OP_5  | OP_6  | OP_7  | OP_8  |
    OP_9 | OP_10 | OP_11 | OP_12 | OP_13 | OP_14 | OP_15 | OP_16 |

    -- Flow control
    OP_VERIFY |

    -- Stack operations
    OP_DUP |

    -- Bitwise logic
    OP_EQUAL |
    OP_EQUALVERIFY | 

    -- Crypto
    OP_HASH160 |
    OP_CHECKSIG |
    OP_CHECKMULTISIG |

    -- Other
    OP_PUBKEY BS.ByteString |
    OP_INVALIDOPCODE Word8
        deriving (Eq, Read, Show)

instance BitcoinProtocol ScriptOp where

    bitcoinGet = go =<< (fromIntegral <$> getWord8) 
        where go op | op == 0x00 = return $ OP_FALSE
                    | op <= 0x4b = do
                        payload <- getByteString (fromIntegral op)
                        return $ OP_PUSHDATA payload
                    | op == 0x4c = do
                        len  <- getWord8
                        payload <- getByteString (fromIntegral len)
                        return $ OP_PUSHDATA payload
                    | op == 0x4d = do
                        len  <- getWord16le
                        payload <- getByteString (fromIntegral len)
                        return $ OP_PUSHDATA payload
                    | op == 0x4e = do
                        len  <- getWord32le
                        payload <- getByteString (fromIntegral len)
                        return $ OP_PUSHDATA payload
                    | op == 0x4f = return $ OP_1NEGATE
                    | op == 0x51 = return $ OP_TRUE
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
                    | op == 0xfe = do
                        payload <- getByteString 59
                        return $ OP_PUBKEY payload
                    | otherwise = return $ OP_INVALIDOPCODE op

    bitcoinPut OP_FALSE = putWord8 $ fromIntegral 0x00

    bitcoinPut (OP_PUSHDATA p) = go p (BS.length p)
        where go p len | len <= 0x4b = do
                            putWord8 $ fromIntegral len
                            putByteString p
                        | len <= 0xff = do
                            putWord8 $ fromIntegral 0x4c
                            putWord8 $ fromIntegral len
                            putByteString p
                        | len <= 0xffff = do
                            putWord8 $ fromIntegral 0x4d
                            putWord16le $ fromIntegral len
                            putByteString p
                        | len <= 0xffffffff = do
                            putWord8 $ fromIntegral 0x4e
                            putWord32le $ fromIntegral len
                            putByteString p
                        | otherwise = 
                            error "bitcoinPut OP_PUSHDATA payload too big"

    bitcoinPut OP_1NEGATE = putWord8 $ fromIntegral 0x4f
    bitcoinPut OP_TRUE = putWord8 $ fromIntegral 0x51
    bitcoinPut OP_2  = putWord8 $ fromIntegral 0x52
    bitcoinPut OP_3  = putWord8 $ fromIntegral 0x53
    bitcoinPut OP_4  = putWord8 $ fromIntegral 0x54
    bitcoinPut OP_5  = putWord8 $ fromIntegral 0x55
    bitcoinPut OP_6  = putWord8 $ fromIntegral 0x56
    bitcoinPut OP_7  = putWord8 $ fromIntegral 0x57
    bitcoinPut OP_8  = putWord8 $ fromIntegral 0x58
    bitcoinPut OP_9  = putWord8 $ fromIntegral 0x59
    bitcoinPut OP_10 = putWord8 $ fromIntegral 0x5a
    bitcoinPut OP_11 = putWord8 $ fromIntegral 0x5b
    bitcoinPut OP_12 = putWord8 $ fromIntegral 0x5c
    bitcoinPut OP_13 = putWord8 $ fromIntegral 0x5d
    bitcoinPut OP_14 = putWord8 $ fromIntegral 0x5e
    bitcoinPut OP_15 = putWord8 $ fromIntegral 0x5f
    bitcoinPut OP_16 = putWord8 $ fromIntegral 0x60

    bitcoinPut OP_VERIFY = putWord8 $ fromIntegral 0x69

    bitcoinPut OP_DUP = putWord8 $ fromIntegral 0x76
    bitcoinPut OP_EQUAL = putWord8 $ fromIntegral 0x87
    bitcoinPut OP_EQUALVERIFY = putWord8 $ fromIntegral 0x88

    bitcoinPut OP_HASH160 = putWord8 $ fromIntegral 0xa9
    bitcoinPut OP_CHECKSIG = putWord8 $ fromIntegral 0xac
    bitcoinPut OP_CHECKMULTISIG = putWord8 $ fromIntegral 0xae

    bitcoinPut (OP_PUBKEY bs) = do
        putWord8 $ fromIntegral 0xfe
        putByteString bs

    bitcoinPut (OP_INVALIDOPCODE _) = putWord8 $ fromIntegral 0xff

