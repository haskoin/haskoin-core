module Haskoin.Wallet.ScriptParser
( ScriptOutput(..)
, ScriptInput(..)
, ScriptHashInput(..)
, SigHash(..)
, TxSignature(..)
, scriptAddr
, isCanonicalSig
, isCanonicalEvenSig
, encodeInput
, decodeInput
, encodeOutput
, decodeOutput
, encodeScriptHash
, decodeScriptHash
, encodeSigHash32
) where

import Control.Monad

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Util

data SigHash = SigAll    
             | SigNone   
             | SigSingle 

             -- Anyone Can Pay
             | SigAllAcp
             | SigNoneAcp
             | SigSingleAcp 
             deriving (Eq, Show)

instance Binary SigHash where

    get = do
        w <- getWord8
        case w of 0x01 -> return SigAll
                  0x02 -> return SigNone
                  0x03 -> return SigSingle
                  0x81 -> return SigAllAcp
                  0x82 -> return SigNoneAcp
                  0x83 -> return SigSingleAcp
                  _    -> fail "Non-canonical signature: unknown hashtype byte"

    put sh = putWord8 $ case sh of
        SigAll       -> 0x01
        SigNone      -> 0x02
        SigSingle    -> 0x03
        SigAllAcp    -> 0x81
        SigNoneAcp   -> 0x82
        SigSingleAcp -> 0x83

encodeSigHash32 :: SigHash -> BS.ByteString
encodeSigHash32 sh = encode' sh `BS.append` BS.pack [0,0,0]

-- Signatures in scripts contain the signature hash type byte
data TxSignature = TxSignature 
    { txSignature :: Signature 
    , sigHashType :: SigHash
    } deriving (Eq, Show)

instance Binary TxSignature where
    get = liftM2 TxSignature get get
    put (TxSignature s h) = put s >> put h

-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- from function IsCanonicalSignature
isCanonicalSig :: BS.ByteString -> Bool
isCanonicalSig s = not $
    -- Non-canonical signature: too short
    (len < 9) ||
    -- Non-canonical signature: too long
    (len > 73) ||
    -- Non-canonical signature: unknown hashtype byte
    (hashtype < 1 || hashtype > 3) ||
    -- Non-canonical signature: wrong type
    (BS.index s 0 /= 0x30) ||
    -- Non-canonical signature: wrong length marker
    (BS.index s 1 /= len - 3) ||
    -- Non-canonical signature: S length misplaced
    (5 + rlen >= len) || 
    -- Non-canonical signature: R+S length mismatch
    (rlen + slen + 7 /= len) ||
    -- Non-canonical signature: R value type mismatch
    (BS.index s 2 /= 0x02) ||
    -- Non-canonical signature: R length is zero
    (rlen == 0) ||
    -- Non-canonical signature: R value negative
    (testBit (BS.index s 4) 7) ||
    -- Non-canonical signature: R value excessively padded
    (  rlen > 1 
    && BS.index s 4 == 0 
    && not (testBit (BS.index s 5) 7)
    ) ||
    -- Non-canonical signature: S value type mismatch
    (BS.index s (fromIntegral rlen+4) /= 0x02) ||
    -- Non-canonical signature: S length is zero
    (slen == 0) ||
    -- Non-canonical signature: S value negative
    (testBit (BS.index s (fromIntegral rlen+6)) 7) ||
    -- Non-canonical signature: S value excessively padded
    (  slen > 1
    && BS.index s (fromIntegral rlen+6) == 0 
    && not (testBit (BS.index s (fromIntegral rlen+7)) 7)
    ) 
    where len = fromIntegral $ BS.length s
          rlen = BS.index s 3
          slen = BS.index s (fromIntegral rlen + 5)
          hashtype = clearBit (BS.last s) 7

-- Stronger condition: Check that 's' component of sig is even
isCanonicalEvenSig :: BS.ByteString -> Bool
isCanonicalEvenSig s = isCanonicalSig s && (not $ testBit val 0)
    -- Second before last byte (drop the sighash byte)
    where val = BS.last $ BS.init s

data ScriptOutput = 
      PayPK         { runPayPubKey      :: !PubKey }
    | PayPKHash     { runPayPubKeyHash  :: !Address }
    | PayMulSig     { payMulSigKeys     :: ![PubKey]
                    , payMulSigRequired :: !Int
                    }
    | PayScriptHash { runPayScriptHash  :: !Address }
    deriving (Eq, Show)

scriptAddr :: ScriptOutput -> Address
scriptAddr = ScriptAddress . hash160 . hash256BS . encode' . encodeOutput
    
encodeOutput :: ScriptOutput -> Maybe Script
encodeOutput s = liftM Script $ case s of
    -- Pay to PubKey
    (PayPK k) -> Just [OP_PUSHDATA $ encode' k, OP_CHECKSIG]
    -- Pay to PubKey Hash Address
    (PayPKHash a) -> case a of
        (PubKeyAddress h) -> Just [ OP_DUP, OP_HASH160, OP_PUSHDATA $ encode' h
                                  , OP_EQUALVERIFY, OP_CHECKSIG 
                                  ] 
        (ScriptAddress _) -> Nothing
    -- Pay to MultiSig Keys
    (PayMulSig ps r) -> do
        guard $ r <= length ps 
        (opN,opM) <- liftM2 (,) (intToScriptOp r) (intToScriptOp $ length ps)
        let keys = map (OP_PUSHDATA . encode') ps
        return $ opN : keys ++ [opM, OP_CHECKMULTISIG]
    -- Pay to Script Hash Address
    (PayScriptHash a) -> case a of
        (ScriptAddress h) -> Just [ OP_HASH160
                                  , OP_PUSHDATA $ encode' h, OP_EQUAL
                                  ]
        (PubKeyAddress _) -> Nothing

decodeOutput :: Script -> Maybe ScriptOutput
decodeOutput s = case runScript s of
    -- Pay to PubKey
    [OP_PUSHDATA k, OP_CHECKSIG] -> decodeEither k Nothing (Just . PayPK)
    -- Pay to PubKey Hash
    [OP_DUP, OP_HASH160, OP_PUSHDATA h, OP_EQUALVERIFY, OP_CHECKSIG] -> 
        decodeEither h Nothing (Just . PayPKHash . PubKeyAddress)
    -- Pay to Script Hash
    [OP_HASH160, OP_PUSHDATA h, OP_EQUAL] -> 
        decodeEither h Nothing (Just . PayScriptHash . ScriptAddress)
    -- Pay to MultiSig Keys
    xs -> matchPayMulSig xs

-- Match [ OP_N, PubKey1, ..., PubKeyM, OP_M, OP_CHECKMULTISIG ]
matchPayMulSig :: Script -> Maybe ScriptOutput
matchPayMulSig s@(Script ops) = case splitAt (length ops - 2) of
    ((n:xs),[m,OP_CHECKMULTISIG]) -> do
        (intN,intM) <- liftM2 (,) (scriptOpToInt n) (scriptOpToInt m)
        guard $ length xs == intM && intN <= intM
        liftM2 PayMulSig (go xs) (Just intN)
    _ -> Nothing
    where go (OP_PUSHDATA bs:xs) = 
              decodeEither bs Nothing $ \pub -> liftM2 (:) (Just pub) (go xs)
          go [] = Just []
          go  _ = Nothing

-- Decode OP_1 to OP_16
intToScriptOp :: Int -> Maybe ScriptOp
intToScriptOp i
    | i `elem` [1..16] = Just op
    |        otherwise = Nothing
    where op = decode' $ BS.singleton $ fromIntegral $ i + 0x50

-- Encode OP_1 to OP_16
scriptOpToInt :: ScriptOp -> Maybe Int
scriptOpToInt s 
    | res `elem` [1..16] = Just res
    | otherwise          = Nothing
    where res = fromIntegral $ BS.head $ encode' s

data ScriptInput = 
      SpendPK     { runSpendSig1      :: !TxSignature }
    | SpendPKHash { runSpendPKHashSig :: !TxSignature 
                  , runSpendPKHashKey :: !PubKey
                  }
    | SpendMulSig { runSpendMulSigs   :: ![TxSignature] 
                  , runRequiredSigs   :: !Int
                  }
    deriving (Eq, Show)

encodeInput :: ScriptInput -> Maybe Script
encodeInput s = liftM Script $ case s of
    -- Spend PubKey Input
    (SpendPK s) -> Just [OP_PUSHDATA $ encode' s]
    -- Spend PubKey Hash Input
    (SpendPKHash ts p) -> Just [ OP_PUSHDATA $ encode' ts
                               , OP_PUSHDATA $ encode' p
                               ]
    -- Spend MultiSig Input
    (SpendMulSig ts r) -> do
        guard $ length ts <= r && length ts <= 16
        let sigs = map (OP_PUSHDATA . encode') ts
        return $ OP_0 : sigs ++ replicate (r - length ts) OP_0

decodeInput :: Script -> Maybe ScriptInput
decodeInput s = case runScript s of
    [OP_PUSHDATA s] -> decodeEither s Nothing (Just . SpendPK)
    [OP_PUSHDATA a, OP_PUSHDATA b] -> 
        decodeEither a Nothing $ \s -> 
        decodeEither b Nothing $ \p -> Just $ SpendPKHash s p
    (OP_0 : xs) -> matchSpendMulSig xs
    _ -> Nothing

matchSpendMulSig :: Script -> Maybe ScriptInput
matchSpendMulSig (Script ops) = liftM2 SpendMulSig (go ops) (Just $ length ops)
    where go (OP_PUSHDATA bs:xs) -> decodeEither bs Nothing $ 
            \sig -> liftM2 (:) (Just sig) (go xs)
          go (OP_0:xs) -> if all (== OP_0) xs then Just [] else Nothing
          go [] = Just []
          go _  = Nothing

data ScriptHashInput = ScriptHashInput 
    { spendSHInput  :: ScriptInput 
    , spendSHOutput :: ScriptOutput
    } deriving (Eq, Show)

encodeScriptHash :: ScriptHashInput -> Maybe Script
encodeScriptHash (ScriptHashInput i o) = do
    (Script i') <- encodeInput i
    (Script o') <- encodeOutput o
    Script $ i' ++ [OP_PUSHDATA $ runPut' $ putScriptOps o']

decodeScriptHash :: Script -> Maybe ScriptHashInput
decodeScriptHash (Script ops) = case splitAt (length ops - 1) ops of
    (is,[OP_PUSHDATA bs]) -> runGetEither getScriptOps bs Nothing $ \os ->
        ScriptHashInput <$> (decodeInput  $ Script is) 
                        <*> (decodeOutput $ Script os)
    _ -> Nothing

