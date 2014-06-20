module Network.Haskoin.Script.SigHash
( SigHash(..)
, encodeSigHash32
, isSigAll
, isSigNone
, isSigSingle
, isSigUnknown
, txSigHash
, TxSignature(..)
, encodeSig
, decodeSig
, decodeCanonicalSig
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (liftM2)

import Data.Word (Word8)
import Data.Bits (testBit, clearBit, setBit)
import Data.Maybe (fromMaybe)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.ByteString as BS 
    ( ByteString
    , index
    , length
    , last
    , append
    , pack
    , splitAt
    , empty
    )

import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Crypto.ECDSA
import Network.Haskoin.Script.Types
import Network.Haskoin.Protocol
import Network.Haskoin.Util

-- | Data type representing the different ways a transaction can be signed.
-- When producing a signature, a hash of the transaction is used as the message
-- to be signed. The 'SigHash' parameter controls which parts of the
-- transaction are used or ignored to produce the transaction hash. The idea is
-- that if some part of a transaction is not used to produce the transaction
-- hash, then you can change that part of the transaction after producing a
-- signature without invalidating that signature.
--
-- If the anyoneCanPay flag is True, then only the current input is signed.
-- Otherwise, all of the inputs of a transaction are signed. The default value
-- for anyoneCanPay is False.
data SigHash 
    -- | Sign all of the outputs of a transaction (This is the default value).
    -- Changing any of the outputs of the transaction will invalidate the
    -- signature.
    = SigAll     { anyoneCanPay :: !Bool }   
    -- | Sign none of the outputs of a transaction. This allows anyone to
    -- change any of the outputs of the transaction.
    | SigNone    { anyoneCanPay :: !Bool }     
    -- | Sign only the output corresponding the the current transaction input.
    -- You care about your own output in the transaction but you don't
    -- care about any of the other outputs.
    | SigSingle  { anyoneCanPay :: !Bool }   
    -- | Unrecognized sighash types will decode to SigUnknown.
    | SigUnknown { anyoneCanPay :: !Bool
                 , getSigCode   :: !Word8 
                 }
    deriving (Eq, Show, Read)

instance NFData SigHash where
    rnf (SigAll a) = rnf a
    rnf (SigNone a) = rnf a
    rnf (SigSingle a) = rnf a
    rnf (SigUnknown a c) = rnf a `seq` rnf c

-- | Returns True if the 'SigHash' has the value SigAll.
isSigAll :: SigHash -> Bool
isSigAll sh = case sh of
    SigAll _ -> True
    _ -> False

-- | Returns True if the 'SigHash' has the value SigNone.
isSigNone :: SigHash -> Bool
isSigNone sh = case sh of
    SigNone _ -> True
    _ -> False

-- | Returns True if the 'SigHash' has the value SigSingle.
isSigSingle :: SigHash -> Bool
isSigSingle sh = case sh of
    SigSingle _ -> True
    _ -> False

-- | Returns True if the 'SigHash' has the value SigUnknown.
isSigUnknown :: SigHash -> Bool
isSigUnknown sh = case sh of
    SigUnknown _ _ -> True
    _ -> False

instance Binary SigHash where

    get = getWord8 >>= \w ->
        let acp = testBit w 7
            in return $ case clearBit w 7 of
                1 -> SigAll acp
                2 -> SigNone acp
                3 -> SigSingle acp
                _ -> SigUnknown acp w

    put sh = putWord8 $ case sh of
        SigAll acp -> if acp then 0x81 else 0x01
        SigNone acp -> if acp then 0x82 else 0x02
        SigSingle acp -> if acp then 0x83 else 0x03
        SigUnknown _ w -> w

-- | Encodes a 'SigHash' to a 32 bit-long bytestring.
encodeSigHash32 :: SigHash -> BS.ByteString
encodeSigHash32 sh = encode' sh `BS.append` BS.pack [0,0,0]

-- | Computes the hash that will be used for signing a transaction.
txSigHash :: Tx      -- ^ Transaction to sign.
          -> Script  -- ^ Output script that is being spent.
          -> Int     -- ^ Index of the input that is being signed.
          -> SigHash -- ^ What parts of the transaction should be signed.
          -> Hash256 -- ^ Result hash to be signed.
txSigHash tx out i sh = do
    let newIn = buildInputs (txIn tx) out i sh
    -- When SigSingle and input index > outputs, then sign integer 1
    fromMaybe (setBit 0 248) $ do
        newOut <- buildOutputs (txOut tx) i sh
        let newTx = tx{ txIn = newIn, txOut = newOut }
        return $ doubleHash256 $ encode' newTx `BS.append` encodeSigHash32 sh

-- Builds transaction inputs for computing SigHashes
buildInputs :: [TxIn] -> Script -> Int -> SigHash -> [TxIn]
buildInputs txins out i sh
    | anyoneCanPay sh   = (txins !! i) { scriptInput = encode' out } : []
    | isSigAll sh || isSigUnknown sh = single
    | otherwise         = map noSeq $ zip single [0..]
  where 
    empty  = map (\ti -> ti{ scriptInput = BS.empty }) txins
    single = updateIndex i empty $ \ti -> ti{ scriptInput = encode' out }
    noSeq (ti,j) = if i == j then ti else ti{ txInSequence = 0 }

-- Build transaction outputs for computing SigHashes
buildOutputs :: [TxOut] -> Int -> SigHash -> Maybe [TxOut]
buildOutputs txos i sh
    | isSigAll sh || isSigUnknown sh = return txos
    | isSigNone sh = return []
    | i >= length txos = Nothing
    | otherwise = return $ buffer ++ [txos !! i]
  where 
    buffer = replicate i $ TxOut (-1) BS.empty

-- | Data type representing a 'Signature' together with a 'SigHash'. The
-- 'SigHash' is serialized as one byte at the end of a regular ECDSA
-- 'Signature'. All signatures in transaction inputs are of type 'TxSignature'.
data TxSignature = TxSignature 
    { txSignature :: !Signature 
    , sigHashType :: !SigHash
    } deriving (Eq, Show, Read)

instance NFData TxSignature where
    rnf (TxSignature s h) = rnf s `seq` rnf h

-- | Serialize a 'TxSignature' to a ByteString.
encodeSig :: TxSignature -> BS.ByteString
encodeSig (TxSignature sig sh) = runPut' $ put sig >> put sh

-- | Decode a 'TxSignature' from a ByteString.
decodeSig :: BS.ByteString -> Either String TxSignature
decodeSig bs = do
    let (h,l) = BS.splitAt (BS.length bs - 1) bs
    liftM2 TxSignature (decodeToEither h) (decodeToEither l)

-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- | Decode a 'TxSignature' from a ByteString. This function will check if
-- the signature is canonical and fail if it is not.
decodeCanonicalSig :: BS.ByteString -> Either String TxSignature
decodeCanonicalSig bs
    | len < 9 = Left "Non-canonical signature: too short"
    | len > 73 = Left "Non-canonical signature: too long"
    | hashtype < 1 || hashtype > 3 = 
        Left" Non-canonical signature: unknown hashtype byte"
    | BS.index bs 0 /= 0x30 = Left "Non-canonical signature: wrong type"
    | BS.index bs 1 /= len - 3 = 
        Left "Non-canonical signature: wrong length marker"
    | 5 + rlen >= len = Left "Non-canonical signature: S length misplaced"
    | rlen + slen + 7 /= len = 
        Left "Non-canonical signature: R+S length mismatch"
    | BS.index bs 2 /= 0x02 = 
        Left "Non-canonical signature: R value type mismatch"
    | rlen == 0 = Left "Non-canonical signature: R length is zero"
    | testBit (BS.index bs 4) 7 = 
        Left "Non-canonical signature: R value negative"
    | rlen > 1 && BS.index bs 4 == 0 && not (testBit (BS.index bs 5) 7) =
        Left "Non-canonical signature: R value excessively padded"
    | BS.index bs (fromIntegral rlen+4) /= 0x02 =
        Left "Non-canonical signature: S value type mismatch"
    | slen == 0 = Left "Non-canonical signature: S length is zero"
    | testBit (BS.index bs (fromIntegral rlen+6)) 7 =
        Left "Non-canonical signature: S value negative"
    | slen > 1 && BS.index bs (fromIntegral rlen+6) == 0 
        && not (testBit (BS.index bs (fromIntegral rlen+7)) 7) =
        Left "Non-canonical signature: S value excessively padded"
    | otherwise = decodeSig bs
  where 
    len = fromIntegral $ BS.length bs
    rlen = BS.index bs 3
    slen = BS.index bs (fromIntegral rlen + 5)
    hashtype = clearBit (BS.last bs) 7

