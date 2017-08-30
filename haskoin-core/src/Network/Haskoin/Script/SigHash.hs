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

import           Control.DeepSeq                   (NFData, rnf)
import           Control.Monad                     (liftM2, mzero, (<=<))
import           Data.Aeson                        (FromJSON, ToJSON,
                                                    Value (String), parseJSON,
                                                    toJSON, withText)
import           Data.Bits                         (clearBit, testBit)
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS (append, empty, init,
                                                          last, length, pack,
                                                          singleton, splitAt)
import           Data.Maybe                        (fromMaybe)
import           Data.Serialize                    (Serialize, decode, encode,
                                                    get, getWord8, put,
                                                    putWord8, runPut)
import           Data.String.Conversions           (cs)
import           Data.Word                         (Word8)
import           Network.Haskoin.Crypto.ECDSA
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Script.Types
import           Network.Haskoin.Transaction.Types
import           Network.Haskoin.Util

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

instance Serialize SigHash where

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

instance ToJSON SigHash where
    toJSON = String . cs . encodeHex . encode

instance FromJSON SigHash where
    parseJSON = withText "sighash" $
        maybe mzero return . (decodeToMaybe <=< decodeHex) . cs

-- | Encodes a 'SigHash' to a 32 bit-long bytestring.
encodeSigHash32 :: SigHash -> ByteString
encodeSigHash32 sh = encode sh `BS.append` BS.pack [0,0,0]

-- | Computes the hash that will be used for signing a transaction.
txSigHash :: Tx      -- ^ Transaction to sign.
          -> Script  -- ^ Output script that is being spent.
          -> Int     -- ^ Index of the input that is being signed.
          -> SigHash -- ^ What parts of the transaction should be signed.
          -> Hash256 -- ^ Result hash to be signed.
txSigHash tx out i sh = do
    let newIn = buildInputs (txIn tx) out i sh
    -- When SigSingle and input index > outputs, then sign integer 1
    fromMaybe one $ do
        newOut <- buildOutputs (txOut tx) i sh
        let newTx = createTx (txVersion tx) newIn newOut (txLockTime tx)
        return $ doubleHash256 $ encode newTx `BS.append` encodeSigHash32 sh
  where
    one = "0100000000000000000000000000000000000000000000000000000000000000"

-- Builds transaction inputs for computing SigHashes
buildInputs :: [TxIn] -> Script -> Int -> SigHash -> [TxIn]
buildInputs txins out i sh
    | anyoneCanPay sh   = (txins !! i) { scriptInput = encode out } : []
    | isSigAll sh || isSigUnknown sh = single
    | otherwise         = map noSeq $ zip single [0..]
  where
    empty  = map (\ti -> ti{ scriptInput = BS.empty }) txins
    single = updateIndex i empty $ \ti -> ti{ scriptInput = encode out }
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
encodeSig :: TxSignature -> ByteString
encodeSig (TxSignature sig sh) = runPut $ put sig >> put sh

-- | Decode a 'TxSignature' from a ByteString.
decodeSig :: ByteString -> Either String TxSignature
decodeSig bs = do
    let (h, l) = BS.splitAt (BS.length bs - 1) bs
    liftM2 TxSignature (decode h) (decode l)

decodeCanonicalSig :: ByteString -> Either String TxSignature
decodeCanonicalSig bs
    | hashtype < 1 || hashtype > 3 =
        Left "Non-canonical signature: unknown hashtype byte"
    | otherwise =
        case decodeStrictSig $ BS.init bs of
            Just sig ->
                TxSignature sig <$> decode (BS.singleton $ BS.last bs)
            Nothing  ->
                Left "Non-canonical signature: could not parse signature"
  where
    hashtype = clearBit (BS.last bs) 7
