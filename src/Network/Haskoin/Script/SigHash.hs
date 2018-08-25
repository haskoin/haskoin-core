{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Network.Haskoin.Script.SigHash
( SigHash
, sigHashAll
, sigHashNone
, sigHashSingle
, hasAnyoneCanPayFlag
, hasForkIdFlag
, setAnyoneCanPayFlag
, setForkIdFlag
, isSigHashAll
, isSigHashNone
, isSigHashSingle
, isSigHashUnknown
, sigHashAddForkId
, sigHashGetForkId
, sigHashAddNetworkId
, txSigHash
, txSigHashForkId
, TxSignature(..)
, encodeTxSig
, decodeTxLaxSig
, decodeTxStrictSig
) where

import           Control.DeepSeq                   (NFData, rnf)
import           Control.Monad
import qualified Data.Aeson                        as J
import           Data.Bits
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import           Data.Maybe
import           Data.Serialize
import           Data.Serialize.Put                (runPut)
import           Data.String                       (IsString, fromString)
import           Data.String.Conversions           (cs)
import           Data.Word
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.ECDSA
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Network
import           Network.Haskoin.Script.Types
import           Network.Haskoin.Transaction.Types
import           Network.Haskoin.Util
import           Text.Read                         as R
import           Text.Read.Lex                     (numberToInteger)

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
newtype SigHash = SigHash Word32
    deriving (Eq, Ord, Enum, Bits, Num, Real, Integral, NFData)

instance Show SigHash where
    showsPrec d sh =
        showParen (d > 10) $ showString "SigHash " . shows (fromIntegral sh)

instance Read SigHash where
    readPrec = do
        R.Ident "SigHash" <- lexP
        R.Number n <- lexP
        maybe pfail return $ fromIntegral <$> numberToInteger n

instance IsString SigHash where
    fromString s =
        let e = error "Could not read sighash from hex string"
        in fromIntegral $
           fromMaybe e $ eitherToMaybe . runGet getWord32be =<< decodeHex (cs s)

instance J.FromJSON SigHash where
    parseJSON = J.withText "sighash" $ \t -> return $ fromString $ cs t

instance J.ToJSON SigHash where
    toJSON = J.String . cs . show

sigHashAll :: SigHash
sigHashAll = 0x01

sigHashNone :: SigHash
sigHashNone = 0x02

sigHashSingle :: SigHash
sigHashSingle = 0x03

setForkIdFlag :: SigHash -> SigHash
setForkIdFlag = (.|. 0x40)

setAnyoneCanPayFlag :: SigHash -> SigHash
setAnyoneCanPayFlag = (.|. 0x80)

hasForkIdFlag :: SigHash -> Bool
hasForkIdFlag = (/= 0) . (.&. 0x40)

hasAnyoneCanPayFlag :: SigHash -> Bool
hasAnyoneCanPayFlag = (/= 0) . (.&. 0x80)

-- | Returns True if the 'SigHash' has the value SigAll.
isSigHashAll :: SigHash -> Bool
isSigHashAll = (== sigHashAll) . (.&. 0x1f)

-- | Returns True if the 'SigHash' has the value SigNone.
isSigHashNone :: SigHash -> Bool
isSigHashNone = (== sigHashNone) . (.&. 0x1f)

-- | Returns True if the 'SigHash' has the value SigSingle.
isSigHashSingle :: SigHash -> Bool
isSigHashSingle = (== sigHashSingle) . (.&. 0x1f)

-- | Returns True if the 'SigHash' has the value SigUnknown.
isSigHashUnknown :: SigHash -> Bool
isSigHashUnknown =
    (`notElem` [sigHashAll, sigHashNone, sigHashSingle]) . (.&. 0x1f)

sigHashAddForkId :: SigHash -> Word32 -> SigHash
sigHashAddForkId sh w = (fromIntegral w `shiftL` 8) .|. (sh .&. 0x000000ff)

sigHashAddNetworkId :: SigHash -> SigHash
sigHashAddNetworkId = (`sigHashAddForkId` fromMaybe 0 sigHashForkId)

sigHashGetForkId :: SigHash -> Word32
sigHashGetForkId = fromIntegral . (`shiftR` 8)

-- | Computes the hash that will be used for signing a transaction.
txSigHash :: Tx      -- ^ Transaction to sign.
          -> Script  -- ^ Output script that is being spent.
          -> Word64  -- ^ Value of the output being spent.
          -> Int     -- ^ Index of the input that is being signed.
          -> SigHash -- ^ What parts of the transaction should be signed.
          -> Hash256 -- ^ Result hash to be signed.
txSigHash tx out v i sh
    | hasForkIdFlag sh && isJust sigHashForkId =
        txSigHashForkId tx out v i sh
    | otherwise = do
        let newIn = buildInputs (txIn tx) fout i sh
        -- When SigSingle and input index > outputs, then sign integer 1
        fromMaybe one $ do
            newOut <- buildOutputs (txOut tx) i sh
            let newTx = Tx (txVersion tx) newIn newOut [] (txLockTime tx)
            return $
                doubleSHA256 $
                runPut $ do
                    put newTx
                    putWord32le $ fromIntegral sh
  where
    fout = Script $ filter (/= OP_CODESEPARATOR) $ scriptOps out
    one = "0100000000000000000000000000000000000000000000000000000000000000"

-- Builds transaction inputs for computing SigHashes
buildInputs :: [TxIn] -> Script -> Int -> SigHash -> [TxIn]
buildInputs txins out i sh
    | hasAnyoneCanPayFlag sh =
        [ (txins !! i) { scriptInput = encode out } ]
    | isSigHashAll sh || isSigHashUnknown sh = single
    | otherwise = zipWith noSeq single [0 ..]
  where
    emptyIn = map (\ti -> ti { scriptInput = BS.empty }) txins
    single =
        updateIndex i emptyIn $ \ti -> ti { scriptInput = encode out }
    noSeq ti j =
        if i == j
        then ti
        else ti { txInSequence = 0 }

-- Build transaction outputs for computing SigHashes
buildOutputs :: [TxOut] -> Int -> SigHash -> Maybe [TxOut]
buildOutputs txos i sh
    | isSigHashAll sh || isSigHashUnknown sh = return txos
    | isSigHashNone sh = return []
    | i >= length txos = Nothing
    | otherwise = return $ buffer ++ [txos !! i]
  where
    buffer = replicate i $ TxOut maxBound BS.empty

-- | Computes the hash that will be used for signing a transaction. This
-- function is used when the sigHashForkId flag is set.
txSigHashForkId
    :: Tx      -- ^ Transaction to sign.
    -> Script  -- ^ Output script that is being spent.
    -> Word64  -- ^ Value of the output being spent.
    -> Int     -- ^ Index of the input that is being signed.
    -> SigHash -- ^ What parts of the transaction should be signed.
    -> Hash256 -- ^ Result hash to be signed.
txSigHashForkId tx out v i sh =
    doubleSHA256 . runPut $ do
        putWord32le $ txVersion tx
        put hashPrevouts
        put hashSequence
        put $ prevOutput $ txIn tx !! i
        putScript out
        putWord64le v
        putWord32le $ txInSequence $ txIn tx !! i
        put hashOutputs
        putWord32le $ txLockTime tx
        putWord32le $ fromIntegral $ sigHashAddNetworkId sh
  where
    hashPrevouts
        | not $ hasAnyoneCanPayFlag sh =
            doubleSHA256 $ runPut $ mapM_ (put . prevOutput) $ txIn tx
        | otherwise = zeros
    hashSequence
        | not (hasAnyoneCanPayFlag sh) &&
              not (isSigHashSingle sh) && not (isSigHashNone sh) =
            doubleSHA256 $ runPut $ mapM_ (putWord32le . txInSequence) $ txIn tx
        | otherwise = zeros
    hashOutputs
        | not (isSigHashSingle sh) && not (isSigHashNone sh) =
            doubleSHA256 $ runPut $ mapM_ put $ txOut tx
        | isSigHashSingle sh && i < length (txOut tx) =
            doubleSHA256 $ encode $ txOut tx !! i
        | otherwise = zeros
    putScript s = do
        let encodedScript = encode s
        put $ VarInt $ fromIntegral $ BS.length encodedScript
        putByteString encodedScript
    zeros :: Hash256
    zeros = "0000000000000000000000000000000000000000000000000000000000000000"

-- | Data type representing a 'Signature' together with a 'SigHash'. The
-- 'SigHash' is serialized as one byte at the end of a regular ECDSA
-- 'Signature'. All signatures in transaction inputs are of type 'TxSignature'.
data TxSignature
    = TxSignature { txSignature        :: !Signature
                  , txSignatureSigHash :: !SigHash
                  }
    | TxSignatureEmpty
    deriving (Eq, Show)

instance NFData TxSignature where
    rnf (TxSignature s h) = rnf s `seq` rnf h
    rnf TxSignatureEmpty  = ()

-- | Serialize a 'TxSignature' to a ByteString.
encodeTxSig :: TxSignature -> ByteString
encodeTxSig TxSignatureEmpty = error "Can not encode an empty signature"
encodeTxSig (TxSignature sig sh) = runPut $ put sig >> putWord8 (fromIntegral sh)

-- | Decode a 'TxSignature' from a ByteString.
decodeTxLaxSig :: ByteString -> Either String TxSignature
decodeTxLaxSig "" = Left "decodeTxLaxSig: empty bytestring"
decodeTxLaxSig bs =
    TxSignature <$> decode (BS.init bs)
                <*> return (fromIntegral $ BS.last bs)

decodeTxStrictSig :: ByteString -> Either String TxSignature
decodeTxStrictSig bs =
    case decodeStrictSig $ BS.init bs of
        Just sig -> do
            let sh = fromIntegral $ BS.last bs
            when (isSigHashUnknown sh) $
                Left "Non-canonical signature: unknown hashtype byte"
            when (isNothing sigHashForkId && hasForkIdFlag sh) $
                Left "Non-canonical signature: invalid network for forkId"
            return $ TxSignature sig sh
        Nothing -> Left "Non-canonical signature: could not parse signature"
