{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskoin.Script.SigHash
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

Transaction signatures and related functions.
-}
module Haskoin.Script.SigHash (
    -- * Script Signatures
    SigHash (..),
    SigHashFlag (..),
    sigHashAll,
    sigHashNone,
    sigHashSingle,
    hasAnyoneCanPayFlag,
    hasForkIdFlag,
    setAnyoneCanPayFlag,
    setForkIdFlag,
    isSigHashAll,
    isSigHashNone,
    isSigHashSingle,
    isSigHashUnknown,
    sigHashAddForkId,
    sigHashGetForkId,
    sigHashAddNetworkId,
    txSigHash,
    txSigHashForkId,
    TxSignature (..),
    encodeTxSig,
    decodeTxSig,
) where

import Control.DeepSeq
import Control.Monad
import qualified Data.Aeson as J
import Data.Bits
import qualified Data.ByteString as BS
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Hashable
import Data.Maybe
import Data.Scientific
import Data.Word
import GHC.Generics (Generic)
import Haskoin.Crypto
import Haskoin.Crypto.Hash
import Haskoin.Data
import Haskoin.Network.Common
import Haskoin.Script.Common
import Haskoin.Transaction.Common
import Haskoin.Util

-- | Constant representing a SIGHASH flag that controls what is being signed.
data SigHashFlag
    = -- | sign all outputs
      SIGHASH_ALL
    | -- | sign no outputs
      SIGHASH_NONE
    | -- | sign the output index corresponding to the input
      SIGHASH_SINGLE
    | -- | replay protection for Bitcoin Cash transactions
      SIGHASH_FORKID
    | -- | new inputs can be added
      SIGHASH_ANYONECANPAY
    deriving (Eq, Ord, Show, Read, Generic)

instance NFData SigHashFlag

instance Hashable SigHashFlag

instance Enum SigHashFlag where
    fromEnum SIGHASH_ALL = 0x01
    fromEnum SIGHASH_NONE = 0x02
    fromEnum SIGHASH_SINGLE = 0x03
    fromEnum SIGHASH_FORKID = 0x40
    fromEnum SIGHASH_ANYONECANPAY = 0x80
    toEnum 0x01 = SIGHASH_ALL
    toEnum 0x02 = SIGHASH_NONE
    toEnum 0x03 = SIGHASH_SINGLE
    toEnum 0x40 = SIGHASH_FORKID
    toEnum 0x80 = SIGHASH_ANYONECANPAY
    toEnum _ = error "Not a valid sighash flag"

{- | Data type representing the different ways a transaction can be signed.
 When producing a signature, a hash of the transaction is used as the message
 to be signed. The 'SigHash' parameter controls which parts of the
 transaction are used or ignored to produce the transaction hash. The idea is
 that if some part of a transaction is not used to produce the transaction
 hash, then you can change that part of the transaction after producing a
 signature without invalidating that signature.

 If the 'SIGHASH_ANYONECANPAY' flag is set (true), then only the current input
 is signed. Otherwise, all of the inputs of a transaction are signed. The
 default value for 'SIGHASH_ANYONECANPAY' is unset (false).
-}
newtype SigHash
    = SigHash Word32
    deriving
        ( Eq
        , Ord
        , Bits
        , Enum
        , Integral
        , Num
        , Real
        , Show
        , Read
        , Generic
        , Hashable
        , NFData
        )

instance J.FromJSON SigHash where
    parseJSON =
        J.withScientific "sighash" $
            maybe mzero (return . SigHash) . toBoundedInteger

instance J.ToJSON SigHash where
    toJSON = J.Number . fromIntegral
    toEncoding (SigHash n) = J.toEncoding n

-- | SIGHASH_NONE as a byte.
sigHashNone :: SigHash
sigHashNone = fromIntegral $ fromEnum SIGHASH_NONE

-- | SIGHASH_ALL as a byte.
sigHashAll :: SigHash
sigHashAll = fromIntegral $ fromEnum SIGHASH_ALL

-- | SIGHASH_SINGLE as a byte.
sigHashSingle :: SigHash
sigHashSingle = fromIntegral $ fromEnum SIGHASH_SINGLE

-- | SIGHASH_FORKID as a byte.
sigHashForkId :: SigHash
sigHashForkId = fromIntegral $ fromEnum SIGHASH_FORKID

-- | SIGHASH_ANYONECANPAY as a byte.
sigHashAnyoneCanPay :: SigHash
sigHashAnyoneCanPay = fromIntegral $ fromEnum SIGHASH_ANYONECANPAY

-- | Set SIGHASH_FORKID flag.
setForkIdFlag :: SigHash -> SigHash
setForkIdFlag = (.|. sigHashForkId)

-- | Set SIGHASH_ANYONECANPAY flag.
setAnyoneCanPayFlag :: SigHash -> SigHash
setAnyoneCanPayFlag = (.|. sigHashAnyoneCanPay)

-- | Is the SIGHASH_FORKID flag set?
hasForkIdFlag :: SigHash -> Bool
hasForkIdFlag = (/= 0) . (.&. sigHashForkId)

-- | Is the SIGHASH_ANYONECANPAY flag set?
hasAnyoneCanPayFlag :: SigHash -> Bool
hasAnyoneCanPayFlag = (/= 0) . (.&. sigHashAnyoneCanPay)

-- | Returns 'True' if the 'SigHash' has the value 'SIGHASH_ALL'.
isSigHashAll :: SigHash -> Bool
isSigHashAll = (== sigHashAll) . (.&. 0x1f)

-- | Returns 'True' if the 'SigHash' has the value 'SIGHASH_NONE'.
isSigHashNone :: SigHash -> Bool
isSigHashNone = (== sigHashNone) . (.&. 0x1f)

-- | Returns 'True' if the 'SigHash' has the value 'SIGHASH_SINGLE'.
isSigHashSingle :: SigHash -> Bool
isSigHashSingle = (== sigHashSingle) . (.&. 0x1f)

-- | Returns 'True' if the 'SigHash' has the value 'SIGHASH_UNKNOWN'.
isSigHashUnknown :: SigHash -> Bool
isSigHashUnknown =
    (`notElem` [sigHashAll, sigHashNone, sigHashSingle]) . (.&. 0x1f)

-- | Add a fork id to a 'SigHash'.
sigHashAddForkId :: SigHash -> Word32 -> SigHash
sigHashAddForkId sh w = (fromIntegral w `shiftL` 8) .|. (sh .&. 0x000000ff)

-- | Add fork id of a particular network to a 'SigHash'.
sigHashAddNetworkId :: Network -> SigHash -> SigHash
sigHashAddNetworkId net =
    (`sigHashAddForkId` fromMaybe 0 (getSigHashForkId net))

-- | Get fork id from 'SigHash'.
sigHashGetForkId :: SigHash -> Word32
sigHashGetForkId (SigHash n) = fromIntegral $ n `shiftR` 8

-- | Computes the hash that will be used for signing a transaction.
txSigHash ::
    Network ->
    -- | transaction to sign
    Tx ->
    -- | script from output being spent
    Script ->
    -- | value of output being spent
    Word64 ->
    -- | index of input being signed
    Int ->
    -- | what to sign
    SigHash ->
    -- | hash to be signed
    Hash256
txSigHash net tx out v i sh
    | hasForkIdFlag sh && isJust (getSigHashForkId net) =
        txSigHashForkId net tx out v i sh
    | otherwise = do
        let newIn = buildInputs (txIn tx) fout i sh
        -- When SigSingle and input index > outputs, then sign integer 1
        fromMaybe one $ do
            newOut <- buildOutputs (txOut tx) i sh
            let newTx = Tx (txVersion tx) newIn newOut [] (txLockTime tx)
            return $
                doubleSHA256 $
                    runPutS $ do
                        serialize newTx
                        putWord32le $ fromIntegral sh
  where
    fout = Script $ filter (/= OP_CODESEPARATOR) $ scriptOps out
    one = "0100000000000000000000000000000000000000000000000000000000000000"

-- | Build transaction inputs for computing sighashes.
buildInputs :: [TxIn] -> Script -> Int -> SigHash -> [TxIn]
buildInputs txins out i sh
    | hasAnyoneCanPayFlag sh =
        [(txins !! i){scriptInput = runPutS $ serialize out}]
    | isSigHashAll sh || isSigHashUnknown sh = single
    | otherwise = zipWith noSeq single [0 ..]
  where
    emptyIn = map (\ti -> ti{scriptInput = BS.empty}) txins
    single =
        updateIndex i emptyIn $ \ti -> ti{scriptInput = runPutS $ serialize out}
    noSeq ti j =
        if i == j
            then ti
            else ti{txInSequence = 0}

-- | Build transaction outputs for computing sighashes.
buildOutputs :: [TxOut] -> Int -> SigHash -> Maybe [TxOut]
buildOutputs txos i sh
    | isSigHashAll sh || isSigHashUnknown sh = return txos
    | isSigHashNone sh = return []
    | i >= length txos = Nothing
    | otherwise = return $ buffer ++ [txos !! i]
  where
    buffer = replicate i $ TxOut maxBound BS.empty

{- | Compute the hash that will be used for signing a transaction. This
 function is used when the 'SIGHASH_FORKID' flag is set.
-}
txSigHashForkId ::
    Network ->
    -- | transaction to sign
    Tx ->
    -- | script from output being spent
    Script ->
    -- | value of output being spent
    Word64 ->
    -- | index of input being signed
    Int ->
    -- | what to sign
    SigHash ->
    -- | hash to be signed
    Hash256
txSigHashForkId net tx out v i sh =
    doubleSHA256 . runPutS $ do
        putWord32le $ txVersion tx
        serialize hashPrevouts
        serialize hashSequence
        serialize $ prevOutput $ txIn tx !! i
        putScript out
        putWord64le v
        putWord32le $ txInSequence $ txIn tx !! i
        serialize hashOutputs
        putWord32le $ txLockTime tx
        putWord32le $ fromIntegral $ sigHashAddNetworkId net sh
  where
    hashPrevouts
        | not $ hasAnyoneCanPayFlag sh =
            doubleSHA256 $ runPutS $ mapM_ (serialize . prevOutput) $ txIn tx
        | otherwise = zeros
    hashSequence
        | not (hasAnyoneCanPayFlag sh)
            && not (isSigHashSingle sh)
            && not (isSigHashNone sh) =
            doubleSHA256 $ runPutS $ mapM_ (putWord32le . txInSequence) $ txIn tx
        | otherwise = zeros
    hashOutputs
        | not (isSigHashSingle sh) && not (isSigHashNone sh) =
            doubleSHA256 $ runPutS $ mapM_ serialize $ txOut tx
        | isSigHashSingle sh && i < length (txOut tx) =
            doubleSHA256 $ runPutS $ serialize $ txOut tx !! i
        | otherwise = zeros
    putScript s = do
        let encodedScript = runPutS $ serialize s
        putVarInt $ BS.length encodedScript
        putByteString encodedScript
    zeros :: Hash256
    zeros = "0000000000000000000000000000000000000000000000000000000000000000"

{- | Data type representing a signature together with a 'SigHash'. The 'SigHash'
 is serialized as one byte at the end of an ECDSA 'Sig'. All signatures in
 transaction inputs are of type 'TxSignature'.
-}
data TxSignature
    = TxSignature
        { txSignature :: !Sig
        , txSignatureSigHash :: !SigHash
        }
    | TxSignatureEmpty
    deriving (Eq, Show, Generic)

instance NFData TxSignature

-- | Serialize a 'TxSignature'.
encodeTxSig :: TxSignature -> BS.ByteString
encodeTxSig TxSignatureEmpty = error "Can not encode an empty signature"
encodeTxSig (TxSignature sig (SigHash n)) =
    runPutS $ putSig sig >> putWord8 (fromIntegral n)

-- | Deserialize a 'TxSignature'.
decodeTxSig :: Network -> BS.ByteString -> Either String TxSignature
decodeTxSig _ bs | BS.null bs = Left "Empty signature candidate"
decodeTxSig net bs =
    case decodeStrictSig $ BS.init bs of
        Just sig -> do
            let sh = fromIntegral $ BS.last bs
            when (isSigHashUnknown sh) $
                Left "Non-canonical signature: unknown hashtype byte"
            when (isNothing (getSigHashForkId net) && hasForkIdFlag sh) $
                Left "Non-canonical signature: invalid network for forkId"
            return $ TxSignature sig sh
        Nothing -> Left "Non-canonical signature: could not parse signature"
