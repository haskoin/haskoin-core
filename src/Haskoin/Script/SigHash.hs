{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module      : Haskoin.Script.SigHash
-- Copyright   : No rights reserved
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Transaction signatures and related functions.
module Haskoin.Script.SigHash
  ( -- * Script Signatures
    SigHash (..),
    SigHashFlag (..),
    sigHashAll,
    sigHashNone,
    sigHashSingle,
    anyoneCanPay,
    hasForkIdFlag,
    setAnyoneCanPay,
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
    decodeTxSig,
    encodeTxSig,
  )
where

import Control.DeepSeq
import Control.Monad
import Crypto.Secp256k1
import Data.Aeson
import Data.Bits
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Hashable
import Data.Maybe
import Data.Scientific
import Data.Word
import GHC.Generics (Generic)
import Haskoin.Crypto.Hash
import Haskoin.Crypto.Signature
import Haskoin.Network.Common
import Haskoin.Network.Data
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

-- | Data type representing the different ways a transaction can be signed.
-- When producing a signature, a hash of the transaction is used as the message
-- to be signed. The 'SigHash' parameter controls which parts of the
-- transaction are used or ignored to produce the transaction hash. The idea is
-- that if some part of a transaction is not used to produce the transaction
-- hash, then you can change that part of the transaction after producing a
-- signature without invalidating that signature.
--
-- If the 'SIGHASH_ANYONECANPAY' flag is set (true), then only the current input
-- is signed. Otherwise, all of the inputs of a transaction are signed. The
-- default value for 'SIGHASH_ANYONECANPAY' is unset (false).
newtype SigHash
  = SigHash Word32
  deriving (Eq, Ord, Enum, Show, Read, Generic)
  deriving newtype (Bits, Integral, Num, Real, Hashable, NFData)

instance FromJSON SigHash where
  parseJSON =
    withScientific "sighash" $
      maybe mzero (return . SigHash) . toBoundedInteger

instance ToJSON SigHash where
  toJSON = Number . fromIntegral
  toEncoding (SigHash n) = toEncoding n

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
setAnyoneCanPay :: SigHash -> SigHash
setAnyoneCanPay = (.|. sigHashAnyoneCanPay)

-- | Is the SIGHASH_FORKID flag set?
hasForkIdFlag :: SigHash -> Bool
hasForkIdFlag = (/= 0) . (.&. sigHashForkId)

-- | Is the SIGHASH_ANYONECANPAY flag set?
anyoneCanPay :: SigHash -> Bool
anyoneCanPay = (/= 0) . (.&. sigHashAnyoneCanPay)

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
  (`sigHashAddForkId` fromMaybe 0 net.sigHashForkId)

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
  | hasForkIdFlag sh && isJust net.sigHashForkId =
      txSigHashForkId net tx out v i sh
  | otherwise = do
      let newIn = buildInputs tx.inputs fout i sh
      -- When SigSingle and input index > outputs, then sign integer 1
      fromMaybe one $ do
        newOut <- buildOutputs tx.outputs i sh
        let newTx = Tx tx.version newIn newOut [] tx.locktime
        return . doubleSHA256 . runPutS $ do
          serialize newTx
          putWord32le $ fromIntegral sh
  where
    fout = Script $ filter (/= OP_CODESEPARATOR) out.ops
    one = "0100000000000000000000000000000000000000000000000000000000000000"

-- | Build transaction inputs for computing sighashes.
buildInputs :: [TxIn] -> Script -> Int -> SigHash -> [TxIn]
buildInputs txins out i sh
  | anyoneCanPay sh = [serialOut (txins !! i)]
  | isSigHashAll sh || isSigHashUnknown sh = single
  | otherwise = zipWith noSeq single [0 ..]
  where
    serialOut TxIn {..} = TxIn {script = runPutS $ serialize out, ..}
    emptyIn TxIn {..} = TxIn {script = B.empty, ..}
    emptyIns = map emptyIn txins
    single = updateIndex i emptyIns serialOut
    noSeq TxIn {..} j = TxIn {sequence = if i == j then sequence else 0, ..}

-- | Build transaction outputs for computing sighashes.
buildOutputs :: [TxOut] -> Int -> SigHash -> Maybe [TxOut]
buildOutputs txos i sh
  | isSigHashAll sh || isSigHashUnknown sh = return txos
  | isSigHashNone sh = return []
  | i >= length txos = Nothing
  | otherwise = return $ buffer ++ [txos !! i]
  where
    buffer = replicate i $ TxOut maxBound B.empty

-- | Compute the hash that will be used for signing a transaction. This
-- function is used when the 'SIGHASH_FORKID' flag is set.
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
    putWord32le tx.version
    serialize hashPrevouts
    serialize hashSequence
    serialize (tx.inputs !! i).outpoint
    putScript out
    putWord64le v
    putWord32le (tx.inputs !! i).sequence
    serialize hashOutputs
    putWord32le tx.locktime
    putWord32le $ fromIntegral $ sigHashAddNetworkId net sh
  where
    hashPrevouts
      | not (anyoneCanPay sh) =
          doubleSHA256 . runPutS $ mapM_ (serialize . (.outpoint)) tx.inputs
      | otherwise = zeros
    hashSequence
      | not (anyoneCanPay sh || isSigHashSingle sh || isSigHashNone sh) =
          doubleSHA256 . runPutS $ mapM_ (putWord32le . (.sequence)) tx.inputs
      | otherwise = zeros
    hashOutputs
      | not (isSigHashSingle sh || isSigHashNone sh) =
          doubleSHA256 . runPutS $ mapM_ serialize tx.outputs
      | isSigHashSingle sh && i < length tx.outputs =
          doubleSHA256 . runPutS $ serialize $ tx.outputs !! i
      | otherwise = zeros
    putScript s = do
      let encodedScript = runPutS $ serialize s
      putVarInt $ B.length encodedScript
      putByteString encodedScript
    zeros :: Hash256
    zeros = "0000000000000000000000000000000000000000000000000000000000000000"

-- | Data type representing a signature together with a 'SigHash'. The 'SigHash'
-- is serialized as one byte at the end of an ECDSA 'Sig'. All signatures in
-- transaction inputs are of type 'TxSignature'.
data TxSignature
  = TxSignature
      { sig :: !Sig,
        hash :: !SigHash
      }
  | TxSignatureEmpty
  deriving (Eq, Show, Read, Generic, NFData)

instance Marshal (Network, Ctx) TxSignature where
  marshalPut (net, ctx) TxSignatureEmpty = return ()
  marshalPut (net, ctx) (TxSignature sig (SigHash n)) = do
    marshalPut ctx sig
    putWord8 (fromIntegral n)

  marshalGet (net, ctx) =
    bool decode empty =<< isEmpty
    where
      empty = return TxSignatureEmpty
      decode = do
        sig <- marshalGet ctx
        sh <- fromIntegral <$> getWord8
        when (isSigHashUnknown sh) $
          fail "Non-canonical signature: unknown hashtype byte"
        when (isNothing net.sigHashForkId && hasForkIdFlag sh) $
          fail "Non-canonical signature: invalid network for forkId"
        return $ TxSignature sig sh

instance MarshalJSON (Network, Ctx) TxSignature where
  marshalValue (net, ctx) = String . encodeHex . encodeTxSig net ctx
  marshalEncoding s = hexEncoding . runPutL . marshalPut s
  unmarshalValue (net, ctx) = 
    withText "TxSignature" $ \t ->
      case decodeHex t of
        Nothing -> fail "Cannot decode hex signature"
        Just b -> case decodeTxSig net ctx b of
          Left e -> fail e
          Right s -> return s

encodeTxSig :: Network -> Ctx -> TxSignature -> ByteString
encodeTxSig net ctx = runPutS . marshalPut (net, ctx)

decodeTxSig :: Network -> Ctx -> ByteString -> Either String TxSignature
decodeTxSig net ctx =
  runGetS $ do
    sig <- marshalGet (net, ctx)
    e <- isEmpty
    unless e $
      fail "Non-canonical signature: multiple hashtype bytes"
    return sig
