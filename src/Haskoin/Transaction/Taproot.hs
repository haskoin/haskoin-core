{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module      : Haskoin.Transaction.Taproot
-- Copyright   : No rights reserved
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides support for reperesenting full taproot outputs and parsing
-- taproot witnesses.  For reference see BIPS 340, 341, and 342.
module Haskoin.Transaction.Taproot
  ( XOnlyPubKey (..),
    TapLeafVersion,
    MAST (..),
    mastCommitment,
    getMerkleProofs,
    TaprootOutput (..),
    taprootOutputKey,
    taprootScriptOutput,
    TaprootWitness (..),
    ScriptPathData (..),
    viewTaprootWitness,
    encodeTaprootWitness,
    verifyScriptPathData,
  )
where

import Control.Applicative (many)
import Control.Monad ((<=<))
import Crypto.Hash
  ( Digest,
    SHA256,
    digestFromByteString,
    hashFinalize,
    hashUpdate,
    hashUpdates,
  )
import Crypto.Secp256k1
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (String),
    withText,
  )
import Data.Aeson.Types (Parser, Value)
import Data.Binary (Binary (..))
import Data.Bits ((.&.), (.|.))
import Data.Bool (bool)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Bytes.Get (MonadGet, getBytes, runGetS)
import Data.Bytes.Put (MonadPut, putByteString, runPutL, runPutS)
import Data.Bytes.Serial (Serial (..), deserialize, serialize)
import Data.Bytes.VarInt (VarInt (VarInt))
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Serialize (Serialize, get, getByteString, getWord8, put)
import Data.Word (Word8)
import Haskoin.Crypto.Hash
import Haskoin.Crypto.Keys.Common
import Haskoin.Crypto.Keys.Extended
import Haskoin.Script.Common
import Haskoin.Script.Standard
import Haskoin.Transaction.Common
import Haskoin.Util

-- | An x-only pubkey corresponds to the keys @(x,y)@ and @(x, -y)@.  The
-- equality test only checks the x-coordinate.  An x-only pubkey serializes to 32
-- bytes.
--
-- @since 0.21.0
newtype XOnlyPubKey = XOnlyPubKey {point :: PubKey}
  deriving (Read, Show)

instance Eq XOnlyPubKey where
  XOnlyPubKey k1 == XOnlyPubKey k2 = f k1 == f k2
    where
      f = BS.take 32 . (.get)

instance Marshal Ctx XOnlyPubKey where
  marshalPut ctx (XOnlyPubKey pk) =
    putByteString
      . BS.drop 1
      . marshal ctx
      $ PublicKey pk True

  marshalGet ctx =
    either fail (pure . XOnlyPubKey . (\PublicKey {point} -> point))
      . unmarshal ctx
      . BS.cons 0x02
      =<< getBytes 32

instance MarshalJSON Ctx XOnlyPubKey where
  unmarshalValue ctx =
    withText "XOnlyPubKey" $ either fail pure . (des <=< hex)
    where
      hex = maybe (Left "Unable to decode hex") Right . decodeHex
      des = runGetS $ marshalGet ctx

  marshalValue ctx =
    String . encodeHex . marshal ctx

  marshalEncoding ctx =
    hexEncoding . runPutL . marshalPut ctx

-- | @since 0.21.0
type TapLeafVersion = Word8

-- | Merklized Abstract Syntax Tree.  This type can represent trees where only a
-- subset of the leaves are known.  Note that the tree is invariant under swapping
-- branches at an internal node.
--
-- @since 0.21.0
data MAST
  = MASTBranch MAST MAST
  | MASTLeaf TapLeafVersion Script
  | MASTCommitment (Digest SHA256)
  deriving (Show)

-- | Get the inclusion proofs for the leaves in the tree.  The proof is ordered
-- leaf-to-root.
--
-- @since 0.21.0
getMerkleProofs :: MAST -> [(TapLeafVersion, Script, [Digest SHA256])]
getMerkleProofs = getProofs mempty
  where
    getProofs proof = \case
      MASTBranch branchL branchR ->
        (updateProof proof (mastCommitment branchR) <$> getMerkleProofs branchL)
          <> (updateProof proof (mastCommitment branchL) <$> getMerkleProofs branchR)
      MASTLeaf v s -> [(v, s, proof)]
      MASTCommitment {} -> mempty
    updateProof proofInit branchCommitment (v, s, proofTail) =
      (v, s, reverse $ proofInit <> (branchCommitment : proofTail))

-- | Calculate the root hash for this tree.
--
-- @since 0.21.0
mastCommitment :: MAST -> Digest SHA256
mastCommitment = \case
  MASTBranch leftBranch rightBranch ->
    hashBranch (mastCommitment leftBranch) (mastCommitment rightBranch)
  MASTLeaf leafVersion leafScript -> leafHash leafVersion leafScript
  MASTCommitment theCommitment -> theCommitment

hashBranch :: Digest SHA256 -> Digest SHA256 -> Digest SHA256
hashBranch hashA hashB =
  hashFinalize $
    hashUpdates
      (initTaggedHash "TapBranch")
      [ min hashA hashB,
        max hashA hashB
      ]

leafHash :: TapLeafVersion -> Script -> Digest SHA256
leafHash leafVersion leafScript =
  hashFinalize
    . hashUpdate (initTaggedHash "TapLeaf")
    . runPutS
    $ do
      serialize leafVersion
      serialize $ VarInt (BS.length scriptBytes)
      putByteString scriptBytes
  where
    scriptBytes = runPutS $ serialize leafScript

-- | Representation of a full taproot output.
--
-- @since 0.21.0
data TaprootOutput = TaprootOutput
  { internalKey :: PubKey,
    mast :: Maybe MAST
  }

-- | @since 0.21.0
taprootOutputKey :: Ctx -> TaprootOutput -> PubKey
taprootOutputKey ctx TaprootOutput {..} =
  fromMaybe keyFail $
    tweak commitment >>= tweakAddPubKey ctx internalKey
  where
    commitment =
      taprootCommitment ctx internalKey $
        mastCommitment <$> mast
    keyFail = error "haskoin-core taprootOutputKey: key derivation failed"

taprootCommitment :: Ctx -> PubKey -> Maybe (Digest SHA256) -> ByteString
taprootCommitment ctx internalKey merkleRoot =
  BA.convert
    . hashFinalize
    . maybe id (flip hashUpdate) merkleRoot
    . (`hashUpdate` keyBytes)
    $ initTaggedHash "TapTweak"
  where
    keyBytes = runPutS . marshalPut ctx $ XOnlyPubKey internalKey

-- | Generate the output script for a taproot output
--
-- @since 0.21.0
taprootScriptOutput :: Ctx -> TaprootOutput -> ScriptOutput
taprootScriptOutput ctx =
  PayWitness 0x01
    . runPutS
    . marshalPut ctx
    . XOnlyPubKey
    . taprootOutputKey ctx

-- | Comprehension of taproot witness data
--
-- @since 0.21.0
data TaprootWitness
  = -- | Signature
    KeyPathSpend ByteString
  | ScriptPathSpend ScriptPathData
  deriving (Eq)

-- | @since 0.21.0
data ScriptPathData = ScriptPathData
  { annex :: Maybe ByteString,
    stack :: [ByteString],
    script :: Script,
    extIsOdd :: Bool,
    -- | This value is masked by 0xFE
    leafVersion :: Word8,
    internalKey :: PubKey,
    control :: [ByteString]
  }
  deriving (Eq)

-- | Try to interpret a 'WitnessStack' as taproot witness data.
--
-- @since 0.21.0
viewTaprootWitness :: Ctx -> WitnessStack -> Maybe TaprootWitness
viewTaprootWitness ctx witnessStack = case reverse witnessStack of
  [sig] -> Just $ KeyPathSpend sig
  annexA : remainingStack
    | 0x50 : _ <- BS.unpack annexA ->
        parseSpendPathData (Just annexA) remainingStack
  remainingStack -> parseSpendPathData Nothing remainingStack
  where
    parseSpendPathData annex = \case
      scriptBytes : controlBytes : stack -> do
        script <- eitherToMaybe $ runGetS deserialize scriptBytes
        (v, internalKey, control) <- deconstructControl controlBytes
        let extIsOdd = odd v
            leafVersion = v .&. 0xFE
        pure $ ScriptPathSpend ScriptPathData {..}
      _ -> Nothing
    deconstructControl = eitherToMaybe . runGetS deserializeControl
    deserializeControl = do
      v <- getWord8
      XOnlyPubKey k <- marshalGet ctx
      proof <- many $ getByteString 32
      pure (v, k, proof)

-- | Transform the high-level representation of taproot witness data into a witness stack
--
-- @since 0.21.0
encodeTaprootWitness :: Ctx -> TaprootWitness -> WitnessStack
encodeTaprootWitness ctx = \case
  KeyPathSpend signature -> pure signature
  ScriptPathSpend scriptPathData -> wit scriptPathData
  where
    wit d = (.stack) d <> [script d, keys d, annex d]
    keys d = mconcat [verpar d, xonlyk d, ctrl d]
    script = runPutS . serialize . (.script)
    verpar d = BS.pack [(.leafVersion) d .|. parity d]
    xonlyk = runPutS . marshalPut ctx . XOnlyPubKey . (.internalKey)
    annex = fromMaybe mempty . (.annex)
    ctrl = mconcat . (.control)
    parity = bool 0 1 . (.extIsOdd)

-- | Verify that the script path spend is valid, except for script execution.
--
-- @since 0.21.0
verifyScriptPathData ::
  Ctx ->
  -- | Output key
  PubKey ->
  ScriptPathData ->
  Bool
verifyScriptPathData ctx outkey spd = fromMaybe False $ do
  tweak commitment
    >>= fmap onComputedKey
      . tweakAddPubKey ctx spd.internalKey
  where
    onComputedKey computedKey =
      XOnlyPubKey outkey == XOnlyPubKey computedKey
        && expectedParity == keyParity ctx computedKey
    commitment =
      taprootCommitment ctx spd.internalKey (Just merkleRoot)
    merkleRoot =
      foldl' hashBranch theLeafHash $
        mapMaybe (digestFromByteString @SHA256) spd.control
    theLeafHash =
      (leafHash <$> (.&. 0xFE) . (.leafVersion) <*> (.script))
        spd
    expectedParity = bool 0 1 spd.extIsOdd

keyParity :: Ctx -> PubKey -> Word8
keyParity ctx key =
  case BS.unpack . marshal ctx $ PublicKey key True of
    0x02 : _ -> 0x00
    _ -> 0x01
