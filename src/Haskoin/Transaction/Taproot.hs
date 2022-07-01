{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Haskoin.Transaction.Taproot
Copyright   : No rights reserved
License     : MIT
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX

This module provides support for reperesenting full taproot outputs and parsing
taproot witnesses.  For reference see BIPS 340, 341, and 342.
-}
module Haskoin.Transaction.Taproot (
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
    getByXCoord,
    toXCoord,

#ifdef BIP340
    ExtFlag,
    signTaprootInput,
    taprootSignatureDigest,
    taprootKeyPathWitness,
#endif
) where

import Control.Applicative (many)
import Control.Monad (unless, when, zipWithM_)
import Crypto.Hash (
    Digest,
    SHA256 (SHA256),
    digestFromByteString,
    hashFinalize,
    hashUpdate,
    hashUpdates,
    hashWith,
 )
import Crypto.Secp256k1 (SecKey)
#ifdef BIP340
import Crypto.Secp256k1 (Bip340Sig, signBip340, Rand32)
#endif
import qualified Crypto.Secp256k1 as C
import Data.Binary (Binary (put), Put)
import Data.Binary.Put (runPut, putWord32le, putWord64le)
import Data.Bits ((.&.), (.|.))
import Data.Bool (bool)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Bytes.Get (getBytes, runGetS)
import Data.Bytes.Put (putByteString, runPutS)
import Data.Bytes.Serial (Serial (..), deserialize, serialize)
import Data.Bytes.VarInt (VarInt (VarInt))
import Data.Foldable (foldl')
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Serialize (Get, getByteString, getWord8)
import qualified Data.Serialize as S
import Data.Word (Word8)
import Haskoin.Crypto (PubKey, initTaggedHash, tweak, tweakAddPubKey)
import Haskoin.Keys.Common (PubKeyI (PubKeyI), pubKeyPoint)
import Haskoin.Network.Common (VarString (VarString))
import Haskoin.Script.Common (Script)
import Haskoin.Script.SigHash (
    SigHash (SigHash),
    hasAnyoneCanPayFlag,
    isSigHashNone,
    isSigHashSingle,
 )
import Haskoin.Script.Standard (ScriptOutput (PayWitness))
import Haskoin.Transaction.Common (
    Tx (..),
    TxIn,
    TxOut,
    WitnessStack,
    outValue,
    prevOutput,
    scriptOutput,
    txInSequence,
 )
import Haskoin.Util (eitherToMaybe)

-- | @since 0.22.0
toXCoord :: PubKey -> ByteString
toXCoord pk = BS.drop 1 . runPutS . serialize $ PubKeyI pk True

-- | @since 0.22.0
getByXCoord :: Get PubKey
getByXCoord =
    either fail (pure . pubKeyPoint)
        . runGetS deserialize
        . BS.cons 0x02
        =<< getBytes 32

-- | @since 0.21.0
type TapLeafVersion = Word8

{- | Merklized Abstract Syntax Tree.  This type can represent trees where only a
subset of the leaves are known.  Note that the tree is invariant under swapping
branches at an internal node.

@since 0.21.0
-}
data MAST
    = MASTBranch MAST MAST
    | MASTLeaf TapLeafVersion Script
    | MASTCommitment (Digest SHA256)
    deriving (Show)

{- | Get the inclusion proofs for the leaves in the tree.  The proof is ordered
leaf-to-root.

@since 0.21.0
-}
getMerkleProofs :: MAST -> [(TapLeafVersion, Script, [Digest SHA256])]
getMerkleProofs = getProofs mempty
  where
    getProofs proof = \case
        MASTBranch branchL branchR ->
            (updateProof proof (mastCommitment branchR) <$> getMerkleProofs branchL)
                <> (updateProof proof (mastCommitment branchL) <$> getMerkleProofs branchR)
        MASTLeaf v s -> [(v, s, proof)]
        MASTCommitment{} -> mempty
    updateProof proofInit branchCommitment (v, s, proofTail) =
        (v, s, reverse $ proofInit <> (branchCommitment : proofTail))

{- | Calculate the root hash for this tree.

@since 0.21.0
-}
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
            [ min hashA hashB
            , max hashA hashB
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

{- | Representation of a full taproot output.

@since 0.21.0
-}
data TaprootOutput = TaprootOutput
    { taprootInternalKey :: PubKey
    , taprootMAST :: Maybe MAST
    }
    deriving (Show)

-- | @since 0.21.0
taprootOutputKey :: TaprootOutput -> PubKey
taprootOutputKey TaprootOutput{taprootInternalKey, taprootMAST} =
    fromMaybe keyFail $ tweak commitment >>= tweakAddPubKey taprootInternalKey
  where
    commitment = taprootCommitment taprootInternalKey $ mastCommitment <$> taprootMAST
    keyFail = error "haskoin-core taprootOutputKey: key derivation failed"

taprootCommitment :: PubKey -> Maybe (Digest SHA256) -> ByteString
taprootCommitment internalKey merkleRoot =
    BA.convert . hashFinalize
        . maybe id (flip hashUpdate) merkleRoot
        . (`hashUpdate` keyBytes)
        $ initTaggedHash "TapTweak"
  where
    keyBytes = toXCoord internalKey

{- | Generate the output script for a taproot output

@since 0.21.0
-}
taprootScriptOutput :: TaprootOutput -> ScriptOutput
taprootScriptOutput = PayWitness 0x01 . toXCoord . taprootOutputKey

{- | Comprehension of taproot witness data

@since 0.21.0
-}
data TaprootWitness
    = -- | Signature
      KeyPathSpend ByteString
    | ScriptPathSpend ScriptPathData
    deriving (Eq, Show)

-- | @since 0.21.0
data ScriptPathData = ScriptPathData
    { scriptPathAnnex :: Maybe ByteString
    , scriptPathStack :: [ByteString]
    , scriptPathScript :: Script
    , scriptPathExternalIsOdd :: Bool
    , scriptPathLeafVersion :: Word8
    -- ^ This value is masked by 0xFE
    , scriptPathInternalKey :: PubKey
    , scriptPathControl :: [ByteString]
    }
    deriving (Eq, Show)

{- | Try to interpret a 'WitnessStack' as taproot witness data.

@since 0.21.0
-}
viewTaprootWitness :: WitnessStack -> Maybe TaprootWitness
viewTaprootWitness witnessStack = case reverse witnessStack of
    [sig] -> Just $ KeyPathSpend sig
    annexA : remainingStack
        | 0x50 : _ <- BS.unpack annexA ->
            parseSpendPathData (Just annexA) remainingStack
    remainingStack -> parseSpendPathData Nothing remainingStack
  where
    parseSpendPathData scriptPathAnnex = \case
        scriptBytes : controlBytes : scriptPathStack -> do
            scriptPathScript <- eitherToMaybe $ runGetS deserialize scriptBytes
            (v, scriptPathInternalKey, scriptPathControl) <- deconstructControl controlBytes
            pure . ScriptPathSpend $
                ScriptPathData
                    { scriptPathAnnex
                    , scriptPathStack
                    , scriptPathScript
                    , scriptPathExternalIsOdd = odd v
                    , scriptPathLeafVersion = v .&. 0xFE
                    , scriptPathInternalKey
                    , scriptPathControl
                    }
        _ -> Nothing
    deconstructControl = eitherToMaybe . runGetS deserializeControl
    deserializeControl = do
        v <- getWord8
        k <- getByXCoord
        proof <- many $ getByteString 32
        pure (v, k, proof)

{- | Transform the high-level representation of taproot witness data into a witness stack

@since 0.21.0
-}
encodeTaprootWitness :: TaprootWitness -> WitnessStack
encodeTaprootWitness = \case
    KeyPathSpend signature -> pure signature
    ScriptPathSpend scriptPathData ->
        scriptPathStack scriptPathData
            <> [ runPutS . serialize $ scriptPathScript scriptPathData
               , mconcat
                    [ BS.pack [scriptPathLeafVersion scriptPathData .|. parity scriptPathData]
                    , toXCoord $ scriptPathInternalKey scriptPathData
                    , mconcat $ scriptPathControl scriptPathData
                    ]
               , fromMaybe mempty $ scriptPathAnnex scriptPathData
               ]
  where
    parity = bool 0 1 . scriptPathExternalIsOdd

{- | Verify that the script path spend is valid, except for script execution.

@since 0.21.0
-}
verifyScriptPathData ::
    -- | Output key
    PubKey ->
    ScriptPathData ->
    Bool
verifyScriptPathData outputKey scriptPathData = fromMaybe False $ do
    tweak commitment >>= fmap onComputedKey . tweakAddPubKey (scriptPathInternalKey scriptPathData)
  where
    onComputedKey computedKey =
        toXCoord outputKey == toXCoord computedKey
            && expectedParity == keyParity computedKey
    commitment = taprootCommitment (scriptPathInternalKey scriptPathData) (Just merkleRoot)
    merkleRoot =
        foldl' hashBranch theLeafHash
            . mapMaybe (digestFromByteString @SHA256)
            $ scriptPathControl scriptPathData
    theLeafHash = (leafHash <$> (.&. 0xFE) . scriptPathLeafVersion <*> scriptPathScript) scriptPathData
    expectedParity = bool 0 1 $ scriptPathExternalIsOdd scriptPathData

keyParity :: PubKey -> Word8
keyParity key = case BS.unpack . runPutS . serialize $ PubKeyI key True of
    0x02 : _ -> 0x00
    _ -> 0x01


#ifdef BIP340

type ExtFlag = Word8

signTaprootInput ::
    ExtFlag ->
    -- | Outputs being spent
    [TxOut] ->
    Tx ->
    -- | Input index
    Int ->
    SigHash ->
    -- | Annex
    Maybe ByteString ->
    -- | Secret key
    SecKey ->
    -- | Extra randomness
    Maybe Rand32 ->
    Maybe (Bip340Sig, SigHash)
signTaprootInput extFlag spentTxOuts tx inputIndex sigHash annex secKey rand32 =
    (,) <$> signBip340 secKey message rand32 <*> pure sigHash
  where
    Just message =
        C.msg
            . BA.convert
            . hashFinalize
            $ hashUpdates (initTaggedHash "TapSighash") ["\x00", sigMsg]

    sigMsg =
        BSL.toStrict $
            taprootSignatureDigest
                extFlag
                spentTxOuts
                tx
                inputIndex
                sigHash
                annex

-- | Create a witness datum for a taproot keypath spend
taprootKeyPathWitness :: Bip340Sig -> SigHash -> ByteString
taprootKeyPathWitness sig (SigHash sigHash) =
    mconcat $
        catMaybes
            [ Just $ S.encode sig
            , if sigHash /= 0x00
                then Just $ (BS.pack . pure . fromIntegral) sigHash
                else Nothing
            ]

-- | Calculate the signature digest for a taproot output
taprootSignatureDigest ::
    ExtFlag ->
    [TxOut] ->
    Tx ->
    -- | Input index for which we sign
    Int ->
    SigHash ->
    -- | Taproot annex
    Maybe ByteString ->
    BSL.ByteString
taprootSignatureDigest extFlag spentTxOuts tx inputIndex sigHash@(SigHash sigHashValue) annexM = runPut $ do
    put $ fromIntegral @_ @Word8 sigHashValue
    putWord32le $ txVersion tx
    putWord32le $ txLockTime tx
    unless (hasAnyoneCanPayFlag sigHash) $ do
        putSpent $ \_ txIn -> put (prevOutput txIn)
        putSpent $ \txOut _ -> putWord64le (outValue txOut)
        putSpent $ \txOut _ -> (put . VarString) (scriptOutput txOut)
        putSpent $ \_ txIn -> putWord32le (txInSequence txIn)
    unless (isSigHashNone sigHash || isSigHashSingle sigHash) . putSha256 $
        mapM_ put (txOut tx)
    put spendType
    if hasAnyoneCanPayFlag sigHash
        then do
            put spentOutPoint
            putWord64le $ outValue spentTxOut
            put . VarString $ scriptOutput spentTxOut
            putWord32le $ txInSequence thisTxIn
        else putWord32le $ fromIntegral inputIndex
    mapM_ (putSha256 . put . VarString) annexM
    when (isSigHashSingle sigHash) . putSha256 . put $ txOut tx !! fromIntegral inputIndex
  where
    spendType = extFlag * 2 + bool 0 1 hasAnnex
    hasAnnex = isJust annexM

    thisTxIn = txIn tx !! inputIndex

    spentOutPoint = prevOutput thisTxIn
    spentTxOut = spentTxOuts !! inputIndex

    putSpent :: (TxOut -> TxIn -> Put) -> Put
    putSpent f = putSha256 $ zipWithM_ f spentTxOuts (txIn tx)

putSha256 :: Put -> Put
putSha256 =
    putByteString
        . BA.convert
        . hashWith SHA256
        . BSL.toStrict
        . runPut

#endif
