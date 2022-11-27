{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides support for reperesenting full taproot outputs and parsing
-- taproot witnesses.  For reference see BIPS 340, 341, and 342.
module Bitcoin.Transaction.Taproot (
    XOnlyPubKey (..),
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
) where

import Bitcoin.Crypto (PubKey, initTaggedHash, tweak, tweakAddPubKey)
import Bitcoin.Keys.Common (PubKeyI (PubKeyI), pubKeyPoint)
import Bitcoin.Network.Common (VarInt (VarInt))
import Bitcoin.Script.Common (Script)
import Bitcoin.Script.Standard (ScriptOutput (PayWitness))
import Bitcoin.Transaction.Common (WitnessStack)
import Bitcoin.Util (eitherToMaybe)
import qualified Bitcoin.Util as U
import Control.Applicative (many)
import Crypto.Hash (
    Digest,
    SHA256,
    digestFromByteString,
    hashFinalize,
    hashUpdate,
    hashUpdates,
 )
import Data.Binary (Binary (..))
import qualified Data.Binary as Bin
import Data.Binary.Get (getByteString, getLazyByteString, getWord8)
import Data.Binary.Put (putLazyByteString, runPut)
import Data.Bits ((.&.), (.|.))
import Data.Bool (bool)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (foldl')
import Data.Function (on)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word8)


-- | An x-only pubkey corresponds to the keys @(x,y)@ and @(x, -y)@.  The
--equality test only checks the x-coordinate.  An x-only pubkey serializes to 32
--bytes.
newtype XOnlyPubKey = XOnlyPubKey {xOnlyPubKey :: PubKey}
    deriving (Show)


instance Eq XOnlyPubKey where
    (==) = (==) `on` Bin.encode


instance Binary XOnlyPubKey where
    put (XOnlyPubKey pk) =
        putLazyByteString
            . BSL.drop 1
            . Bin.encode
            $ PubKeyI pk True
    get =
        either fail (pure . XOnlyPubKey . pubKeyPoint)
            . U.decode
            . BSL.cons 0x02
            =<< getLazyByteString 32


type TapLeafVersion = Word8


-- | Merklized Abstract Syntax Tree.  This type can represent trees where only a
--subset of the leaves are known.  Note that the tree is invariant under swapping
--branches at an internal node.
data MAST
    = MASTBranch MAST MAST
    | MASTLeaf TapLeafVersion Script
    | MASTCommitment (Digest SHA256)
    deriving (Show)


-- | Get the inclusion proofs for the leaves in the tree.  The proof is ordered
--leaf-to-root.
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


-- | Calculate the root hash for this tree.
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
        . hashUpdates (initTaggedHash "TapLeaf")
        . BSL.toChunks
        . runPut
        $ do
            put leafVersion
            put $ (VarInt . fromIntegral . BSL.length) scriptBytes
            putLazyByteString scriptBytes
  where
    scriptBytes = Bin.encode leafScript


-- | Representation of a full taproot output.
data TaprootOutput = TaprootOutput
    { taprootInternalKey :: PubKey
    , taprootMAST :: Maybe MAST
    }
    deriving (Show)


taprootOutputKey :: TaprootOutput -> PubKey
taprootOutputKey TaprootOutput{taprootInternalKey, taprootMAST} =
    fromMaybe keyFail $ tweak commitment >>= tweakAddPubKey taprootInternalKey
  where
    commitment = taprootCommitment taprootInternalKey $ mastCommitment <$> taprootMAST
    keyFail = error "bitcoin taprootOutputKey: key derivation failed"


taprootCommitment :: PubKey -> Maybe (Digest SHA256) -> ByteString
taprootCommitment internalKey merkleRoot =
    BA.convert
        . hashFinalize
        . maybe id (flip hashUpdate) merkleRoot
        . (`hashUpdates` BSL.toChunks keyBytes)
        $ initTaggedHash "TapTweak"
  where
    keyBytes = Bin.encode $ XOnlyPubKey internalKey


-- | Generate the output script for a taproot output
taprootScriptOutput :: TaprootOutput -> ScriptOutput
taprootScriptOutput = PayWitness 0x01 . U.encodeS . XOnlyPubKey . taprootOutputKey


-- | Comprehension of taproot witness data
data TaprootWitness
    = -- | Signature
      KeyPathSpend ByteString
    | ScriptPathSpend ScriptPathData
    deriving (Eq, Show)


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


-- | Try to interpret a 'WitnessStack' as taproot witness data.
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
            scriptPathScript <- eitherToMaybe . U.decode $ BSL.fromStrict scriptBytes
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
    deconstructControl = eitherToMaybe . U.runGet deserializeControl . BSL.fromStrict
    deserializeControl = do
        v <- getWord8
        k <- xOnlyPubKey <$> get
        proof <- many $ getByteString 32
        pure (v, k, proof)


-- | Transform the high-level representation of taproot witness data into a witness stack
encodeTaprootWitness :: TaprootWitness -> WitnessStack
encodeTaprootWitness = \case
    KeyPathSpend signature -> pure signature
    ScriptPathSpend scriptPathData ->
        scriptPathStack scriptPathData
            <> [ U.encodeS $ scriptPathScript scriptPathData
               , mconcat
                    [ BS.pack [scriptPathLeafVersion scriptPathData .|. parity scriptPathData]
                    , U.encodeS . XOnlyPubKey $ scriptPathInternalKey scriptPathData
                    , mconcat $ scriptPathControl scriptPathData
                    ]
               , fromMaybe mempty $ scriptPathAnnex scriptPathData
               ]
  where
    parity = bool 0 1 . scriptPathExternalIsOdd


-- | Verify that the script path spend is valid, except for script execution.
verifyScriptPathData ::
    -- | Output key
    PubKey ->
    ScriptPathData ->
    Bool
verifyScriptPathData outputKey scriptPathData = fromMaybe False $ do
    tweak commitment >>= fmap onComputedKey . tweakAddPubKey (scriptPathInternalKey scriptPathData)
  where
    onComputedKey computedKey =
        XOnlyPubKey outputKey == XOnlyPubKey computedKey
            && expectedParity == keyParity computedKey
    commitment = taprootCommitment (scriptPathInternalKey scriptPathData) (Just merkleRoot)
    merkleRoot =
        foldl' hashBranch theLeafHash
            . mapMaybe (digestFromByteString @SHA256)
            $ scriptPathControl scriptPathData
    theLeafHash = (leafHash <$> (.&. 0xFE) . scriptPathLeafVersion <*> scriptPathScript) scriptPathData
    expectedParity = bool 0 1 $ scriptPathExternalIsOdd scriptPathData


keyParity :: PubKey -> Word8
keyParity key = case BSL.unpack . Bin.encode $ PubKeyI key True of
    0x02 : _ -> 0x00
    _ -> 0x01
