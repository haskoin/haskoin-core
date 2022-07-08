{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Transaction.TaprootSpec (spec) where

import Control.Applicative ((<|>))
import Control.Monad (zipWithM, (<=<), (>=>))
import Data.Aeson (FromJSON (parseJSON), Value, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize)
import Data.Foldable (foldl')
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Data.Serialize (Get, Serialize)
import qualified Data.Serialize as S
import Data.String (fromString)
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Haskoin (
    Input (Input),
    MAST (..),
    Msg,
    PubKey,
    PubKeyI (PubKeyI),
    ScriptOutput,
    ScriptPathData (..),
    SecKey,
    SigHash (SigHash),
    TaprootOutput (TaprootOutput),
    TaprootWitness (ScriptPathSpend),
    Tweak,
    Tx,
    TxOut (TxOut),
    XOnlyPubKey,
    addrToText,
    btc,
    decodeHex,
    encodeTaprootWitness,
    getMerkleProofs,
    mastCommitment,
    outputAddress,
    taprootInternalKey,
    taprootMAST,
    taprootOutputKey,
    taprootScriptOutput,
    verifyScriptPathData,
 )
#ifdef BIP340
import Haskoin (signTaprootInput, taprootKeyPathWitness)
#endif
import qualified Haskoin as H
import Haskoin.Transaction.Taproot (getByXCoord, toXCoord)
import Haskoin.UtilSpec (readTestFile)
import Test.HUnit (assertBool, (@?=))
import Test.Hspec (Spec, describe, it, runIO)

spec :: Spec
spec = do
#ifdef BIP340
    TestVector{testScriptPubKey, testKeyPathSpending} <- runIO $ readTestFile "bip341.json"
#else
    TestVector{testScriptPubKey} <- runIO $ readTestFile "bip341.json"
#endif
    describe "Taproot" $ do
        describe "Structure tests" $ do
            it "should calculate the correct hashes" $ mapM_ testHashes testScriptPubKey
            it "should build the correct output key" $ mapM_ testOutputKey testScriptPubKey
            it "should build the correct script output" $ mapM_ testScriptOutput testScriptPubKey
            it "should calculate the correct control blocks" $ mapM_ testControlBlocks testScriptPubKey
            it "should arrive at the correct address" $ mapM_ testAddress testScriptPubKey
#ifdef BIP340
        describe "Signing tests" $ do
            it "should sign inputs correctly" $ testSignInputs testKeyPathSpending
#endif

testHashes :: TestScriptPubKey -> IO ()
testHashes testData =
    mapM_ checkMASTDetails $ (taprootMAST . tspkGiven) testData
  where
    checkMASTDetails theMAST = do
        -- Leaf hashes
        (Just . getLeafHashes) theMAST @?= (spkiLeafHashes . tspkIntermediary) testData
        -- Merkle root
        (Just . BA.convert . mastCommitment) theMAST @?= (spkiMerkleRoot . tspkIntermediary) testData

    getLeafHashes = \case
        MASTBranch branchL branchR -> getLeafHashes branchL <> getLeafHashes branchR
        leaf@MASTLeaf{} -> [BA.convert $ mastCommitment leaf]
        MASTCommitment{} -> mempty -- The test vectors have complete trees

testOutputKey :: TestScriptPubKey -> IO ()
testOutputKey testData = do
    (toXCoord . taprootOutputKey) theOutput @?= toXCoord theOutputKey
  where
    theOutput = tspkGiven testData
    theOutputKey = spkiTweakedPubKey $ tspkIntermediary testData

testScriptOutput :: TestScriptPubKey -> IO ()
testScriptOutput testData =
    taprootScriptOutput (tspkGiven testData) @?= (spkeScriptPubKey . tspkExpected) testData

testControlBlocks :: TestScriptPubKey -> IO ()
testControlBlocks testData = do
    mapM_ onExamples exampleControlBlocks
    mapM_ checkVerification scriptPathSpends
  where
    theOutput = tspkGiven testData
    theOutputKey = taprootOutputKey theOutput
    exampleControlBlocks = spkeControlBlocks $ tspkExpected testData
    calculatedControlBlocks =
        (!! 1) . encodeTaprootWitness . ScriptPathSpend <$> scriptPathSpends
    scriptPathSpends =
        fmap mkScriptPathSpend
            . maybe mempty getMerkleProofs
            $ taprootMAST theOutput
    mkScriptPathSpend (scriptPathLeafVersion, scriptPathScript, proof) =
        ScriptPathData
            { scriptPathAnnex = Nothing
            , scriptPathStack = mempty
            , scriptPathScript
            , scriptPathExternalIsOdd = odd $ keyParity theOutputKey
            , scriptPathLeafVersion
            , scriptPathInternalKey = taprootInternalKey theOutput
            , scriptPathControl = BA.convert <$> proof
            }
    onExamples = zipWithM (@?=) calculatedControlBlocks
    checkVerification = assertBool "Script verifies" . verifyScriptPathData theOutputKey

keyParity :: PubKey -> Word8
keyParity key = case BS.unpack . runPutS . serialize $ PubKeyI key True of
    0x02 : _ -> 0x00
    _ -> 0x01

testAddress :: TestScriptPubKey -> IO ()
testAddress testData = computedAddress @?= (Just . spkeAddress . tspkExpected) testData
  where
    computedAddress = (addrToText btc <=< outputAddress) . taprootScriptOutput $ tspkGiven testData

#ifdef BIP340
testSignInputs :: TestKeyPathSpending -> IO ()
testSignInputs tkps = mapM_ trySign $ tkpsInputSpending tkps
  where
    trySign kpis =
        assertBool "Witness matches" $
            witness == kpisWitness kpis
      where
        Just witness =
            uncurry taprootKeyPathWitness
                <$> (sign <$> kpisTxInIndex <*> kpisSigHashType <*> kpisTweakedPrivkey) kpis

    sign ix theSigHashType theSecKey =
        signTaprootInput
            0x00
            (tkpsUtxosSpent tkps)
            (tkpsRawUnsignedTx tkps)
            ix
            theSigHashType
            Nothing
            theSecKey
            Nothing
#endif

newtype SpkGiven = SpkGiven {unSpkGiven :: TaprootOutput}

instance FromJSON SpkGiven where
    parseJSON = withObject "SpkGiven" $ \obj ->
        fmap SpkGiven $
            TaprootOutput
                <$> (obj .: "internalPubkey" >>= hexFieldBy getByXCoord)
                <*> (obj .:? "scriptTree" >>= traverse parseScriptTree)
      where
        parseScriptTree v =
            parseScriptLeaf v
                <|> parseScriptBranch v
                <|> fail "Unable to parse scriptTree"
        parseScriptLeaf = withObject "ScriptTree leaf" $ \obj ->
            MASTLeaf
                <$> obj .: "leafVersion"
                <*> (obj .: "script" >>= hexField)
        parseScriptBranch v =
            parseJSON v >>= \case
                [v1, v2] -> MASTBranch <$> parseScriptTree v1 <*> parseScriptTree v2
                _ -> fail "ScriptTree branch"

data SpkIntermediary = SpkIntermediary
    { spkiLeafHashes :: Maybe [ByteString]
    , spkiMerkleRoot :: Maybe ByteString
    , spkiTweakedPubKey :: PubKey
    }

instance FromJSON SpkIntermediary where
    parseJSON = withObject "SpkIntermediary" $ \obj ->
        SpkIntermediary
            <$> (obj .:? "leafHashes" >>= (traverse . traverse) jsonHex)
            <*> (obj .: "merkleRoot" >>= traverse jsonHex)
            <*> (obj .: "tweakedPubkey" >>= hexFieldBy getByXCoord)

data SpkExpected = SpkExpected
    { spkeScriptPubKey :: ScriptOutput
    , spkeControlBlocks :: Maybe [ByteString]
    , spkeAddress :: Text
    }

instance FromJSON SpkExpected where
    parseJSON = withObject "SpkExpected" $ \obj ->
        SpkExpected
            <$> obj .: "scriptPubKey"
            <*> (obj .:? "scriptPathControlBlocks" >>= (traverse . traverse) jsonHex)
            <*> obj .: "bip350Address"

data TestScriptPubKey = TestScriptPubKey
    { tspkGiven :: TaprootOutput
    , tspkIntermediary :: SpkIntermediary
    , tspkExpected :: SpkExpected
    }

instance FromJSON TestScriptPubKey where
    parseJSON = withObject "TestScriptPubKey" $ \obj ->
        TestScriptPubKey
            <$> (unSpkGiven <$> obj .: "given")
            <*> obj .: "intermediary"
            <*> obj .: "expected"

#ifdef BIP340

data KeyPathInputSpending = KeyPathInputSpending
    { kpisTxInIndex :: Int
    , kpisInternalPrivKey :: SecKey
    , kpisInternalPubKey :: XOnlyPubKey
    , kpisMerkleRoot :: Maybe ByteString
    , kpisSigHashType :: SigHash
    , kpisTweak :: Tweak
    , kpisTweakedPrivkey :: SecKey
    , kpisSigMsg :: ByteString
    , kpisPrecomputedUsed :: [Text]
    , kpisSigHash :: Msg
    , kpisWitness :: ByteString
    }

data TestKeyPathSpending = TestKeyPathSpending
    { tkpsRawUnsignedTx :: Tx
    , tkpsUtxosSpent :: [TxOut]
    , tkpsHashAmounts :: ByteString
    , tkpsHashOutpus :: ByteString
    , tkpsHasPrevouts :: ByteString
    , tkpsHashScriptPubKeys :: ByteString
    , tkpsHashSequences :: ByteString
    , tkpsInputSpending :: [KeyPathInputSpending]
    , tkpsFullySignedTx :: Tx
    }

instance FromJSON TestKeyPathSpending where
    parseJSON v =
        TestKeyPathSpending
            <$> (deepField ["given", "rawUnsignedTx"] v >>= hexField)
            <*> (deepField ["given", "utxosSpent"] v >>= traverse decodeUtxoSpent)
            <*> (deepField ["intermediary", "hashAmounts"] v >>= jsonHex)
            <*> (deepField ["intermediary", "hashOutputs"] v >>= jsonHex)
            <*> (deepField ["intermediary", "hashPrevouts"] v >>= jsonHex)
            <*> (deepField ["intermediary", "hashScriptPubkeys"] v >>= jsonHex)
            <*> (deepField ["intermediary", "hashSequences"] v >>= jsonHex)
            <*> (deepField ["inputSpending"] v >>= traverse decodeInputSpending)
            <*> (deepField ["auxiliary", "fullySignedTx"] v >>= hexField)
      where
        deepField :: FromJSON a => [String] -> Value -> Parser a
        deepField = \case
            [name] -> withObject ("." <> name) $ (.: fromString name)
            name : names -> withObject ("." <> name) $ (.: fromString name) >=> deepField names
            _ -> const $ fail "empty path"
        decodeUtxoSpent = withObject "uxto spent" $ \obj ->
            TxOut
                <$> obj .: "amountSats"
                <*> (obj .: "scriptPubKey" >>= jsonHex)
        decodeInputSpending vIS =
            KeyPathInputSpending
                <$> deepField ["given", "txinIndex"] vIS
                <*> (deepField ["given", "internalPrivkey"] vIS >>= hexField)
                <*> (deepField ["intermediary", "internalPubkey"] vIS >>= hexField)
                <*> (deepField ["given", "merkleRoot"] vIS >>= traverse jsonHex)
                <*> deepField ["given", "hashType"] vIS
                <*> (deepField ["intermediary", "tweak"] vIS >>= hexField)
                <*> (deepField ["intermediary", "tweakedPrivkey"] vIS >>= hexField)
                <*> (deepField ["intermediary", "sigMsg"] vIS >>= jsonHex)
                <*> deepField ["intermediary", "precomputedUsed"] vIS
                <*> (deepField ["intermediary", "sigHash"] vIS >>= hexField)
                <*> (deepField ["expected", "witness"] vIS >>= traverse jsonHex >>= oneElement)

data TestVector = TestVector
    { testScriptPubKey :: [TestScriptPubKey]
    , testKeyPathSpending :: TestKeyPathSpending
    }

instance FromJSON TestVector where
    parseJSON = withObject "TestVector" $ \obj ->
        TestVector
            <$> obj .: "scriptPubKey"
            <*> (obj .: "keyPathSpending" >>= oneElement)

oneElement :: [a] -> Parser a
oneElement = maybe (fail "Must be exactly one element") pure . listToMaybe

#else

newtype TestVector = TestVector
    { testScriptPubKey :: [TestScriptPubKey]
    }

instance FromJSON TestVector where
    parseJSON = withObject "TestVector" $ \obj ->
        TestVector <$> obj .: "scriptPubKey"

#endif

hexField :: Serialize a => Text -> Parser a
hexField = either fail pure . S.decode <=< jsonHex

hexFieldBy :: Get a -> Text -> Parser a
hexFieldBy parser = either fail pure . runGetS parser <=< jsonHex

jsonHex :: Text -> Parser ByteString
jsonHex = maybe (fail "Unable to decode hex") pure . decodeHex
