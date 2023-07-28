{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Transaction.TaprootSpec (spec) where

import Control.Applicative ((<|>))
import Control.Monad (zipWithM, (<=<))
import Data.Aeson
import Data.Aeson.Types
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (deserialize, serialize)
import Data.Text (Text)
import Data.Word (Word8)
import Haskoin
import Test.HUnit (assertBool, (@?=))
import Test.Hspec (Spec, describe, it, runIO)

spec :: Spec
spec = prepareContext $ \ctx -> do
  TestVector {testScriptPubKey} <-
    runIO $
      readTestFileParser (testVectorParseJSON ctx) "bip341.json"
  describe "Taproot" $ do
    it "should calculate the correct hashes" $
      mapM_ testHashes testScriptPubKey
    it "should build the correct output key" $
      mapM_ (testOutputKey ctx) testScriptPubKey
    it "should build the correct script output" $
      mapM_ (testScriptOutput ctx) testScriptPubKey
    it "should calculate the correct control blocks" $
      mapM_ (testControlBlocks ctx) testScriptPubKey
    it "should arrive at the correct address" $
      mapM_ (testAddress ctx) testScriptPubKey

testHashes :: TestScriptPubKey -> IO ()
testHashes testData =
  mapM_ checkMASTDetails $ ((.mast) . tspkGiven) testData
  where
    checkMASTDetails theMAST = do
      -- Leaf hashes
      (Just . getLeafHashes) theMAST @?= (spkiLeafHashes . tspkIntermediary) testData
      -- Merkle root
      (Just . BA.convert . mastCommitment) theMAST @?= (spkiMerkleRoot . tspkIntermediary) testData

    getLeafHashes = \case
      MASTBranch branchL branchR -> getLeafHashes branchL <> getLeafHashes branchR
      leaf@MASTLeaf {} -> [BA.convert $ mastCommitment leaf]
      MASTCommitment {} -> mempty -- The test vectors have complete trees

testOutputKey :: Ctx -> TestScriptPubKey -> IO ()
testOutputKey ctx testData = do
  XOnlyPubKey (taprootOutputKey ctx theOutput) @?= theOutputKey
  where
    theOutput = tspkGiven testData
    theOutputKey = XOnlyPubKey . spkiTweakedPubKey $ tspkIntermediary testData

testScriptOutput :: Ctx -> TestScriptPubKey -> IO ()
testScriptOutput ctx testData =
  taprootScriptOutput ctx (tspkGiven testData) @?= (spkeScriptPubKey . tspkExpected) testData

testControlBlocks :: Ctx -> TestScriptPubKey -> IO ()
testControlBlocks ctx testData = do
  mapM_ onExamples exampleControlBlocks
  mapM_ checkVerification scriptPathSpends
  where
    theOutput = tspkGiven testData
    theOutputKey = taprootOutputKey ctx theOutput
    exampleControlBlocks = spkeControlBlocks $ tspkExpected testData
    calculatedControlBlocks =
      (!! 1) . encodeTaprootWitness ctx . ScriptPathSpend <$> scriptPathSpends
    scriptPathSpends =
      mkScriptPathSpend <$> maybe mempty getMerkleProofs theOutput.mast
    mkScriptPathSpend (leafVersion, script, proof) =
      ScriptPathData
        { annex = Nothing,
          stack = mempty,
          script,
          extIsOdd = odd $ keyParity ctx theOutputKey,
          leafVersion,
          internalKey = theOutput.internalKey,
          control = BA.convert <$> proof
        }
    onExamples = zipWithM (@?=) calculatedControlBlocks
    checkVerification = assertBool "Script verifies" . verifyScriptPathData ctx theOutputKey

keyParity :: Ctx -> PubKey -> Word8
keyParity ctx key =
  case BS.unpack . marshal ctx $ PublicKey key True of
    0x02 : _ -> 0x00
    _ -> 0x01

testAddress :: Ctx -> TestScriptPubKey -> IO ()
testAddress ctx testData =
  computedAddress @?= (Just . spkeAddress . tspkExpected) testData
  where
    computedAddress =
      (addrToText btc <=< outputAddress ctx)
        . taprootScriptOutput ctx
        $ tspkGiven testData

newtype SpkGiven = SpkGiven {unSpkGiven :: TaprootOutput}

spkGivenParseJSON :: Ctx -> Value -> Parser SpkGiven
spkGivenParseJSON ctx = withObject "SpkGiven" $ \obj -> do
  pxopk@XOnlyPubKey {} <- unmarshalValue ctx =<< obj .: "internalPubkey"
  tree <- traverse parseScriptTree =<< obj .:? "scriptTree"
  return $ SpkGiven $ TaprootOutput pxopk.point tree
  where
    parseScriptTree v =
      parseScriptLeaf v
        <|> parseScriptBranch v
        <|> fail "Unable to parse scriptTree"
    parseScriptLeaf = withObject "ScriptTree leaf" $ \obj ->
      MASTLeaf
        <$> obj .: "leafVersion"
        <*> (obj .: "script" >>= hexScript)
    parseScriptBranch v =
      parseJSON v >>= \case
        [v1, v2] -> MASTBranch <$> parseScriptTree v1 <*> parseScriptTree v2
        _ -> fail "ScriptTree branch"
    hexScript = either fail pure . runGetS deserialize <=< jsonHex

data SpkIntermediary = SpkIntermediary
  { spkiLeafHashes :: Maybe [ByteString],
    spkiMerkleRoot :: Maybe ByteString,
    spkiTweakedPubKey :: PubKey
  }

spkIntermediaryParseJSON :: Ctx -> Value -> Parser SpkIntermediary
spkIntermediaryParseJSON ctx = withObject "SpkIntermediary" $ \obj ->
  SpkIntermediary
    <$> (obj .:? "leafHashes" >>= (traverse . traverse) jsonHex)
    <*> (obj .: "merkleRoot" >>= traverse jsonHex)
    <*> fmap
      (\(XOnlyPubKey k) -> k)
      (unmarshalValue ctx =<< obj .: "tweakedPubkey")

data SpkExpected = SpkExpected
  { spkeScriptPubKey :: ScriptOutput,
    spkeControlBlocks :: Maybe [ByteString],
    spkeAddress :: Text
  }

spkExpectedParseJSON :: Ctx -> Value -> Parser SpkExpected
spkExpectedParseJSON ctx = withObject "SpkExpected" $ \obj ->
  SpkExpected
    <$> (unmarshalValue ctx =<< obj .: "scriptPubKey")
    <*> ((traverse . traverse) jsonHex =<< obj .:? "scriptPathControlBlocks")
    <*> obj .: "bip350Address"

data TestScriptPubKey = TestScriptPubKey
  { tspkGiven :: TaprootOutput,
    tspkIntermediary :: SpkIntermediary,
    tspkExpected :: SpkExpected
  }

testScriptPubKeyParseJSON :: Ctx -> Value -> Parser TestScriptPubKey
testScriptPubKeyParseJSON ctx = withObject "TestScriptPubKey" $ \obj -> do
  given <- unSpkGiven <$> (spkGivenParseJSON ctx =<< obj .: "given")
  inter <- spkIntermediaryParseJSON ctx =<< obj .: "intermediary"
  expect <- spkExpectedParseJSON ctx =<< obj .: "expected"
  return $ TestScriptPubKey given inter expect

newtype TestVector = TestVector
  { testScriptPubKey :: [TestScriptPubKey]
  }

testVectorParseJSON :: Ctx -> Value -> Parser TestVector
testVectorParseJSON ctx = withObject "TestVector" $ \obj ->
  TestVector <$> (mapM (testScriptPubKeyParseJSON ctx) =<< obj .: "scriptPubKey")

jsonHex :: Text -> Parser ByteString
jsonHex = maybe (fail "Unable to decode hex") pure . decodeHex
