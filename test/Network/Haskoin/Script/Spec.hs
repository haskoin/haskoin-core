{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Script.Spec (spec) where

import           Control.Monad
import qualified Crypto.Secp256k1            as EC
import qualified Data.Aeson                  as J
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.Either
import           Data.List
import           Data.List
import           Data.Maybe
import           Data.Monoid                 ((<>))
import           Data.Serialize
import           Data.String
import           Data.String.Conversions     (cs)
import           Data.Word
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Script
import           Network.Haskoin.Test
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.QuickCheck
import           Text.Read

spec :: Network -> Spec
spec net = do
    describe "scripts" $ do
        standardSpec net
        strictSigSpec net
        scriptSpec net
        txSigHashForkIdSpec net
        forkIdScriptSpec net
    describe "sighash" $ do
        sigHashSpec net
        txSigHashSpec net

standardSpec :: Network -> Spec
standardSpec net = do
    it "has intToScriptOp . scriptOpToInt identity" $
        property $
        forAll arbitraryIntScriptOp $ \i ->
            intToScriptOp <$> scriptOpToInt i `shouldBe` Right i
    it "has decodeOutput . encodeOutput identity" $
        property $
        forAll (arbitraryScriptOutput net) $ \so ->
            decodeOutput (encodeOutput so) `shouldBe` Right so
    it "has decodeInput . encodeOutput identity" $
        property $
        forAll arbitraryScriptInput $ \si ->
            decodeInput net (encodeInput si) `shouldBe` Right si
    it "can sort multisig scripts" $
        forAll arbitraryMSOutput $ \out ->
            map encode (getOutputMulSigKeys (sortMulSig out)) `shouldSatisfy` \xs ->
                xs == sort xs
    it "can decode inputs with empty signatures" $ do
        decodeInput net (Script [OP_0]) `shouldBe`
            Right (RegularInput (SpendPK TxSignatureEmpty))
        decodeInput net (Script [opPushData ""]) `shouldBe`
            Right (RegularInput (SpendPK TxSignatureEmpty))
        let pk =
                derivePubKey $
                makePrvKey $ fromJust $ EC.secKey $ BS.replicate 32 1
        decodeInput net (Script [OP_0, opPushData $ encode pk]) `shouldBe`
            Right (RegularInput (SpendPKHash TxSignatureEmpty pk))
        decodeInput net (Script [OP_0, OP_0]) `shouldBe`
            Right (RegularInput (SpendMulSig [TxSignatureEmpty]))
        decodeInput net (Script [OP_0, OP_0, OP_0, OP_0]) `shouldBe`
            Right (RegularInput (SpendMulSig $ replicate 3 TxSignatureEmpty))

scriptSpec :: Network -> Spec
scriptSpec net =
    when (getNetworkName net == "btc") $
    it "can verify standard scripts from script_tests.json file" $ do
        xs <- readTestFile "script_tests" :: IO [J.Value]
        let vectorsA =
                mapMaybe (J.decode . J.encode) xs :: [( String
                                                      , String
                                                      , String
                                                      , String
                                                      , String)]
            vectorsB =
                mapMaybe (J.decode . J.encode) xs :: [( [Word64]
                                                      , String
                                                      , String
                                                      , String
                                                      , String
                                                      , String)]
            vectors =
                map (\(a, b, c, d, e) -> ([0], a, b, c, d, e)) vectorsA <>
                vectorsB
        length vectors `shouldBe` 86
        forM_ vectors $ \([val], siStr, soStr, flags, res, _)
          -- We can disable specific tests by adding a DISABLED flag in the data
         ->
            unless ("DISABLED" `isInfixOf` flags) $ do
                let strict =
                        "DERSIG" `isInfixOf` flags ||
                        "STRICTENC" `isInfixOf` flags ||
                        "NULLDUMMY" `isInfixOf` flags
                    scriptSig = parseScript siStr
                    scriptPubKey = parseScript soStr
                    decodedOutput =
                        fromRight (error $ "Could not decode output: " <> soStr) $
                        decodeOutputBS scriptPubKey
                    ver =
                        verifyStdInput
                            net
                            strict
                            (spendTx scriptPubKey 0 scriptSig)
                            0
                            decodedOutput
                            (val * 100000000)
                case res of
                    "OK" -> ver `shouldBe` True
                    _ -> ver `shouldBe` False

forkIdScriptSpec :: Network -> Spec
forkIdScriptSpec net =
    when (isJust (getSigHashForkId net)) $
    it "can verify scripts from forkid_script_tests.json file" $ do
        xs <- readTestFile "forkid_script_tests" :: IO [J.Value]
        let vectors =
                mapMaybe (J.decode . J.encode) xs :: [( [Word64]
                                                      , String
                                                      , String
                                                      , String
                                                      , String
                                                      , String)]
        length vectors `shouldBe` 3
        forM_ vectors $ \([valBTC], siStr, soStr, _, res, _) -> do
            let val = valBTC * 100000000
                scriptSig = parseScript siStr
                scriptPubKey = parseScript soStr
                decodedOutput =
                    fromRight (error $ "Could not decode output: " <> soStr) $
                    decodeOutputBS scriptPubKey
                ver =
                    verifyStdInput
                        net
                        True -- Always strict
                        (spendTx scriptPubKey val scriptSig)
                        0
                        decodedOutput
                        val
            case res of
                "OK" -> ver `shouldBe` True
                _ -> ver `shouldBe` False

creditTx :: BS.ByteString -> Word64 -> Tx
creditTx scriptPubKey val =
    Tx 1 [txI] [txO] [] 0
  where
    txO = TxOut {outValue = val, scriptOutput = scriptPubKey}
    txI =
        TxIn
        { prevOutput = nullOutPoint
        , scriptInput = encode $ Script [OP_0, OP_0]
        , txInSequence = maxBound
        }

spendTx :: BS.ByteString -> Word64 -> BS.ByteString -> Tx
spendTx scriptPubKey val scriptSig =
    Tx 1 [txI] [txO] [] 0
  where
    txO = TxOut {outValue = val, scriptOutput = BS.empty}
    txI =
        TxIn
        { prevOutput = OutPoint (txHash $ creditTx scriptPubKey val) 0
        , scriptInput = scriptSig
        , txInSequence = maxBound
        }

parseScript :: String -> BS.ByteString
parseScript str =
    BS.concat $ fromMaybe err $ mapM f $ words str
  where
    f = decodeHex . cs . dropHex . replaceToken
    dropHex ('0':'x':xs) = xs
    dropHex xs           = xs
    err = error $ "Could not decode script: " <> str

replaceToken :: String -> String
replaceToken str = case readMaybe $ "OP_" <> str of
    Just opcode -> "0x" <> cs (encodeHex $ encode (opcode :: ScriptOp))
    _           -> str

strictSigSpec :: Network -> Spec
strictSigSpec net =
    when (getNetworkName net == "btc") $ do
        it "can decode strict signatures" $ do
            xs <- readTestFile "sig_strict"
            let vectors = mapMaybe (decodeHex . cs) (xs :: [String])
            length vectors `shouldBe` 3
            forM_ vectors $ \sig ->
                decodeTxStrictSig net sig `shouldSatisfy` isRight
        it "can detect non-strict signatures" $ do
            xs <- readTestFile "sig_nonstrict"
            let vectors = mapMaybe (decodeHex . cs) (xs :: [String])
            length vectors `shouldBe` 17
            forM_ vectors $ \sig ->
                decodeTxStrictSig net sig `shouldSatisfy` isLeft

txSigHashSpec :: Network -> Spec
txSigHashSpec net =
    when (getNetworkName net == "btc") $
    it "can produce valid sighashes from sighash.json test vectors" $ do
        xs <- readTestFile "sighash" :: IO [J.Value]
        let vectors =
                mapMaybe (J.decode . J.encode) xs :: [( String
                                                      , String
                                                      , Int
                                                      , Integer
                                                      , String)]
        length vectors `shouldBe` 500
        forM_ vectors $ \(txStr, scpStr, i, shI, resStr) -> do
            let tx = fromString txStr
                s =
                    fromMaybe (error $ "Could not decode script: " <> cs scpStr) $
                    eitherToMaybe . decode =<< decodeHex (cs scpStr)
                sh = fromIntegral shI
                res =
                    eitherToMaybe . decode . BS.reverse =<<
                    decodeHex (cs resStr)
            Just (txSigHash net tx s 0 i sh) `shouldBe` res

txSigHashForkIdSpec :: Network -> Spec
txSigHashForkIdSpec net =
    when (getNetworkName net == "btc") $
    it "can produce valid sighashes from forkid_sighash.json test vectors" $ do
        xs <- readTestFile "forkid_sighash" :: IO [J.Value]
        let vectors =
                mapMaybe (J.decode . J.encode) xs :: [( String
                                                      , String
                                                      , Int
                                                      , Word64
                                                      , Integer
                                                      , String)]
        length vectors `shouldBe` 13
        forM_ vectors $ \(txStr, scpStr, i, val, shI, resStr) -> do
            let tx = fromString txStr
                s =
                    fromMaybe (error $ "Could not decode script: " <> cs scpStr) $
                    eitherToMaybe . decode =<< decodeHex (cs scpStr)
                sh = fromIntegral shI
                res = eitherToMaybe . decode =<< decodeHex (cs resStr)
            Just (txSigHashForkId net tx s val i sh) `shouldBe` res

sigHashSpec :: Network -> Spec
sigHashSpec net = do
    it "can read . show" $
        property $ forAll arbitrarySigHash $ \sh -> read (show sh) `shouldBe` sh
    it "can correctly show" $ do
        show (0x00 :: SigHash) `shouldBe` "SigHash " <> show 0x00
        show (0x01 :: SigHash) `shouldBe` "SigHash " <> show 0x01
        show (0xff :: SigHash) `shouldBe` "SigHash " <> show 0xff
        show (0xabac3344 :: SigHash) `shouldBe` "SigHash " <> show 0xabac3344
    it "can add a forkid" $ do
        0x00 `sigHashAddForkId` 0x00 `shouldBe` 0x00
        0xff `sigHashAddForkId` 0x00ffffff `shouldBe` 0xffffffff
        0xffff `sigHashAddForkId` 0x00aaaaaa `shouldBe` 0xaaaaaaff
        0xffff `sigHashAddForkId` 0xaaaaaaaa `shouldBe` 0xaaaaaaff
        0xffff `sigHashAddForkId` 0x00004444 `shouldBe` 0x004444ff
        0xff01 `sigHashAddForkId` 0x44440000 `shouldBe` 0x44000001
        0xff03 `sigHashAddForkId` 0x00550000 `shouldBe` 0x55000003
    it "can extract a forkid" $ do
        sigHashGetForkId 0x00000000 `shouldBe` 0x00000000
        sigHashGetForkId 0x80000000 `shouldBe` 0x00800000
        sigHashGetForkId 0xffffffff `shouldBe` 0x00ffffff
        sigHashGetForkId 0xabac3403 `shouldBe` 0x00abac34
    it "can build some vectors" $ do
        sigHashAll `shouldBe` 0x01
        sigHashNone `shouldBe` 0x02
        sigHashSingle `shouldBe` 0x03
        setForkIdFlag sigHashAll `shouldBe` 0x41
        setAnyoneCanPayFlag sigHashAll `shouldBe` 0x81
        setAnyoneCanPayFlag (setForkIdFlag sigHashAll) `shouldBe` 0xc1
    it "can test flags" $ do
        hasForkIdFlag sigHashAll `shouldBe` False
        hasForkIdFlag (setForkIdFlag sigHashAll) `shouldBe` True
        hasAnyoneCanPayFlag sigHashAll `shouldBe` False
        hasAnyoneCanPayFlag (setAnyoneCanPayFlag sigHashAll) `shouldBe` True
        isSigHashAll sigHashNone `shouldBe` False
        isSigHashAll sigHashAll `shouldBe` True
        isSigHashNone sigHashSingle `shouldBe` False
        isSigHashNone sigHashNone `shouldBe` True
        isSigHashSingle sigHashAll `shouldBe` False
        isSigHashSingle sigHashSingle `shouldBe` True
        isSigHashUnknown sigHashAll `shouldBe` False
        isSigHashUnknown sigHashNone `shouldBe` False
        isSigHashUnknown sigHashSingle `shouldBe` False
        isSigHashUnknown 0x00 `shouldBe` True
        isSigHashUnknown 0x04 `shouldBe` True
    it "can decodeTxLaxSig . encode a TxSignature" $
        property $
        forAll arbitraryTxSignature $ \(_, _, ts) ->
            decodeTxLaxSig (encodeTxSig ts) `shouldBe` Right ts
    when (getNetworkName net == "btc") $
        it "can decodeTxStrictSig . encode a TxSignature" $
        property $
        forAll arbitraryTxSignature $ \(_, _, ts@(TxSignature _ sh)) ->
            if isSigHashUnknown sh || hasForkIdFlag sh
                then decodeTxStrictSig net (encodeTxSig ts) `shouldSatisfy`
                     isLeft
                else decodeTxStrictSig net (encodeTxSig ts) `shouldBe` Right ts
    it "can produce the sighash one" $
        property $
        forAll (arbitraryTx net) $ forAll arbitraryScript . testSigHashOne net

testSigHashOne :: Network -> Tx -> Script -> Word64 -> Bool -> Property
testSigHashOne net tx s val acp =
    not (null $ txIn tx) ==>
    if length (txIn tx) > length (txOut tx)
        then res `shouldBe` one
        else res `shouldNotBe` one
  where
    res = txSigHash net tx s val (length (txIn tx) - 1) (f sigHashSingle)
    one = "0100000000000000000000000000000000000000000000000000000000000000"
    f =
        if acp
            then setAnyoneCanPayFlag
            else id

{-- Test Utilities --}

readTestFile :: J.FromJSON a => FilePath -> IO a
readTestFile fp = do
    bs <- BL.readFile $ "data/" <> fp <> ".json"
    maybe (error $ "Could not read test file " <> fp) return $ J.decode bs
