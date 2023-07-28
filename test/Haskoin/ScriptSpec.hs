{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Haskoin.ScriptSpec (spec) where

import Control.Monad
import Data.Aeson as A
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Either
import Data.List
import Data.Maybe
import Data.String
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word
import Haskoin.Address
import Haskoin.Crypto
import Haskoin.Network.Constants
import Haskoin.Network.Data
import Haskoin.Script
import Haskoin.Transaction
import Haskoin.Util
import Haskoin.Util.Arbitrary
import Test.HUnit as HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Read

serialVals :: [SerialBox]
serialVals =
  [ SerialBox arbitraryScriptOp,
    SerialBox arbitraryScript
  ]

readVals :: Ctx -> [ReadBox]
readVals ctx =
  [ ReadBox arbitrarySigHash,
    ReadBox arbitrarySigHashFlag,
    ReadBox arbitraryScript,
    ReadBox arbitraryPushDataType,
    ReadBox arbitraryScriptOp,
    ReadBox ((`arbitraryScriptOutput` ctx) =<< arbitraryNetwork)
  ]

jsonVals :: Ctx -> [JsonBox]
jsonVals ctx =
  [ JsonBox $
      fmap (marshalValue ctx) $
        arbitraryNetwork >>= flip arbitraryScriptOutput ctx,
    JsonBox arbitraryOutPoint,
    JsonBox arbitrarySigHash,
    JsonBox $
      fmap (marshalValue ctx . fst) $
        arbitraryNetwork >>= flip arbitrarySigInput ctx
  ]

netVals :: Ctx -> [NetBox]
netVals ctx =
  [ NetBox
      ( marshalValue . (,ctx),
        marshalEncoding . (,ctx),
        unmarshalValue . (,ctx),
        do
          net <- arbitraryNetwork
          (_, _, txsig) <- arbitraryTxSignature net ctx
          return (net, txsig)
      )
  ]

spec :: Spec
spec = prepareContext $ \ctx -> do
  testIdentity serialVals (readVals ctx) (jsonVals ctx) (netVals ctx)
  describe "btc scripts" $ props btc ctx
  describe "bch scripts" $ props bch ctx
  describe "multi signatures" $
    zipWithM_ (curry (mapMulSigVector ctx)) mulSigVectors [0 ..]
  describe "signature decoding" $
    zipWithM_ (curry (sigDecodeMap btc ctx)) scriptSigSignatures [0 ..]
  describe "SigHashFlag fromEnum/toEnum" $
    prop "fromEnum/toEnum" $
      forAll arbitrarySigHashFlag $
        \f -> toEnum (fromEnum f) `shouldBe` f
  describe "Script vectors" $
    it "Can encode script vectors" encodeScriptVector

props :: Network -> Ctx -> Spec
props net ctx = do
  standardSpec net ctx
  strictSigSpec net ctx
  scriptSpec net ctx
  txSigHashForkIdSpec net
  forkIdScriptSpec net ctx
  sigHashSpec net ctx
  txSigHashSpec net

standardSpec :: Network -> Ctx -> Spec
standardSpec net ctx = do
  prop "has intToScriptOp . scriptOpToInt identity" $
    forAll arbitraryIntScriptOp $ \i ->
      intToScriptOp <$> scriptOpToInt i `shouldBe` Right i
  prop "has decodeOutput . encodeOutput identity" $
    forAll (arbitraryScriptOutput net ctx) $ \so ->
      decodeOutput ctx (encodeOutput ctx so) `shouldBe` Right so
  prop "has decodeInput . encodeOutput identity" $
    forAll (arbitraryScriptInput net ctx) $ \si ->
      (decodeInput net ctx . encodeInput net ctx) si `shouldBe` Right si
  prop "can sort multisig scripts" $
    forAll (arbitraryMSOutput ctx) $ \out ->
      let keyList = map (marshal ctx) (sortMulSig ctx out).keys
          isSorted xs = xs == sort xs
       in keyList `shouldSatisfy` isSorted
  it "can decode inputs with empty signatures" $ do
    decodeInput net ctx (Script [OP_0])
      `shouldBe` Right (RegularInput (SpendPK TxSignatureEmpty))
    decodeInput net ctx (Script [opPushData ""])
      `shouldBe` Right (RegularInput (SpendPK TxSignatureEmpty))
    let Just sk = secKey (B.replicate 32 1)
        pk = derivePublicKey ctx (wrapSecKey True sk)
    decodeInput net ctx (Script [OP_0, opPushData $ marshal ctx pk])
      `shouldBe` Right (RegularInput (SpendPKHash TxSignatureEmpty pk))
    decodeInput net ctx (Script [OP_0, OP_0])
      `shouldBe` Right (RegularInput (SpendMulSig [TxSignatureEmpty]))
    decodeInput net ctx (Script [OP_0, OP_0, OP_0, OP_0])
      `shouldBe` Right (RegularInput (SpendMulSig $ replicate 3 TxSignatureEmpty))

scriptSpec :: Network -> Ctx -> Spec
scriptSpec net ctx =
  when (net.name == "btc") $
    it "can verify standard scripts from script_tests.json file" $ do
      xs <- readTestFile "script_tests.json" :: IO [A.Value]
      let vectorsA =
            mapMaybe (A.decode . A.encode) xs ::
              [(String, String, String, String, String)]
          vectorsB =
            mapMaybe (A.decode . A.encode) xs ::
              [([Word64], String, String, String, String, String)]
          vectors =
            map (\(a, b, c, d, e) -> ([0], a, b, c, d, e)) vectorsA
              <> vectorsB
      length vectors `shouldBe` 86
      forM_ vectors $ \([val], siStr, soStr, flags, res, desc) ->
        -- We can disable specific tests by adding a DISABLED flag in the data
        unless ("DISABLED" `isInfixOf` flags) $ do
          let _strict =
                any
                  (`isInfixOf` flags)
                  ["DERSIG", "STRICTENC", "NULLDUMMY"]
              scriptSig = parseScript siStr
              scriptPubKey = parseScript soStr
              out = unmarshal ctx scriptPubKey
              tx = spendTx scriptPubKey 0 scriptSig
              sat = val * 100000000
              ver o = verifyStdInput net ctx tx 0 o sat
              valid = either (const False) ver out
          assertBool desc $ if res == "OK" then valid else not valid

forkIdScriptSpec :: Network -> Ctx -> Spec
forkIdScriptSpec net ctx =
  when (isJust net.sigHashForkId) $
    it "can verify scripts from forkid_script_tests.json file" $ do
      xs <- readTestFile "forkid_script_tests.json" :: IO [A.Value]
      let vectors =
            mapMaybe (A.decode . A.encode) xs ::
              [ ( [Word64],
                  String,
                  String,
                  String,
                  String,
                  String
                )
              ]
      length vectors `shouldBe` 3
      forM_ vectors $ \([valBTC], siStr, soStr, _, res, _) -> do
        let val = valBTC * 100000000
            scriptSig = parseScript siStr
            scriptPubKey = parseScript soStr
            out = unmarshal ctx scriptPubKey
            tx = spendTx scriptPubKey val scriptSig
            ver o = verifyStdInput net ctx tx 0 o val
            valid = either (const False) ver out
        case res of
          "OK" -> valid `shouldBe` True
          _ -> valid `shouldBe` False

creditTx :: ByteString -> Word64 -> Tx
creditTx scriptPubKey val =
  Tx 1 [txI] [txO] [] 0
  where
    txO = TxOut {value = val, script = scriptPubKey}
    txI =
      TxIn
        { outpoint = nullOutPoint,
          script = runPutS $ serialize $ Script [OP_0, OP_0],
          sequence = maxBound
        }

spendTx :: ByteString -> Word64 -> ByteString -> Tx
spendTx scriptPubKey val scriptSig =
  Tx 1 [txI] [txO] [] 0
  where
    txO = TxOut {value = val, script = B.empty}
    txI =
      TxIn
        { outpoint = OutPoint (txHash $ creditTx scriptPubKey val) 0,
          script = scriptSig,
          sequence = maxBound
        }

parseScript :: String -> ByteString
parseScript str =
  B.concat $ fromMaybe err $ mapM f $ words str
  where
    f = decodeHex . cs . dropHex . replaceToken
    dropHex ('0' : 'x' : xs) = xs
    dropHex xs = xs
    err = error $ "Could not decode script: " <> str

replaceToken :: String -> String
replaceToken str = case readMaybe $ "OP_" <> str of
  Just opcode -> "0x" <> cs (encodeHex $ runPutS $ serialize (opcode :: ScriptOp))
  _ -> str

strictSigSpec :: Network -> Ctx -> Spec
strictSigSpec net ctx =
  when (net.name == "btc") $ do
    it "can decode strict signatures" $ do
      xs <- readTestFile "sig_strict.json"
      let vectors = mapMaybe decodeHex xs
      length vectors `shouldBe` 3
      forM_ vectors $ \sig ->
        let eitherSig :: Either String TxSignature
            eitherSig = decodeTxSig net ctx sig
         in eitherSig `shouldSatisfy` isRight
    it "can detect non-strict signatures" $ do
      xs <- readTestFile "sig_nonstrict.json"
      let vectors = mapMaybe decodeHex xs
      length vectors `shouldBe` 17
      forM_ vectors $ \sig ->
        let eitherSig = decodeTxSig net ctx sig
         in eitherSig `shouldSatisfy` isLeft

txSigHashSpec :: Network -> Spec
txSigHashSpec net =
  when (net.name == "btc") $
    it "can produce valid sighashes from sighash.json test vectors" $ do
      xs <- readTestFile "sighash.json" :: IO [A.Value]
      let vectors =
            mapMaybe (A.decode . A.encode) xs ::
              [ ( String,
                  String,
                  Int,
                  Integer,
                  String
                )
              ]
      length vectors `shouldBe` 500
      forM_ vectors $ \(txStr, scpStr, i, shI, resStr) -> do
        let tx = fromString txStr
            s =
              fromMaybe (error $ "Could not decode script: " <> cs scpStr) $
                eitherToMaybe . runGetS deserialize =<< decodeHex (cs scpStr)
            sh = fromIntegral shI
            res =
              eitherToMaybe . runGetS deserialize . B.reverse
                =<< decodeHex (cs resStr)
        Just (txSigHash net tx s 0 i sh) `shouldBe` res

txSigHashForkIdSpec :: Network -> Spec
txSigHashForkIdSpec net =
  when (net.name == "btc") $
    it "can produce valid sighashes from forkid_sighash.json test vectors" $ do
      xs <- readTestFile "forkid_sighash.json" :: IO [A.Value]
      let vectors =
            mapMaybe (A.decode . A.encode) xs ::
              [ ( String,
                  String,
                  Int,
                  Word64,
                  Integer,
                  String
                )
              ]
      length vectors `shouldBe` 13
      forM_ vectors $ \(txStr, scpStr, i, val, shI, resStr) -> do
        let tx = fromString txStr
            s =
              fromMaybe (error $ "Could not decode script: " <> cs scpStr) $
                eitherToMaybe . runGetS deserialize =<< decodeHex (cs scpStr)
            sh = fromIntegral shI
            res = eitherToMaybe . runGetS deserialize =<< decodeHex (cs resStr)
        Just (txSigHashForkId net tx s val i sh) `shouldBe` res

sigHashSpec :: Network -> Ctx -> Spec
sigHashSpec net ctx = do
  it "can correctly show" $ do
    show (0x00 :: SigHash) `shouldBe` "SigHash " <> show (0x00 :: Word32)
    show (0x01 :: SigHash) `shouldBe` "SigHash " <> show (0x01 :: Word32)
    show (0xff :: SigHash) `shouldBe` "SigHash " <> show (0xff :: Word32)
    show (0xabac3344 :: SigHash)
      `shouldBe` "SigHash "
        <> show (0xabac3344 :: Word32)
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
    setAnyoneCanPay sigHashAll `shouldBe` 0x81
    setAnyoneCanPay (setForkIdFlag sigHashAll) `shouldBe` 0xc1
  it "can test flags" $ do
    hasForkIdFlag sigHashAll `shouldBe` False
    hasForkIdFlag (setForkIdFlag sigHashAll) `shouldBe` True
    anyoneCanPay sigHashAll `shouldBe` False
    anyoneCanPay (setAnyoneCanPay sigHashAll) `shouldBe` True
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
  it "can decodeTxSig . encode a TxSignature" $
    property $
      forAll (arbitraryTxSignature net ctx) $ \(_, _, ts) ->
        let f = decodeTxSig net ctx . encodeTxSig net ctx
         in f ts `shouldBe` Right ts
  it "can produce the sighash one" $
    property $
      forAll (arbitraryTx net ctx) $
        forAll arbitraryScript . testSigHashOne net

testSigHashOne :: Network -> Tx -> Script -> Word64 -> Bool -> Property
testSigHashOne net tx s val acp =
  not (null tx.inputs) ==>
    if length tx.inputs > length tx.outputs
      then res `shouldBe` one
      else res `shouldNotBe` one
  where
    res = txSigHash net tx s val (length tx.inputs - 1) (f sigHashSingle)
    one = "0100000000000000000000000000000000000000000000000000000000000000"
    f =
      if acp
        then setAnyoneCanPay
        else id

{- Parse tests from bitcoin-qt repository -}

mapMulSigVector :: Ctx -> ((Text, Text), Int) -> Spec
mapMulSigVector ctx (v, i) =
  it name $ runMulSigVector ctx v
  where
    name = "check multisig vector " <> show i

runMulSigVector :: Ctx -> (Text, Text) -> Assertion
runMulSigVector ctx (a, ops) = assertBool "multisig vector" $ Just a == b
  where
    s = do
      s' <- decodeHex ops
      eitherToMaybe $ runGetS deserialize s'
    b = do
      o <- s
      d <- eitherToMaybe $ decodeOutput ctx o
      addrToText btc $ payToScriptAddress ctx d

sigDecodeMap :: Network -> Ctx -> (Text, Int) -> Spec
sigDecodeMap net ctx (_, i) =
  it ("check signature " ++ show i) func
  where
    func = testSigDecode net ctx $ scriptSigSignatures !! i

testSigDecode :: Network -> Ctx -> Text -> Assertion
testSigDecode net ctx str =
  let bs = fromJust $ decodeHex str
      eitherSig = decodeTxSig net ctx bs
   in assertBool
        ( unwords
            [ "Decode failed:",
              fromLeft (error "Decode did not fail") eitherSig
            ]
        )
        $ isRight eitherSig

mulSigVectors :: [(Text, Text)]
mulSigVectors =
  [ ( "3QJmV3qfvL9SuYo34YihAf3sRCW3qSinyC",
      "52410491bba2510912a5bd37da1fb5b1673010e43d2c6d812c514e91bfa9f2eb\
      \129e1c183329db55bd868e209aac2fbc02cb33d98fe74bf23f0c235d6126b1d8\
      \334f864104865c40293a680cb9c020e7b1e106d8c1916d3cef99aa431a56d253\
      \e69256dac09ef122b1a986818a7cb624532f062c1d1f8722084861c5c3291ccf\
      \fef4ec687441048d2455d2403e08708fc1f556002f1b6cd83f992d085097f997\
      \4ab08a28838f07896fbab08f39495e15fa6fad6edbfb1e754e35fa1c7844c41f\
      \322a1863d4621353ae"
    )
  ]

scriptSigSignatures :: [Text]
scriptSigSignatures =
  -- Signature in input of txid
  -- 1983a69265920c24f89aac81942b1a59f7eb30821a8b3fb258f88882b6336053
  [ "304402205ca6249f43538908151fe67b26d020306c0e59fa206cf9f3ccf641f333\
    \57119d02206c82f244d04ac0a48024fb9cc246b66e58598acf206139bdb7b75a29\
    \41a2b1e401"
    -- Signature in input of txid
    -- fb0a1d8d34fa5537e461ac384bac761125e1bfa7fec286fa72511240fa66864d.
    -- Strange DER sizes, but in Blockchain. Now invalid as Haskoin can only
    -- decode strict signatures.
    -- "3048022200002b83d59c1d23c08efd82ee0662fec23309c3adbcbd1f0b8695378d\
    -- \b4b14e736602220000334a96676e58b1bb01784cb7c556dd8ce1c220171904da22\
    -- \e18fe1e7d1510db501"
  ]

encodeScriptVector :: Assertion
encodeScriptVector =
  assertEqual "Encode script" res (encodeHex $ runPutS $ serialize s)
  where
    res =
      "514104cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58b\
      \bfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d\
      \348ac4410461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2f\
      \cfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b\
      \25c15342af52ae"
    s =
      Script
        [ OP_1,
          opPushData $
            d
              "04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef5\
              \8bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d\
              \11fcdd0d348ac4",
          opPushData $
            d
              "0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcf\
              \deb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39\
              \f58b25c15342af",
          OP_2,
          OP_CHECKMULTISIG
        ]
    d = fromJust . decodeHex
