{-# LANGUAGE OverloadedStrings #-}

module Haskoin.TransactionSpec (spec) where

import           Data.Aeson                 as A
import qualified Data.ByteString            as B
import           Data.Either
import           Data.Map.Strict            (singleton)
import           Data.Maybe
import           Data.Serialize             as S
import           Data.String                (fromString)
import           Data.String.Conversions
import           Data.Text                  (Text)
import           Data.Word                  (Word32, Word64)
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Keys
import           Haskoin.Script
import           Haskoin.Test
import           Haskoin.Transaction
import           Haskoin.Transaction.Segwit (isSegwit)
import           Haskoin.Util
import           Safe                       (readMay)
import           Test.Hspec
import           Test.HUnit                 (Assertion, assertBool)
import           Test.QuickCheck

spec :: Spec
spec = do
    let net = btc
    describe "transaction unit tests" $ do
        it "compute txid from tx" $
            mapM_ runTxIDVec txIDVec
        it "build pkhash transaction (generated from bitcoind)" $
            mapM_ runPKHashVec pkHashVec
        it "encode satoshi core script pubkey" tEncodeSatoshiCoreScriptPubKey
        --
        -- These tests depend on signatures matching exactly those in the spec.
        -- Fedora includes a version of libsecp256k1 that computes signatures
        -- using a slighlty different deterministic nonce algorithm.
        --
        -- it "agrees with BIP143 p2wpkh example" testBip143p2wpkh
        -- it "agrees with BIP143 p2sh-p2wpkh example" testBip143p2shp2wpkh
        -- it "builds a p2wsh multisig transaction" testP2WSHMulsig
        -- it "agrees with BIP143 p2sh-p2wsh multisig example" testBip143p2shp2wpkhMulsig
    describe "btc transaction" $ do
        it "decode and encode txid" $
            property $
            forAll arbitraryTxHash $ \h -> hexToTxHash (txHashToHex h) == Just h
        it "from string transaction id" $
            property $
            forAll arbitraryTxHash $ \h -> fromString (cs $ txHashToHex h) == h
        it "building address tx" $
            property $
            forAll arbitraryAddress $
            forAll (arbitrarySatoshi net) . testBuildAddrTx net
        it "guess transaction size" $
            property $ forAll (arbitraryAddrOnlyTxFull net) (testGuessSize net)
        it "choose coins" $
            property $ forAll (listOf (arbitrarySatoshi net)) testChooseCoins
        it "choose multisig coins" $
            property $
            forAll arbitraryMSParam $
            forAll (listOf (arbitrarySatoshi net)) . testChooseMSCoins
        it "sign and validate transaction" $
            property $ forAll (arbitrarySigningData net) (testDetSignTx net)
        it "sign and validate (nested) transaction" $
            property $ forAll (arbitrarySigningData net) (testDetSignNestedTx net)
        it "merge partially signed transactions" $
            property $ forAll (arbitraryPartialTxs net) (testMergeTx net)
    describe "json serialization" $ do
        it "encodes and decodes transaction" $
            property $ forAll (arbitraryTx net) testID
        it "encodes and decodes transaction hash" $
            property $ forAll arbitraryTxHash testID
    describe "transaction serialization" $ do
        it "encodes and decodes tx input" $
            property $ forAll (arbitraryTxIn net) cerealID
        it "encodes and decodes tx output" $
            property $ forAll (arbitraryTxOut net) cerealID
        it "encodes and decodes outpoint" $
            property $ forAll arbitraryOutPoint cerealID
        it "encodes and decodes transaction" $
            property $ forAll (arbitraryTx net) cerealID
        it "encodes and decodes witness transaction" $
            property $ forAll (arbitraryWitnessTx net) cerealID
        it "encodes and decodes legacy transaction" $
            property $ forAll (arbitraryLegacyTx net) cerealID

cerealID :: (Serialize a, Eq a) => a -> Bool
cerealID x = S.decode (S.encode x) == Right x

runTxIDVec :: (Text, Text) -> Assertion
runTxIDVec (tid, tx) = assertBool "txid" $ txHashToHex (txHash txBS) == tid
  where
    txBS = fromJust $ either (const Nothing) return . S.decode =<< decodeHex tx

txIDVec :: [(Text, Text)]
txIDVec =
    [ ( "23b397edccd3740a74adb603c9756370fafcde9bcc4483eb271ecad09a94dd63"
      , "0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba26000000000490047304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000"
      )
    , ( "c99c49da4c38af669dea436d3e73780dfdb6c1ecf9958baa52960e8baee30e73"
      , "01000000010276b76b07f4935c70acf54fbf1f438a4c397a9fb7e633873c4dd3bc062b6b40000000008c493046022100d23459d03ed7e9511a47d13292d3430a04627de6235b6e51a40f9cd386f2abe3022100e7d25b080f0bb8d8d5f878bba7d54ad2fda650ea8d158a33ee3cbd11768191fd004104b0e2c879e4daf7b9ab68350228c159766676a14f5815084ba166432aab46198d4cca98fa3e9981d0a90b2effc514b76279476550ba3663fdcaff94c38420e9d5000000000100093d00000000001976a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac00000000"
      )
    , ( "f7fdd091fa6d8f5e7a8c2458f5c38faffff2d3f1406b6e4fe2c99dcc0d2d1cbb"
      , "01000000023d6cf972d4dff9c519eff407ea800361dd0a121de1da8b6f4138a2f25de864b4000000008a4730440220ffda47bfc776bcd269da4832626ac332adfca6dd835e8ecd83cd1ebe7d709b0e022049cffa1cdc102a0b56e0e04913606c70af702a1149dc3b305ab9439288fee090014104266abb36d66eb4218a6dd31f09bb92cf3cfa803c7ea72c1fc80a50f919273e613f895b855fb7465ccbc8919ad1bd4a306c783f22cd3227327694c4fa4c1c439affffffff21ebc9ba20594737864352e95b727f1a565756f9d365083eb1a8596ec98c97b7010000008a4730440220503ff10e9f1e0de731407a4a245531c9ff17676eda461f8ceeb8c06049fa2c810220c008ac34694510298fa60b3f000df01caa244f165b727d4896eb84f81e46bcc4014104266abb36d66eb4218a6dd31f09bb92cf3cfa803c7ea72c1fc80a50f919273e613f895b855fb7465ccbc8919ad1bd4a306c783f22cd3227327694c4fa4c1c439affffffff01f0da5200000000001976a914857ccd42dded6df32949d4646dfa10a92458cfaa88ac00000000"
      )
    , ( "afd9c17f8913577ec3509520bd6e5d63e9c0fd2a5f70c787993b097ba6ca9fae"
      , "010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000006a4730440220360d20baff382059040ba9be98947fd678fb08aab2bb0c172efa996fd8ece9b702201b4fb0de67f015c90e7ac8a193aeab486a1f587e0f54d0fb9552ef7f5ce6caec032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e804010000006b483045022100c714310be1e3a9ff1c5f7cacc65c2d8e781fc3a88ceb063c6153bf950650802102200b2d0979c76e12bb480da635f192cc8dc6f905380dd4ac1ff35a4f68f462fffd032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff3f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000006c493046022100b663499ef73273a3788dea342717c2640ac43c5a1cf862c9e09b206fcb3f6bb8022100b09972e75972d9148f2bdd462e5cb69b57c1214b88fc55ca638676c07cfc10d8032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff0380841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac80841e00000000001976a9149857cc07bed33a5cf12b9c5e0500b675d500c81188ace0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac00000000"
      )
    ]

runPKHashVec :: ([(Text, Word32)], [(Text, Word64)], Text) -> Assertion
runPKHashVec (xs, ys, res) =
    assertBool "Build PKHash Tx" $ encodeHex (S.encode tx) == res
  where
    tx =
        fromRight (error "Could not decode transaction") $
        buildAddrTx btc (map f xs) ys
    f (tid, ix) = OutPoint (fromJust $ hexToTxHash tid) ix

-- These test vectors have been generated from bitcoind raw transaction api

pkHashVec :: [([(Text, Word32)], [(Text, Word64)], Text)]
pkHashVec =
    [
      ( [("eb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db",14)]
      , [("14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb",90000000)]
      , "0100000001db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0e00000000ffffffff01804a5d05000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac00000000"
      )
    , ( [ ("eb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db",0)
        , ("0001000000000000000000000000000000000000000000000000000000000000",2147483647)
        ]
      , [ ("14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb",1)
        , ("19VCgS642vzEA1sdByoSn6GsWBwraV8D4n",2100000000000000)
        ]
      , "0100000002db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0000000000ffffffff0000000000000000000000000000000000000000000000000000000000000100ffffff7f00ffffffff0201000000000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac0040075af07507001976a9145d16672f53981ff21c5f42b40d1954993cbca54f88ac00000000"
      )
    , ( [ ("eb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db",0)
        , ("0001000000000000000000000000000000000000000000000000000000000000",2147483647)
        ]
      , []
      , "0100000002db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0000000000ffffffff0000000000000000000000000000000000000000000000000000000000000100ffffff7f00ffffffff0000000000"
      )
    , ( []
      , [ ("14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb",1)
        , ("19VCgS642vzEA1sdByoSn6GsWBwraV8D4n",2100000000000000)
        ]
      , "01000000000201000000000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac0040075af07507001976a9145d16672f53981ff21c5f42b40d1954993cbca54f88ac00000000"
      )
    ]

tEncodeSatoshiCoreScriptPubKey :: Assertion
tEncodeSatoshiCoreScriptPubKey = assertBool "tEncodeSatoshiCoreScriptPubKey" $
  t1BsOutputScriptPubKey == encodeSatoshiCoreScriptPubKey t1SatoshiCoreJsonScriptPubKey
  where
    t1BsOutputScriptPubKey :: Text
    t1BsOutputScriptPubKey = "514104cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4410461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af52ae"
    t1SatoshiCoreJsonScriptPubKey :: String
    t1SatoshiCoreJsonScriptPubKey = "1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG"

encodeSatoshiCoreScriptPubKey :: String -> Text
encodeSatoshiCoreScriptPubKey =
  mconcat . map encodeSatoshiCoreScriptPiece . words
  where
    encodeSatoshiCoreScriptPiece :: String -> Text
    encodeSatoshiCoreScriptPiece s = case (readMay ("OP_" ++ s) :: Maybe ScriptOp) of
      Just op -> encodeHex . S.encode $ op
      Nothing -> case take 2 s of
          "OP" -> encodeHex . S.encode . (read :: String -> ScriptOp) $ s
          "0x" -> (fromString . drop 2 :: String -> Text) s
          _ -> case (readMay s :: Maybe Int) of -- can we get rid of this case now?
            Just i  -> encodeHex . S.encode . intToScriptOp $ i
            Nothing -> error $ "encodeSatoshiCoreScriptPubKey: " ++ s

--
-- Following tests depend on specific secp256k1 deterministic nonce computation.
-- Fedora distributes the Bitcoin ABC fork of this library, which computes nonces differently.
--

-- -- Reproduce the P2WPKH example from BIP 143
-- testBip143p2wpkh :: Assertion
-- testBip143p2wpkh = assertEqual "BIP143 p2wpkh" (Right exampleSignedTx) generatedSignedTx
--   where
--     exampleSignedTx = "01000000000102fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f00000000494830450221008b9d1dc26ba6a9cb62127b02742fa9d754cd3bebf337f7a55d114c8e5cdd30be022040529b194ba3f9281a99f2b1c0a19c0489bc22ede944ccf4ecbab4cc618ef3ed01eeffffffef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a0100000000ffffffff02202cb206000000001976a9148280b37df378db99f66f85c95a783a76ac7a6d5988ac9093510d000000001976a9143bde42dbee7e4dbe6a21b2d50ce2f0167faa815988ac000247304402203609e17b84f6a7d30c80bfa610b5b4542f32a8a0d5447a12fb1366d7f01cc44a0220573a954c4518331561406f90300e8f3358f51928d43c212a8caed02de67eebee0121025476c2e83188368da1ff3e292e7acafcdb3566bb0ad253f62fc70f07aeee635711000000"

--     unsignedTx = "0100000002fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f0000000000eeffffffef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a0100000000ffffffff02202cb206000000001976a9148280b37df378db99f66f85c95a783a76ac7a6d5988ac9093510d000000001976a9143bde42dbee7e4dbe6a21b2d50ce2f0167faa815988ac11000000"

--     Just key0 = secHexKey "bbc27228ddcb9209d7fd6f36b02f7dfa6252af40bb2f1cbc7a557da8027ff866"
--     pubKey0   = toPubKey key0

--     Just key1 = secHexKey "619c335025c7f4012e556c2a58b2506e30b8511b53ade95ea316fd8c3286feb9"

--     [op0, op1] = prevOutput <$> txIn unsignedTx

--     sigIn0 = SigInput (PayPK pubKey0) 625000000 op0 sigHashAll Nothing

--     WitnessPubKeyAddress h = pubKeyWitnessAddr $ toPubKey key1
--     sigIn1                 = SigInput (PayWitnessPKHash h) 600000000 op1 sigHashAll Nothing

--     generatedSignedTx = signTx btc unsignedTx [sigIn0, sigIn1] [key0, key1]

-- -- Reproduce the P2SH-P2WPKH example from BIP 143
-- testBip143p2shp2wpkh :: Assertion
-- testBip143p2shp2wpkh = assertEqual "BIP143 p2sh-p2wpkh" (Right exampleSignedTx) generatedSignedTx
--   where
--     exampleSignedTx = "01000000000101db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477010000001716001479091972186c449eb1ded22b78e40d009bdf0089feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac02473044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb012103ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a2687392040000"

--     unsignedTx = "0100000001db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a54770100000000feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac92040000"

--     Just key0 = secHexKey "eb696a065ef48a2192da5b28b694f87544b30fae8327c4510137a922f32c6dcf"

--     op0                    = prevOutput . head $ txIn unsignedTx
--     WitnessPubKeyAddress h = pubKeyWitnessAddr $ toPubKey key0
--     sigIn0                 = SigInput (PayWitnessPKHash h) 1000000000 op0 sigHashAll Nothing

--     generatedSignedTx = signNestedWitnessTx btc unsignedTx [sigIn0] [key0]

-- -- P2WSH multisig example (tested against bitcoin-core 0.19.0.1)
-- testP2WSHMulsig :: Assertion
-- testP2WSHMulsig = assertEqual "p2wsh multisig" (Right exampleSignedTx) generatedSignedTx
--   where
--     exampleSignedTx = "01000000000101d2e34df5d7ee565208eddd231548916b9b0e99f4f5071f896134a448c5fb07bf0100000000ffffffff01f0b9f505000000001976a9143d5a352cab583b12fbcb26d1269b4a2c951a33ad88ac0400483045022100fad4fedd2bb4c439c64637eb8e9150d9020a7212808b8dc0578d5ff5b4ad65fe0220714640f261b37eb3106310bf853f4b706e51436fb6b64c2ab00768814eb55b9801473044022100baff4e4ceea4022b9725a2e6f6d77997a554f858165b91ac8c16c9833008bee9021f5f70ebc3f8580dc0a5e96451e3697bdf1f1f5883944f0f33ab0cfb272354040169522102ba46d3bb8db74c77c6cf082db57fc0548058fcdea811549e186526e3d10caf6721038ac8aef2dd9cea5e7d66e2f6e23f177a6c21f69ea311fa0c85d81badb6b37ceb2103d96d2bfbbc040faaf93491d69e2bfe9695e2d8e007a7f26db96c2ee42db15dc953ae00000000"

--     unsignedTx = "0100000001d2e34df5d7ee565208eddd231548916b9b0e99f4f5071f896134a448c5fb07bf0100000000ffffffff01f0b9f505000000001976a9143d5a352cab583b12fbcb26d1269b4a2c951a33ad88ac00000000"

--     op0 = head $ prevOutput <$> txIn unsignedTx

--     Just keys = traverse secHexKey [ "3030303030303030303030303030303030303030303030303030303030303031"
--                                    , "3030303030303030303030303030303030303030303030303030303030303032"
--                                    , "3030303030303030303030303030303030303030303030303030303030303033"
--                                    ]

--     rdm               = PayMulSig (toPubKey <$> keys) 2
--     sigIn             = SigInput (toP2WSH $ encodeOutput rdm) 100000000 op0 sigHashAll (Just rdm)
--     generatedSignedTx = signTx btc unsignedTx [sigIn] (take 2 keys)

-- -- Reproduce the P2SH-P2WSH multisig example from BIP 143
-- testBip143p2shp2wpkhMulsig :: Assertion
-- testBip143p2shp2wpkhMulsig =
--     assertEqual "BIP143 p2sh-p2wsh multisig" (Right exampleSignedTx) generatedSignedTx
--   where
--     exampleSignedTx = "0100000000010136641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e0100000023220020a16b5755f7f6f96dbd65f5f0d6ab9418b89af4b1f14a1bb8a09062c35f0dcb54ffffffff0200e9a435000000001976a914389ffce9cd9ae88dcc0631e88a821ffdbe9bfe2688acc0832f05000000001976a9147480a33f950689af511e6e84c138dbbd3c3ee41588ac080047304402206ac44d672dac41f9b00e28f4df20c52eeb087207e8d758d76d92c6fab3b73e2b0220367750dbbe19290069cba53d096f44530e4f98acaa594810388cf7409a1870ce01473044022068c7946a43232757cbdf9176f009a928e1cd9a1a8c212f15c1e11ac9f2925d9002205b75f937ff2f9f3c1246e547e54f62e027f64eefa2695578cc6432cdabce271502473044022059ebf56d98010a932cf8ecfec54c48e6139ed6adb0728c09cbe1e4fa0915302e022007cd986c8fa870ff5d2b3a89139c9fe7e499259875357e20fcbb15571c76795403483045022100fbefd94bd0a488d50b79102b5dad4ab6ced30c4069f1eaa69a4b5a763414067e02203156c6a5c9cf88f91265f5a942e96213afae16d83321c8b31bb342142a14d16381483045022100a5263ea0553ba89221984bd7f0b13613db16e7a70c549a86de0cc0444141a407022005c360ef0ae5a5d4f9f2f87a56c1546cc8268cab08c73501d6b3be2e1e1a8a08824730440220525406a1482936d5a21888260dc165497a90a15669636d8edca6b9fe490d309c022032af0c646a34a44d1f4576bf6a4a74b67940f8faa84c7df9abe12a01a11e2b4783cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56ae00000000"

--     unsignedTx = "010000000136641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e0100000000ffffffff0200e9a435000000001976a914389ffce9cd9ae88dcc0631e88a821ffdbe9bfe2688acc0832f05000000001976a9147480a33f950689af511e6e84c138dbbd3c3ee41588ac00000000"

--     op0 = head $ prevOutput <$> txIn unsignedTx

--     rawKeys = [ "730fff80e1413068a05b57d6a58261f07551163369787f349438ea38ca80fac6"
--               , "11fa3d25a17cbc22b29c44a484ba552b5a53149d106d3d853e22fdd05a2d8bb3"
--               , "77bf4141a87d55bdd7f3cd0bdccf6e9e642935fec45f2f30047be7b799120661"
--               , "14af36970f5025ea3e8b5542c0f8ebe7763e674838d08808896b63c3351ffe49"
--               , "fe9a95c19eef81dde2b95c1284ef39be497d128e2aa46916fb02d552485e0323"
--               , "428a7aee9f0c2af0cd19af3cf1c78149951ea528726989b2e83e4778d2c3f890"
--               ]

--     Just keys@[key0, key1, key2, key3, key4, key5] = traverse secHexKey rawKeys

--     rdm      = PayMulSig (toPubKey <$> keys) 6
--     sigIn sh = SigInput (toP2WSH $ encodeOutput rdm) 987654321 op0 sh (Just rdm)

--     sigHashesA = [sigHashAll, sigHashNone, sigHashSingle]
--     sigHashesB = setAnyoneCanPayFlag <$> sigHashesA
--     sigIns     = sigIn <$> (sigHashesA <> sigHashesB)

--     generatedSignedTx      = foldM addSig unsignedTx $ zip sigIns keys
--     addSig tx (sigIn, key) = signNestedWitnessTx btc tx [sigIn] [key]

-- secHexKey :: Text -> Maybe SecKey
-- secHexKey = decodeHex >=> secKey

-- toPubKey :: SecKey -> PubKeyI
-- toPubKey = derivePubKeyI . wrapSecKey True

{- Building Transactions -}

testBuildAddrTx :: Network -> Address -> TestCoin -> Bool
testBuildAddrTx net a (TestCoin v)
    | isPubKeyAddress a = Right (PayPKHash (getAddrHash160 a)) == out
    | isScriptAddress a = Right (PayScriptHash (getAddrHash160 a)) == out
    | otherwise = undefined
  where
    tx = buildAddrTx net [] [(fromJust (addrToString net a), v)]
    out =
        decodeOutputBS $
        scriptOutput $
        head $ txOut (fromRight (error "Could not build transaction") tx)

testGuessSize :: Network -> Tx -> Bool
testGuessSize net tx
    -- We compute an upper bound but it should be close enough to the real size
    -- We give 2 bytes of slack on every signature (1 on r and 1 on s)
 = guess >= len && guess <= len + 2 * delta
  where
    delta = pki + sum (map fst msi)
    guess = guessTxSize pki msi pkout msout
    len = B.length $ S.encode tx
    ins = map f $ txIn tx
    f i =
        fromRight (error "Could not decode input") $
        decodeInputBS net $ scriptInput i
    pki = length $ filter isSpendPKHash ins
    msi = concatMap shData ins
    shData (ScriptHashInput _ (PayMulSig keys r)) = [(r, length keys)]
    shData _                                      = []
    out =
        map
            (fromRight (error "Could not decode transaction output") .
             decodeOutputBS . scriptOutput) $
        txOut tx
    pkout = length $ filter isPayPKHash out
    msout = length $ filter isPayScriptHash out

testChooseCoins :: [TestCoin] -> Word64 -> Word64 -> Int -> Property
testChooseCoins coins target byteFee nOut = nOut >= 0 ==>
    case chooseCoins target byteFee nOut True coins of
        Right (chosen, change) ->
            let outSum = sum $ map coinValue chosen
                fee    = guessTxFee byteFee nOut (length chosen)
            in outSum == target + change + fee
        Left _ ->
            let fee = guessTxFee byteFee nOut (length coins)
            in target == 0 || s < target + fee
  where
    s  = sum $ map coinValue coins

testChooseMSCoins :: (Int, Int) -> [TestCoin]
                  -> Word64 -> Word64 -> Int -> Property
testChooseMSCoins (m, n) coins target byteFee nOut = nOut >= 0 ==>
    case chooseMSCoins target byteFee (m,n) nOut True coins of
        Right (chosen,change) ->
            let outSum = sum $ map coinValue chosen
                fee    = guessMSTxFee byteFee (m,n) nOut (length chosen)
            in outSum == target + change + fee
        Left _ ->
            let fee = guessMSTxFee byteFee (m,n) nOut (length coins)
            in target == 0 || s < target + fee
  where
    s = sum $ map coinValue coins

{- Signing Transactions -}

testDetSignTx :: Network -> (Tx, [SigInput], [SecKeyI]) -> Bool
testDetSignTx net  (tx, sigis, prv) =
    not (verifyStdTx net tx verData) &&
    not (verifyStdTx net txSigP verData) && verifyStdTx net txSigC verData
  where
    txSigP =
        fromRight (error "Could not decode transaction") $
        signTx net tx sigis (map secKeyData (tail prv))
    txSigC =
        fromRight (error "Could not decode transaction") $
        signTx net txSigP sigis [secKeyData (head prv)]
    verData = map (\(SigInput s v o _ _) -> (s, v, o)) sigis

testDetSignNestedTx :: Network -> (Tx, [SigInput], [SecKeyI]) -> Bool
testDetSignNestedTx net  (tx, sigis, prv) =
    not (verifyStdTx net tx verData) &&
    not (verifyStdTx net txSigP verData) && verifyStdTx net txSigC verData
  where
    txSigP =
        fromRight (error "Could not decode transaction") $
        signNestedWitnessTx net tx sigis (secKeyData <$> tail prv)
    txSigC =
        fromRight (error "Could not decode transaction") $
        signNestedWitnessTx net txSigP sigis [secKeyData (head prv)]
    verData = handleSegwit <$> sigis
    handleSegwit (SigInput s v o _ _)
        | isSegwit s = (toP2SH $ encodeOutput s, v, o)
        | otherwise  = (s, v, o)

testMergeTx :: Network -> ([Tx], [(ScriptOutput, Word64, OutPoint, Int, Int)]) -> Bool
testMergeTx net (txs, os) = and
    [ isRight mergeRes
    , length (txIn mergedTx) == length os
    , if enoughSigs then isValid else not isValid
    -- Signature count == min (length txs) (sum required signatures)
    , sum (map snd sigMap) == min (length txs) (sum (map fst sigMap))
    ]
  where
    outs = map (\(so, val, op, _, _) -> (so, val, op)) os
    mergeRes = mergeTxs net txs outs
    mergedTx = fromRight (error "Could not merge") mergeRes
    isValid = verifyStdTx net mergedTx outs
    enoughSigs = all (\(m,c) -> c >= m) sigMap
    sigMap = map (\((_,_,_,m,_), inp) -> (m, sigCnt inp)) $ zip os $ txIn mergedTx
    sigCnt inp = case decodeInputBS net $ scriptInput inp of
        Right (RegularInput (SpendMulSig sigs)) -> length sigs
        Right (ScriptHashInput (SpendMulSig sigs) _) -> length sigs
        _ -> error "Invalid input script type"

testID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
testID x =
    (A.decode . A.encode) (singleton ("object" :: String) x) ==
    Just (singleton ("object" :: String) x)
