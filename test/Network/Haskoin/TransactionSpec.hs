{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.TransactionSpec (spec) where

import           Data.Aeson                  as A
import qualified Data.ByteString             as B
import           Data.Either
import           Data.Map.Strict             (singleton)
import           Data.Maybe
import           Data.Serialize              as S
import           Data.String                 (fromString)
import           Data.String.Conversions
import           Data.Text                   (Text)
import           Data.Word                   (Word32, Word64)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Script
import           Network.Haskoin.Test
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Safe                        (readMay)
import           Test.Hspec
import           Test.HUnit                  (Assertion, assertBool)
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
