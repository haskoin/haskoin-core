{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Script.Tests
( tests
, execScriptIO
, testValid
, testInvalid
, runTests
) where

import           Control.Monad                        (when)
import qualified Data.Aeson                           as A
import           Data.Bits                            (testBit)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as C
import qualified Data.ByteString.Lazy.Char8           as CL
import           Data.Char                            (ord)
import           Data.Either                          (fromRight, isRight, isLeft)
import           Data.Int                             (Int64)
import           Data.List                            (isPrefixOf)
import           Data.List.Split                      (splitOn)
import           Data.Maybe                           (catMaybes, isNothing)
import           Data.Serialize                       (decode, encode)
import           Data.Word                            (Word32, Word8)
import           Network.Haskoin.Crypto
import           Network.Haskoin.Internals            (Flag, cltvDecodeInt,
                                                       decodeBool,
                                                       decodeFullInt, decodeInt,
                                                       dumpStack, encodeBool,
                                                       encodeInt, execScript,
                                                       runStack)
import           Network.Haskoin.Script
import           Network.Haskoin.Test
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Numeric                              (readHex)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Runners.Console       (defaultMainWithArgs)
import qualified Test.HUnit                           as HUnit
import           Test.QuickCheck                      (Property, forAll, (==>))
import           Text.Read                            (readMaybe)

tests :: [Test]
tests =
    [ testGroup
          "Script Parser"
          [ testProperty "decode . encode OP_1 .. OP_16" $
            forAll arbitraryIntScriptOp $ \i ->
                (intToScriptOp <$> scriptOpToInt i) == Right i
          , testProperty "decode . encode ScriptOutput" $
            forAll arbitraryScriptOutput $ \so ->
                decodeOutput (encodeOutput so) == Right so
          , testProperty "decode . encode ScriptInput" $
            forAll arbitraryScriptInput $ \si ->
                decodeInput (encodeInput si) == Right si
          , testProperty "sorting MultiSig scripts" $
            forAll arbitraryMSOutput testSortMulSig
          ]
    , testGroup
          "Script SigHash"
          [ testProperty "canonical signatures" $
            forAll arbitraryTxSignature $ testCanonicalSig . lst3
          , testProperty "decode SigHash from Word8" binSigHashByte
          , testProperty "encodeSigHash32 is 4 bytes long" $
            forAll arbitrarySigHash testEncodeSH32
          , testProperty "decode . encode TxSignature" $
            forAll arbitraryTxSignature $ binTxSig . lst3
          , testProperty "decodeCanonical . encode TxSignature" $
            forAll arbitraryTxSignature $ binTxSigCanonical . lst3
          , testProperty "Testing txSigHash with SigSingle" $
            forAll arbitraryTx $ forAll arbitraryScript . testSigHashOne
          ]
    , testGroup
          "Integer Types"
          [ testProperty "decodeInt . encodeInt Int" testEncodeInt
          , testProperty "decodeFullInt . encodeInt Int" testEncodeInt64
          , testProperty "cltvDecodeInt . encodeInt Int" testEncodeCltv
          , testProperty "decodeBool . encodeBool Bool" testEncodeBool
          ]
    , testFile
          "Canonical Valid Script Test Cases"
          "test/data/script_valid.json"
          True
    , testFile
          "Canonical Invalid Script Test Cases"
          "test/data/script_invalid.json"
          False
    ]

{- Script Parser -}

testSortMulSig :: ScriptOutput -> Bool
testSortMulSig out =
    snd $ foldl f (head pubs,True) $ tail pubs
  where
    pubs = getOutputMulSigKeys $ sortMulSig out
    f (a,t) b | t && encode a <= encode b = (b,True)
              | otherwise                 = (b,False)

{- Script SigHash -}

testCanonicalSig :: TxSignature -> Bool
testCanonicalSig ts@(TxSignature _ sh)
    | isSigUnknown sh = isLeft $ decodeCanonicalSig bs
    | otherwise =
        isRight (decodeCanonicalSig bs) && isCanonicalHalfOrder (txSignature ts)
  where
    bs = encodeSig ts

binSigHashByte :: Word8 -> Bool
binSigHashByte w
    | w == 0x01 = res == SigAll False
    | w == 0x02 = res == SigNone False
    | w == 0x03 = res == SigSingle False
    | w == 0x81 = res == SigAll True
    | w == 0x82 = res == SigNone True
    | w == 0x83 = res == SigSingle True
    | testBit w 7 = res == SigUnknown True w
    | otherwise = res == SigUnknown False w
  where
    res = either error id . decode $ BS.singleton w

testEncodeSH32 :: SigHash -> Bool
testEncodeSH32 sh =
    BS.length bs == 4 &&
    BS.head bs == BS.head (encode sh) &&
    BS.tail bs == BS.pack [0,0,0]
  where
    bs = encodeSigHash32 sh

binTxSig :: TxSignature -> Bool
binTxSig ts = decodeSig (encodeSig ts) == Right ts

binTxSigCanonical :: TxSignature -> Bool
binTxSigCanonical ts@(TxSignature _ sh)
    | isSigUnknown sh = isLeft $ decodeCanonicalSig $ encodeSig ts
    | otherwise =
        fromRight
            (error "Colud not decode sig")
            (decodeCanonicalSig $ encodeSig ts) ==
        ts

testSigHashOne :: Tx -> Script -> Bool -> Property
testSigHashOne tx s acp = not (null $ txIn tx) ==>
    if length (txIn tx) > length (txOut tx)
        then res == one
        else res /= one
  where
    res = txSigHash tx s (length (txIn tx) - 1) (SigSingle acp)
    one = "0100000000000000000000000000000000000000000000000000000000000000"

{- Script Evaluation Primitives -}

testEncodeInt :: Int64 -> Bool
testEncodeInt i
    | i >  0x7fffffff = isNothing i'
    | i < -0x7fffffff = isNothing i'
    | otherwise       = i' == Just i
  where
    i' = decodeInt $ encodeInt i

testEncodeCltv :: Int64 -> Bool
testEncodeCltv i
    -- As 'cltvEncodeInt' is just a wrapper for 'encodeInt',
    -- we use 'encodeInt' for encoding, to simultaneously
    -- test the handling of out-of-range integers by 'cltvDecodeInt'.
    | i < 0 || i > fromIntegral (maxBound :: Word32) =
        isNothing $ cltvDecodeInt (encodeInt i)
    | otherwise =
        cltvDecodeInt (encodeInt i) == Just (fromIntegral i)

testEncodeInt64 :: Int64 -> Bool
testEncodeInt64 i = decodeFullInt (encodeInt i) == Just i

testEncodeBool :: Bool -> Bool
testEncodeBool b = decodeBool (encodeBool b) == b

{- Script Evaluation -}

rejectSignature :: SigCheck
rejectSignature _ _ _ = False

{- Parse tests from bitcoin-qt repository -}

type ParseError = String

parseHex' :: String -> Maybe [Word8]
parseHex' (a:b:xs) =
    case readHex [a, b] :: [(Integer, String)] of
        [(i, "")] ->
            case parseHex' xs of
                Just ops -> Just $ fromIntegral i : ops
                Nothing  -> Nothing
        _ -> Nothing
parseHex' [_] = Nothing
parseHex' [] = Just []

parseFlags :: String -> [ Flag ]
parseFlags "" = []
parseFlags s  = map read . splitOn "," $ s

parseScript :: String -> Either ParseError Script
parseScript scriptString = do
    bytes <- BS.pack <$> parseBytes scriptString
    script <- decodeScript bytes
    when (encode script /= bytes) $ Left "encode script /= bytes"
    when
        (fromRight (error "Could not decode script") (decode (encode script)) /=
         script) $
        Left "decode (encode script) /= script"
    return script
  where
    decodeScript bytes =
        case decode bytes of
            Left e -> Left $ "decode error: " ++ e
            Right (Script s) -> Right $ Script s
    parseBytes :: String -> Either ParseError [Word8]
    parseBytes string = concat <$> mapM parseToken (words string)
    parseToken :: String -> Either ParseError [Word8]
    parseToken tok =
        case alternatives of
            (ops:_) -> Right ops
            _ -> Left $ "unknown token " ++ tok
      where
        alternatives :: [[Word8]]
        alternatives = catMaybes [parseHex, parseInt, parseQuote, parseOp]
        parseHex
            | "0x" `isPrefixOf` tok = parseHex' (drop 2 tok)
            | otherwise = Nothing
        parseInt = fromInt . fromIntegral <$> (readMaybe tok :: Maybe Integer)
        parseQuote
            | tok == "''" = Just [0]
            | head tok == '\'' && last tok == '\'' =
                Just $
                encodeBytes $
                opPushData $
                BS.pack $ map (fromIntegral . ord) $ init . tail $ tok
            | otherwise = Nothing
        fromInt :: Int64 -> [Word8]
        fromInt n
            | n == 0 = [0x00]
            | n == -1 = [0x4f]
            | 1 <= n && n <= 16 = [0x50 + fromIntegral n]
            | otherwise = encodeBytes $ opPushData $ BS.pack $ encodeInt n
        parseOp = encodeBytes <$> readMaybe ("OP_" ++ tok)
        encodeBytes = BS.unpack . encode

testFile :: String -> String -> Bool -> Test
testFile groupLabel path expected =
    buildTest $ do
        dat <- CL.readFile path
        case A.decode dat :: Maybe [[String]] of
            Nothing ->
                return $
                testCase groupLabel $
                HUnit.assertFailure $ "can't read test file " ++ path
            Just testDefs ->
                return $
                testGroup groupLabel $
                map parseTest $ filterPureComments testDefs
  where
    parseTest :: [String] -> Test
    parseTest s =
        case testParts s of
            Nothing ->
                testCase "can't parse test case" $
                HUnit.assertFailure $ "json element " ++ show s
            Just (sig, pubKey, flags, label) -> makeTest label sig pubKey flags
    makeTest :: String -> String -> String -> String -> Test
    makeTest label sig pubKey flags =
        testCase label' $
        case (parseScript sig, parseScript pubKey) of
            (Left e, _) ->
                parseError $ "can't parse sig: " ++ show sig ++ " error: " ++ e
            (_, Left e) ->
                parseError $
                "can't parse key: " ++ show pubKey ++ " error: " ++ e
            (Right scriptSig, Right scriptPubKey) ->
                runTest scriptSig scriptPubKey (parseFlags flags)
      where
        label' =
            if null label
                then "sig: [" ++ sig ++ "] " ++ " pubKey: [" ++ pubKey ++ "] "
                else " label: " ++ label
    parseError message =
        HUnit.assertBool
            ("parse error in valid script: " ++ message)
            (not expected)
    filterPureComments = filter (not . null . tail)
    runTest scriptSig scriptPubKey scriptFlags =
        HUnit.assertBool
            (" eval error: " ++ errorMessage)
            (expected == scriptPairTestExec scriptSig scriptPubKey scriptFlags)
      where
        run f = f scriptSig scriptPubKey rejectSignature scriptFlags
        errorMessage =
            case run execScript of
                Left e  -> show e
                Right _ -> " none"

-- | Splits the JSON test into the different parts.  No processing,
-- just handling the fact that comments may not be there or might have
-- junk before it.  Output is the tuple ( sig, pubKey, flags, comment
-- ) as strings
testParts :: [String] -> Maybe (String, String, String, String)
testParts l =
    let (x, r) = splitAt 3 l
        comment =
            if null r
                then ""
                else last r
    in if length x < 3
           then Nothing
           else let [sig, pubKey, flags] = x
                in Just (sig, pubKey, flags, comment)

-- repl utils

execScriptIO :: String -> String -> String -> IO ()
execScriptIO sig key flgs =
    case (parseScript sig, parseScript key) of
        (Left e, _) -> print $ "sig parse error: " ++ e
        (_, Left e) -> print $ "key parse error: " ++ e
        (Right scriptSig, Right scriptPubKey) ->
            case execScript
                     scriptSig
                     scriptPubKey
                     rejectSignature
                     (parseFlags flgs) of
                Left e -> putStrLn $ "error " ++ show e
                Right p -> do
                    putStrLn "successful execution"
                    C.putStrLn $ dumpStack $ runStack p

testValid :: Test
testValid = testFile "Canonical Valid Script Test Cases"
            "test/data/script_valid.json" True

testInvalid :: Test
testInvalid = testFile "Canonical Valid Script Test Cases"
              "test/data/script_invalid.json" False

-- | Maximum value of sequence number
maxSeqNum :: Word32
maxSeqNum = 0xffffffff -- Perhaps this should be moved to constants.

-- | Null output used to create CoinbaseTx
nullOutPoint :: OutPoint
nullOutPoint =
    OutPoint
    { outPointHash =
          "0000000000000000000000000000000000000000000000000000000000000000"
    , outPointIndex = maxBound
    }

-- | Some of the scripts tests require transactions be built in a
-- standard way.  This function builds the crediting transaction.
-- Quoting the top comment of script_valid.json: "It is evaluated as
-- if there was a crediting coinbase transaction with two 0 pushes as
-- scriptSig, and one output of 0 satoshi and given scriptPubKey,
-- followed by a spending transaction which spends this output as only
-- input (and correct prevout hash), using the given scriptSig. All
-- nLockTimes are 0, all nSequences are max."
buildCreditTx :: ByteString -> Tx
buildCreditTx scriptPubKey =
    createTx 1 [ txI ] [ txO ] 0
  where
    txO = TxOut { outValue = 0
                , scriptOutput = scriptPubKey
                }
    txI = TxIn { prevOutput = nullOutPoint
               , scriptInput = encode $ Script [ OP_0, OP_0 ]
               , txInSequence = maxSeqNum
               }

-- | Build a spending transaction for the tests.  Takes as input the
-- crediting transaction
buildSpendTx :: ByteString  -- ScriptSig
             -> Tx          -- Creditting Tx
             -> Tx
buildSpendTx scriptSig creditTx =
    createTx 1 [ txI ] [ txO ] 0
  where
    txI = TxIn { prevOutput = OutPoint { outPointHash = txHash creditTx
                                       , outPointIndex = 0
                                       }
               , scriptInput  = scriptSig
               , txInSequence = maxSeqNum
               }
    txO = TxOut { outValue = 0, scriptOutput = BS.empty }

-- | Executes the test of a scriptSig, pubKeyScript pair, including
-- building the required transactions and verifying the spending
-- transaction.
scriptPairTestExec :: Script    -- scriptSig
                   -> Script    -- pubKey
                   -> [ Flag ] -- Evaluation flags
                   -> Bool
scriptPairTestExec scriptSig pubKey flags =
    let bsScriptSig = encode scriptSig
        bsPubKey = encode pubKey
        spendTx = buildSpendTx bsScriptSig (buildCreditTx bsPubKey)
    in verifySpend spendTx 0 pubKey flags

runTests :: [Test] -> IO ()
runTests ts = defaultMainWithArgs ts ["--hide-success"]

