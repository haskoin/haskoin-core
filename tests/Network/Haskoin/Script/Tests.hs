module Network.Haskoin.Script.Tests 
( tests
, execScriptIO
, testValid
, testInvalid
, runTests
) where

import Test.QuickCheck.Property (Property, (==>))
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.Framework.Runners.Console (defaultMainWithArgs)

import qualified Test.HUnit as HUnit

import Control.Applicative ((<$>))
import Control.Monad (when)

import Numeric (readHex)

import qualified Data.Aeson as A (decode)
import Data.Bits (setBit, testBit)
import Text.Read (readMaybe)
import Data.List (isPrefixOf)
import Data.Char (ord)
import qualified Data.ByteString as BS
    ( singleton
    , length
    , tail
    , head
    , pack
    , empty
    , ByteString
    )

import qualified Data.ByteString.Lazy as LBS
    ( pack
    , unpack
    )

import Data.Maybe
    ( catMaybes
    , isNothing
    )

import Data.Binary
    ( Word8
    , encode
    , decode
    , decodeOrFail)

import qualified Data.ByteString.Lazy.Char8 as C (readFile)

import Data.Int (Int64)
import Data.Word (Word32)

import Network.Haskoin.Test.Script
import Network.Haskoin.Test.Transaction

import Network.Haskoin.Transaction.Types
import Network.Haskoin.Script
import Network.Haskoin.Script.Evaluator
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Script Parser"
        [ testProperty "decode . encode OP_1 .. OP_16" testScriptOpInt
        , testProperty "decode . encode ScriptOutput" testScriptOutput
        , testProperty "decode . encode ScriptInput" testScriptInput
        , testProperty "sorting MultiSig scripts" testSortMulSig
        ]
    , testGroup "Script SigHash"
        [ testProperty "canonical signatures" $
            \(ArbitraryTxSignature _ _ _ sig) -> testCanonicalSig sig
        , testProperty "canonical deterministic signatures" $
            \(ArbitraryDetTxSignature _ _ sig) -> testCanonicalSig sig
        , testProperty "decode SigHash from Word8" binSigHashByte
        , testProperty "encodeSigHash32 is 4 bytes long" testEncodeSH32
        , testProperty "decode . encode TxSignature" $
            \(ArbitraryTxSignature _ _ _ sig) -> binTxSig sig
        , testProperty "decode . encode deterministic TxSignature" $
            \(ArbitraryDetTxSignature _ _ sig) -> binTxSig sig
        , testProperty "decodeCanonical . encode TxSignature" $
            \(ArbitraryTxSignature _ _ _ sig) -> binTxSigCanonical sig
        , testProperty "decodeCanonical . encode deterministic TxSignature" $
            \(ArbitraryDetTxSignature _ _ sig) -> binTxSigCanonical sig
        , testProperty "Testing txSigHash with SigSingle" testSigHashOne
        ]
    , testGroup "Integer Types"
        [ testProperty "decodeInt . encodeInt Int"  testEncodeInt
        , testProperty "decodeBool . encodeBool Bool" testEncodeBool
        ]
    , testFile "Canonical Valid Script Test Cases"
               "tests/data/script_valid.json"
               True
    , testFile "Canonical Invalid Script Test Cases"
               "tests/data/script_invalid.json"
               False
    ]

{- Script Parser -}

testScriptOpInt :: ArbitraryIntScriptOp -> Bool
testScriptOpInt (ArbitraryIntScriptOp i) = 
    (intToScriptOp <$> scriptOpToInt i) == Right i

testScriptOutput :: ArbitraryScriptOutput -> Bool
testScriptOutput (ArbitraryScriptOutput so) = 
    decodeOutput (encodeOutput so) == Right so

testScriptInput :: ArbitraryScriptInput -> Bool
testScriptInput (ArbitraryScriptInput si) = 
    decodeInput (encodeInput si) == Right si

testSortMulSig :: ArbitraryMSOutput -> Bool
testSortMulSig (ArbitraryMSOutput out) = 
    snd $ foldl f (head pubs,True) $ tail pubs
  where 
    pubs = getOutputMulSigKeys $ sortMulSig out
    f (a,t) b | t && encode' a <= encode' b = (b,True)
              | otherwise                   = (b,False)

{- Script SigHash -}

testCanonicalSig :: TxSignature -> Bool
testCanonicalSig ts@(TxSignature _ sh)
    | isSigUnknown sh = isLeft $ decodeCanonicalSig bs
    | otherwise       = isRight (decodeCanonicalSig bs) && 
                        isCanonicalHalfOrder (txSignature ts)
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
    res = decode' $ BS.singleton w

testEncodeSH32 :: ArbitrarySigHash -> Bool
testEncodeSH32 (ArbitrarySigHash sh) = 
    BS.length bs == 4 && 
    BS.head bs == (BS.head $ encode' sh) && 
    BS.tail bs == BS.pack [0,0,0]
  where 
    bs = encodeSigHash32 sh

binTxSig :: TxSignature -> Bool
binTxSig ts = decodeSig (encodeSig ts) == Right ts

binTxSigCanonical :: TxSignature -> Bool
binTxSigCanonical ts@(TxSignature _ sh) 
    | isSigUnknown sh = isLeft $ decodeCanonicalSig $ encodeSig ts
    | otherwise = (fromRight $ decodeCanonicalSig $ encodeSig ts) == ts

testSigHashOne :: ArbitraryTx -> ArbitraryScript -> Bool -> Property
testSigHashOne (ArbitraryTx tx) (ArbitraryScript s) acp = not (null $ txIn tx) ==> 
    if length (txIn tx) > length (txOut tx) 
        then res == (setBit 0 248)
        else res /= (setBit 0 248)
    where res = txSigHash tx s (length (txIn tx) - 1) (SigSingle acp)

{- Script Evaluation Primitives -}

testEncodeInt :: Int64 -> Bool
testEncodeInt i 
    | i >  0x7fffffff = isNothing i'
    | i < -0x7fffffff = isNothing i'
    | otherwise       = i' == Just i
  where 
    i' = decodeInt $ encodeInt i

testEncodeBool :: Bool -> Bool
testEncodeBool b = decodeBool (encodeBool b) == b

{- Script Evaluation -}

rejectSignature :: SigCheck
rejectSignature _ _ _ = False

{- Parse tests from bitcoin-qt repository -}

type ParseError = String

parseHex' :: String -> Maybe [Word8]
parseHex' (a:b:xs) = case readHex $ [a, b] :: [(Integer, String)] of
                      [(i, "")] -> case parseHex' xs of
                                    Just ops -> Just $ fromIntegral i:ops
                                    Nothing -> Nothing
                      _ -> Nothing
parseHex' [_] = Nothing
parseHex' [] = Just []

parseScript :: String -> Either ParseError Script
parseScript scriptString =
      do bytes <- LBS.pack <$> parseBytes scriptString
         script <- decodeScript bytes
         when (encode script /= bytes) $
            Left "encode script /= bytes"
         when (decode (encode script) /= script) $
            Left "decode (encode script) /= script"
         return script
      where
          decodeScript bytes = case decodeOrFail bytes of
            Left (_, _, e) -> Left $ "decode error: " ++ e
            Right (_, _, Script s) -> Right $ Script s
          parseBytes :: String -> Either ParseError [Word8]
          parseBytes string = concat <$> mapM parseToken (words string)
          parseToken :: String -> Either ParseError [Word8]
          parseToken tok =
              case alternatives of
                    (ops:_) -> Right ops
                    _ -> Left $ "unknown token " ++ tok
              where alternatives :: [[Word8]]
                    alternatives = catMaybes  [ parseHex
                                              , parseInt
                                              , parseQuote
                                              , parseOp
                                              ]
                    parseHex | "0x" `isPrefixOf` tok = parseHex' (drop 2 tok)
                             | otherwise = Nothing
                    parseInt = fromInt . fromIntegral <$>
                               (readMaybe tok :: Maybe Integer)
                    parseQuote | tok == "''" = Just [0]
                               | (head tok) == '\'' && (last tok) == '\'' =
                                 Just $ encodeBytes $ opPushData $ BS.pack
                                      $ map (fromIntegral . ord)
                                      $ init . tail $ tok
                               | otherwise = Nothing
                    fromInt :: Int64 -> [Word8]
                    fromInt n | n ==  0 = [0x00]
                              | n == -1 = [0x4f]
                              | 1 <= n && n <= 16 = [0x50 + fromIntegral n]
                              | otherwise = encodeBytes
                                                $ opPushData $ BS.pack
                                                $ encodeInt n
                    parseOp = encodeBytes <$> (readMaybe $ "OP_" ++ tok)
                    encodeBytes = LBS.unpack . encode

testFile :: String -> String -> Bool -> Test
testFile groupLabel path expected = buildTest $ do
    dat <- C.readFile path
    case (A.decode dat) :: Maybe [[String]] of
        Nothing -> return $
                    testCase groupLabel $
                    HUnit.assertFailure $ "can't read test file " ++ path
        Just testDefs -> return $ testGroup groupLabel
                                $ map parseTest
                                $ filterPureComments testDefs

    where   parseTest :: [String] -> Test
            parseTest s = case testParts s of
                Nothing -> testCase "can't parse test case" $
                               HUnit.assertFailure $ "json element " ++ show s
                Just ( sig, pubKey, flags, label ) -> makeTest label sig pubKey

            makeTest :: String -> String -> String -> Test
            makeTest label sig pubKey =
                testCase label' $ case (parseScript sig, parseScript pubKey) of
                    (Left e, _) -> parseError $ "can't parse sig: " ++
                                                show sig ++ " error: " ++ e
                    (_, Left e) -> parseError $ "can't parse key: " ++
                                                show pubKey ++ " error: " ++ e
                    (Right scriptSig, Right scriptPubKey) ->
                        runTest scriptSig scriptPubKey

                where label' =  if null label
                                    then "sig: [" ++ sig ++ "] " ++
                                        " pubKey: [" ++ pubKey ++ "] "  
                                    else " label: " ++ label

            parseError message = HUnit.assertBool
                                ("parse error in valid script: " ++ message)
                                (expected == False)

            filterPureComments = filter ( not . null . tail )

            runTest scriptSig scriptPubKey =
                HUnit.assertBool
                  (" eval error: " ++ errorMessage)
                  (expected == scriptPairTestExec scriptSig scriptPubKey)

                where run f = f scriptSig scriptPubKey rejectSignature
                      errorMessage = case run execScript of
                        Left e -> show e
                        Right _ -> " none"

-- | Splits the JSON test into the different parts.  No processing,
-- just handling the fact that comments may not be there or might have
-- junk before it.  Output is the tuple ( sig, pubKey, flags, comment
-- ) as strings
testParts :: [ String ] -> Maybe ( String, String, String, String )
testParts l = let ( x, r ) = splitAt 3 l
                  comment = if null r then "" else last r
              in if length x < 3
                 then Nothing
                 else let ( sig:pubKey:flags:[] ) = x in
                      Just ( sig, pubKey, flags, comment )

-- repl utils

execScriptIO :: String -> String -> IO ()
execScriptIO sig key = case (parseScript sig, parseScript key) of
  (Left e, _) -> print $ "sig parse error: " ++ e
  (_, Left e) -> print $ "key parse error: " ++ e
  (Right scriptSig, Right scriptPubKey) ->
      case execScript scriptSig scriptPubKey rejectSignature of
          Left e -> putStrLn $ "error " ++ show e
          Right p -> do putStrLn $ "successful execution"
                        putStrLn $ dumpStack $ runStack p

testValid :: Test
testValid = testFile "Canonical Valid Script Test Cases"
            "tests/data/script_valid.json" True

testInvalid :: Test
testInvalid = testFile "Canonical Valid Script Test Cases"
              "tests/data/script_invalid.json" False

-- | Maximum value of sequence number
maxSeqNum :: Word32
maxSeqNum = 0xffffffff -- Perhaps this should be moved to constants.

-- | Null output used to create CoinbaseTx
nullOutPoint :: OutPoint
nullOutPoint = OutPoint { 
                 outPointHash  = 0
               , outPointIndex = -1
               }

-- | Some of the scripts tests require transactions be built in a
-- standard way.  This function builds the crediting transaction.
-- Quoting the top comment of script_valid.json: "It is evaluated as
-- if there was a crediting coinbase transaction with two 0 pushes as
-- scriptSig, and one output of 0 satoshi and given scriptPubKey,
-- followed by a spending transaction which spends this output as only
-- input (and correct prevout hash), using the given scriptSig. All
-- nLockTimes are 0, all nSequences are max."
buildCreditTx :: BS.ByteString -> Tx
buildCreditTx scriptPubKey = Tx { 
                 txVersion    = 1
               , txIn         = [ txI ]
               , txOut        = [ txO ]
               , txLockTime   = 0
               }
    where txO = TxOut {
                       outValue = 0
                     , scriptOutput = scriptPubKey
                     }
          txI = TxIn {
                        prevOutput = nullOutPoint
                      , scriptInput = encode' $ Script [ OP_0, OP_0 ]
                      , txInSequence = maxSeqNum
                      }

-- | Build a spending transaction for the tests.  Takes as input the
-- crediting transaction
buildSpendTx :: BS.ByteString  -- ScriptSig
             -> Tx     -- Creditting Tx
             -> Tx
buildSpendTx scriptSig creditTx = Tx { 
         txVersion  = 1
       , txIn       = [ txI ]
       , txOut      = [ txO ]
       , txLockTime = 0
       }
    where txI = TxIn { 
               prevOutput   = OutPoint { outPointHash = txHash creditTx , outPointIndex = 0 }
             , scriptInput  = scriptSig
             , txInSequence = maxSeqNum
             }
          txO = TxOut { outValue = 0, scriptOutput = BS.empty }

-- | Executes the test of a scriptSig, pubKeyScript pair, including
-- building the required transactions and verifying the spending
-- transaction.
scriptPairTestExec :: Script -- scriptSig 
                   -> Script -- pubKey
                   -> Bool
scriptPairTestExec scriptSig pubKey = 
    let bsScriptSig = encode' scriptSig
        bsPubKey = encode' pubKey
        spendTx = buildSpendTx bsScriptSig ( buildCreditTx bsPubKey )
    in verifySpend spendTx 0 pubKey

runTests :: [Test] -> IO ()
runTests ts = defaultMainWithArgs ts ["--hide-success"]
