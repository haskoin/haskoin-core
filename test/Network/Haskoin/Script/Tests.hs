{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Script.Tests
( spec
) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                  as A
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy.Char8  as CL
import           Data.Char                   (ord)
import           Data.Either                 (fromRight)
import           Data.Int                    (Int64)
import           Data.List                   (isPrefixOf)
import           Data.List.Split             (splitOn)
import           Data.Maybe                  (catMaybes, isNothing)
import           Data.Serialize
import           Data.Word
import           Network.Haskoin.Constants
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Numeric                     (readHex)
import           Test.Hspec
import           Test.HUnit                  as HUnit
import           Test.QuickCheck
import           Text.Read                   (readMaybe)

spec :: Spec
spec = do
    describe "integer types" $ do
        it "decodeInt . encodeInt Int" $ property testEncodeInt
        it "decodeFullInt . encodeInt Int" $ property testEncodeInt64
        it "cltvDecodeInt . encodeInt Int" $ property testEncodeCltv
        it "decodeBool . encodeBool Bool" $ property testEncodeBool
    describe "script file tests" $ do
        it "runs all canonical valid scripts" $
            testFile "data/script_valid.json" True
        it "runs all canonical invalid scripts" $
            testFile "data/script_invalid.json" False

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
            Left e           -> Left $ "decode error: " ++ e
            Right (Script s) -> Right $ Script s
    parseBytes :: String -> Either ParseError [Word8]
    parseBytes string = concat <$> mapM parseToken (words string)
    parseToken :: String -> Either ParseError [Word8]
    parseToken tok =
        case alternatives of
            (ops:_) -> Right ops
            _       -> Left $ "unknown token " ++ tok
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

testFile :: String -> Bool -> Assertion
testFile path expected =
    do
        dat <- liftIO $ CL.readFile path
        case A.decode dat :: Maybe [[String]] of
            Nothing -> assertFailure $ "can't read test file " ++ path
            Just testDefs ->
                mapM_ parseTest $ filterPureComments testDefs
  where
    parseTest s =
        case testParts s of
            Nothing -> assertFailure $ "json element " ++ show s
            Just (sig, pubKey, flags, l) -> makeTest l sig pubKey flags
    makeTest l sig pubKey flags =
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
            if null l
                then "sig: [" ++ sig ++ "] " ++ " pubKey: [" ++ pubKey ++ "] "
                else " label: " ++ l
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

-- | Maximum value of sequence number
maxSeqNum :: Word32
maxSeqNum = 0xffffffff -- Perhaps this should be moved to constants.

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
    Tx 1 [ txI ] [ txO ] [] 0
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
    Tx 1 [ txI ] [ txO ] [] 0
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
    in verifySpend btc spendTx 0 pubKey 0 flags
