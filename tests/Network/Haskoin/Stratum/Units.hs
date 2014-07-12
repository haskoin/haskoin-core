{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Stratum.Units (tests) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B8
import Network.Haskoin.Stratum.RPC
import Network.Haskoin.Stratum.Types
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit as HUnit

tests :: [Test]
tests =
    [ testSingle "Decode JSON-RPC request"
        (\x -> isRequest x && not (isNotif x))
        "tests/data/requests.json"
    , testSingle "Decode JSON-RPC notification"
        (\x -> isNotif x && not (isRequest x))
        "tests/data/notifications.json"
    , testSingle "Decode invalid JSON-RPC request or notification"
        (\x -> not (isRequest x) && not (isNotif x))
        "tests/data/invalid.json"
    , testPair "Decode JSON-RPC response"
        isResponse
        "tests/data/requests.json"
        "tests/data/responses.json"
    , testPair "Decode JSON-RPC error"
        isError
        "tests/data/requests.json"
        "tests/data/errors.json"
    , testPair "Decode invalid JSON-RPC response"
        (\r x -> not (isResponse r x) && not (isError r x))
        "tests/data/requests.json"
        "tests/data/invalid.json"
    , testPair "Decode invalid JSON-RPC error"
        (\r x -> not (isResponse r x) && not (isError r x))
        "tests/data/requests.json"
        "tests/data/invalid.json"
    ]

testSingle :: String -> (Maybe Value -> Bool) -> String -> Test
testSingle label f file = buildTest $ do
    vectors <- liftM lines $ readFile file
    return $ testGroup label $ do
        (vS, c) <- zip vectors ([1..] :: [Int])
        let t = f $ decodeStrict' (B8.pack vS)
            l = label ++ " " ++ show c
        return $ testCase l $ HUnit.assertBool (fl vS) t
  where
    fl = ("Failed to decode: " ++)

testPair :: String -> (Maybe StratumRequest -> Maybe Value -> Bool)
         -> String -> String -> Test
testPair label f rF vF = buildTest $ do
    rs <- liftM lines $ readFile rF
    vs <- liftM lines $ readFile vF
    return $ testGroup label $ do
        (rS, vS, count) <- zip3 rs vs ([1..] :: [Int])
        let r = p rS
            t = f r $ decodeStrict' (B8.pack vS)
            l = label ++ " " ++ show count
        return $ testCase l $ HUnit.assertBool (fl vS) t
  where
    p :: String -> Maybe StratumRequest
    p s = fst <$> (parseMaybe parseRPCRequest =<< decodeStrict' (B8.pack s))
    fl = ("Failed to decode: " ++)

isRequest :: Maybe Value -> Bool
isRequest vM = case vM of
    Nothing -> False
    Just v -> case f v of
        Error   _ -> False
        Success _ -> True
  where
    f :: Value -> Result StratumRequest
    f v = fst <$> parse parseRPCRequest v

isNotif :: Maybe Value -> Bool
isNotif vM = case vM of
    Nothing -> False
    Just v -> case f v of
        Error   _ -> False
        Success _ -> True
  where
    f :: Value -> Result StratumNotif
    f = parse parseRPCNotif

isResponse :: Maybe StratumRequest -> Maybe Value -> Bool
isResponse rM vM = maybe False id $ do
    r <- rM
    v <- vM
    return $ case (fst <$> parse (parseRPCResponse r) v) of
        Success (Right _) -> True
        Success _ -> False
        Error   _ -> False

isError :: Maybe StratumRequest -> Maybe Value -> Bool
isError rM vM = maybe False id $ do
    r <- rM
    v <- vM
    return $ case (fst <$> parse (parseRPCResponse r) v) of
        Success (Left _) -> True
        Success _ -> False
        Error   _ -> False
