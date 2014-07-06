{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Stratum.Units (tests) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.ByteString.Char8 as B8
import Network.Haskoin.Stratum.RPC
import Network.Haskoin.Stratum.Types
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit as HUnit

tests :: [Test]
tests =
    [ testSingle "Decode JSON-RPC request"
        isRequest "tests/data/requests.json"
    , testSingle "Decode JSON-RPC notification"
        isNotif "tests/data/notifications.json"
    , testPair "Decode JSON-RPC response"
        isResponse "tests/data/requests.json" "tests/data/responses.json"
    , testPair "Decode JSON-RPC error"
        isError "tests/data/error_requests.json" "tests/data/errors.json"
    ]

testSingle :: String -> (Value -> Bool) -> String -> Test
testSingle label f file = buildTest $ do
    vectors <- liftM lines $ readFile file
    let test = testGroup label $ do
        (vS, c) <- zip vectors ([1..] :: [Int])
        let v = fromJust $ decodeStrict' $ B8.pack vS
            l = label ++ " " ++ show c
        return $ testCase l $ HUnit.assertBool (failure vS) $ f v
    return test
  where
    failure vector = "Failed to decode: " ++ vector

testPair ::
    String -> (StratumRequest -> Value -> Bool) -> String -> String -> Test
testPair label f rF vF = buildTest $ do
    rs <- liftM lines $ readFile rF
    vs <- liftM lines $ readFile vF
    let test = testGroup label $ do
        (rS, vS, count) <- zip3 rs vs ([1..] :: [Int])
        let r = p rS
            v = fromJust $ decodeStrict' $ B8.pack vS
            l = label ++ " " ++ show count
        return $ testCase l $ HUnit.assertBool (failure vS) $ f r v
    return test
  where
    p :: String -> StratumRequest
    p = fromJust . parseMaybe parseRequest . fromJust . decodeStrict' . B8.pack
    failure = ("Failed to decode: " ++)

isRequest :: Value -> Bool
isRequest v = case (parse parseRequest v :: Result StratumRequest) of
    Error _ -> False
    Success _ -> True

isNotif :: Value -> Bool
isNotif v = case (parse parseNotif v :: Result StratumNotif) of
    Error _ -> False
    Success _ -> True

isResponse :: StratumRequest -> Value -> Bool
isResponse r v = case (parse (parseResponse r) v) of
    Success (Right _) -> True
    Success _ -> False
    Error _ -> False

isError :: StratumRequest -> Value -> Bool
isError r v = case (parse (parseResponse r) v) of
    Success (Left _) -> True
    Success _ -> False
    Error _ -> False
