{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Stratum.Units (tests) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B8
import Data.Maybe

import Network.Haskoin.Network
import Network.Haskoin.Stratum
import Network.Haskoin.Util

import Network.JsonRpc

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HUnit

type Net = Prodnet

tests :: [Test]
tests =
    [ testGroup "JSON-RPC Static Files"
        [ testSingle "Decode JSON-RPC request"
            isRequest
            "tests/data/requests.json"
        , testSingle "Decode JSON-RPC notification"
            isNotif
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

testPair :: String
         -> (Maybe (Request (StratumRequest Net)) -> Maybe Value -> Bool)
         -> String -> String -> Test
testPair label f rF vF = buildTest $ do
    rs <- liftM lines $ readFile rF
    vs <- liftM lines $ readFile vF
    return $ testGroup label $ do
        (rS, vS, count) <- zip3 rs vs ([1..] :: [Int])
        let r = p rS
            t = f r (decodeStrict' $ B8.pack vS)
            l = label ++ " " ++ show count
        return $ testCase l $ HUnit.assertBool (fl vS) t
  where
    p :: String -> Maybe (Request (StratumRequest Net))
    p s = fromRight <$> (parseMaybe parseRequest =<< decodeStrict' (B8.pack s))
    fl = ("Failed to decode: " ++)

isRequest :: Maybe Value -> Bool
isRequest vM = case vM of
    Nothing -> False
    Just v -> case f v of
        Error _ -> False
        Success (Right _) -> True
        Success _ -> False
  where
    f :: Value -> Result (Either ErrorObj (Request (StratumRequest Net)))
    f v = parse parseRequest v

isNotif :: Maybe Value -> Bool
isNotif vM = case vM of
    Nothing -> False
    Just v -> case f v of
        Error _ -> False
        Success (Right _) -> True
        Success _ -> False
  where
    f :: Value -> Result (Either ErrorObj (Notif (StratumNotif Net)))
    f v = parse parseNotif v

isResponse :: Maybe (Request (StratumRequest Net)) -> Maybe Value -> Bool
isResponse rM vM = fromMaybe False $ do
    r <- rM
    v <- vM
    let resR :: Result (Either ErrorObj (Response (StratumResult Net)))
        resR = parse (parseResponse r) v
    return $ case resR of
        Success (Right _) -> True
        _ -> False

isError :: Maybe (Request (StratumRequest Net)) -> Maybe Value -> Bool
isError rM vM = fromMaybe False $ do
    r <- rM
    v <- vM
    let resR :: Result (Either ErrorObj (Response (StratumResult Net)))
        resR = parse (parseResponse r) v
    return $ case resR of
        Success (Left _) -> True
        _ -> False

