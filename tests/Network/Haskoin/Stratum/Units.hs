{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Stratum.Units (tests) where

import Control.Monad (liftM)
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Network.Haskoin.Stratum.Message
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit as HUnit

tests :: [Test]
tests =
    [ testFile "Decode JSON-RPC request"
      isRequest "tests/data/requests.json"
    , testFile "Decode JSON-RPC notification"
      isNotif "tests/data/notifications.json"
    , testFile "Decode JSON-RPC response"
      isResponse "tests/data/responses.json"
    , testFile "Decode JSON-RPC error"
      isError "tests/data/errors.json"
    , testFile "Decode invalid JSON-RPC"
      isInvalid "tests/data/invalid.json"
    ]

testFile :: String -> (Maybe MessageValue -> Bool) -> String -> Test
testFile label f file = buildTest $ do
    vectors <- liftM lines $ readFile file
    let test = g vectors
    return test
  where
    g vectors = testGroup label $ do
        (vector, count) <- zip vectors [0..]
        let msg = decode $ pack vector
            lbl = label ++ " " ++ show (count :: Int)
        return . testCase lbl . HUnit.assertBool (failure vector) $ f msg
    failure vector = "Failed to decode: " ++ vector

isRequest :: Maybe MessageValue -> Bool
isRequest (Just (MsgRequest (Request _ _ (Just _)))) = True
isRequest _ = False

isNotif :: Maybe MessageValue -> Bool
isNotif (Just (MsgRequest (Request _ _ Nothing))) = True
isNotif _ = False

isResponse :: Maybe MessageValue -> Bool
isResponse (Just (MsgResponse (Response (Right _) _))) = True
isResponse _ = False

isError :: Maybe MessageValue -> Bool
isError (Just (MsgResponse (Response (Left _) _))) = True
isError _ = False

isInvalid :: Maybe MessageValue -> Bool
isInvalid Nothing = True
isInvalid _ = False
