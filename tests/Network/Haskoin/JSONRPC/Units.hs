{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.JSONRPC.Units (tests) where

import Control.Monad (liftM)
import Data.Aeson (FromJSON, Value, decode)
import Data.Char (toLower)
import Control.Applicative ((<$>))
import Data.ByteString.Lazy.Char8 (pack)
import Network.Haskoin.JSONRPC.Message
import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Runners.Console (defaultMain)
import qualified Test.HUnit as HUnit
import System.Exit (exitFailure)

tests :: [Test]
tests =
    [ testFile "Request" isRequest "tests/data/requests.json"
    , testFile "Notification" isNotif "tests/data/notifications.json"
    , testFile "Response" isResponse "tests/data/responses.json"
    , testFile "Error" isError "tests/data/errors.json"
    , testFile "Invalid" isInvalid "tests/data/invalid.json"
    ]

testFile :: String -> (Maybe MessageValue -> Bool) -> String -> Test
testFile label f file = buildTest $ do
    vectors <- liftM lines $ readFile file
    let test = g vectors
    return test
  where
    g vectors = testGroup label $ do
        vector <- (decode . pack) <$> vectors
        return . testCase label . HUnit.assertBool failure $ f vector
    failure = "Could not decode as " ++ map toLower label ++ "."

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
