{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (liftM)
import Data.Aeson (FromJSON, Value, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Network.Haskoin.JSONRPC
import Test.HUnit
import System.Exit (exitFailure)

-- Feed description string, list of documents to test, and test to perform.
testList :: FromJSON a => [String] -> (Maybe a -> Bool) -> [Test]
testList docs tst = do
    doc <- docs
    let decoded = decode $ pack doc
    return . TestCase . assertBool doc $ tst decoded

isReq :: Maybe (JSONMsg Value Value Value Value) -> Bool
isReq (Just (JSONMsgReq (JSONReq _ _ (Just _)))) = True
isReq _ = False

isNot :: Maybe (JSONMsg Value Value Value Value) -> Bool
isNot (Just (JSONMsgReq (JSONReq _ _ Nothing))) = True
isNot _ = False

isRes :: Maybe (JSONMsg Value Value Value Value) -> Bool
isRes (Just (JSONMsgRes (JSONRes (Right _) _))) = True
isRes _ = False

isErr :: Maybe (JSONMsg Value Value Value Value) -> Bool
isErr (Just (JSONMsgRes (JSONRes (Left _) _))) = True
isErr _ = False

isInv :: Maybe (JSONMsg Value Value Value Value) -> Bool
isInv Nothing = True
isInv _ = False

main :: IO ()
main  = do
    reqs <- liftM lines $ readFile "tests/data/requests.json"
    nots <- liftM lines $ readFile "tests/data/notifications.json"
    ress <- liftM lines $ readFile "tests/data/responses.json"
    errs <- liftM lines $ readFile "tests/data/errors.json"
    invs <- liftM lines $ readFile "tests/data/invalid.json"
    let tests =
            [ TestLabel "Request"      . TestList $ testList reqs isReq
            , TestLabel "Notification" . TestList $ testList nots isNot
            , TestLabel "Response"     . TestList $ testList ress isRes
            , TestLabel "Error"        . TestList $ testList errs isErr
            , TestLabel "Invalid"      . TestList $ testList invs isInv
            ]
    c <- runTestTT $ TestList tests
    if (failures c > 0) then exitFailure else return ()
