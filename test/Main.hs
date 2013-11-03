{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as C

import Data.Maybe

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Aeson.Types
import Data.JSONRPC.Message

import Text.Show.Pretty

toJSONandBack :: (ToJSON a, FromJSON a, Eq a) => a -> Bool
toJSONandBack o = (fromJust . decode $ encode o) == o

jsonStrings :: [C.ByteString]
jsonStrings =
[ "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42, 23], \"id\": 1}"
, "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 1}"
, "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [23, 42], \"id\": 2}"
, "{\"jsonrpc\": \"2.0\", \"result\": -19, \"id\": 2}"
, "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": {\"subtrahend\": 23, \"minuend\": 42}, \"id\": 3}"
, "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 3}"
, "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": {\"minuend\": 42, \"subtrahend\": 23}, \"id\": 4}"
, "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 4}"
, "{\"jsonrpc\": \"2.0\", \"method\": \"update\", \"params\": [1,2,3,4,5]}"
, "{\"jsonrpc\": \"2.0\", \"method\": \"foobar\"}"
, "{\"jsonrpc\": \"2.0\", \"method\": \"foobar\", \"id\": \"1\"}"
, "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32601, \"message\": \"Method not found\"}, \"id\": \"1\"}"
, "{\"jsonrpc\": \"2.0\", \"method\": \"foobar, \"params\": \"bar\", \"baz]"
, "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}"
, "{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}"
, "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}"
, "[
{\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"},
{\"jsonrpc\": \"2.0\", \"method\"
]"
, "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}"
, "[]"
, "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}"
, "[1]"
, "[
{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}
]"
, "[1,2,3]"
, "[
{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}
]"
, "[
{\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"},
{\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", \"params\": [7]},
{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42,23], \"id\": \"2\"},
{\"foo\": \"boo\"},
{\"jsonrpc\": \"2.0\", \"method\": \"foo.get\", \"params\": {\"name\": \"myself\"}, \"id\": \"5\"},
{\"jsonrpc\": \"2.0\", \"method\": \"get_data\", \"id\": \"9\"} 
]"
, "[
{\"jsonrpc\": \"2.0\", \"result\": 7, \"id\": \"1\"},
{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": \"2\"},
{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null},
{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32601, \"message\": \"Method not found\"}, \"id\": \"5\"},
{\"jsonrpc\": \"2.0\", \"result\": [\"hello\", 5], \"id\": \"9\"}
]"
, "[
{\"jsonrpc\": \"2.0\", \"method\": \"notify_sum\", \"params\": [1,2,4]},
{\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", \"params\": [7]}
]"
]

messages :: [Message]
messages = map decode messagesJSON

showTell :: (ToJSON a, FromJSON a, Eq a, Show a) => [a] -> IO ()
showTell d = do
    putStrLn $ ppShow d
    mapM_ C.putStrLn $ map encode d
    putStr $ "Test: "
    print $ foldr (&&) True $ map test d

main :: IO ()
main = do
    showTell messages
