{-# LANGUAGE OverloadedStrings #-}
import Data.JSONRPC.Types
import Data.Maybe
import Data.Aeson
import Test.HUnit
import System.Exit (exitFailure)
import qualified Data.Vector                    as V
import qualified Data.ByteString.Lazy.Char8     as C
import qualified Data.HashMap.Strict            as H

documents :: [Maybe Document]
documents = map decode jsonStrings

encoding :: Test
encoding = TestLabel "Encoding" $
    TestList [ o ~=? (decode . encode $ fromJust o) | o <- objects, isJust o ]

decoding :: Test
decoding = TestLabel "Decoding" $
    TestList [ o ~=? d | (o, d) <- zip objects documents ]

main :: IO ()
main  = do
    Counts _ _ e f <- runTestTT . TestList $ [decoding, encoding]
    if (e > 0 || f > 0)
        then
            exitFailure
        else
            return ()

jsonStrings :: [C.ByteString]
jsonStrings = map C.pack
    [ "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", "
      ++ "\"params\": [42, 23], \"id\": 1}"
    , "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 1}"
    , "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", "
      ++ "\"params\": [23, 42], \"id\": 2}"
    , "{\"jsonrpc\": \"2.0\", \"result\": -19, \"id\": 2}"
    , "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", "
      ++ "\"params\": {\"subtrahend\": 23, \"minuend\": 42}, \"id\": 3}"
    , "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 3}"
    , "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", "
      ++ "\"params\": {\"minuend\": 42, \"subtrahend\": 23}, \"id\": 4}"
    , "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 4}"
    , "{\"jsonrpc\": \"2.0\", \"method\": \"update\", \"params\": [1,2,3,4,5]}"
    , "{\"jsonrpc\": \"2.0\", \"method\": \"foobar\"}"
    , "{\"jsonrpc\": \"2.0\", \"method\": \"foobar\", \"id\": \"1\"}"
    , "{\"jsonrpc\": \"2.0\", \"error\": "
      ++ "{\"code\": -32601, \"message\": \"Method not found\"}, \"id\": \"1\"}"
    , "{\"jsonrpc\": \"2.0\", \"method\": \"foobar, \"params\": \"bar\", \"baz]"
    , "{\"jsonrpc\": \"2.0\", \"error\": "
      ++ "{\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}"
    , "{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}"
    , "{\"jsonrpc\": \"2.0\", \"error\": "
      ++ "{\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}"
    , unlines
        [ "["
        , "{\"jsonrpc\": \"2.0\", \"method\": \"sum\", "
          ++ "\"params\": [1,2,4], \"id\": \"1\"},"
        , "{\"jsonrpc\": \"2.0\", \"method\""
        , "]"
        ]
    , "{\"jsonrpc\": \"2.0\", \"error\": "
      ++ "{\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}"
    , "[]"
    , "{\"jsonrpc\": \"2.0\", \"error\": "
      ++ "{\"code\": -32600, \"message\": \"Invalid Request\"}, \"id\": null}"
    , "[1]"
    , unlines
        [ "["
        , "{\"jsonrpc\": \"2.0\", \"error\": "
          ++ "{\"code\": -32600, \"message\": \"Invalid Request\"}, "
          ++ "\"id\": null}"
        , "]"
        ]
    , "[1,2,3]"
    , unlines
        [ "["
        , "{\"jsonrpc\": \"2.0\", \"error\": "
          ++ "{\"code\": -32600, \"message\": \"Invalid Request\"}, "
          ++ "\"id\": null},"
        , "{\"jsonrpc\": \"2.0\", \"error\": "
          ++ "{\"code\": -32600, \"message\": \"Invalid Request\"}, "
          ++ "\"id\": null},"
        , "{\"jsonrpc\": \"2.0\", \"error\": "
          ++ "{\"code\": -32600, \"message\": \"Invalid Request\"}, "
          ++ "\"id\": null}"
        , "]"
        ]
    , unlines
        [ "["
        , "{\"jsonrpc\": \"2.0\", \"method\": \"sum\", "
          ++ "\"params\": [1,2,4], \"id\": \"1\"},"
        , "{\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", "
          ++ "\"params\": [7]},"
        , "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", "
          ++ "\"params\": [42,23], \"id\": \"2\"},"
        , "{\"foo\": \"boo\"},"
        , "{\"jsonrpc\": \"2.0\", \"method\": \"foo.get\", "
          ++ "\"params\": {\"name\": \"myself\"}, \"id\": \"5\"},"
        , "{\"jsonrpc\": \"2.0\", \"method\": \"get_data\", \"id\": \"9\"} "
        , "]"
        ]
    , unlines
        [ "["
        , "{\"jsonrpc\": \"2.0\", \"result\": 7, \"id\": \"1\"},"
        , "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": \"2\"},"
        , "{\"jsonrpc\": \"2.0\", \"error\": "
          ++ "{\"code\": -32600, \"message\": \"Invalid Request\"}, "
          ++ "\"id\": null},"
        , "{\"jsonrpc\": \"2.0\", \"error\": "
          ++ "{\"code\": -32601, \"message\": \"Method not found\"}, "
          ++ "\"id\": \"5\"},"
        , "{\"jsonrpc\": \"2.0\", \"result\": [\"hello\", 5], \"id\": \"9\"}"
        , "]"
        ]
    , unlines
        [ "["
        , "{\"jsonrpc\": \"2.0\", \"method\": \"notify_sum\", "
          ++ "\"params\": [1,2,4]},"
        , "{\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", "
          ++ "\"params\": [7]}"
        , "]"
        ]
    ]

objects :: [Maybe Document]
objects =
    [ Just
      (Single
         (Right
            (MRequest
               Request
                 { requestID = IntID 1
                 , requestMethod = "subtract"
                 , requestParams =
                     Just (paramsArray [ Number 42 , Number 23 ])
                 })))
    , Just
      (Single
         (Right
            (MResponse
               Response { responseID = IntID 1 , responseResult = Number 19 })))
    , Just
      (Single
         (Right
            (MRequest
               Request
                 { requestID = IntID 2
                 , requestMethod = "subtract"
                 , requestParams =
                     Just (paramsArray [ Number 23 , Number 42 ])
                 })))
    , Just
      (Single
         (Right
            (MResponse
               Response
                 { responseID = IntID 2
                 , responseResult = Number (-19)
                 })))
    , Just
      (Single
         (Right
            (MRequest
               Request
                 { requestID = IntID 3
                 , requestMethod = "subtract"
                 , requestParams =
                     Just
                       (paramsObject
                          [ ( "subtrahend" .= Number 23 )
                          , ( "minuend" .= Number 42 ) ])
                 })))
    , Just
      (Single
         (Right
            (MResponse
               Response { responseID = IntID 3 , responseResult = Number 19 })))
    , Just
      (Single
         (Right
            (MRequest
               Request
                 { requestID = IntID 4
                 , requestMethod = "subtract"
                 , requestParams =
                     Just
                       (paramsObject
                          [ ( "subtrahend" .= Number 23 )
                          , ( "minuend" .= Number 42 )
                          ])
                 })))
    , Just
      (Single
         (Right
            (MResponse
               Response { responseID = IntID 4 , responseResult = Number 19 })))
    , Just
      (Single
         (Right
            (MRequest
               Notification
                 { requestMethod = "update"
                 , requestParams =
                     Just
                       (paramsArray
                             [ Number 1
                             , Number 2
                             , Number 3
                             , Number 4
                             , Number 5
                             ])
                 })))
    , Just
      (Single
         (Right
            (MRequest
               Notification
                 { requestMethod = "foobar" , requestParams = Nothing })))
    , Just
      (Single
         (Right
            (MRequest
               Request
                 { requestID = TextID "1"
                 , requestMethod = "foobar"
                 , requestParams = Nothing
                 })))
    , Just
      (Single
         (Right
            (MResponse
               ErrorResponse
                 { responseID = TextID "1"
                 , errorCode = -32601
                 , errorMessage = "Method not found"
                 , errorData = Nothing
                 })))
    , Nothing
    , Just
      (Single
         (Right
            (MResponse
               ErrorResponse
                 { responseID = NullID
                 , errorCode = -32700
                 , errorMessage = "Parse error"
                 , errorData = Nothing
                 })))
    , Just
      (Single
         (Left
            (Object $
               H.fromList
               [ ( "jsonrpc" , String "2.0" )
               , ( "params" , String "bar" )
               , ( "method" , Number 1 )
               ])))
    , Just
      (Single
         (Right
            (MResponse
               ErrorResponse
                 { responseID = NullID
                 , errorCode = -32600
                 , errorMessage = "Invalid Request"
                 , errorData = Nothing
                 })))
    , Nothing
    , Just
      (Single
         (Right
            (MResponse
               ErrorResponse
                 { responseID = NullID
                 , errorCode = -32700
                 , errorMessage = "Parse error"
                 , errorData = Nothing
                 })))
    , Just (Single (Left (Array (V.fromList []))))
    , Just
      (Single
         (Right
            (MResponse
               ErrorResponse
                 { responseID = NullID
                 , errorCode = -32600
                 , errorMessage = "Invalid Request"
                 , errorData = Nothing
                 })))
    , Just (Batch [ Left (Number 1) ])
    , Just
      (Batch
         [ Right
             (MResponse
                ErrorResponse
                  { responseID = NullID
                  , errorCode = -32600
                  , errorMessage = "Invalid Request"
                  , errorData = Nothing
                  })
         ])
    , Just
      (Batch [ Left (Number 1) , Left (Number 2) , Left (Number 3) ])
    , Just
      (Batch
         [ Right
             (MResponse
                ErrorResponse
                  { responseID = NullID
                  , errorCode = -32600
                  , errorMessage = "Invalid Request"
                  , errorData = Nothing
                  })
         , Right
             (MResponse
                ErrorResponse
                  { responseID = NullID
                  , errorCode = -32600
                  , errorMessage = "Invalid Request"
                  , errorData = Nothing
                  })
         , Right
             (MResponse
                ErrorResponse
                  { responseID = NullID
                  , errorCode = -32600
                  , errorMessage = "Invalid Request"
                  , errorData = Nothing
                  })
         ])
    , Just
      (Batch
         [ Right
             (MRequest
                Request
                  { requestID = TextID "1"
                  , requestMethod = "sum"
                  , requestParams =
                      Just (paramsArray [ Number 1 , Number 2 , Number 4 ])
                  })
         , Right
             (MRequest
                Notification
                  { requestMethod = "notify_hello"
                  , requestParams = Just (paramsArray [ Number 7 ])
                  })
         , Right
             (MRequest
                Request
                  { requestID = TextID "2"
                  , requestMethod = "subtract"
                  , requestParams =
                      Just (paramsArray [ Number 42 , Number 23 ])
                  })
         , Left (Object $ H.fromList [ ( "foo" , String "boo" ) ])
         , Right
             (MRequest
                Request
                  { requestID = TextID "5"
                  , requestMethod = "foo.get"
                  , requestParams =
                      Just (paramsObject [ ( "name", String "myself" ) ])
                  })
         , Right
             (MRequest
                Request
                  { requestID = TextID "9"
                  , requestMethod = "get_data"
                  , requestParams = Nothing
                  })
         ])
    , Just
      (Batch
         [ Right
             (MResponse
                Response { responseID = TextID "1"
                         , responseResult = Number 7 })
         , Right
             (MResponse
                Response { responseID = TextID "2"
                         , responseResult = Number 19 })
         , Right
             (MResponse
                ErrorResponse
                  { responseID = NullID
                  , errorCode = -32600
                  , errorMessage = "Invalid Request"
                  , errorData = Nothing
                  })
         , Right
             (MResponse
                ErrorResponse
                  { responseID = TextID "5"
                  , errorCode = -32601
                  , errorMessage = "Method not found"
                  , errorData = Nothing
                  })
         , Right
             (MResponse
                Response
                  { responseID = TextID "9"
                  , responseResult =
                      Array(V.fromList [ String "hello" , Number 5 ])
                  })
         ])
    , Just
      (Batch
         [ Right
             (MRequest
                Notification
                  { requestMethod = "notify_sum"
                  , requestParams =
                      Just (paramsArray [ Number 1 , Number 2 , Number 4 ])
                  })
         , Right
             (MRequest
                Notification
                  { requestMethod = "notify_hello"
                  , requestParams = Just (paramsArray [ Number 7 ])
                  })
         ])
    ]
