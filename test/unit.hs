{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson hiding (Error)
import Data.JSONRPC.Message
import Data.Maybe
import Text.Show.Pretty

test :: (ToJSON a, FromJSON a, Eq a) => a -> Bool
test o = (fromJust . decode $ encode o) == o

requests :: [Request]
requests =
    [ Request (Just 11) "get.crap" . Just $
        object ["one" .= (1 :: Int), "two" .= (2 :: Int)]
    , Notification "sort.crap" Nothing
    ]

responses :: [Response]
responses =
    [ Error (Just 12) (-32122) "Fucked" . Just $
        object ["text" .= ("computation screwed" :: String)]
    , Response (Just 11) $
        object ["crap" .= ("served" :: String)]
    , Error Nothing (-31133) "Fucked" Nothing
    ]

messages = BRequest requests
         : BResponse responses
         : map MRequest requests
        ++ map MResponse responses

showTell :: (ToJSON a, FromJSON a, Eq a, Show a) => [a] -> IO ()
showTell d = do
    putStrLn $ ppShow d
    mapM_ C.putStrLn $ map encode d
    putStr $ "Test: "
    print $ foldr (&&) True $ map test d

main :: IO ()
main = do
    showTell requests
    showTell responses
    showTell messages
