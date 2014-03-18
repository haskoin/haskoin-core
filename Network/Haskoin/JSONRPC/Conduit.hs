{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.JSONRPC.Conduit
    ( msgSrc
    , msgSnk
    ) where

import qualified Data.ByteString.Lazy.Char8 as C

import Data.Conduit.Network
import Data.Conduit.Attoparsec
import Network.Haskoin.JSONRPC.Message

type AppStack = MonadIO m => Application (StateT Integer m)

msgSnk :: Monad m => Consumer C.ByteString m Message
msgSnk = sinkParser $ fromJSON <$> json

msgSrc :: Monad m => Message -> Producer m C.ByteString
msgSrc msg = sourceLbs $ (encode $ msg) `C.append` "\n"
