{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.JSONRPC.Types
( HeightHash(..)
) where

import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import Data.Text.Read (hexadecimal)
import Network.Haskoin.Crypto
import Network.Haskoin.Util

data HeightHash = HeightHash
    { txHeight :: Integer
    , txHash :: Hash256
    } deriving (Eq, Show)

instance FromJSON HeightHash where
    parseJSON (Object v) = do
        h <- v .: "height"
        t <- v .: "tx_hash"
        return $ HeightHash h t
    parseJSON _ = mzero

instance ToJSON HeightHash where
    toJSON (HeightHash h t) = object
        [ "height" .= h
        , "tx_hash" .= t
        ]

instance FromJSON Hash256 where
    parseJSON (String t) =
        return . fromInteger . fst . fromRight $ hexadecimal t
    parseJSON _ = mzero

instance ToJSON Hash256 where
    toJSON = toJSON . bsToHex . integerToBS . fromIntegral

instance FromJSON Address where
    parseJSON (String a) = do
        let addrS = T.unpack a
            addrM = base58ToAddr addrS
        case addrM of
            Nothing -> fail $ "Not a bitcoin address: " ++ addrS
            Just addr -> return addr
    parseJSON _ = mzero

instance ToJSON Address where
    toJSON = String . T.pack . addrToBase58
