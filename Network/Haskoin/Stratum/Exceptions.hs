{-# LANGUAGE DeriveDataTypeable #-}
module Network.Haskoin.Stratum.Exceptions where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data StratumException
    = ParseException String
    | ParserNotFound String
    | ConnectException String
    | NoRequestsException String
    | NoIdException String
    | NoNumericIdException String
    | ErrorResponseException String
    deriving (Eq, Read, Show, Typeable)

instance Exception StratumException

