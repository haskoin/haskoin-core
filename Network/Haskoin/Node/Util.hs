{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.Haskoin.Node.Util where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Database.Persist.TH 

import Network.Haskoin.Protocol

data NodeException
    = NodeException String
    deriving (Eq, Read, Show, Typeable)

instance Exception NodeException

data BlockChainException 
    = BlockChainException String
    deriving (Eq, Read, Show, Typeable)

instance Exception BlockChainException

