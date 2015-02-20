{-# LANGUAGE TemplateHaskell #-}
module Network.Haskoin.Node.SpvWallet
( withSpvWallet
) where

import Control.Applicative ((<$>))
import Control.Monad ( when, unless, forM, forM_, foldM, forever, liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async (Async, withAsync)
import qualified Control.Monad.State as S (gets, modify)
import Control.Monad.Logger (logInfo, logWarn, logDebug, logError)

import qualified Data.Text as T (pack)
import Data.Maybe (isJust, isNothing, fromJust, catMaybes, fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (nub, partition, delete, (\\))
import Data.Conduit.TMChan (TBMChan, writeTBMChan)
import qualified Data.Map as M 
    ( Map, member, delete, lookup, fromList, fromListWith
    , keys, elems, toList, toAscList, empty, map, filter
    , adjust, update, singleton, unionWith
    )

import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction.Types
import Network.Haskoin.Node.Bloom
import Network.Haskoin.Node.Types
import Network.Haskoin.Node.Message
import Network.Haskoin.Node.PeerManager
import Network.Haskoin.Node.Peer

data WalletSession = WalletSession
    { -- Blockchain message channel
    , bkchChan :: !(TBMChan BlockChainMessage)
      -- Transactions that have not been sent in a merkle block.
      -- We stall solo transactions until the merkle blocks are synced.
    , soloTxs :: ![Tx]
      -- Inflight transaction requests for each peer. We are waiting for
      -- the GetData response. We stall merkle blocks if there are pending
      -- transaction downloads.
    , peerInflightTxs :: !(M.Map RemoteHost [(TxHash, Timestamp)])
    }

