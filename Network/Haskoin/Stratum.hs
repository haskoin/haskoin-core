module Network.Haskoin.Stratum
( -- * Stratum
  -- ** Stratum Client
  StratumClient
, StratumSrc(..)
, getSrc
, sendReq
, runStratumTCP
  -- ** Bitcoin Data
, Balance(..)
, Coin(..)
, TxHeight(..)
  -- ** Stratum Data
, StratumNotif(..)
, StratumQuery(..)
, StratumResponse(..)
, StratumSession
  -- ** JSON-RPC Messages
, MsgStratum
, NotifStratum
, RequestStratum
, ResponseStratum
, ResultStratum
  -- ** Exceptions
, StratumException(..)
  -- ** Basic Functions
, toRequest
, parseResult
, parseNotif
, newStratumReq
) where

-- import Network.Haskoin.Stratum.Message
-- import Network.Haskoin.Stratum.Conduit
import Network.Haskoin.Stratum.Types
import Network.Haskoin.Stratum.Exceptions
import Network.Haskoin.Stratum.Client
