{-# LANGUAGE OverloadedStrings, Rank2Types #-}
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Data.Aeson.Types (Value)
import Data.ByteString (ByteString)
import Data.Conduit (Conduit, ($$), (=$), ($$+), ($$+-))
import Data.Conduit.Binary (lines)
import Data.Conduit.List (consume)
import Data.Conduit.Network
import Data.Either (rights)
import Data.Foldable (forM_)
import Data.Maybe (fromJust)
import Network.Haskoin.Crypto (base58ToAddr)
import Network.Haskoin.JSONRPC
import Network.Haskoin.JSONRPC.Conduit
import Network.Haskoin.JSONRPC.Stratum
import Prelude hiding (lines)
import System.Environment (getArgs)

type R = JSONReq StratumReq
type S = Session R StratumRes
type D = Either String (JSONRes StratumRes Value String)
type C = Conduit ByteString IO D

app :: AppData IO -> IO ()
app ad = do
    addrs <- map (fromJust . base58ToAddr) <$> getArgs
    s <- initSession :: IO S
    forM_ addrs $ f s . ReqHistory
    _ <- forkIO $ reqSource s $$ appSink ad
    (r, resHs) <- appSource ad $$+ lines =$ (resConduit s :: C) =$ consume
    let txIds = [ txHash t | h <- rights resHs, i <- rights [resResult h], t <- resHistory i ]
    -- let txIds = map txHash
    --           . concat . map resHistory
    --           . rights . map resResult
    --           . rights $ resHs
    forM_ txIds $ f s . ReqTx
    resTxs <- r $$+- lines =$ (resConduit s :: C) =$ consume
    mapM_ print resTxs
  where
    f s r = newReq s (toJSONReq r) (parseResult r)

main :: IO ()
main = runTCPClient (clientSettings 50001 "electrum.chroot.eu") app
