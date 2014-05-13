{-# LANGUAGE OverloadedStrings, Rank2Types #-}
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (($$), (=$))
import Data.Conduit.Binary (lines)
import Data.Conduit.List (consume, iterM, mapMaybeM, sinkNull)
import Data.Conduit.Network
import Data.Foldable (forM_)
import Data.Maybe (fromJust)
import Network.Haskoin.Crypto (base58ToAddr)
import Network.Haskoin.JSONRPC.Message
import Network.Haskoin.JSONRPC.Conduit
import Network.Haskoin.JSONRPC.Stratum
import Network.Haskoin.Util (bsToHex, encode')
import Prelude hiding (lines)
import System.Environment (getArgs)

app :: AppData IO -> IO ()
app ad = do
    as <- map (fromJust . base58ToAddr) <$> getArgs
    s <- initSession (Just parseNotif) :: IO StratumSession
    _ <- forkIO $ reqSource s $$ appSink ad
    forM_ as $ newStratumReq s . SubAddress
    _ <- appSource ad $$ lines
        =$ resConduit s
        =$ iterM (liftIO . print)
        =$ sinkNull
    return ()

main :: IO ()
main = runTCPClient (clientSettings 50001 "electrum.chroot.eu") app
