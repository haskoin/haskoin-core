{-# LANGUAGE OverloadedStrings, Rank2Types #-}
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Data.Conduit (($$), (=$))
import Data.Conduit.Binary (lines)
import Data.Conduit.List (consume, mapMaybeM)
import Data.Conduit.Network
import Data.Foldable (forM_)
import Data.Maybe (fromJust)
import Network.Haskoin.Crypto (base58ToAddr)
import Network.Haskoin.JSONRPC
import Network.Haskoin.JSONRPC.Conduit
import Network.Haskoin.JSONRPC.Stratum
import Network.Haskoin.Util (bsToHex, encode')
import Prelude hiding (lines)
import System.Environment (getArgs)

app :: AppData IO -> IO ()
app ad = do
    as <- map (fromJust . base58ToAddr) <$> getArgs
    s <- initSession Nothing :: IO StratumSession
    _ <- forkIO $ reqSource s $$ appSink ad
    forM_ as $ newStratumReq s . QueryHistory
    hs <- appSource ad
        $$ lines
        =$ resConduit s
        =$ mapMaybeM (f s)
        =$ consume
    mapM_ (putStrLn . bsToHex . encode') hs
  where
    f s (Right (MsgResponse (Response (Right r) _))) = g s r
    f _ _ = return Nothing
    g s (AddressHistory hs) = do
        forM_ hs (newStratumReq s . QueryTx . txHash)
        return Nothing
    g _ (Transaction tx) = return $ Just tx
    g _ _ = error "Unexpected response."

main :: IO ()
main = runTCPClient (clientSettings 50001 "electrum.chroot.eu") app
