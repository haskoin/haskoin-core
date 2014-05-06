{-# LANGUAGE OverloadedStrings, Rank2Types #-}
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Data.Conduit (($$), (=$), ($$+), ($$+-))
import Data.Conduit.Binary (lines)
import Data.Conduit.List (consume)
import Data.Conduit.Network
import Data.Either (rights)
import Data.Foldable (forM_)
import Data.Maybe (fromJust)
import Network.Haskoin.Crypto (base58ToAddr)
import Network.Haskoin.JSONRPC.Conduit
import Network.Haskoin.JSONRPC.Stratum
import Network.Haskoin.Util (bsToHex, encode')
import Prelude hiding (lines)
import System.Environment (getArgs)

app :: AppData IO -> IO ()
app ad = do
    as <- map (fromJust . base58ToAddr) <$> getArgs
    s <- initSession
    _ <- forkIO $ reqSource s $$ appSink ad

    forM_ as $ newStratumReq s . ReqHistory
    (r, hs) <- appSource ad $$+ lines =$ resConduit s =$ consume

    let is = [txHash t | h <- rights hs, i <- rights [h], t <- resHistory i]

    forM_ is $ newStratumReq s . ReqTx
    xs <- r $$+- lines =$ resConduit s =$ consume

    let ts = [resTx t | h <- rights xs, t <- rights [h]]
        it = zip (map (bsToHex . encode') is) (map (bsToHex . encode') ts)
    
    forM_ it $ \x -> do
        putStrLn ("id: " ++ fst x)
        putStrLn ("tx: " ++ snd x)
        putStrLn ""

main :: IO ()
main = runTCPClient (clientSettings 50001 "electrum.chroot.eu") app
