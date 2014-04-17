{-# LANGUAGE OverloadedStrings, Rank2Types #-}

import Control.Monad.Error
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Maybe
import Network.Haskoin.Crypto
import Network.Haskoin.JSONRPC
import Network.Haskoin.JSONRPC.Conduit
import Network.Haskoin.JSONRPC.Stratum
import System.Environment

app :: AppTCP Response Result StratumResponse
app ad = do
    addrs <- liftIO getArgs
    when (length addrs < 1) $ error "bitcoin addresses required"
    let as = map (fromJust . base58ToAddr) addrs

    vreq <- newReq (toRequest reqVer) (cb reqVer)
    hrqs <- mapM (\a -> newReq (toRequest $ reqHist a) (cb $ reqHist a)) as

    CL.sourceList (vreq:hrqs)
        $= CL.map (C.toStrict . flip C.append "\n" . encode)
        $$ (transPipe liftIO $ appSink ad)

    rs <- (transPipe liftIO $ appSource ad)
        $$ CB.lines
        =$ CL.isolate (length addrs + 1)
        =$ CL.mapM (p . decode' . C.fromStrict)
        =$ CL.consume -- decodeStrict has bug

    liftIO $ mapM_ print rs
  where
    reqVer = ReqVersion "Haskoin 0.0.1" "0.9"
    reqHist = ReqHistory
    cb = fromResponse
    gi s = case resID s of IntID i -> i; TxtID i -> read $ show i
    p (Just res) = recvRes gi res
    p _ = error "could not parse response"

main :: IO ()
main = runAppTCP (clientSettings 50001 "electrum.datemas.de") app
