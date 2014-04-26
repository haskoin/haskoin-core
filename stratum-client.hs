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
import System.Exit

app :: AppTCP Response (Either String) StratumResponse
app ad = do
    addrs <- liftIO getArgs
    when (length addrs < 1) $ error "bitcoin addresses required"
    let as = map (fromJust . base58ToAddr) addrs

    vreq <- newReq (toRequest reqVer) (cb reqVer)
    hrqs <- mapM (\a -> newReq (toRequest $ reqHist a) (cb $ reqHist a)) as

    let sl = vreq : hrqs

    CL.sourceList sl
        $= CL.map (C.toStrict . flip C.append "\n" . encode)
        $$ (transPipe liftIO $ appSink ad)

    transPipe liftIO (appSource ad)
        $$ CB.lines
        =$ CL.mapMaybe (decode' . C.fromStrict) -- Response
        =$ CL.mapMaybeM (recvRes gi) -- StratumResponse
        =$ CL.iterM (\x -> liftIO (print x) >>= q)
        =$ CL.sinkNull

  where
    reqVer = ReqVersion "Haskoin 0.0.1" "0.9"
    reqHist = ReqHistory
    cb req = fromResponse req
    gi s = case resID s of IntID i -> i; TxtID i -> read $ show i
    q _ = do
        b <- noMoreData
        if b then liftIO exitSuccess else liftIO $ return ()

main :: IO ()
main = runAppTCP (clientSettings 50001 "electrum.datemas.de") app
