{-# LANGUAGE OverloadedStrings, Rank2Types #-}
import Data.Conduit.Network
import Data.Foldable
import Data.Maybe
import Network.Haskoin.Crypto
import Network.Haskoin.JSONRPC.Conduit
import Network.Haskoin.JSONRPC.Stratum
import Network.Haskoin.Util
import System.Environment

app :: AppData IO -> IO ()
app ad = do
    us <- getArgs
    let as = map (fromJust . base58ToAddr) us
    s <- initSession
    newReq s (toRequest rv) (cb rv)
    forM_ as (\a -> let r = ReqHistory a in newReq s (toRequest r) (hcb s r))
    _ <- sourceThread s ad
    responsePipe s ad
  where
    rv = ReqVersion "Haskoin 0.0.1" "0.9"
    cb r = print . fromResponse r
    hcb s req res = do
        let rh@(ResHistory h) = fromRight $ fromResponse req res
        print rh
        forM_ h $ \(TxHeight _ t) ->
            let r = ReqTx t in newReq s (toRequest r) (cb r)

main :: IO ()
main = runTCPClient (clientSettings 50001 "electrum.chroot.eu") app
