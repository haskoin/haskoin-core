module Network.Haskoin.Stratum.Tests (tests) where

import Data.Aeson
import Data.Aeson.Types
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Network.JsonRpc
import Network.Haskoin.Stratum
import Network.Haskoin.Test.Stratum

tests :: [Test]
tests =
    [ testGroup "Stratum JSON-RPC"
        [ testProperty "Encode/decode requests"
            (reqDecode :: Request StratumRequest -> Bool)
        , testProperty "Encode/decode notifications"
            (notifDecode :: Notif StratumNotif -> Bool)
        , testProperty "Encode/decode responses"
            (resDecode :: ReqRes StratumRequest StratumResult -> Bool)
        ]
    ]

reqDecode :: (Eq a, ToRequest a, ToJSON a, FromRequest a) => Request a -> Bool
reqDecode rq = case parseMaybe parseRequest (toJSON rq) of
    Nothing  -> False
    Just rqE -> either (const False) (rq ==) rqE

notifDecode :: (Eq a, ToNotif a, ToJSON a, FromNotif a) => Notif a -> Bool
notifDecode rn = case parseMaybe parseNotif (toJSON rn) of
    Nothing  -> False
    Just rnE -> either (const False) (rn ==) rnE

resDecode :: (Eq r, ToJSON r, FromResponse r) => ReqRes q r -> Bool
resDecode (ReqRes rq rs) = case parseMaybe (parseResponse rq) (toJSON rs) of
    Nothing -> False
    Just rsE -> either (const False) (rs ==) rsE

