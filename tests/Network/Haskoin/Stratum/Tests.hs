module Network.Haskoin.Stratum.Tests (tests) where

import Data.Aeson (ToJSON, toJSON)
import Data.Aeson.Types (parseMaybe)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Network.JsonRpc 
    ( Request(..)
    , ToRequest
    , FromRequest
    , FromResponse
    , Notif
    , ToNotif
    , FromNotif
    , parseRequest
    , parseResponse
    , parseNotif
    )

import Network.Haskoin.Network
import Network.Haskoin.Stratum
import Network.Haskoin.Test

type Net = Prodnet

tests :: [Test]
tests =
    [ testGroup "Stratum JSON-RPC"
        [ testProperty "Encode/decode requests"
            (reqDecode :: Request (StratumRequest Net) -> Bool)
        , testProperty "Encode/decode notifications"
            (notifDecode :: Notif (StratumNotif Net) -> Bool)
        , testProperty "Encode/decode responses"
            (resDecode
                :: ReqRes (StratumRequest Net) (StratumResult Net) -> Bool)
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

