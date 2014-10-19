module Network.Haskoin.Protocol.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Network.Haskoin.Protocol.Arbitrary()
import Network.Haskoin.Protocol.ArbitraryMsg()
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize protocol messages"
        [ testProperty "VarInt" (metaBinary :: VarInt -> Bool)
        , testProperty "VarString" (metaBinary :: VarString -> Bool)
        , testProperty "NetworkAddress" (metaBinary :: NetworkAddress -> Bool)
        , testProperty "InvType" (metaBinary :: InvType -> Bool)
        , testProperty "InvVector" (metaBinary :: InvVector -> Bool)
        , testProperty "Inv" (metaBinary :: Inv -> Bool)
        , testProperty "Version" (metaBinary :: Version -> Bool)
        , testProperty "Addr" (metaBinary :: Addr -> Bool)
        , testProperty "Alert" (metaBinary :: Alert -> Bool)
        , testProperty "Reject" (metaBinary :: Reject -> Bool)
        , testProperty "GetData" (metaBinary :: GetData -> Bool)
        , testProperty "FilterLoad" (metaBinary :: FilterLoad -> Bool)
        , testProperty "FilterAdd" (metaBinary :: FilterAdd -> Bool)
        , testProperty "NotFound" (metaBinary :: NotFound -> Bool)
        , testProperty "Ping" (metaBinary :: Ping -> Bool)
        , testProperty "Pong" (metaBinary :: Pong -> Bool)
        , testProperty "MessageCommand" (metaBinary :: MessageCommand -> Bool)
        , testProperty "MessageHeader" (metaBinary :: MessageHeader -> Bool)
        , testProperty "Message" (metaBinary :: Message -> Bool)
        ]
    ]

metaBinary :: (Binary a, Eq a) => a -> Bool
metaBinary x = (decode' $ encode' x) == x

