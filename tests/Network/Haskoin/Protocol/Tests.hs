module Network.Haskoin.Protocol.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Network.Haskoin.Test.Protocol
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Serialize & de-serialize protocol messages"
        [ testProperty "VarInt" $ \(ArbitraryVarInt x) -> metaBinary x
        , testProperty "VarString" $ \(ArbitraryVarString x) -> metaBinary x
        , testProperty "NetworkAddress" $ \(ArbitraryNetworkAddress x) -> metaBinary x
        , testProperty "InvType" $ \(ArbitraryInvType x) -> metaBinary x
        , testProperty "InvVector" $ \(ArbitraryInvVector x) -> metaBinary x
        , testProperty "Inv" $ \(ArbitraryInv x) -> metaBinary x
        , testProperty "Version" $ \(ArbitraryVersion x) -> metaBinary x
        , testProperty "Addr" $ \(ArbitraryAddr x) -> metaBinary x
        , testProperty "Alert" $ \(ArbitraryAlert x) -> metaBinary x
        , testProperty "Reject" $ \(ArbitraryReject x) -> metaBinary x
        , testProperty "GetData" $ \(ArbitraryGetData x) -> metaBinary x
        , testProperty "NotFound" $ \(ArbitraryNotFound x) -> metaBinary x
        , testProperty "Ping" $ \(ArbitraryPing x) -> metaBinary x
        , testProperty "Pong" $ \(ArbitraryPong x) -> metaBinary x
        , testProperty "MessageCommand" $ \(ArbitraryMessageCommand x) -> metaBinary x
       -- , testProperty "MessageHeader" $ \(ArbitraryMessageHeader x) -> metaBinary x
       -- , testProperty "Message" $ \(ArbitraryMessage x) -> metaBinary x
        ]
    ]

metaBinary :: (Binary a, Eq a) => a -> Bool
metaBinary x = (decode' $ encode' x) == x

