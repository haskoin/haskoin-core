module Network.Haskoin.Crypto.Base58.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe (fromJust)
import qualified Data.ByteString as BS (ByteString)

import Network.Haskoin.Crypto.Arbitrary()
import Network.Haskoin.Util.Arbitrary()

import Network.Haskoin.Crypto.Base58

tests :: [Test]
tests = 
    [ testGroup "Address and Base58"
        [ testProperty "decode58( encode58(i) ) = i" decodeEncode58
        , testProperty "decode58Chk( encode58Chk(i) ) = i" decodeEncode58Check
        , testProperty "decode58( encode58(address) ) = address" decEncAddr
        ]
    ]

decodeEncode58 :: BS.ByteString -> Bool
decodeEncode58 bs = (fromJust $ decodeBase58 $ encodeBase58 bs) == bs

decodeEncode58Check :: BS.ByteString -> Bool
decodeEncode58Check bs = 
    (fromJust $ decodeBase58Check $ encodeBase58Check bs) == bs

decEncAddr :: Address -> Bool
decEncAddr a = (fromJust $ base58ToAddr $ addrToBase58 a) == a

