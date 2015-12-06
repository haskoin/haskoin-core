{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Crypto.Base58.Units (tests) where

import Test.HUnit (Assertion, assertBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (append, pack, empty)

import Network.Haskoin.Crypto

tests :: [Test]
tests =
    [ testGroup "Test base58 encodings"
        ( map mapBase58Vec $ zip vectors [0..] )
    ]

mapBase58Vec :: ((ByteString, ByteString, ByteString), Int) -> Test.Framework.Test
mapBase58Vec (v, i) =
    testCase (unwords [ "Test base58 vector", show i ]) $ runVector v

runVector :: (ByteString, ByteString, ByteString) -> Assertion
runVector (bs, e, chk) = do
    assertBool "encodeBase58" $ e == b58
    assertBool "encodeBase58Check" $ chk == b58Chk
    assertBool "decodeBase58" $ Just bs == decodeBase58 b58
    assertBool "decodeBase58Check" $ Just bs == decodeBase58Check b58Chk
  where
    b58    = encodeBase58 bs
    b58Chk = encodeBase58Check bs

vectors :: [(ByteString, ByteString, ByteString)]
vectors =
    [ ( BS.empty, "", "3QJmnh" )
    , ( BS.pack [0], "1", "1Wh4bh" )
    , ( BS.pack [0,0,0,0], "1111", "11114bdQda" )
    , ( BS.pack [0,0,1,0,0], "11LUw", "113CUwsFVuo" )
    , ( BS.pack [255], "5Q", "VrZDWwe" )
    , ( BS.pack [0,0,0,0] `BS.append` BS.pack [1..255]
      , "1111cWB5HCBdLjAuqGGReWE3R3CguuwSjw6RHn39s2yuDRTS5NsBgNiFpWgAnEx6VQi8csexkgYw3mdYrMHr8x9i7aEwP8kZ7vccXWqKDvGv3u1GxFKPuAkn8JCPPGDMf3vMMnbzm6Nh9zh1gcNsMvH3ZNLmP5fSG6DGbbi2tuwMWPthr4boWwCxf7ewSgNQeacyozhKDDQQ1qL5fQFUW52QKUZDZ5fw3KXNQJMcNTcaB723LchjeKun7MuGW5qyCBZYzA1KjofN1gYBV3NqyhQJ3Ns746GNuf9N2pQPmHz4xpnSrrfCvy6TVVz5d4PdrjeshsWQwpZsZGzvbdAdN8MKV5QsBDY"
      , "111151KWPPBRzdWPr1ASeu172gVgLf1YfUp6VJyk6K9t4cLqYtFHcMa2iX8S3NJEprUcW7W5LvaPRpz7UG7puBj5STE3nKhCGt5eckYq7mMn5nT7oTTic2BAX6zDdqrmGCnkszQkzkz8e5QLGDjf7KeQgtEDm4UER6DMSdBjFQVa6cHrrJn9myVyyhUrsVnfUk2WmNFZvkWv3Tnvzo2cJ1xW62XDfUgYz1pd97eUGGPuXvDFfLsBVd1dfdUhPwxW7pMPgdWHTmg5uqKGFF6vE4xXpAqZTbTxRZjCDdTn68c2wrcxApm8hq3JX65Hix7VtcD13FF8b7BzBtwjXq1ze6NMjKgUcqpJTN9vt"
      )
    ]

