module Units (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Control.Monad.Trans

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Wallet.TxBuilder
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

tests =
    [ testGroup "BIP32 derivation vector 1" 
        [ testCase "Chain m" $ runXKeyVec (xKeyVec !! 0)
        , testCase "Chain m/0'" $ runXKeyVec (xKeyVec !! 1)
        , testCase "Chain m/0'/1" $ runXKeyVec (xKeyVec !! 2)
        , testCase "Chain m/0'/1/2'" $ runXKeyVec (xKeyVec !! 3)
        , testCase "Chain m/0'/1/2'/2" $ runXKeyVec (xKeyVec !! 4)
        , testCase "Chain m/0'/1/2'/2/1000000000" $ 
            runXKeyVec (xKeyVec !! 5)
        ] 
    , testGroup "BIP32 subkey derivation vector 2" 
        [ testCase "Chain m" $ runXKeyVec (xKeyVec2 !! 0)
        , testCase "Chain m/0" $ runXKeyVec (xKeyVec2 !! 1)
        , testCase "Chain m/0/2147483647'" $ 
            runXKeyVec (xKeyVec2 !! 2)
        , testCase "Chain m/0/2147483647'/1" $ 
            runXKeyVec (xKeyVec2 !! 3)
        , testCase "Chain m/0/2147483647'/1/2147483646'" $ 
            runXKeyVec (xKeyVec2 !! 4)
        , testCase "Chain m/0/2147483647'/1/2147483646'/2" $ 
            runXKeyVec (xKeyVec2 !! 5)
        ] 
    , testGroup "Build PKHash Transaction (generated from bitcoind)" 
        ( map mapPKHashVec $ zip pkHashVec [0..] )
    , testGroup "Verify transaction (bitcoind /test/data/tx_valid.json)" 
        ( map mapVerifyVec $ zip verifyVec [0..] )
    ]


runXKeyVec :: ([String],XPrvKey) -> Assertion
runXKeyVec (v,m) = do
    assertBool "xPrvID" $ (bsToHex $ encode' $ xPrvID m) == v !! 0
    assertBool "xPrvFP" $ (bsToHex $ encode' $ xPrvFP m) == v !! 1
    assertBool "xPrvAddr" $ 
        (addrToBase58 $ xPubAddr $ deriveXPubKey m) == v !! 2
    assertBool "prvKey" $ (bsToHex $ runPut' $ putPrvKey $ xPrvKey m) == v !! 3
    assertBool "xPrvWIF" $ xPrvWIF m == v !! 4
    assertBool "pubKey" $ 
        (bsToHex $ encode' $ xPubKey $ deriveXPubKey m) == v !! 5
    assertBool "chain code" $ (bsToHex $ encode' $ xPrvChain m) == v !! 6
    assertBool "Hex PubKey" $ (bsToHex $ encode' $ deriveXPubKey m) == v !! 7
    assertBool "Hex PrvKey" $ (bsToHex $ encode' m) == v !! 8
    assertBool "Base58 PubKey" $ (xPubExport $ deriveXPubKey m) == v !! 9
    assertBool "Base58 PrvKey" $ xPrvExport m == v !! 10

mapPKHashVec :: (([(String,Word32)],[(String,Word64)],String),Int)
            -> Test.Framework.Test
mapPKHashVec (v,i) = testCase name $ runPKHashVec v
    where name = "Build PKHash Tx " ++ (show i)

runPKHashVec :: ([(String,Word32)],[(String,Word64)],String) -> Assertion
runPKHashVec (xs,ys,res) = 
    assertBool "Build PKHash Tx" $ (bsToHex $ encode' tx) == res
    where tx = fromRight $ buildPKHashTx (map f xs) ys
          f (id,ix) = OutPoint (decode' $ BS.reverse $ fromJust $ hexToBS id) ix

mapVerifyVec :: (([(String,String,String)],String),Int) 
             -> Test.Framework.Test
mapVerifyVec (v,i) = testCase name $ runVerifyVec v i
    where name = "Verify Tx " ++ (show i)

runVerifyVec :: ([(String,String,String)],String) -> Int -> Assertion
runVerifyVec (is,bsTx) i = 
    assertBool name $ verifyTx tx $ map f is
    where name = "    > Verify transaction " ++ (show i)
          tx  = decode' (fromJust $ hexToBS bsTx)
          f (o1,o2,bsScript) = 
              let ops = runGet' getScriptOps (fromJust $ hexToBS bsScript)
                  op  = OutPoint (decode' $ BS.reverse $ fromJust $ hexToBS o1) 
                                 (runGet' getWord32le $ fromJust $ hexToBS o2)
                  in (Script ops,op)

-- BIP 0032 Test Vectors
-- https://en.bitcoin.it/wiki/BIP_0032_TestVectors

xKeyVec :: [([String],XPrvKey)]
xKeyVec = zip xKeyResVec $ catMaybes $ foldl f [m] der
    where f acc d = acc ++ [d =<< last acc]
          m   = makeXPrvKey $ fromJust $ hexToBS m0
          der = [ flip primeSubKey 0
                , flip prvSubKey 1
                , flip primeSubKey 2
                , flip prvSubKey 2
                , flip prvSubKey 1000000000
                ]

xKeyVec2 :: [([String],XPrvKey)]
xKeyVec2 = zip xKeyResVec2 $ catMaybes $ foldl f [m] der
    where f acc d = acc ++ [d =<< last acc]
          m   = makeXPrvKey $ fromJust $ hexToBS m1
          der = [ flip prvSubKey 0
                , flip primeSubKey 2147483647
                , flip prvSubKey 1
                , flip primeSubKey 2147483646
                , flip prvSubKey 2
                ]

m0 = "000102030405060708090a0b0c0d0e0f"

xKeyResVec :: [[String]]
xKeyResVec =
    [
      -- m
      [ "3442193e1bb70916e914552172cd4e2dbc9df811"
      , "3442193e"
      , "15mKKb2eos1hWa6tisdPwwDC1a5J1y9nma"
      , "e8f32e723decf4051aefac8e2c93c9c5b214313817cdb01a1494b917c8436b35"
      , "L52XzL2cMkHxqxBXRyEpnPQZGUs3uKiL3R11XbAdHigRzDozKZeW"
      , "0339a36013301597daef41fbe593a02cc513d0b55527ec2df1050e2e8ff49c85c2"
      , "873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d508"
      , "0488b21e000000000000000000873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d5080339a36013301597daef41fbe593a02cc513d0b55527ec2df1050e2e8ff49c85c2"
      , "0488ade4000000000000000000873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d50800e8f32e723decf4051aefac8e2c93c9c5b214313817cdb01a1494b917c8436b35"
      , "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8"
      , "xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi"
      ]
      -- m/0'
    , [ "5c1bd648ed23aa5fd50ba52b2457c11e9e80a6a7"
      , "5c1bd648"
      , "19Q2WoS5hSS6T8GjhK8KZLMgmWaq4neXrh"
      , "edb2e14f9ee77d26dd93b4ecede8d16ed408ce149b6cd80b0715a2d911a0afea"
      , "L5BmPijJjrKbiUfG4zbiFKNqkvuJ8usooJmzuD7Z8dkRoTThYnAT"
      , "035a784662a4a20a65bf6aab9ae98a6c068a81c52e4b032c0fb5400c706cfccc56"
      , "47fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae6236141"
      , "0488b21e013442193e8000000047fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae6236141035a784662a4a20a65bf6aab9ae98a6c068a81c52e4b032c0fb5400c706cfccc56"
      , "0488ade4013442193e8000000047fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae623614100edb2e14f9ee77d26dd93b4ecede8d16ed408ce149b6cd80b0715a2d911a0afea"
      , "xpub68Gmy5EdvgibQVfPdqkBBCHxA5htiqg55crXYuXoQRKfDBFA1WEjWgP6LHhwBZeNK1VTsfTFUHCdrfp1bgwQ9xv5ski8PX9rL2dZXvgGDnw"
      , "xprv9uHRZZhk6KAJC1avXpDAp4MDc3sQKNxDiPvvkX8Br5ngLNv1TxvUxt4cV1rGL5hj6KCesnDYUhd7oWgT11eZG7XnxHrnYeSvkzY7d2bhkJ7"
      ]
      -- m/0'/1
    , [ "bef5a2f9a56a94aab12459f72ad9cf8cf19c7bbe"
      , "bef5a2f9"
      , "1JQheacLPdM5ySCkrZkV66G2ApAXe1mqLj"
      , "3c6cb8d0f6a264c91ea8b5030fadaa8e538b020f0a387421a12de9319dc93368"
      , "KyFAjQ5rgrKvhXvNMtFB5PCSKUYD1yyPEe3xr3T34TZSUHycXtMM"
      , "03501e454bf00751f24b1b489aa925215d66af2234e3891c3b21a52bedb3cd711c"
      , "2a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19"
      , "0488b21e025c1bd648000000012a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c1903501e454bf00751f24b1b489aa925215d66af2234e3891c3b21a52bedb3cd711c"
      , "0488ade4025c1bd648000000012a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19003c6cb8d0f6a264c91ea8b5030fadaa8e538b020f0a387421a12de9319dc93368"
      , "xpub6ASuArnXKPbfEwhqN6e3mwBcDTgzisQN1wXN9BJcM47sSikHjJf3UFHKkNAWbWMiGj7Wf5uMash7SyYq527Hqck2AxYysAA7xmALppuCkwQ"
      , "xprv9wTYmMFdV23N2TdNG573QoEsfRrWKQgWeibmLntzniatZvR9BmLnvSxqu53Kw1UmYPxLgboyZQaXwTCg8MSY3H2EU4pWcQDnRnrVA1xe8fs"
      ]
      -- m/0'/1/2'
    , [ "ee7ab90cde56a8c0e2bb086ac49748b8db9dce72"
      , "ee7ab90c"
      , "1NjxqbA9aZWnh17q1UW3rB4EPu79wDXj7x"
      , "cbce0d719ecf7431d88e6a89fa1483e02e35092af60c042b1df2ff59fa424dca"
      , "L43t3od1Gh7Lj55Bzjj1xDAgJDcL7YFo2nEcNaMGiyRZS1CidBVU"
      , "0357bfe1e341d01c69fe5654309956cbea516822fba8a601743a012a7896ee8dc2"
      , "04466b9cc8e161e966409ca52986c584f07e9dc81f735db683c3ff6ec7b1503f"
      , "0488b21e03bef5a2f98000000204466b9cc8e161e966409ca52986c584f07e9dc81f735db683c3ff6ec7b1503f0357bfe1e341d01c69fe5654309956cbea516822fba8a601743a012a7896ee8dc2"
      , "0488ade403bef5a2f98000000204466b9cc8e161e966409ca52986c584f07e9dc81f735db683c3ff6ec7b1503f00cbce0d719ecf7431d88e6a89fa1483e02e35092af60c042b1df2ff59fa424dca"
      , "xpub6D4BDPcP2GT577Vvch3R8wDkScZWzQzMMUm3PWbmWvVJrZwQY4VUNgqFJPMM3No2dFDFGTsxxpG5uJh7n7epu4trkrX7x7DogT5Uv6fcLW5"
      , "xprv9z4pot5VBttmtdRTWfWQmoH1taj2axGVzFqSb8C9xaxKymcFzXBDptWmT7FwuEzG3ryjH4ktypQSAewRiNMjANTtpgP4mLTj34bhnZX7UiM"
      ]
      -- m/0'/1/2'/2
    , [ "d880d7d893848509a62d8fb74e32148dac68412f"
      , "d880d7d8"
      , "1LjmJcdPnDHhNTUgrWyhLGnRDKxQjoxAgt"
      , "0f479245fb19a38a1954c5c7c0ebab2f9bdfd96a17563ef28a6a4b1a2a764ef4"
      , "KwjQsVuMjbCP2Zmr3VaFaStav7NvevwjvvkqrWd5Qmh1XVnCteBR"
      , "02e8445082a72f29b75ca48748a914df60622a609cacfce8ed0e35804560741d29"
      , "cfb71883f01676f587d023cc53a35bc7f88f724b1f8c2892ac1275ac822a3edd"
      , "0488b21e04ee7ab90c00000002cfb71883f01676f587d023cc53a35bc7f88f724b1f8c2892ac1275ac822a3edd02e8445082a72f29b75ca48748a914df60622a609cacfce8ed0e35804560741d29"
      , "0488ade404ee7ab90c00000002cfb71883f01676f587d023cc53a35bc7f88f724b1f8c2892ac1275ac822a3edd000f479245fb19a38a1954c5c7c0ebab2f9bdfd96a17563ef28a6a4b1a2a764ef4"
      , "xpub6FHa3pjLCk84BayeJxFW2SP4XRrFd1JYnxeLeU8EqN3vDfZmbqBqaGJAyiLjTAwm6ZLRQUMv1ZACTj37sR62cfN7fe5JnJ7dh8zL4fiyLHV"
      , "xprvA2JDeKCSNNZky6uBCviVfJSKyQ1mDYahRjijr5idH2WwLsEd4Hsb2Tyh8RfQMuPh7f7RtyzTtdrbdqqsunu5Mm3wDvUAKRHSC34sJ7in334"
      ]
      -- m/0'/1/2'/2/1000000000
    , [ "d69aa102255fed74378278c7812701ea641fdf32"
      , "d69aa102"
      , "1LZiqrop2HGR4qrH1ULZPyBpU6AUP49Uam"
      , "471b76e389e528d6de6d816857e012c5455051cad6660850e58372a6c3e6e7c8"
      , "Kybw8izYevo5xMh1TK7aUr7jHFCxXS1zv8p3oqFz3o2zFbhRXHYs"
      , "022a471424da5e657499d1ff51cb43c47481a03b1e77f951fe64cec9f5a48f7011"
      , "c783e67b921d2beb8f6b389cc646d7263b4145701dadd2161548a8b078e65e9e"
      , "0488b21e05d880d7d83b9aca00c783e67b921d2beb8f6b389cc646d7263b4145701dadd2161548a8b078e65e9e022a471424da5e657499d1ff51cb43c47481a03b1e77f951fe64cec9f5a48f7011"
      , "0488ade405d880d7d83b9aca00c783e67b921d2beb8f6b389cc646d7263b4145701dadd2161548a8b078e65e9e00471b76e389e528d6de6d816857e012c5455051cad6660850e58372a6c3e6e7c8"
      , "xpub6H1LXWLaKsWFhvm6RVpEL9P4KfRZSW7abD2ttkWP3SSQvnyA8FSVqNTEcYFgJS2UaFcxupHiYkro49S8yGasTvXEYBVPamhGW6cFJodrTHy"
      , "xprvA41z7zogVVwxVSgdKUHDy1SKmdb533PjDz7J6N6mV6uS3ze1ai8FHa8kmHScGpWmj4WggLyQjgPie1rFSruoUihUZREPSL39UNdE3BBDu76"
      ]
    ]

m1 = "fffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542"

xKeyResVec2 :: [[String]]
xKeyResVec2 =
    [
      -- m
      [ "bd16bee53961a47d6ad888e29545434a89bdfe95"
      , "bd16bee5"
      , "1JEoxevbLLG8cVqeoGKQiAwoWbNYSUyYjg"
      , "4b03d6fc340455b363f51020ad3ecca4f0850280cf436c70c727923f6db46c3e"
      , "KyjXhyHF9wTphBkfpxjL8hkDXDUSbE3tKANT94kXSyh6vn6nKaoy"
      , "03cbcaa9c98c877a26977d00825c956a238e8dddfbd322cce4f74b0b5bd6ace4a7"
      , "60499f801b896d83179a4374aeb7822aaeaceaa0db1f85ee3e904c4defbd9689"
      , "0488b21e00000000000000000060499f801b896d83179a4374aeb7822aaeaceaa0db1f85ee3e904c4defbd968903cbcaa9c98c877a26977d00825c956a238e8dddfbd322cce4f74b0b5bd6ace4a7"
      , "0488ade400000000000000000060499f801b896d83179a4374aeb7822aaeaceaa0db1f85ee3e904c4defbd9689004b03d6fc340455b363f51020ad3ecca4f0850280cf436c70c727923f6db46c3e"
      , "xpub661MyMwAqRbcFW31YEwpkMuc5THy2PSt5bDMsktWQcFF8syAmRUapSCGu8ED9W6oDMSgv6Zz8idoc4a6mr8BDzTJY47LJhkJ8UB7WEGuduB"
      , "xprv9s21ZrQH143K31xYSDQpPDxsXRTUcvj2iNHm5NUtrGiGG5e2DtALGdso3pGz6ssrdK4PFmM8NSpSBHNqPqm55Qn3LqFtT2emdEXVYsCzC2U"
      ]
      -- m/0
    , [ "5a61ff8eb7aaca3010db97ebda76121610b78096"
      , "5a61ff8e"
      , "19EuDJdgfRkwCmRzbzVBHZWQG9QNWhftbZ"
      , "abe74a98f6c7eabee0428f53798f0ab8aa1bd37873999041703c742f15ac7e1e"
      , "L2ysLrR6KMSAtx7uPqmYpoTeiRzydXBattRXjXz5GDFPrdfPzKbj"
      , "02fc9e5af0ac8d9b3cecfe2a888e2117ba3d089d8585886c9c826b6b22a98d12ea"
      , "f0909affaa7ee7abe5dd4e100598d4dc53cd709d5a5c2cac40e7412f232f7c9c"
      , "0488b21e01bd16bee500000000f0909affaa7ee7abe5dd4e100598d4dc53cd709d5a5c2cac40e7412f232f7c9c02fc9e5af0ac8d9b3cecfe2a888e2117ba3d089d8585886c9c826b6b22a98d12ea"
      , "0488ade401bd16bee500000000f0909affaa7ee7abe5dd4e100598d4dc53cd709d5a5c2cac40e7412f232f7c9c00abe74a98f6c7eabee0428f53798f0ab8aa1bd37873999041703c742f15ac7e1e"
      , "xpub69H7F5d8KSRgmmdJg2KhpAK8SR3DjMwAdkxj3ZuxV27CprR9LgpeyGmXUbC6wb7ERfvrnKZjXoUmmDznezpbZb7ap6r1D3tgFxHmwMkQTPH"
      , "xprv9vHkqa6EV4sPZHYqZznhT2NPtPCjKuDKGY38FBWLvgaDx45zo9WQRUT3dKYnjwih2yJD9mkrocEZXo1ex8G81dwSM1fwqWpWkeS3v86pgKt"
      ]
      -- m/0/2147483647'
    , [ "d8ab493736da02f11ed682f88339e720fb0379d1"
      , "d8ab4937"
      , "1Lke9bXGhn5VPrBuXgN12uGUphrttUErmk"
      , "877c779ad9687164e9c2f4f0f4ff0340814392330693ce95a58fe18fd52e6e93"
      , "L1m5VpbXmMp57P3knskwhoMTLdhAAaXiHvnGLMribbfwzVRpz2Sr"
      , "03c01e7425647bdefa82b12d9bad5e3e6865bee0502694b94ca58b666abc0a5c3b"
      , "be17a268474a6bb9c61e1d720cf6215e2a88c5406c4aee7b38547f585c9a37d9"
      , "0488b21e025a61ff8effffffffbe17a268474a6bb9c61e1d720cf6215e2a88c5406c4aee7b38547f585c9a37d903c01e7425647bdefa82b12d9bad5e3e6865bee0502694b94ca58b666abc0a5c3b"
      , "0488ade4025a61ff8effffffffbe17a268474a6bb9c61e1d720cf6215e2a88c5406c4aee7b38547f585c9a37d900877c779ad9687164e9c2f4f0f4ff0340814392330693ce95a58fe18fd52e6e93"
      , "xpub6ASAVgeehLbnwdqV6UKMHVzgqAG8Gr6riv3Fxxpj8ksbH9ebxaEyBLZ85ySDhKiLDBrQSARLq1uNRts8RuJiHjaDMBU4Zn9h8LZNnBC5y4a"
      , "xprv9wSp6B7kry3Vj9m1zSnLvN3xH8RdsPP1Mh7fAaR7aRLcQMKTR2vidYEeEg2mUCTAwCd6vnxVrcjfy2kRgVsFawNzmjuHc2YmYRmagcEPdU9"
      ]
      -- m/0/2147483647'/1
    , [ "78412e3a2296a40de124307b6485bd19833e2e34"
      , "78412e3a"
      , "1BxrAr2pHpeBheusmd6fHDP2tSLAUa3qsW"
      , "704addf544a06e5ee4bea37098463c23613da32020d604506da8c0518e1da4b7"
      , "KzyzXnznxSv249b4KuNkBwowaN3akiNeEHy5FWoPCJpStZbEKXN2"
      , "03a7d1d856deb74c508e05031f9895dab54626251b3806e16b4bd12e781a7df5b9"
      , "f366f48f1ea9f2d1d3fe958c95ca84ea18e4c4ddb9366c336c927eb246fb38cb"
      , "0488b21e03d8ab493700000001f366f48f1ea9f2d1d3fe958c95ca84ea18e4c4ddb9366c336c927eb246fb38cb03a7d1d856deb74c508e05031f9895dab54626251b3806e16b4bd12e781a7df5b9"
      , "0488ade403d8ab493700000001f366f48f1ea9f2d1d3fe958c95ca84ea18e4c4ddb9366c336c927eb246fb38cb00704addf544a06e5ee4bea37098463c23613da32020d604506da8c0518e1da4b7"
      , "xpub6DF8uhdarytz3FWdA8TvFSvvAh8dP3283MY7p2V4SeE2wyWmG5mg5EwVvmdMVCQcoNJxGoWaU9DCWh89LojfZ537wTfunKau47EL2dhHKon"
      , "xprv9zFnWC6h2cLgpmSA46vutJzBcfJ8yaJGg8cX1e5StJh45BBciYTRXSd25UEPVuesF9yog62tGAQtHjXajPPdbRCHuWS6T8XA2ECKADdw4Ef"
      ]
      -- m/0/2147483647'/1/2147483646'
    , [ "31a507b815593dfc51ffc7245ae7e5aee304246e"
      , "31a507b8"
      , "15XVotxCAV7sRx1PSCkQNsGw3W9jT9A94R"
      , "f1c7c871a54a804afe328b4c83a1c33b8e5ff48f5087273f04efa83b247d6a2d"
      , "L5KhaMvPYRW1ZoFmRjUtxxPypQ94m6BcDrPhqArhggdaTbbAFJEF"
      , "02d2b36900396c9282fa14628566582f206a5dd0bcc8d5e892611806cafb0301f0"
      , "637807030d55d01f9a0cb3a7839515d796bd07706386a6eddf06cc29a65a0e29"
      , "0488b21e0478412e3afffffffe637807030d55d01f9a0cb3a7839515d796bd07706386a6eddf06cc29a65a0e2902d2b36900396c9282fa14628566582f206a5dd0bcc8d5e892611806cafb0301f0"
      , "0488ade40478412e3afffffffe637807030d55d01f9a0cb3a7839515d796bd07706386a6eddf06cc29a65a0e2900f1c7c871a54a804afe328b4c83a1c33b8e5ff48f5087273f04efa83b247d6a2d"
      , "xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL"
      , "xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc"
      ]
      -- m/0/2147483647'/1/2147483646'/2
    , [ "26132fdbe7bf89cbc64cf8dafa3f9f88b8666220"
      , "26132fdb"
      , "14UKfRV9ZPUp6ZC9PLhqbRtxdihW9em3xt"
      , "bb7d39bdb83ecf58f2fd82b6d918341cbef428661ef01ab97c28a4842125ac23"
      , "L3WAYNAZPxx1fr7KCz7GN9nD5qMBnNiqEJNJMU1z9MMaannAt4aK"
      , "024d902e1a2fc7a8755ab5b694c575fce742c48d9ff192e63df5193e4c7afe1f9c"
      , "9452b549be8cea3ecb7a84bec10dcfd94afe4d129ebfd3b3cb58eedf394ed271"
      , "0488b21e0531a507b8000000029452b549be8cea3ecb7a84bec10dcfd94afe4d129ebfd3b3cb58eedf394ed271024d902e1a2fc7a8755ab5b694c575fce742c48d9ff192e63df5193e4c7afe1f9c"
      , "0488ade40531a507b8000000029452b549be8cea3ecb7a84bec10dcfd94afe4d129ebfd3b3cb58eedf394ed27100bb7d39bdb83ecf58f2fd82b6d918341cbef428661ef01ab97c28a4842125ac23"
      , "xpub6FnCn6nSzZAw5Tw7cgR9bi15UV96gLZhjDstkXXxvCLsUXBGXPdSnLFbdpq8p9HmGsApME5hQTZ3emM2rnY5agb9rXpVGyy3bdW6EEgAtqt"
      , "xprvA2nrNbFZABcdryreWet9Ea4LvTJcGsqrMzxHx98MMrotbir7yrKCEXw7nadnHM8Dq38EGfSh6dqA9QWTyefMLEcBYJUuekgW4BYPJcr9E7j"
      ]
    ]

-- These test vectors have been generated from bitcoind raw transaction api

pkHashVec :: [([(String,Word32)],[(String,Word64)],String)]
pkHashVec =
    [
      ( [("eb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db",14)]
      , [("14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb",90000000)]
      , "0100000001db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0e00000000ffffffff01804a5d05000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac00000000"
      )
    , ( [ ("eb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db",0)
        , ("0001000000000000000000000000000000000000000000000000000000000000",2147483647)
        ]
      , [ ("14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb",1)
        , ("19VCgS642vzEA1sdByoSn6GsWBwraV8D4n",2100000000000000)
        ]
      , "0100000002db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0000000000ffffffff0000000000000000000000000000000000000000000000000000000000000100ffffff7f00ffffffff0201000000000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac0040075af07507001976a9145d16672f53981ff21c5f42b40d1954993cbca54f88ac00000000"
      )
    , ( [ ("eb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db",0)
        , ("0001000000000000000000000000000000000000000000000000000000000000",2147483647)
        ]
      , []
      , "0100000002db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0000000000ffffffff0000000000000000000000000000000000000000000000000000000000000100ffffff7f00ffffffff0000000000"
      )
    , ( []
      , [ ("14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb",1)
        , ("19VCgS642vzEA1sdByoSn6GsWBwraV8D4n",2100000000000000)
        ]
      , "01000000000201000000000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac0040075af07507001976a9145d16672f53981ff21c5f42b40d1954993cbca54f88ac00000000"
      )
    ]

{- Test vectors from bitcoind -}
-- github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json


verifyVec :: [([(String,String,String)],String)]
verifyVec = 
    [
      -- It is of particular interest because it contains an invalidly-encoded signature which OpenSSL accepts
      ( [ 
          ( "60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1"
          , "00000000"
          , "514104cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4410461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af52ae"
          )
        ]
      , "0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba26000000000490047304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000"
      )
      -- It has an arbitrary extra byte stuffed into the signature at pos length - 2
    , ( [
          ( "60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1"
          , "00000000"
          , "514104cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4410461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af52ae"
          )
        ]
      , "0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba260000000004A0048304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2bab01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000"
      )
      -- it is of interest because it contains a 0-sequence as well as a signature of SIGHASH type 0 (which is not a real type)
    , ( [
          ( "406b2b06bcd34d3c8733e6b79f7a394c8a431fbf4ff5ac705c93f4076bb77602"
          , "00000000"
          , "76a914dc44b1164188067c3a32d4780f5996fa14a4f2d988ac"
          )
        ]
      , "01000000010276b76b07f4935c70acf54fbf1f438a4c397a9fb7e633873c4dd3bc062b6b40000000008c493046022100d23459d03ed7e9511a47d13292d3430a04627de6235b6e51a40f9cd386f2abe3022100e7d25b080f0bb8d8d5f878bba7d54ad2fda650ea8d158a33ee3cbd11768191fd004104b0e2c879e4daf7b9ab68350228c159766676a14f5815084ba166432aab46198d4cca98fa3e9981d0a90b2effc514b76279476550ba3663fdcaff94c38420e9d5000000000100093d00000000001976a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac00000000"
      )
      -- It caught a bug in the workaround for 23b397edccd3740a74adb603c9756370fafcde9bcc4483eb271ecad09a94dd63 in an overly simple implementation
    , ( [
          ( "b464e85df2a238416f8bdae11d120add610380ea07f4ef19c5f9dfd472f96c3d"
          , "00000000"
          , "76a914bef80ecf3a44500fda1bc92176e442891662aed288ac"
          )
        , ( "b7978cc96e59a8b13e0865d3f95657561a7f725be952438637475920bac9eb21"
          , "01000000"
          , "76a914bef80ecf3a44500fda1bc92176e442891662aed288ac"
          )
        ]
      , "01000000023d6cf972d4dff9c519eff407ea800361dd0a121de1da8b6f4138a2f25de864b4000000008a4730440220ffda47bfc776bcd269da4832626ac332adfca6dd835e8ecd83cd1ebe7d709b0e022049cffa1cdc102a0b56e0e04913606c70af702a1149dc3b305ab9439288fee090014104266abb36d66eb4218a6dd31f09bb92cf3cfa803c7ea72c1fc80a50f919273e613f895b855fb7465ccbc8919ad1bd4a306c783f22cd3227327694c4fa4c1c439affffffff21ebc9ba20594737864352e95b727f1a565756f9d365083eb1a8596ec98c97b7010000008a4730440220503ff10e9f1e0de731407a4a245531c9ff17676eda461f8ceeb8c06049fa2c810220c008ac34694510298fa60b3f000df01caa244f165b727d4896eb84f81e46bcc4014104266abb36d66eb4218a6dd31f09bb92cf3cfa803c7ea72c1fc80a50f919273e613f895b855fb7465ccbc8919ad1bd4a306c783f22cd3227327694c4fa4c1c439affffffff01f0da5200000000001976a914857ccd42dded6df32949d4646dfa10a92458cfaa88ac00000000"
      )
      -- It results in signing the constant 1, instead of something generated based on the transaction,
      -- when the input doing the signing has an index greater than the maximum output index
    , ( [ 
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "76a914e52b482f2faa8ecbf0db344f93c84ac908557f3388ac"
          )
        , ( "0000000000000000000000000000000000000000000000000000000000000200"
          , "00000000"
          , "76a914751e76e8199196d454941c45d1b3a323f1433bd688ac"
          )
        ]
        , "01000000020002000000000000000000000000000000000000000000000000000000000000000000006a47304402200469f169b8091cd18a2770136be7411f079b3ac2b5c199885eb66a80aa3ed75002201fa89f3e6f80974e1b3474e70a0fbe907c766137ff231e4dd05a555d8544536701210279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ffffffff0001000000000000000000000000000000000000000000000000000000000000000000006b483045022100c9cdd08798a28af9d1baf44a6c77bcc7e279f47dc487c8c899911bc48feaffcc0220503c5c50ae3998a733263c5c0f7061b483e2b56c4c41b456e7d2f5a78a74c077032102d5c25adb51b61339d2b05315791e21bbe80ea470a49db0135720983c905aace0ffffffff010000000000000000015100000000"
      )
      -- A valid P2SH Transaction using the standard transaction type put forth in BIP 16
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "a9148febbed40483661de6958d957412f82deed8e2f787"
          )
        ]
      , "01000000010001000000000000000000000000000000000000000000000000000000000000000000006e493046022100c66c9cdf4c43609586d15424c54707156e316d88b0a1534c9e6b0d4f311406310221009c0fe51dbc9c4ab7cc25d3fdbeccf6679fe6827f08edf2b4a9f16ee3eb0e438a0123210338e8034509af564c62644c07691942e0c056752008a173c89f60ab2a88ac2ebfacffffffff010000000000000000015100000000"
      )
      -- MAX_MONEY output
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "a91432afac281462b822adbec5094b8d4d337dd5bd6a87"
          )
        ]
      , "01000000010001000000000000000000000000000000000000000000000000000000000000000000006e493046022100e1eadba00d9296c743cb6ecc703fd9ddc9b3cd12906176a226ae4c18d6b00796022100a71aef7d2874deff681ba6080f1b278bac7bb99c61b08a85f4311970ffe7f63f012321030c0588dc44d92bdcbf8e72093466766fdc265ead8db64517b0c542275b70fffbacffffffff010040075af0750700015100000000"
      )
      -- MAX_MONEY output + 0 output
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "a914b558cbf4930954aa6a344363a15668d7477ae71687"
          )
        ]
      , "01000000010001000000000000000000000000000000000000000000000000000000000000000000006d483045022027deccc14aa6668e78a8c9da3484fbcd4f9dcc9bb7d1b85146314b21b9ae4d86022100d0b43dece8cfb07348de0ca8bc5b86276fa88f7f2138381128b7c36ab2e42264012321029bb13463ddd5d2cc05da6e84e37536cb9525703cfd8f43afdb414988987a92f6acffffffff020040075af075070001510000000000000000015100000000"
      )
      -- Simple transaction with first input is signed with SIGHASH_ALL, second with SIGHASH_ANYONECANPAY
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "21035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac"
          )
        , ( "0000000000000000000000000000000000000000000000000000000000000200"
          , "00000000"
          , "21035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac"
          )
        ]
      , "010000000200010000000000000000000000000000000000000000000000000000000000000000000049483045022100d180fd2eb9140aeb4210c9204d3f358766eb53842b2a9473db687fa24b12a3cc022079781799cd4f038b85135bbe49ec2b57f306b2bb17101b17f71f000fcab2b6fb01ffffffff0002000000000000000000000000000000000000000000000000000000000000000000004847304402205f7530653eea9b38699e476320ab135b74771e1c48b81a5d041e2ca84b9be7a802200ac8d1f40fb026674fe5a5edd3dea715c27baa9baca51ed45ea750ac9dc0a55e81ffffffff010100000000000000015100000000"
      )
      -- Same as above, but we change the sequence number of the first input to check that SIGHASH_ANYONECANPAY is being followed
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "21035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac"
          )
        , ( "0000000000000000000000000000000000000000000000000000000000000200"
          , "00000000"
          , "21035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac"
          )
        ]
      , "01000000020001000000000000000000000000000000000000000000000000000000000000000000004948304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df101010000000002000000000000000000000000000000000000000000000000000000000000000000004847304402205f7530653eea9b38699e476320ab135b74771e1c48b81a5d041e2ca84b9be7a802200ac8d1f40fb026674fe5a5edd3dea715c27baa9baca51ed45ea750ac9dc0a55e81ffffffff010100000000000000015100000000"
      )
      -- several SIGHASH_SINGLE signatures
    , ( [
          ( "63cfa5a09dc540bf63e53713b82d9ea3692ca97cd608c384f2aa88e51a0aac70"
          , "00000000"
          , "76a914dcf72c4fd02f5a987cf9b02f2fabfcac3341a87d88ac"
          )
        , ( "04e8d0fcf3846c6734477b98f0f3d4badfb78f020ee097a0be5fe347645b817d"
          , "01000000"
          , "76a914dcf72c4fd02f5a987cf9b02f2fabfcac3341a87d88ac"
          )
        , ( "ee1377aff5d0579909e11782e1d2f5f7b84d26537be7f5516dd4e43373091f3f"
          , "01000000"
          , "76a914dcf72c4fd02f5a987cf9b02f2fabfcac3341a87d88ac"
          )
        ]
      , "010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000006a4730440220360d20baff382059040ba9be98947fd678fb08aab2bb0c172efa996fd8ece9b702201b4fb0de67f015c90e7ac8a193aeab486a1f587e0f54d0fb9552ef7f5ce6caec032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e804010000006b483045022100c714310be1e3a9ff1c5f7cacc65c2d8e781fc3a88ceb063c6153bf950650802102200b2d0979c76e12bb480da635f192cc8dc6f905380dd4ac1ff35a4f68f462fffd032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff3f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000006c493046022100b663499ef73273a3788dea342717c2640ac43c5a1cf862c9e09b206fcb3f6bb8022100b09972e75972d9148f2bdd462e5cb69b57c1214b88fc55ca638676c07cfc10d8032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff0380841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac80841e00000000001976a9149857cc07bed33a5cf12b9c5e0500b675d500c81188ace0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac00000000"
      )
    ]

