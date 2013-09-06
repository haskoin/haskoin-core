module Units (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Wallet.Keys
import Haskoin.Crypto
import Haskoin.Util

-- BIP 0032 Test Vectors
-- https://en.bitcoin.it/wiki/BIP_0032_TestVectors

m1 :: XPrvKey
m1 = fromJust $ makeXPrvKey $ BS.pack [0..15]

tests =
    [ testGroup "Test vector 1" 
        [ testCase "Chain m" v1c1
        , testCase "Chain m/0'" v1c2
        , testCase "Chain m/0'/1" v1c3
        ] 
    ]

v1c1 = do
    assertBool "xPrvID" $
        xPrvID m1 == 0x3442193e1bb70916e914552172cd4e2dbc9df811
    assertBool "xPrvFP" $
        xPrvFP m1 == 0x3442193e
    assertBool "xPrvAddr" $
        bsToString (xPrvAddr m1) == "15mKKb2eos1hWa6tisdPwwDC1a5J1y9nma"
    assertBool "prvKey" $
        bsToInteger (toStrictBS $ runPut $ putPrvKey $ xPrvKey m1) ==
        0xe8f32e723decf4051aefac8e2c93c9c5b214313817cdb01a1494b917c8436b35
    assertBool "xPrvWIF" $
        bsToString (xPrvWIF m1) == 
        "L52XzL2cMkHxqxBXRyEpnPQZGUs3uKiL3R11XbAdHigRzDozKZeW"
    assertBool "pubKey" $
        bsToInteger (encode' $ xPubKey $ deriveXPubKey m1) ==
        0x0339a36013301597daef41fbe593a02cc513d0b55527ec2df1050e2e8ff49c85c2
    assertBool "chain code" $
        bsToInteger (encode' $ xPrvChain m1) ==
        0x873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d508
    assertBool "Base58 PrvKey" $
        bsToString (xPrvExport m1) ==
        ("xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqj"
          ++ "iChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi")
    assertBool "Base58 PubKey" $
        bsToString (xPubExport $ deriveXPubKey m1) ==
        ("xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY"
          ++ "2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8")

v1c2 = do
    -- m/0'
    let m' = fromJust $ prvSubKey' m1 0
    assertBool "xPrvID" $
        xPrvID m' == 0x5c1bd648ed23aa5fd50ba52b2457c11e9e80a6a7
    assertBool "xPrvFP" $
        xPrvFP m' == 0x5c1bd648
    assertBool "xPrvAddr" $
        bsToString (xPrvAddr m') == "19Q2WoS5hSS6T8GjhK8KZLMgmWaq4neXrh"
    assertBool "prvKey" $
        bsToInteger (toStrictBS $ runPut $ putPrvKey $ xPrvKey m') ==
        0xedb2e14f9ee77d26dd93b4ecede8d16ed408ce149b6cd80b0715a2d911a0afea
    assertBool "xPrvWIF" $
        bsToString (xPrvWIF m') == 
        "L5BmPijJjrKbiUfG4zbiFKNqkvuJ8usooJmzuD7Z8dkRoTThYnAT"
    assertBool "pubKey" $
        bsToInteger (encode' $ xPubKey $ deriveXPubKey m') ==
        0x035a784662a4a20a65bf6aab9ae98a6c068a81c52e4b032c0fb5400c706cfccc56
    assertBool "chain code" $
        bsToInteger (encode' $ xPrvChain m') ==
        0x47fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae6236141
    assertBool "Base58 PrvKey" $
        bsToString (xPrvExport m') ==
        ("xprv9uHRZZhk6KAJC1avXpDAp4MDc3sQKNxDiPvvkX8Br5ngLNv1Txv" 
        ++ "Uxt4cV1rGL5hj6KCesnDYUhd7oWgT11eZG7XnxHrnYeSvkzY7d2bhkJ7")
    assertBool "Base58 PubKey" $
        bsToString (xPubExport $ deriveXPubKey m') ==
        ("xpub68Gmy5EdvgibQVfPdqkBBCHxA5htiqg55crXYuXoQRKfDBFA1WEj" 
        ++ "WgP6LHhwBZeNK1VTsfTFUHCdrfp1bgwQ9xv5ski8PX9rL2dZXvgGDnw")

v1c3 = do
    -- m/0'/1
    let m' = fromJust $ prvSubKey (fromJust $ prvSubKey' m1 0) 1
    assertBool "xPrvID" $
        xPrvID m' == 0xbef5a2f9a56a94aab12459f72ad9cf8cf19c7bbe
    assertBool "xPrvFP" $
        xPrvFP m' == 0xbef5a2f9
    assertBool "xPrvAddr" $
        bsToString (xPrvAddr m') == "1JQheacLPdM5ySCkrZkV66G2ApAXe1mqLj"
    assertBool "prvKey" $
        bsToInteger (toStrictBS $ runPut $ putPrvKey $ xPrvKey m') ==
        0x3c6cb8d0f6a264c91ea8b5030fadaa8e538b020f0a387421a12de9319dc93368
    assertBool "xPrvWIF" $
        bsToString (xPrvWIF m') == 
        "KyFAjQ5rgrKvhXvNMtFB5PCSKUYD1yyPEe3xr3T34TZSUHycXtMM"
    assertBool "pubKey" $
        bsToInteger (encode' $ xPubKey $ deriveXPubKey m') ==
        0x03501e454bf00751f24b1b489aa925215d66af2234e3891c3b21a52bedb3cd711c
    assertBool "chain code" $
        bsToInteger (encode' $ xPrvChain m') ==
        0x2a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19
    assertBool "Base58 PrvKey" $
        bsToString (xPrvExport m') ==
        ("xprv9wTYmMFdV23N2TdNG573QoEsfRrWKQgWeibmLntzniatZvR9Bm" 
        ++ "LnvSxqu53Kw1UmYPxLgboyZQaXwTCg8MSY3H2EU4pWcQDnRnrVA1xe8fs")
    assertBool "Base58 PubKey" $
        bsToString (xPubExport $ deriveXPubKey m') ==
        ("xpub6ASuArnXKPbfEwhqN6e3mwBcDTgzisQN1wXN9BJcM47sSikHjJ" 
        ++ "f3UFHKkNAWbWMiGj7Wf5uMash7SyYq527Hqck2AxYysAA7xmALppuCkwQ")

