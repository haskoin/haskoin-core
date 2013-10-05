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
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Tx
import Haskoin.Wallet.ScriptParser
import Haskoin.Protocol
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
    , testGroup "Build Transactions" 
        [ testCase "Build PKHash Tx 1" buildPKHashTx1
        , testCase "Build PKHash Tx 2" buildPKHashTx2
        , testCase "Build PKHash Tx 3" buildPKHashTx3
        , testCase "Build PKHash Tx 4" buildPKHashTx4
        ] 
    , testGroup "Canonical Signatures" 
        [ testCase "Canonical Sig 1" testCanonicalSig1
        , testCase "Canonical Sig 2" testCanonicalSig2
        , testCase "Canonical Sig 3" testCanonicalSig3
        , testCase "Canonical Sig 4" testCanonicalSig4
        , testCase "Canonical Sig 5" testCanonicalSig5
        , testCase "Not Canonical Sig 1" testNotCanonical1
        , testCase "Not Canonical Sig 2" testNotCanonical2
        , testCase "Not Canonical Sig 3" testNotCanonical3
        , testCase "Not Canonical Sig 4" testNotCanonical4
        , testCase "Not Canonical Sig 5" testNotCanonical5
        , testCase "Not Canonical Sig 6" testNotCanonical6
        , testCase "Not Canonical Sig 7" testNotCanonical7
        , testCase "Not Canonical Sig 8" testNotCanonical8
        , testCase "Not Canonical Sig 9" testNotCanonical9
        , testCase "Not Canonical Sig 10" testNotCanonical10
        , testCase "Not Canonical Sig 11" testNotCanonical11
        , testCase "Not Canonical Sig 12" testNotCanonical12
        , testCase "Not Canonical Sig 13" testNotCanonical13
        , testCase "Not Canonical Sig 14" testNotCanonical14
        , testCase "Not Canonical Sig 15" testNotCanonical15
        ] 
    ]

v1c1 = do
    assertBool "xPrvID" $
        xPrvID m1 == 0x3442193e1bb70916e914552172cd4e2dbc9df811
    assertBool "xPrvFP" $
        xPrvFP m1 == 0x3442193e
    assertBool "xPrvAddr" $
        (addrToBase58 $ xPubAddr $ deriveXPubKey m1) == 
        "15mKKb2eos1hWa6tisdPwwDC1a5J1y9nma"
    assertBool "prvKey" $
        bsToInteger (toStrictBS $ runPut $ putPrvKey $ xPrvKey m1) ==
        0xe8f32e723decf4051aefac8e2c93c9c5b214313817cdb01a1494b917c8436b35
    assertBool "xPrvWIF" $
        xPrvWIF m1 == 
        "L52XzL2cMkHxqxBXRyEpnPQZGUs3uKiL3R11XbAdHigRzDozKZeW"
    assertBool "pubKey" $
        bsToInteger (encode' $ xPubKey $ deriveXPubKey m1) ==
        0x0339a36013301597daef41fbe593a02cc513d0b55527ec2df1050e2e8ff49c85c2
    assertBool "chain code" $
        bsToInteger (encode' $ xPrvChain m1) ==
        0x873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d508
    assertBool "Base58 PrvKey" $
        xPrvExport m1 ==
        "xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi"
    assertBool "Base58 PubKey" $
        (xPubExport $ deriveXPubKey m1) ==
        "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8"

v1c2 = do
    -- m/0'
    let m' = fromJust $ primeSubKey m1 0
    assertBool "xPrvID" $
        xPrvID m' == 0x5c1bd648ed23aa5fd50ba52b2457c11e9e80a6a7
    assertBool "xPrvFP" $
        xPrvFP m' == 0x5c1bd648
    assertBool "xPrvAddr" $
        (addrToBase58 $ xPubAddr $ deriveXPubKey m') == 
        "19Q2WoS5hSS6T8GjhK8KZLMgmWaq4neXrh"
    assertBool "prvKey" $
        bsToInteger (toStrictBS $ runPut $ putPrvKey $ xPrvKey m') ==
        0xedb2e14f9ee77d26dd93b4ecede8d16ed408ce149b6cd80b0715a2d911a0afea
    assertBool "xPrvWIF" $
        xPrvWIF m' == 
        "L5BmPijJjrKbiUfG4zbiFKNqkvuJ8usooJmzuD7Z8dkRoTThYnAT"
    assertBool "pubKey" $
        bsToInteger (encode' $ xPubKey $ deriveXPubKey m') ==
        0x035a784662a4a20a65bf6aab9ae98a6c068a81c52e4b032c0fb5400c706cfccc56
    assertBool "chain code" $
        bsToInteger (encode' $ xPrvChain m') ==
        0x47fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae6236141
    assertBool "Base58 PrvKey" $
        xPrvExport m' ==
        "xprv9uHRZZhk6KAJC1avXpDAp4MDc3sQKNxDiPvvkX8Br5ngLNv1TxvUxt4cV1rGL5hj6KCesnDYUhd7oWgT11eZG7XnxHrnYeSvkzY7d2bhkJ7"
    assertBool "Base58 PubKey" $
        (xPubExport $ deriveXPubKey m') ==
        "xpub68Gmy5EdvgibQVfPdqkBBCHxA5htiqg55crXYuXoQRKfDBFA1WEjWgP6LHhwBZeNK1VTsfTFUHCdrfp1bgwQ9xv5ski8PX9rL2dZXvgGDnw"

v1c3 = do
    -- m/0'/1
    let m' = fromJust $ prvSubKey (fromJust $ primeSubKey m1 0) 1
    assertBool "xPrvID" $
        xPrvID m' == 0xbef5a2f9a56a94aab12459f72ad9cf8cf19c7bbe
    assertBool "xPrvFP" $
        xPrvFP m' == 0xbef5a2f9
    assertBool "xPrvAddr" $
        (addrToBase58 $ xPubAddr $ deriveXPubKey m') == 
        "1JQheacLPdM5ySCkrZkV66G2ApAXe1mqLj"
    assertBool "prvKey" $
        bsToInteger (toStrictBS $ runPut $ putPrvKey $ xPrvKey m') ==
        0x3c6cb8d0f6a264c91ea8b5030fadaa8e538b020f0a387421a12de9319dc93368
    assertBool "xPrvWIF" $
        xPrvWIF m' == 
        "KyFAjQ5rgrKvhXvNMtFB5PCSKUYD1yyPEe3xr3T34TZSUHycXtMM"
    assertBool "pubKey" $
        bsToInteger (encode' $ xPubKey $ deriveXPubKey m') ==
        0x03501e454bf00751f24b1b489aa925215d66af2234e3891c3b21a52bedb3cd711c
    assertBool "chain code" $
        bsToInteger (encode' $ xPrvChain m') ==
        0x2a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19
    assertBool "Base58 PrvKey" $
        xPrvExport m' ==
        "xprv9wTYmMFdV23N2TdNG573QoEsfRrWKQgWeibmLntzniatZvR9BmLnvSxqu53Kw1UmYPxLgboyZQaXwTCg8MSY3H2EU4pWcQDnRnrVA1xe8fs"
    assertBool "Base58 PubKey" $
        (xPubExport $ deriveXPubKey m') ==
        "xpub6ASuArnXKPbfEwhqN6e3mwBcDTgzisQN1wXN9BJcM47sSikHjJf3UFHKkNAWbWMiGj7Wf5uMash7SyYq527Hqck2AxYysAA7xmALppuCkwQ"

flipEndian :: Hash256 -> Hash256
flipEndian = decode' . BS.reverse . encode'

-- These test vectors have been generated from bitcoind raw transaction api

buildPKHashTx1 =
    assertBool "Build TX 1" $ (bsToHex $ encode' tx) == bitcoindTx
    where tx = fromJust $ buildPKHashTx 
                      [OutPoint prevId 14] 
                      [(toAddr,90000000)]
          prevId = flipEndian 
            0xeb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db
          toAddr = "14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb"
          bitcoindTx = stringToBS "0100000001db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0e00000000ffffffff01804a5d05000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac00000000"

buildPKHashTx2 =
    assertBool "Build TX 2" $ (bsToHex $ encode' tx) == bitcoindTx
    where tx = fromJust $ buildPKHashTx 
                   [OutPoint prevId1 0, OutPoint prevId2 2147483647] 
                   [(toAddr1,1),(toAddr2,2100000000000000)]
          prevId1 = flipEndian 
            0xeb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db
          prevId2 = flipEndian 
            0x01000000000000000000000000000000000000000000000000000000000000
          toAddr1 = "14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb"
          toAddr2 = "19VCgS642vzEA1sdByoSn6GsWBwraV8D4n"
          bitcoindTx = stringToBS "0100000002db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0000000000ffffffff0000000000000000000000000000000000000000000000000000000000000100ffffff7f00ffffffff0201000000000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac0040075af07507001976a9145d16672f53981ff21c5f42b40d1954993cbca54f88ac00000000"

buildPKHashTx3 =
    assertBool "Build TX 3" $ (bsToHex $ encode' tx) == bitcoindTx
    where tx = fromJust $ buildPKHashTx 
                   [OutPoint prevId1 0, OutPoint prevId2 2147483647] 
                   []
          prevId1 = flipEndian 
            0xeb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db
          prevId2 = flipEndian 
            0x01000000000000000000000000000000000000000000000000000000000000
          bitcoindTx = stringToBS "0100000002db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0000000000ffffffff0000000000000000000000000000000000000000000000000000000000000100ffffff7f00ffffffff0000000000"

buildPKHashTx4 =
    assertBool "Build TX 4" $ (bsToHex $ encode' tx) == bitcoindTx
    where tx = fromJust $ buildPKHashTx 
                   [] 
                   [(toAddr1,1),(toAddr2,2100000000000000)]
          toAddr1 = "14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb"
          toAddr2 = "19VCgS642vzEA1sdByoSn6GsWBwraV8D4n"
          bitcoindTx = stringToBS "01000000000201000000000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac0040075af07507001976a9145d16672f53981ff21c5f42b40d1954993cbca54f88ac00000000"


{- Canonical Signatures -}

-- Test vectors from bitcoind
-- http://github.com/bitcoin/bitcoin/blob/master/src/test/data/sig_canonical.json

testCanonicalSig1 = assertBool "Canonical Sig1" $ 
    isCanonicalSig $ fromJust $ hexToBS $ stringToBS 
    "300602010002010001"

testCanonicalSig2 = assertBool "Canonical Sig2" $ 
    isCanonicalSig $ fromJust $ hexToBS $ stringToBS 
    "3008020200ff020200ff01"

testCanonicalSig3 = assertBool "Canonical Sig3" $ 
    isCanonicalSig $ fromJust $ hexToBS $ stringToBS 
    "304402203932c892e2e550f3af8ee4ce9c215a87f9bb831dcac87b2838e2c2eaa891df0c022030b61dd36543125d56b9f9f3a1f9353189e5af33cdda8d77a5209aec03978fa001"

testCanonicalSig4 = assertBool "Canonical Sig4" $ 
    isCanonicalSig $ fromJust $ hexToBS $ stringToBS 
    "30450220076045be6f9eca28ff1ec606b833d0b87e70b2a630f5e3a496b110967a40f90a0221008fffd599910eefe00bc803c688c2eca1d2ba7f6b180620eaa03488e6585db6ba01"

testCanonicalSig5 = assertBool "Canonical Sig5" $ 
    isCanonicalSig $ fromJust $ hexToBS $ stringToBS 
    "3046022100876045be6f9eca28ff1ec606b833d0b87e70b2a630f5e3a496b110967a40f90a0221008fffd599910eefe00bc803c688c2eca1d2ba7f6b180620eaa03488e6585db6ba01"

testNotCanonical1 = assertBool "Too short" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "30050201ff020001"

testNotCanonical2 = assertBool "Too long" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "30470221005990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba6105022200002d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical3 = assertBool "Hashtype" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed11"

testNotCanonical4 = assertBool "Type" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "314402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical5 = assertBool "Total length" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical6 = assertBool "S length OOB" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "301f01205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb101"

testNotCanonical7 = assertBool "R+S" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed0001"

testNotCanonical8 = assertBool "R type" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304401205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical9 = assertBool "R len = 0" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "3024020002202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical10 = assertBool "R < -" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304402208990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical11 = assertBool "R padded" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "30450221005990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical12 = assertBool "S type" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610501202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical13 = assertBool "S len = 0" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "302402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba6105020001"

testNotCanonical14 = assertBool "S < 0" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba61050220fd5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical15 = assertBool "S padded" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba61050221002d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"


