{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Keys.ExtendedSpec (spec) where

import Bitcoin.Address (addrToText)
import Bitcoin.Constants (Network, btc)
import Bitcoin.Keys (
    DerivPath,
    DerivPathI (Deriv, (:/), (:|)),
    ParsedPath (getParsedPath),
    SoftPath,
    XKey (XPrv, XPub),
    XPrvKey (xPrvChain, xPrvKey),
    XPubKey (xPubKey),
    applyPath,
    derivePath,
    derivePubPath,
    deriveXPubKey,
    exportPubKey,
    getSecKey,
    getXPrvKey,
    getXPubKey,
    hardSubKey,
    listToPath,
    makeXPrvKey,
    parsePath,
    pathToList,
    pathToStr,
    prvSubKey,
    pubSubKey,
    putXPrvKey,
    putXPubKey,
    toHard,
    toSoft,
    xPrvExport,
    xPrvFP,
    xPrvID,
    xPrvImport,
    xPrvWif,
    xPubAddr,
    xPubExport,
    xPubImport,
 )
import Bitcoin.Orphans ()
import Bitcoin.Util (decodeHex, encodeHex)
import qualified Bitcoin.Util as U
import Bitcoin.Util.Arbitrary (
    arbitraryBip32PathIndex,
    arbitraryDerivPath,
    arbitraryHardPath,
    arbitraryNetwork,
    arbitraryParsedPath,
    arbitrarySoftPath,
    arbitraryXPrvKey,
    arbitraryXPubKey,
    genNetData,
 )
import Bitcoin.UtilSpec (JsonBox (..), NetBox (..), ReadBox (..), SerialBox (..), testIdentity)
import Control.Monad (forM_)
import Data.Aeson as A (
    Encoding,
    Value (String),
    decode,
    encode,
    withText,
 )
import Data.Aeson.Encoding (text)
import Data.Aeson.Types (Parser)
import Data.Binary.Put (runPut)
import Data.Bits ((.&.))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Either (isLeft)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.String (fromString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32)
import Test.HUnit (Assertion, assertBool, assertEqual)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll)


serialVals :: [SerialBox]
serialVals =
    [ SerialBox arbitraryDerivPath
    , SerialBox arbitraryHardPath
    , SerialBox arbitrarySoftPath
    ]


readVals :: [ReadBox]
readVals =
    [ ReadBox arbitraryDerivPath
    , ReadBox arbitraryHardPath
    , ReadBox arbitrarySoftPath
    , ReadBox arbitraryXPrvKey
    , ReadBox (snd <$> arbitraryXPubKey)
    , ReadBox arbitraryParsedPath
    , ReadBox arbitraryBip32PathIndex
    ]


jsonVals :: [JsonBox]
jsonVals =
    [ JsonBox arbitraryDerivPath
    , JsonBox arbitraryHardPath
    , JsonBox arbitrarySoftPath
    , JsonBox arbitraryParsedPath
    ]


netVals :: [NetBox]
netVals =
    [ NetBox
        ( xPrvToJSON
        , xPrvToEncoding
        , xPrvFromJSON
        , genNetData arbitraryXPrvKey
        )
    , NetBox
        ( xPubToJSON
        , xPubToEncoding
        , xPubFromJSON
        , genNetData (snd <$> arbitraryXPubKey)
        )
    ]
  where
    xPrvToJSON :: Network -> XPrvKey -> Value
    xPrvToJSON net = A.String . xPrvExport net
    xPrvToEncoding :: Network -> XPrvKey -> Encoding
    xPrvToEncoding net = text . xPrvExport net
    xPrvFromJSON :: Network -> Value -> Parser XPrvKey
    xPrvFromJSON net =
        withText "xprv" $ \t ->
            case xPrvImport net t of
                Nothing -> fail "could not read xprv"
                Just x -> return x
    xPubFromJSON :: Network -> Value -> Parser XPubKey
    xPubFromJSON net =
        withText "xpub" $ \t ->
            case xPubImport net t of
                Nothing -> fail "could not read xpub"
                Just x -> return x
    xPubToJSON :: Network -> XPubKey -> Value
    xPubToJSON net = A.String . xPubExport net
    xPubToEncoding :: Network -> XPubKey -> Encoding
    xPubToEncoding net = text . xPubExport net


spec :: Spec
spec = do
    testIdentity serialVals readVals jsonVals netVals
    describe "Custom identity tests" $ do
        prop "encodes and decodes extended private key" $
            forAll arbitraryNetwork $ \net ->
                forAll arbitraryXPrvKey $ \x ->
                    (U.runGet (getXPrvKey net) . runPut) (putXPrvKey net x) == Right x
        prop "encodes and decodes extended public key" $
            forAll arbitraryNetwork $ \net ->
                forAll arbitraryXPubKey $ \(_, x) ->
                    (U.runGet (getXPubKey net) . runPut) (putXPubKey net x) == Right x
    describe "bip32 subkey derivation vector 1" $ vectorSpec m1 vector1
    describe "bip32 subkey derivation vector 2" $ vectorSpec m2 vector2
    describe "bip32 subkey derivation vector 3" $ vectorSpec m3 vector3
    describe "bip32 subkey derivation using string path" $ do
        it "either derivations" testApplyPath
        it "either derivations" testBadApplyPath
        it "dublic derivations" testDerivePubPath
        it "private derivations" testDerivePrvPath
        it "path parsing" testParsePath
        it "from json" testFromJsonPath
        it "to json" testToJsonPath
    describe "Derivation Paths" $ do
        prop "from string derivation path" $
            forAll arbitraryDerivPath $
                \p -> fromString (cs $ pathToStr p) == p
        prop "from string hard derivation path" $
            forAll arbitraryHardPath $
                \p -> fromString (cs $ pathToStr p) == p
        prop "from string soft derivation path" $
            forAll arbitrarySoftPath $
                \p -> fromString (cs $ pathToStr p) == p
        prop "from and to lists of derivation paths" $
            forAll arbitraryDerivPath $
                \p -> listToPath (pathToList p) == p
        prop "from and to lists of hard derivation paths" $
            forAll arbitraryHardPath $ \p ->
                toHard (listToPath $ pathToList p) == Just p
        prop "from and to lists of soft derivation paths" $
            forAll arbitrarySoftPath $ \p ->
                toSoft (listToPath $ pathToList p) == Just p
    describe "Extended Keys" $ do
        let net = btc
        prop "computes pubkey of a subkey is subkey of the pubkey" $
            forAll arbitraryXPrvKey pubKeyOfSubKeyIsSubKeyOfPubKey
        prop "exports and imports extended private key" $
            forAll arbitraryXPrvKey $ \k ->
                xPrvImport net (xPrvExport net k) == Just k
        prop "exports and imports extended public key" $
            forAll arbitraryXPubKey $ \(_, k) ->
                xPubImport net (xPubExport net k) == Just k


pubKeyOfSubKeyIsSubKeyOfPubKey :: XPrvKey -> Word32 -> Bool
pubKeyOfSubKeyIsSubKeyOfPubKey k i =
    deriveXPubKey (prvSubKey k i') == pubSubKey (deriveXPubKey k) i'
  where
    i' = fromIntegral $ i .&. 0x7fffffff -- make it a public derivation


testFromJsonPath :: Assertion
testFromJsonPath =
    sequence_ $ do
        path <- jsonPathVectors
        return $
            assertEqual
                path
                (Just [fromString path :: DerivPath])
                (A.decode $ B8.pack $ "[\"" ++ path ++ "\"]")


testToJsonPath :: Assertion
testToJsonPath =
    sequence_ $ do
        path <- jsonPathVectors
        return $
            assertEqual
                path
                (B8.pack $ "[\"" ++ path ++ "\"]")
                (A.encode [fromString path :: ParsedPath])


jsonPathVectors :: [String]
jsonPathVectors =
    [ "m"
    , "m/0"
    , "m/0'"
    , "M/0'"
    , "m/2147483647"
    , "M/2147483647"
    , "m/1/2/3/4/5/6/7/8"
    , "M/1/2/3/4/5/6/7/8"
    , "m/1'/2'/3/4"
    , "M/1'/2'/3/4"
    ]


testParsePath :: Assertion
testParsePath =
    sequence_ $ do
        (path, t) <- parsePathVectors
        return $ assertBool path (t $ parsePath path)


parsePathVectors :: [(String, Maybe ParsedPath -> Bool)]
parsePathVectors =
    [ ("m", isJust)
    , ("m/0'", isJust)
    , ("M/0'", isJust)
    , ("m/2147483648", isNothing)
    , ("m/2147483647", isJust)
    , ("M/2147483648", isNothing)
    , ("M/2147483647", isJust)
    , ("M/-1", isNothing)
    , ("M/-2147483648", isNothing)
    , ("m/1/2/3/4/5/6/7/8", isJust)
    , ("M/1/2/3/4/5/6/7/8", isJust)
    , ("m/1'/2'/3/4", isJust)
    , ("M/1'/2'/3/4", isJust)
    , ("m/1/2'/3/4'", isJust)
    , ("M/1/2'/3/4'", isJust)
    , ("meh", isNothing)
    , ("infinity", isNothing)
    , ("NaN", isNothing)
    ]


testApplyPath :: Assertion
testApplyPath =
    sequence_ $ do
        (key, path, final) <- applyPathVectors
        return $
            assertEqual path final $
                applyPath (fromJust $ parsePath path) key


testBadApplyPath :: Assertion
testBadApplyPath =
    sequence_ $ do
        (key, path) <- badApplyPathVectors
        return $
            assertBool path $
                isLeft $
                    applyPath (fromJust $ parsePath path) key


testDerivePubPath :: Assertion
testDerivePubPath =
    sequence_ $ do
        (key, path, final) <- derivePubPathVectors
        return $
            assertEqual path final $
                derivePubPath (fromString path :: SoftPath) key


testDerivePrvPath :: Assertion
testDerivePrvPath =
    sequence_ $ do
        (key, path, final) <- derivePrvPathVectors
        return $
            assertEqual path final $
                derivePath (fromString path :: DerivPath) key


derivePubPathVectors :: [(XPubKey, String, XPubKey)]
derivePubPathVectors =
    [ (xpub, "M", xpub)
    , (xpub, "M/8", pubSubKey xpub 8)
    , (xpub, "M/8/30/1", foldl pubSubKey xpub [8, 30, 1])
    ]
  where
    xprv =
        fromJust $
            xPrvImport
                btc
                "xprv9s21ZrQH143K46iDVRSyFfGfMgQjzC4BV3ZUfNbG7PHQrJjE53ofAn5gYkp6KQ\
                \WzGmb8oageSRxBY8s4rjr9VXPVp2HQDbwPt4H31Gg4LpB"
    xpub = deriveXPubKey xprv


derivePrvPathVectors :: [(XPrvKey, String, XPrvKey)]
derivePrvPathVectors =
    [ (xprv, "m", xprv)
    , (xprv, "M", xprv)
    , (xprv, "m/8'", hardSubKey xprv 8)
    , (xprv, "M/8'", hardSubKey xprv 8)
    ,
        ( xprv
        , "m/8'/30/1"
        , foldl prvSubKey (hardSubKey xprv 8) [30, 1]
        )
    ,
        ( xprv
        , "M/8'/30/1"
        , foldl prvSubKey (hardSubKey xprv 8) [30, 1]
        )
    ,
        ( xprv
        , "m/3/20"
        , foldl prvSubKey xprv [3, 20]
        )
    ,
        ( xprv
        , "M/3/20"
        , foldl prvSubKey xprv [3, 20]
        )
    ]
  where
    xprv =
        fromJust $
            xPrvImport
                btc
                "xprv9s21ZrQH143K46iDVRSyFfGfMgQjzC4BV3ZUfNbG7PHQrJjE53ofAn5gYkp6KQ\
                \WzGmb8oageSRxBY8s4rjr9VXPVp2HQDbwPt4H31Gg4LpB"


applyPathVectors :: [(XKey, String, Either String XKey)]
applyPathVectors =
    [ (XPrv xprv btc, "m", Right (XPrv xprv btc))
    , (XPrv xprv btc, "M", Right (XPub xpub btc))
    , (XPrv xprv btc, "m/8'", Right (XPrv (hardSubKey xprv 8) btc))
    ,
        ( XPrv xprv btc
        , "M/8'"
        , Right (XPub (deriveXPubKey (hardSubKey xprv 8)) btc)
        )
    ,
        ( XPrv xprv btc
        , "m/8'/30/1"
        , Right (XPrv (foldl prvSubKey (hardSubKey xprv 8) [30, 1]) btc)
        )
    ,
        ( XPrv xprv btc
        , "M/8'/30/1"
        , Right
            ( XPub
                (deriveXPubKey (foldl prvSubKey (hardSubKey xprv 8) [30, 1]))
                btc
            )
        )
    , (XPrv xprv btc, "m/3/20", Right (XPrv (foldl prvSubKey xprv [3, 20]) btc))
    ,
        ( XPrv xprv btc
        , "M/3/20"
        , Right (XPub (deriveXPubKey (foldl prvSubKey xprv [3, 20])) btc)
        )
    ,
        ( XPub xpub btc
        , "M/3/20"
        , Right (XPub (deriveXPubKey (foldl prvSubKey xprv [3, 20])) btc)
        )
    ]
  where
    xprv =
        fromJust $
            xPrvImport
                btc
                "xprv9s21ZrQH143K46iDVRSyFfGfMgQjzC4BV3ZUfNbG7PHQrJjE53ofAn5gYkp6KQ\
                \WzGmb8oageSRxBY8s4rjr9VXPVp2HQDbwPt4H31Gg4LpB"
    xpub = deriveXPubKey xprv


badApplyPathVectors :: [(XKey, String)]
badApplyPathVectors =
    [ (XPub xpub btc, "m/8'")
    , (XPub xpub btc, "M/8'")
    , (XPub xpub btc, "M/1/2/3'/4/5")
    ]
  where
    xprv =
        fromJust $
            xPrvImport
                btc
                "xprv9s21ZrQH143K46iDVRSyFfGfMgQjzC4BV3ZUfNbG7PHQrJjE53ofAn5gYkp6KQ\
                \WzGmb8oageSRxBY8s4rjr9VXPVp2HQDbwPt4H31Gg4LpB"
    xpub = deriveXPubKey xprv


-- BIP 0032 Test Vectors
-- https://en.bitcoin.it/wiki/BIP_0032_TestVectors

bip44Addr :: DerivPath
bip44Addr = Deriv :| 44 :| 0 :| 0 :/ 0 :/ 0


vectorSpec :: TestKey -> [TestVector] -> Spec
vectorSpec mTxt vecTxt =
    forM_ (parseVector mTxt vecTxt) $ \(d, m, v) ->
        it ("chain " <> cs d) $ runVector m v


runVector :: XPrvKey -> TestVector -> Assertion
runVector m v = do
    assertBool "xPrvID" $ encodeHex (U.encodeS $ xPrvID m) == v !! 0
    assertBool "xPrvFP" $ encodeHex (U.encodeS $ xPrvFP m) == v !! 1
    assertBool "xPrvAddr" $
        addrToText btc (xPubAddr $ deriveXPubKey m) == Just (v !! 2)
    assertBool "bip44Addr" $
        addrToText btc (xPubAddr $ deriveXPubKey $ derivePath bip44Addr m)
            == Just (v !! 3)
    assertBool "prvKey" $ encodeHex (getSecKey $ xPrvKey m) == v !! 4
    assertBool "xPrvWIF" $ xPrvWif btc m == v !! 5
    assertBool "pubKey" $
        encodeHex (exportPubKey True $ xPubKey $ deriveXPubKey m) == v !! 6
    assertBool "chain code" $ encodeHex (U.encodeS $ xPrvChain m) == v !! 7
    assertBool "Hex PubKey" $
        (encodeHex . BSL.toStrict . runPut . putXPubKey btc) (deriveXPubKey m) == v !! 8
    assertBool "Hex PrvKey" $ (encodeHex . BSL.toStrict . runPut . putXPrvKey btc) m == v !! 9
    assertBool "Base58 PubKey" $ xPubExport btc (deriveXPubKey m) == v !! 10
    assertBool "Base58 PrvKey" $ xPrvExport btc m == v !! 11


parseVector :: TestKey -> [TestVector] -> [(Text, XPrvKey, TestVector)]
parseVector mTxt vs =
    go <$> vs
  where
    mast = makeXPrvKey $ fromJust $ decodeHex mTxt
    go (d : vec) =
        let deriv = getParsedPath $ fromJust $ parsePath $ cs d
         in (d, derivePath deriv mast, vec)
    go _ = undefined


type TestVector = [Text]


type TestKey = Text


m1 :: TestKey
m1 = "000102030405060708090a0b0c0d0e0f"


vector1 :: [TestVector]
vector1 =
    [
        [ "m"
        , "3442193e1bb70916e914552172cd4e2dbc9df811"
        , "3442193e"
        , "15mKKb2eos1hWa6tisdPwwDC1a5J1y9nma"
        , "1NQpH6Nf8QtR2HphLRcvuVqfhXBXsiWn8r"
        , "e8f32e723decf4051aefac8e2c93c9c5b214313817cdb01a1494b917c8436b35"
        , "L52XzL2cMkHxqxBXRyEpnPQZGUs3uKiL3R11XbAdHigRzDozKZeW"
        , "0339a36013301597daef41fbe593a02cc513d0b55527ec2df1050e2e8ff49c85c2"
        , "873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d508"
        , "0488b21e000000000000000000873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d5080339a36013301597daef41fbe593a02cc513d0b55527ec2df1050e2e8ff49c85c2"
        , "0488ade4000000000000000000873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d50800e8f32e723decf4051aefac8e2c93c9c5b214313817cdb01a1494b917c8436b35"
        , "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8"
        , "xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi"
        ]
    ,
        [ "m/0'"
        , "5c1bd648ed23aa5fd50ba52b2457c11e9e80a6a7"
        , "5c1bd648"
        , "19Q2WoS5hSS6T8GjhK8KZLMgmWaq4neXrh"
        , "1DDVw6BRKUv9U8Hzg5rGsia13nDrgJQpBd"
        , "edb2e14f9ee77d26dd93b4ecede8d16ed408ce149b6cd80b0715a2d911a0afea"
        , "L5BmPijJjrKbiUfG4zbiFKNqkvuJ8usooJmzuD7Z8dkRoTThYnAT"
        , "035a784662a4a20a65bf6aab9ae98a6c068a81c52e4b032c0fb5400c706cfccc56"
        , "47fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae6236141"
        , "0488b21e013442193e8000000047fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae6236141035a784662a4a20a65bf6aab9ae98a6c068a81c52e4b032c0fb5400c706cfccc56"
        , "0488ade4013442193e8000000047fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae623614100edb2e14f9ee77d26dd93b4ecede8d16ed408ce149b6cd80b0715a2d911a0afea"
        , "xpub68Gmy5EdvgibQVfPdqkBBCHxA5htiqg55crXYuXoQRKfDBFA1WEjWgP6LHhwBZeNK1VTsfTFUHCdrfp1bgwQ9xv5ski8PX9rL2dZXvgGDnw"
        , "xprv9uHRZZhk6KAJC1avXpDAp4MDc3sQKNxDiPvvkX8Br5ngLNv1TxvUxt4cV1rGL5hj6KCesnDYUhd7oWgT11eZG7XnxHrnYeSvkzY7d2bhkJ7"
        ]
    ,
        [ "m/0'/1"
        , "bef5a2f9a56a94aab12459f72ad9cf8cf19c7bbe"
        , "bef5a2f9"
        , "1JQheacLPdM5ySCkrZkV66G2ApAXe1mqLj"
        , "1KMg6dRggXSkpz9fFyU76ru83TUSwPePEZ"
        , "3c6cb8d0f6a264c91ea8b5030fadaa8e538b020f0a387421a12de9319dc93368"
        , "KyFAjQ5rgrKvhXvNMtFB5PCSKUYD1yyPEe3xr3T34TZSUHycXtMM"
        , "03501e454bf00751f24b1b489aa925215d66af2234e3891c3b21a52bedb3cd711c"
        , "2a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19"
        , "0488b21e025c1bd648000000012a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c1903501e454bf00751f24b1b489aa925215d66af2234e3891c3b21a52bedb3cd711c"
        , "0488ade4025c1bd648000000012a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19003c6cb8d0f6a264c91ea8b5030fadaa8e538b020f0a387421a12de9319dc93368"
        , "xpub6ASuArnXKPbfEwhqN6e3mwBcDTgzisQN1wXN9BJcM47sSikHjJf3UFHKkNAWbWMiGj7Wf5uMash7SyYq527Hqck2AxYysAA7xmALppuCkwQ"
        , "xprv9wTYmMFdV23N2TdNG573QoEsfRrWKQgWeibmLntzniatZvR9BmLnvSxqu53Kw1UmYPxLgboyZQaXwTCg8MSY3H2EU4pWcQDnRnrVA1xe8fs"
        ]
    ,
        [ "m/0'/1/2'"
        , "ee7ab90cde56a8c0e2bb086ac49748b8db9dce72"
        , "ee7ab90c"
        , "1NjxqbA9aZWnh17q1UW3rB4EPu79wDXj7x"
        , "1WykKhR25y7VDT21nZEwUUKSKDz9pENJh"
        , "cbce0d719ecf7431d88e6a89fa1483e02e35092af60c042b1df2ff59fa424dca"
        , "L43t3od1Gh7Lj55Bzjj1xDAgJDcL7YFo2nEcNaMGiyRZS1CidBVU"
        , "0357bfe1e341d01c69fe5654309956cbea516822fba8a601743a012a7896ee8dc2"
        , "04466b9cc8e161e966409ca52986c584f07e9dc81f735db683c3ff6ec7b1503f"
        , "0488b21e03bef5a2f98000000204466b9cc8e161e966409ca52986c584f07e9dc81f735db683c3ff6ec7b1503f0357bfe1e341d01c69fe5654309956cbea516822fba8a601743a012a7896ee8dc2"
        , "0488ade403bef5a2f98000000204466b9cc8e161e966409ca52986c584f07e9dc81f735db683c3ff6ec7b1503f00cbce0d719ecf7431d88e6a89fa1483e02e35092af60c042b1df2ff59fa424dca"
        , "xpub6D4BDPcP2GT577Vvch3R8wDkScZWzQzMMUm3PWbmWvVJrZwQY4VUNgqFJPMM3No2dFDFGTsxxpG5uJh7n7epu4trkrX7x7DogT5Uv6fcLW5"
        , "xprv9z4pot5VBttmtdRTWfWQmoH1taj2axGVzFqSb8C9xaxKymcFzXBDptWmT7FwuEzG3ryjH4ktypQSAewRiNMjANTtpgP4mLTj34bhnZX7UiM"
        ]
    ,
        [ "m/0'/1/2'/2"
        , "d880d7d893848509a62d8fb74e32148dac68412f"
        , "d880d7d8"
        , "1LjmJcdPnDHhNTUgrWyhLGnRDKxQjoxAgt"
        , "1asQ3smHhv2nv5R6hPpiUfkEorJpsdwwx"
        , "0f479245fb19a38a1954c5c7c0ebab2f9bdfd96a17563ef28a6a4b1a2a764ef4"
        , "KwjQsVuMjbCP2Zmr3VaFaStav7NvevwjvvkqrWd5Qmh1XVnCteBR"
        , "02e8445082a72f29b75ca48748a914df60622a609cacfce8ed0e35804560741d29"
        , "cfb71883f01676f587d023cc53a35bc7f88f724b1f8c2892ac1275ac822a3edd"
        , "0488b21e04ee7ab90c00000002cfb71883f01676f587d023cc53a35bc7f88f724b1f8c2892ac1275ac822a3edd02e8445082a72f29b75ca48748a914df60622a609cacfce8ed0e35804560741d29"
        , "0488ade404ee7ab90c00000002cfb71883f01676f587d023cc53a35bc7f88f724b1f8c2892ac1275ac822a3edd000f479245fb19a38a1954c5c7c0ebab2f9bdfd96a17563ef28a6a4b1a2a764ef4"
        , "xpub6FHa3pjLCk84BayeJxFW2SP4XRrFd1JYnxeLeU8EqN3vDfZmbqBqaGJAyiLjTAwm6ZLRQUMv1ZACTj37sR62cfN7fe5JnJ7dh8zL4fiyLHV"
        , "xprvA2JDeKCSNNZky6uBCviVfJSKyQ1mDYahRjijr5idH2WwLsEd4Hsb2Tyh8RfQMuPh7f7RtyzTtdrbdqqsunu5Mm3wDvUAKRHSC34sJ7in334"
        ]
    ,
        [ "m/0'/1/2'/2/1000000000"
        , "d69aa102255fed74378278c7812701ea641fdf32"
        , "d69aa102"
        , "1LZiqrop2HGR4qrH1ULZPyBpU6AUP49Uam"
        , "1HXJog342VFdc68AB9Cb6LwVmCjvcLMiwm"
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


m2 :: TestKey
m2 = "fffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542"


vector2 :: [TestVector]
vector2 =
    [
        [ "m"
        , "bd16bee53961a47d6ad888e29545434a89bdfe95"
        , "bd16bee5"
        , "1JEoxevbLLG8cVqeoGKQiAwoWbNYSUyYjg"
        , "148CGtv7bwcC933EHtcDfzDQVneur1R8Y1"
        , "4b03d6fc340455b363f51020ad3ecca4f0850280cf436c70c727923f6db46c3e"
        , "KyjXhyHF9wTphBkfpxjL8hkDXDUSbE3tKANT94kXSyh6vn6nKaoy"
        , "03cbcaa9c98c877a26977d00825c956a238e8dddfbd322cce4f74b0b5bd6ace4a7"
        , "60499f801b896d83179a4374aeb7822aaeaceaa0db1f85ee3e904c4defbd9689"
        , "0488b21e00000000000000000060499f801b896d83179a4374aeb7822aaeaceaa0db1f85ee3e904c4defbd968903cbcaa9c98c877a26977d00825c956a238e8dddfbd322cce4f74b0b5bd6ace4a7"
        , "0488ade400000000000000000060499f801b896d83179a4374aeb7822aaeaceaa0db1f85ee3e904c4defbd9689004b03d6fc340455b363f51020ad3ecca4f0850280cf436c70c727923f6db46c3e"
        , "xpub661MyMwAqRbcFW31YEwpkMuc5THy2PSt5bDMsktWQcFF8syAmRUapSCGu8ED9W6oDMSgv6Zz8idoc4a6mr8BDzTJY47LJhkJ8UB7WEGuduB"
        , "xprv9s21ZrQH143K31xYSDQpPDxsXRTUcvj2iNHm5NUtrGiGG5e2DtALGdso3pGz6ssrdK4PFmM8NSpSBHNqPqm55Qn3LqFtT2emdEXVYsCzC2U"
        ]
    ,
        [ "m/0"
        , "5a61ff8eb7aaca3010db97ebda76121610b78096"
        , "5a61ff8e"
        , "19EuDJdgfRkwCmRzbzVBHZWQG9QNWhftbZ"
        , "1KVyTSpsBGYs7NdyZmArEpVTfWJQSgiDCx"
        , "abe74a98f6c7eabee0428f53798f0ab8aa1bd37873999041703c742f15ac7e1e"
        , "L2ysLrR6KMSAtx7uPqmYpoTeiRzydXBattRXjXz5GDFPrdfPzKbj"
        , "02fc9e5af0ac8d9b3cecfe2a888e2117ba3d089d8585886c9c826b6b22a98d12ea"
        , "f0909affaa7ee7abe5dd4e100598d4dc53cd709d5a5c2cac40e7412f232f7c9c"
        , "0488b21e01bd16bee500000000f0909affaa7ee7abe5dd4e100598d4dc53cd709d5a5c2cac40e7412f232f7c9c02fc9e5af0ac8d9b3cecfe2a888e2117ba3d089d8585886c9c826b6b22a98d12ea"
        , "0488ade401bd16bee500000000f0909affaa7ee7abe5dd4e100598d4dc53cd709d5a5c2cac40e7412f232f7c9c00abe74a98f6c7eabee0428f53798f0ab8aa1bd37873999041703c742f15ac7e1e"
        , "xpub69H7F5d8KSRgmmdJg2KhpAK8SR3DjMwAdkxj3ZuxV27CprR9LgpeyGmXUbC6wb7ERfvrnKZjXoUmmDznezpbZb7ap6r1D3tgFxHmwMkQTPH"
        , "xprv9vHkqa6EV4sPZHYqZznhT2NPtPCjKuDKGY38FBWLvgaDx45zo9WQRUT3dKYnjwih2yJD9mkrocEZXo1ex8G81dwSM1fwqWpWkeS3v86pgKt"
        ]
    ,
        [ "m/0/2147483647'"
        , "d8ab493736da02f11ed682f88339e720fb0379d1"
        , "d8ab4937"
        , "1Lke9bXGhn5VPrBuXgN12uGUphrttUErmk"
        , "14MFLsfx1nc4RKiaH9khqDTNL9CRz3q347"
        , "877c779ad9687164e9c2f4f0f4ff0340814392330693ce95a58fe18fd52e6e93"
        , "L1m5VpbXmMp57P3knskwhoMTLdhAAaXiHvnGLMribbfwzVRpz2Sr"
        , "03c01e7425647bdefa82b12d9bad5e3e6865bee0502694b94ca58b666abc0a5c3b"
        , "be17a268474a6bb9c61e1d720cf6215e2a88c5406c4aee7b38547f585c9a37d9"
        , "0488b21e025a61ff8effffffffbe17a268474a6bb9c61e1d720cf6215e2a88c5406c4aee7b38547f585c9a37d903c01e7425647bdefa82b12d9bad5e3e6865bee0502694b94ca58b666abc0a5c3b"
        , "0488ade4025a61ff8effffffffbe17a268474a6bb9c61e1d720cf6215e2a88c5406c4aee7b38547f585c9a37d900877c779ad9687164e9c2f4f0f4ff0340814392330693ce95a58fe18fd52e6e93"
        , "xpub6ASAVgeehLbnwdqV6UKMHVzgqAG8Gr6riv3Fxxpj8ksbH9ebxaEyBLZ85ySDhKiLDBrQSARLq1uNRts8RuJiHjaDMBU4Zn9h8LZNnBC5y4a"
        , "xprv9wSp6B7kry3Vj9m1zSnLvN3xH8RdsPP1Mh7fAaR7aRLcQMKTR2vidYEeEg2mUCTAwCd6vnxVrcjfy2kRgVsFawNzmjuHc2YmYRmagcEPdU9"
        ]
    ,
        [ "m/0/2147483647'/1"
        , "78412e3a2296a40de124307b6485bd19833e2e34"
        , "78412e3a"
        , "1BxrAr2pHpeBheusmd6fHDP2tSLAUa3qsW"
        , "19ou31MGyGW9VFx7woKBqwLe5JHhQBYaDD"
        , "704addf544a06e5ee4bea37098463c23613da32020d604506da8c0518e1da4b7"
        , "KzyzXnznxSv249b4KuNkBwowaN3akiNeEHy5FWoPCJpStZbEKXN2"
        , "03a7d1d856deb74c508e05031f9895dab54626251b3806e16b4bd12e781a7df5b9"
        , "f366f48f1ea9f2d1d3fe958c95ca84ea18e4c4ddb9366c336c927eb246fb38cb"
        , "0488b21e03d8ab493700000001f366f48f1ea9f2d1d3fe958c95ca84ea18e4c4ddb9366c336c927eb246fb38cb03a7d1d856deb74c508e05031f9895dab54626251b3806e16b4bd12e781a7df5b9"
        , "0488ade403d8ab493700000001f366f48f1ea9f2d1d3fe958c95ca84ea18e4c4ddb9366c336c927eb246fb38cb00704addf544a06e5ee4bea37098463c23613da32020d604506da8c0518e1da4b7"
        , "xpub6DF8uhdarytz3FWdA8TvFSvvAh8dP3283MY7p2V4SeE2wyWmG5mg5EwVvmdMVCQcoNJxGoWaU9DCWh89LojfZ537wTfunKau47EL2dhHKon"
        , "xprv9zFnWC6h2cLgpmSA46vutJzBcfJ8yaJGg8cX1e5StJh45BBciYTRXSd25UEPVuesF9yog62tGAQtHjXajPPdbRCHuWS6T8XA2ECKADdw4Ef"
        ]
    ,
        [ "m/0/2147483647'/1/2147483646'"
        , "31a507b815593dfc51ffc7245ae7e5aee304246e"
        , "31a507b8"
        , "15XVotxCAV7sRx1PSCkQNsGw3W9jT9A94R"
        , "18GYmRm4nyjk8ydvoVXFxMWQvxhksEFDZR"
        , "f1c7c871a54a804afe328b4c83a1c33b8e5ff48f5087273f04efa83b247d6a2d"
        , "L5KhaMvPYRW1ZoFmRjUtxxPypQ94m6BcDrPhqArhggdaTbbAFJEF"
        , "02d2b36900396c9282fa14628566582f206a5dd0bcc8d5e892611806cafb0301f0"
        , "637807030d55d01f9a0cb3a7839515d796bd07706386a6eddf06cc29a65a0e29"
        , "0488b21e0478412e3afffffffe637807030d55d01f9a0cb3a7839515d796bd07706386a6eddf06cc29a65a0e2902d2b36900396c9282fa14628566582f206a5dd0bcc8d5e892611806cafb0301f0"
        , "0488ade40478412e3afffffffe637807030d55d01f9a0cb3a7839515d796bd07706386a6eddf06cc29a65a0e2900f1c7c871a54a804afe328b4c83a1c33b8e5ff48f5087273f04efa83b247d6a2d"
        , "xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL"
        , "xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc"
        ]
    ,
        [ "m/0/2147483647'/1/2147483646'/2"
        , "26132fdbe7bf89cbc64cf8dafa3f9f88b8666220"
        , "26132fdb"
        , "14UKfRV9ZPUp6ZC9PLhqbRtxdihW9em3xt"
        , "1758mgwNZhyzpRLe4u7FjqpJtqKpaGhXh7"
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


m3 :: TestKey
m3 = "4b381541583be4423346c643850da4b320e46a87ae3d2a4e6da11eba819cd4acba45d239319ac14f863b8d5ab5a0d0c64d2e8a1e7d1457df2e5a3c51c73235be"


vector3 :: [TestVector]
vector3 =
    [
        [ "m"
        , "41d63b50d8dd5e730cdf4c79a56fc929a757c548"
        , "41d63b50"
        , "1717ZYpXhZW5CqAbWSjDJbCey3FyKUmCSf"
        , "17rxURoF96VhmkcEGCj5LNQkmN9HVhWb7F"
        , "00ddb80b067e0d4993197fe10f2657a844a384589847602d56f0c629c81aae32"
        , "KwFPqAq9SKx1sPg15Qk56mqkHwrfGPuywtLUxoWPkiTSBoxCs8am"
        , "03683af1ba5743bdfc798cf814efeeab2735ec52d95eced528e692b8e34c4e5669"
        , "01d28a3e53cffa419ec122c968b3259e16b65076495494d97cae10bbfec3c36f"
        , "0488b21e00000000000000000001d28a3e53cffa419ec122c968b3259e16b65076495494d97cae10bbfec3c36f03683af1ba5743bdfc798cf814efeeab2735ec52d95eced528e692b8e34c4e5669"
        , "0488ade400000000000000000001d28a3e53cffa419ec122c968b3259e16b65076495494d97cae10bbfec3c36f0000ddb80b067e0d4993197fe10f2657a844a384589847602d56f0c629c81aae32"
        , "xpub661MyMwAqRbcEZVB4dScxMAdx6d4nFc9nvyvH3v4gJL378CSRZiYmhRoP7mBy6gSPSCYk6SzXPTf3ND1cZAceL7SfJ1Z3GC8vBgp2epUt13"
        , "xprv9s21ZrQH143K25QhxbucbDDuQ4naNntJRi4KUfWT7xo4EKsHt2QJDu7KXp1A3u7Bi1j8ph3EGsZ9Xvz9dGuVrtHHs7pXeTzjuxBrCmmhgC6"
        ]
    ,
        [ "m/0'"
        , "c61368bb50e066acd95bd04a0b23d3837fb75698"
        , "c61368bb"
        , "1K4L3YxEwg8HkSEapM4iSiGuR6HeQ53KPX"
        , "13QeQVJNNakdUU55P1fc8xPMkkeYVzn4o6"
        , "491f7a2eebc7b57028e0d3faa0acda02e75c33b03c48fb288c41e2ea44e1daef"
        , "KyfrPaeirL5yYAoZvfzyoKXSdszeLqg5vb6dNy9ymvjzZrMZY8GW"
        , "026557fdda1d5d43d79611f784780471f086d58e8126b8c40acb82272a7712e7f2"
        , "e5fea12a97b927fc9dc3d2cb0d1ea1cf50aa5a1fdc1f933e8906bb38df3377bd"
        , "0488b21e0141d63b5080000000e5fea12a97b927fc9dc3d2cb0d1ea1cf50aa5a1fdc1f933e8906bb38df3377bd026557fdda1d5d43d79611f784780471f086d58e8126b8c40acb82272a7712e7f2"
        , "0488ade40141d63b5080000000e5fea12a97b927fc9dc3d2cb0d1ea1cf50aa5a1fdc1f933e8906bb38df3377bd00491f7a2eebc7b57028e0d3faa0acda02e75c33b03c48fb288c41e2ea44e1daef"
        , "xpub68NZiKmJWnxxS6aaHmn81bvJeTESw724CRDs6HbuccFQN9Ku14VQrADWgqbhhTHBaohPX4CjNLf9fq9MYo6oDaPPLPxSb7gwQN3ih19Zm4Y"
        , "xprv9uPDJpEQgRQfDcW7BkF7eTya6RPxXeJCqCJGHuCJ4GiRVLzkTXBAJMu2qaMWPrS7AANYqdq6vcBcBUdJCVVFceUvJFjaPdGZ2y9WACViL4L"
        ]
    ]
