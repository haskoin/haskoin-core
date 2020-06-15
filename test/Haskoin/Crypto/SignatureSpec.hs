{-# LANGUAGE OverloadedStrings #-}
module Haskoin.Crypto.SignatureSpec (spec) where

import           Data.Bits              (testBit)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS (index, length)
import           Data.Serialize         as S
import           Data.Text              (Text)
import           Haskoin.Crypto
import           Haskoin.Util
import           Haskoin.Util.Arbitrary
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "Signature properties" $ do
        prop "verify signature" $
            forAll arbitrarySignature $ \(m, key, sig) ->
                verifyHashSig m sig (derivePubKey key)
        prop "s component less than half order" $
            forAll arbitrarySignature $ isCanonicalHalfOrder . lst3
        prop "encoded signature is canonical" $
            forAll arbitrarySignature $ testIsCanonical . lst3
        prop "decodeStrictSig . exportSig identity" $
            forAll arbitrarySignature $
            (\s -> decodeStrictSig (exportSig s) == Just s) . lst3
        prop "importSig . exportSig identity" $
            forAll arbitrarySignature $
            (\s -> importSig (exportSig s) == Just s) . lst3
        prop "getSig . putSig identity" $
            forAll arbitrarySignature $
            (\s -> runGet getSig (runPut $ putSig s) == Right s) . lst3
    describe "Signature vectors" $ do
        it "Passes the trezor rfc6979 test vectors" $
            mapM_ testRFC6979Vector rfc6979Vectors 
        it "Passes the rfc6979 DER test vectors" $
            mapM_ testRFC6979DERVector rfc6979DERVectors 

-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- from function IsCanonicalSignature
testIsCanonical :: Sig -> Bool
testIsCanonical sig = not $
    -- Non-canonical signature: too short
    (len < 8) ||
    -- Non-canonical signature: too long
    (len > 72) ||
    -- Non-canonical signature: wrong type
    (BS.index s 0 /= 0x30) ||
    -- Non-canonical signature: wrong length marker
    (BS.index s 1 /= len - 2) ||
    -- Non-canonical signature: S length misplaced
    (5 + rlen >= len) ||
    -- Non-canonical signature: R+S length mismatch
    (rlen + slen + 6 /= len) ||
    -- Non-canonical signature: R value type mismatch
    (BS.index s 2 /= 0x02) ||
    -- Non-canonical signature: R length is zero
    (rlen == 0) ||
    -- Non-canonical signature: R value negative
    testBit (BS.index s 4) 7 ||
    -- Non-canonical signature: R value excessively padded
    (  rlen > 1
    && BS.index s 4 == 0
    && not (testBit (BS.index s 5) 7)
    ) ||
    -- Non-canonical signature: S value type mismatch
    (BS.index s (fromIntegral rlen + 4) /= 0x02) ||
    -- Non-canonical signature: S length is zero
    (slen == 0) ||
    -- Non-canonical signature: S value negative
    testBit (BS.index s (fromIntegral rlen+6)) 7 ||
    -- Non-canonical signature: S value excessively padded
    (  slen > 1
    && BS.index s (fromIntegral rlen + 6) == 0
    && not (testBit (BS.index s (fromIntegral rlen + 7)) 7)
    )
  where
    s = exportSig sig
    len = fromIntegral $ BS.length s
    rlen = BS.index s 3
    slen = BS.index s (fromIntegral rlen + 5)

{- Trezor RFC 6979 Test Vectors -}
-- github.com/trezor/python-ecdsa/blob/master/ecdsa/test_pyecdsa.py

testRFC6979Vector :: (SecKey, ByteString, Text) -> Assertion
testRFC6979Vector (prv, m, res) = do
    assertEqual "RFC 6979 Vector" res (encodeHex $ encode $ exportCompactSig s)
    assertBool "Signature is valid" $ verifyHashSig h s (derivePubKey prv)
    assertBool "Signature is canonical" $ testIsCanonical s
    assertBool "Signature is normalized" $ isCanonicalHalfOrder s
  where
    h = sha256 m
    s = signHash prv h

rfc6979Vectors :: [(SecKey, ByteString, Text)]
rfc6979Vectors =
    [
      ( "0000000000000000000000000000000000000000000000000000000000000001"
      , "Satoshi Nakamoto"
      , "934b1ea10a4b3c1757e2b0c017d0b6143ce3c9a7e6a4a49860d7a6ab210ee3d82442ce9d2b916064108014783e923ec36b49743e2ffa1c4496f01a512aafd9e5"
      )
    , ( "0000000000000000000000000000000000000000000000000000000000000001"
      , "All those moments will be lost in time, like tears in rain. Time to die..."
      , "8600dbd41e348fe5c9465ab92d23e3db8b98b873beecd930736488696438cb6b547fe64427496db33bf66019dacbf0039c04199abb0122918601db38a72cfc21"
      )
    , ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
      , "Satoshi Nakamoto"
      , "fd567d121db66e382991534ada77a6bd3106f0a1098c231e47993447cd6af2d06b39cd0eb1bc8603e159ef5c20a5c8ad685a45b06ce9bebed3f153d10d93bed5"
      )
    , ( "f8b8af8ce3c7cca5e300d33939540c10d45ce001b8f252bfbc57ba0342904181"
      , "Alan Turing"
      , "7063ae83e7f62bbb171798131b4a0564b956930092b33b07b395615d9ec7e15c58dfcc1e00a35e1572f366ffe34ba0fc47db1e7189759b9fb233c5b05ab388ea"
      )
    , ( "e91671c46231f833a6406ccbea0e3e392c76c167bac1cb013f6f1013980455c2"
      , "There is a computer disease that anybody who works with computers knows about. It's a very serious disease and it interferes completely with the work. The trouble with computers is that you 'play' with them!"
      , "b552edd27580141f3b2a5463048cb7cd3e047b97c9f98076c32dbdf85a68718b279fa72dd19bfae05577e06c7c0c1900c371fcd5893f7e1d56a37d30174671f6"
      )
    , ( "0000000000000000000000000000000000000000000000000000000000000001"
      , "Everything should be made as simple as possible, but not simpler."
      , "33a69cd2065432a30f3d1ce4eb0d59b8ab58c74f27c41a7fdb5696ad4e6108c96f807982866f785d3f6418d24163ddae117b7db4d5fdf0071de069fa54342262"
      )
    , ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
      , "Equations are more important to me, because politics is for the present, but an equation is something for eternity."
      , "54c4a33c6423d689378f160a7ff8b61330444abb58fb470f96ea16d99d4a2fed07082304410efa6b2943111b6a4e0aaa7b7db55a07e9861d1fb3cb1f421044a5"
      )
    , ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
      , "Not only is the Universe stranger than we think, it is stranger than we can think."
      , "ff466a9f1b7b273e2f4c3ffe032eb2e814121ed18ef84665d0f515360dab3dd06fc95f5132e5ecfdc8e5e6e616cc77151455d46ed48f5589b7db7771a332b283"
      )
    , ( "0000000000000000000000000000000000000000000000000000000000000001"
      , "How wonderful that we have met with a paradox. Now we have some hope of making progress."
      , "c0dafec8251f1d5010289d210232220b03202cba34ec11fec58b3e93a85b91d375afdc06b7d6322a590955bf264e7aaa155847f614d80078a90292fe205064d3"
      )
    , ( "69ec59eaa1f4f2e36b639716b7c30ca86d9a5375c7b38d8918bd9c0ebc80ba64"
      , "Computer science is no more about computers than astronomy is about telescopes."
      , "7186363571d65e084e7f02b0b77c3ec44fb1b257dee26274c38c928986fea45d0de0b38e06807e46bda1f1e293f4f6323e854c86d58abdd00c46c16441085df6"
      )
    , ( "00000000000000000000000000007246174ab1e92e9149c6e446fe194d072637"
      , "...if you aren't, at any given time, scandalized by code you wrote five or even three years ago, you're not learning anywhere near enough"
      , "fbfe5076a15860ba8ed00e75e9bd22e05d230f02a936b653eb55b61c99dda4870e68880ebb0050fe4312b1b1eb0899e1b82da89baa5b895f612619edf34cbd37"
      )
    , ( "000000000000000000000000000000000000000000056916d0f9b31dc9b637f3"
      , "The question of whether computers can think is like the question of whether submarines can swim."
      , "cde1302d83f8dd835d89aef803c74a119f561fbaef3eb9129e45f30de86abbf906ce643f5049ee1f27890467b77a6a8e11ec4661cc38cd8badf90115fbd03cef"
      )
    ]

-- Test vectors from:
-- https://crypto.stackexchange.com/questions/20838/request-for-data-to-test-deterministic-ecdsa-signature-algorithm-for-secp256k1

testRFC6979DERVector :: (SecKey, ByteString, Text) -> Assertion
testRFC6979DERVector (prv, m, res) = do
    assertEqual "RFC 6979 DER Vector" res (encodeHex $ exportSig s)
    assertBool "DER Signature is valid" $ verifyHashSig h s (derivePubKey prv)
    assertBool "DER Signature is canonical" $ testIsCanonical s
    assertBool "DER Signature is normalized" $ isCanonicalHalfOrder s
  where
    h = sha256 m
    s = signHash prv h

rfc6979DERVectors :: [(SecKey, ByteString, Text)]
rfc6979DERVectors =
    [ ( "0000000000000000000000000000000000000000000000000000000000000001"
      , "Absence makes the heart grow fonder."
      , "3045022100afff580595971b8c1700e77069d73602aef4c2a760dbd697881423dfff845de80220579adb6a1ac03acde461b5821a049ebd39a8a8ebf2506b841b15c27342d2e342")
    , ( "0000000000000000000000000000000000000000000000000000000000000002"
      , "Actions speak louder than words."
      , "304502210085f28bbc90975b1907a51cbfe7bf0dc1ac74ade49318ee97498dbbde3894a31c0220241d24da8d263e7af7ff49bca6a7a850f0e087faf6fef44f85851b0283c3f026")
    , ( "0000000000000000000000000000000000000000000000000000000000000003"
      , "All for one and one for all."
      , "30440220502c6ac38e1c68ce68f044f5ab680f2880a6c1cd34e70f2b4f945c6fd30abd03022018ef5c6c3392b9d67ad5109c85476a0e159425d7f6ace2cebeaa65f02f210bbb")
    , ( "0000000000000000000000000000000000000000000000000000000000000004"
      , "All's fair in love and war."
      , "30440220452d4ab234891cf6e5432cd5472bdca1cfc6fb28563333885f068da02ee216d8022056c368d16a64d29cff92f17203d926e113064527af0480d3bcc1d3fadfde9364")
    , ( "0000000000000000000000000000000000000000000000000000000000000005"
      , "All work and no play makes Jack a dull boy."
      , "3045022100995025b4880eeb1ecedba945fe8c9b2ddf2b07dbc293c2586c079d7b663ef38a022022fb54ab95014616d014277e05c97a7ed9e22596a0420bbd2d749ca9a2f876fe")
    , ( "0000000000000000000000000000000000000000000000000000000000000006"
      , "All's well that ends well."
      , "3045022100a9c1593fa6459777b2eba6d7e2a206e3bb119e85b2163973cf28ffaf24ec381c02202f166f13230b3853b928efb649d30375ec6a4b1a64a8d56fbcc0a9d86a0943e9")
    , ( "0000000000000000000000000000000000000000000000000000000000000007"
      , "An apple a day keeps the doctor away."
      , "304402202fc9c8b749621241c33fd51b57fc5140c1d7fc1594f91b073953e79da2f5e8f60220345e4ea7693b5069c0251771ea476cbe236586ed24b90aeeea7b7c2814edf477")
    , ( "0000000000000000000000000000000000000000000000000000000000000008"
      , "An apple never falls far from the tree."
      , "3044022052b6e2c49a6f6adbe52fb6bbe744caa3f49364085db118eab8670bc766be160302207d96a42866637ca3d4caf36e597a460eb305adac0220b027410c821a7191a1c4")
    , ( "0000000000000000000000000000000000000000000000000000000000000009"
      , "An ounce of prevention is worth a pound of cure."
      , "3045022100be53e7c00788e4417083d7511800f18c7c6f5f259de39bc6f8b1bebcd5056bd002201f389e13cfe7d1dbd8d2d1bff18138219f57de166673762009686a28fbc44df6")
    , ( "000000000000000000000000000000000000000000000000000000000000000a"
      , "Appearances can be deceiving."
      , "304402202f2413a1673f642c30ea2e23fcae45776bc77a94f96920aea3c14303b1469428022053ac3e8ea0a488e9159d56e429a51f207bf04e462f8d4ba2c69b1b1635f30217")
    , ( "4bf5122f344554c53bde2ebb8cd2b7e3d1600ad631c385a5d7cce23c7785459a"
      , "Absence makes the heart grow fonder."
      , "3045022100996d79fba54b24e9394fc5fab6bf94d173f3752645075de6e32574fe08625f770220345e638b373dcb0ce0c09e5799695ef64ffc5e01dd8367b9a205ce25f28870f6")
    , ( "dbc1b4c900ffe48d575b5da5c638040125f65db0fe3e24494b76ea986457d986"
      , "Actions speak louder than words."
      , "304502210088164430985a4437471417c2386faa536e1fe8ec91bd0f1f642bc22a776891530220090dc83d6e3b54a1a54dc2e79c693144179a512d9c9e686a6c25e7641a2101a8")
    , ( "084fed08b978af4d7d196a7446a86b58009e636b611db16211b65a9aadff29c5"
      , "All for one and one for all."
      , "30450221009f1073c9c09b664498d4b216983330b01c29a0fb55dd61aa145b4ebd0579905502204592fb6626f672d4f3ad4bb2d0a1ed6c2a161cc35c6bb77e6f0fd3b63feab36f")
    , ( "e52d9c508c502347344d8c07ad91cbd6068afc75ff6292f062a09ca381c89e71"
      , "All's fair in love and war."
      , "304502210080eabf24117b492635043886e7229b9705b970cbb6828c4e03a39dae7ac34bda022070e8a32ca1df82add53facbd58b4f2d3984d0a17b6b13c44460238d9ff74e41f")
    , ( "e77b9a9ae9e30b0dbdb6f510a264ef9de781501d7b6b92ae89eb059c5ab743db"
      , "All work and no play makes Jack a dull boy."
      , "3045022100a43ff5edea7ea0b9716d4359574e990a6859cdaeb9d7d6b4964afd40be11bd35022067f9d82e22fc447a122997335525f117f37b141c3efa9f8c6d77b586753f962f")
    , ( "67586e98fad27da0b9968bc039a1ef34c939b9b8e523a8bef89d478608c5ecf6"
      , "All's well that ends well."
      , "3044022053ce16251f4fae7eb87e2ab040a6f334e08687fb445566256cd217ece389e0440220576506a168cbc9ee0dd485d6c418961e7a0861b0f05d22a93401812978d0b215")
    , ( "ca358758f6d27e6cf45272937977a748fd88391db679ceda7dc7bf1f005ee879"
      , "An apple a day keeps the doctor away."
      , "3045022100df8744cc06a304b041e88149acfd84a68d8f4a2a4047056644e1ec8357e11ebe02204ba2d5499a26d072c797a86c7851533f287ceb8b818cae2c5d4483c37c62750c")
    , ( "beead77994cf573341ec17b58bbf7eb34d2711c993c1d976b128b3188dc1829a"
      , "An apple never falls far from the tree."
      , "3045022100878372d211ed0dbde1273ae3dd85aec577c08a06a55960f2e274f97cc9f2f38f02203f992caa66f472a64f6ccdd8076c0a12202c674155a6a61b8cd23c1ded08aab7")
    , ( "2b4c342f5433ebe591a1da77e013d1b72475562d48578dca8b84bac6651c3cb9"
      , "An ounce of prevention is worth a pound of cure."
      , "3045022100d5cb4e148c0a29ce37f1542be416e8ef575da522666b19b541960d726c99662b022045c951c1ca938c90dad6c3eede7c5df67fcf0d14f90faf201e8d215f215c5c18")
    , ( "01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b"
      , "Appearances can be deceiving."
      , "304402203e2f0118062306e2239c873828a7275dd35545a143797e224148c5bbbd59dd08022073a8c9e17be75c66362913b5e05d81fd619b434edda766fae6c352e86987809d")
    ]
