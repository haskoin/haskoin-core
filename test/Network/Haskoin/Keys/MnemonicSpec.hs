{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Keys.MnemonicSpec (spec) where

import           Control.Monad           (zipWithM_)
import           Data.Bits               (shiftR, (.&.))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C
import           Data.Either             (fromRight)
import           Data.List               (isPrefixOf)
import           Data.Maybe              (fromJust)
import           Data.Serialize          (Serialize, encode)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Word               (Word32, Word64)
import           Network.Haskoin.Crypto
import           Network.Haskoin.Keys
import           Network.Haskoin.Test
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck         hiding ((.&.))

spec :: Spec
spec =
    describe "mnemonic" $ do
        it "entropy to mnemonic sentence" toMnemonicTest
        it "mnemonic sentence to entropy" fromMnemonicTest
        it "mnemonic sentence to seed" mnemonicToSeedTest
        it "mnemonic sentence with invalid checksum" fromMnemonicInvalidTest
        it "empty mnemonic sentence is invalid" $ sequence_ [emptyMnemonicTest]
        it "generate 12 words" $ property toMnemonic128
        it "generate 18 words" $ property toMnemonic160
        it "generate 24 words" $ property toMnemonic256
        it "generate 48 words" $ property toMnemonic512
        it "generate any number of words" $ property toMnemonicVar
        it "encode and decode 128-bit entropy" $ property fromToMnemonic128
        it "encode and decode 160-bit entropy" $ property fromToMnemonic160
        it "encode and decode 256-bit entropy" $ property fromToMnemonic256
        it "encode and decode 512-bit entropy" $ property fromToMnemonic512
        it "encode and decode n-bit entropy" $ property fromToMnemonicVar
        it "convert 128-bit mnemonic to seed" $ property mnemonicToSeed128
        it "convert 160-bit mnemonic to seed" $ property mnemonicToSeed160
        it "convert 256-bit mnemonic to seed" $ property mnemonicToSeed256
        it "convert 512-bit mnemonic to seed" $ property mnemonicToSeed512
        it "convert n-bit mnemonic to seed" $ property mnemonicToSeedVar
        it "get bits" $ property getBitsByteCount
        it "get end bits" $ property getBitsEndBits

toMnemonicTest :: Assertion
toMnemonicTest = zipWithM_ f ents mss
  where
    f e m = assertEqual "" m . h $ e
    h =
        fromRight (error "Could not decode mnemonic sentence") .
        toMnemonic . fromJust . decodeHex

fromMnemonicTest :: Assertion
fromMnemonicTest = zipWithM_ f ents mss
  where
    f e = assertEqual "" e . h
    h =
        encodeHex .
        fromRight (error "Could not decode mnemonic sentence") . fromMnemonic

mnemonicToSeedTest :: Assertion
mnemonicToSeedTest = zipWithM_ f mss seeds
  where
    f m s = assertEqual "" s . h $ m
    h =
        encodeHex .
        fromRight (error "Could not decode mnemonic seed") .
        mnemonicToSeed "TREZOR"

fromMnemonicInvalidTest :: Assertion
fromMnemonicInvalidTest = mapM_ f invalidMss
  where
    f = assertBool "" . h
    h m = case fromMnemonic m of
            Right _  -> False
            Left err -> "fromMnemonic: checksum failed:" `isPrefixOf` err

emptyMnemonicTest :: Assertion
emptyMnemonicTest =
    assertBool "" $
    case fromMnemonic "" of
        Right _  -> False
        Left err -> "fromMnemonic: empty mnemonic" `isPrefixOf` err

ents :: [Text]
ents =
    [ "00000000000000000000000000000000"
    , "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f"
    , "80808080808080808080808080808080"
    , "ffffffffffffffffffffffffffffffff"
    , "000000000000000000000000000000000000000000000000"
    , "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f"
    , "808080808080808080808080808080808080808080808080"
    , "ffffffffffffffffffffffffffffffffffffffffffffffff"
    , "0000000000000000000000000000000000000000000000000000000000000000"
    , "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f"
    , "8080808080808080808080808080808080808080808080808080808080808080"
    , "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    , "77c2b00716cec7213839159e404db50d"
    , "b63a9c59a6e641f288ebc103017f1da9f8290b3da6bdef7b"
    , "3e141609b97933b66a060dcddc71fad1d91677db872031e85f4c015c5e7e8982"
    , "0460ef47585604c5660618db2e6a7e7f"
    , "72f60ebac5dd8add8d2a25a797102c3ce21bc029c200076f"
    , "2c85efc7f24ee4573d2b81a6ec66cee209b2dcbd09d8eddc51e0215b0b68e416"
    , "eaebabb2383351fd31d703840b32e9e2"
    , "7ac45cfe7722ee6c7ba84fbc2d5bd61b45cb2fe5eb65aa78"
    , "4fa1a8bc3e6d80ee1316050e862c1812031493212b7ec3f3bb1b08f168cabeef"
    , "18ab19a9f54a9274f03e5209a2ac8a91"
    , "18a2e1d81b8ecfb2a333adcb0c17a5b9eb76cc5d05db91a4"
    , "15da872c95a13dd738fbf50e427583ad61f18fd99f628c417a61cf8343c90419"
    ]

mss :: [Mnemonic]
mss =
    [ "abandon abandon abandon abandon abandon abandon abandon abandon abandon\
      \ abandon abandon about"
    , "legal winner thank year wave sausage worth useful legal winner thank\
      \ yellow"
    , "letter advice cage absurd amount doctor acoustic avoid letter advice\
      \ cage above"
    , "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong"
    , "abandon abandon abandon abandon abandon abandon abandon abandon abandon\
      \ abandon abandon abandon abandon abandon abandon abandon abandon agent"
    , "legal winner thank year wave sausage worth useful legal winner thank\
      \ year wave sausage worth useful legal will"
    , "letter advice cage absurd amount doctor acoustic avoid letter advice\
      \ cage absurd amount doctor acoustic avoid letter always"
    , "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo\
      \ when"
    , "abandon abandon abandon abandon abandon abandon abandon abandon abandon\
      \ abandon abandon abandon abandon abandon abandon abandon abandon abandon\
      \ abandon abandon abandon abandon abandon art"
    , "legal winner thank year wave sausage worth useful legal winner thank\
      \ year wave sausage worth useful legal winner thank year wave sausage\
      \ worth title"
    , "letter advice cage absurd amount doctor acoustic avoid letter advice\
      \ cage absurd amount doctor acoustic avoid letter advice cage absurd\
      \ amount doctor acoustic bless"
    , "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo\
      \ zoo zoo zoo zoo zoo vote"
    , "jelly better achieve collect unaware mountain thought cargo oxygen act\
      \ hood bridge"
    , "renew stay biology evidence goat welcome casual join adapt armor shuffle\
      \ fault little machine walk stumble urge swap"
    , "dignity pass list indicate nasty swamp pool script soccer toe leaf photo\
      \ multiply desk host tomato cradle drill spread actor shine dismiss\
      \ champion exotic"
    , "afford alter spike radar gate glance object seek swamp infant panel\
      \ yellow"
    , "indicate race push merry suffer human cruise dwarf pole review arch keep\
      \ canvas theme poem divorce alter left"
    , "clutch control vehicle tonight unusual clog visa ice plunge glimpse\
      \ recipe series open hour vintage deposit universe tip job dress radar\
      \ refuse motion taste"
    , "turtle front uncle idea crush write shrug there lottery flower risk\
      \ shell"
    , "kiss carry display unusual confirm curtain upgrade antique rotate hello\
      \ void custom frequent obey nut hole price segment"
    , "exile ask congress lamp submit jacket era scheme attend cousin alcohol\
      \ catch course end lucky hurt sentence oven short ball bird grab wing top"
    , "board flee heavy tunnel powder denial science ski answer betray cargo\
      \ cat"
    , "board blade invite damage undo sun mimic interest slam gaze truly\
      \ inherit resist great inject rocket museum chief"
    , "beyond stage sleep clip because twist token leaf atom beauty genius food\
      \ business side grid unable middle armed observe pair crouch tonight away\
      \ coconut"
    ]

seeds :: [Text]
seeds =
    [ "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a69\
      \87599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04"
    , "2e8905819b8723fe2c1d161860e5ee1830318dbf49a83bd451cfb8440c28bd6fa457fe1\
      \296106559a3c80937a1c1069be3a3a5bd381ee6260e8d9739fce1f607"
    , "d71de856f81a8acc65e6fc851a38d4d7ec216fd0796d0a6827a3ad6ed5511a30fa280f1\
      \2eb2e47ed2ac03b5c462a0358d18d69fe4f985ec81778c1b370b652a8"
    , "ac27495480225222079d7be181583751e86f571027b0497b5b5d11218e0a8a133325729\
      \17f0f8e5a589620c6f15b11c61dee327651a14c34e18231052e48c069"
    , "035895f2f481b1b0f01fcf8c289c794660b289981a78f8106447707fdd9666ca06da5a9\
      \a565181599b79f53b844d8a71dd9f439c52a3d7b3e8a79c906ac845fa"
    , "f2b94508732bcbacbcc020faefecfc89feafa6649a5491b8c952cede496c214a0c7b3c3\
      \92d168748f2d4a612bada0753b52a1c7ac53c1e93abd5c6320b9e95dd"
    , "107d7c02a5aa6f38c58083ff74f04c607c2d2c0ecc55501dadd72d025b751bc27fe913f\
      \fb796f841c49b1d33b610cf0e91d3aa239027f5e99fe4ce9e5088cd65"
    , "0cd6e5d827bb62eb8fc1e262254223817fd068a74b5b449cc2f667c3f1f985a76379b43\
      \348d952e2265b4cd129090758b3e3c2c49103b5051aac2eaeb890a528"
    , "bda85446c68413707090a52022edd26a1c9462295029f2e60cd7c4f2bbd3097170af7a4\
      \d73245cafa9c3cca8d561a7c3de6f5d4a10be8ed2a5e608d68f92fcc8"
    , "bc09fca1804f7e69da93c2f2028eb238c227f2e9dda30cd63699232578480a4021b146a\
      \d717fbb7e451ce9eb835f43620bf5c514db0f8add49f5d121449d3e87"
    , "c0c519bd0e91a2ed54357d9d1ebef6f5af218a153624cf4f2da911a0ed8f7a09e2ef61a\
      \f0aca007096df430022f7a2b6fb91661a9589097069720d015e4e982f"
    , "dd48c104698c30cfe2b6142103248622fb7bb0ff692eebb00089b32d22484e1613912f0\
      \a5b694407be899ffd31ed3992c456cdf60f5d4564b8ba3f05a69890ad"
    , "b5b6d0127db1a9d2226af0c3346031d77af31e918dba64287a1b44b8ebf63cdd52676f6\
      \72a290aae502472cf2d602c051f3e6f18055e84e4c43897fc4e51a6ff"
    , "9248d83e06f4cd98debf5b6f010542760df925ce46cf38a1bdb4e4de7d21f5c39366941\
      \c69e1bdbf2966e0f6e6dbece898a0e2f0a4c2b3e640953dfe8b7bbdc5"
    , "ff7f3184df8696d8bef94b6c03114dbee0ef89ff938712301d27ed8336ca89ef9635da2\
      \0af07d4175f2bf5f3de130f39c9d9e8dd0472489c19b1a020a940da67"
    , "65f93a9f36b6c85cbe634ffc1f99f2b82cbb10b31edc7f087b4f6cb9e976e9faf76ff41\
      \f8f27c99afdf38f7a303ba1136ee48a4c1e7fcd3dba7aa876113a36e4"
    , "3bbf9daa0dfad8229786ace5ddb4e00fa98a044ae4c4975ffd5e094dba9e0bb289349db\
      \e2091761f30f382d4e35c4a670ee8ab50758d2c55881be69e327117ba"
    , "fe908f96f46668b2d5b37d82f558c77ed0d69dd0e7e043a5b0511c48c2f1064694a956f\
      \86360c93dd04052a8899497ce9e985ebe0c8c52b955e6ae86d4ff4449"
    , "bdfb76a0759f301b0b899a1e3985227e53b3f51e67e3f2a65363caedf3e32fde42a66c4\
      \04f18d7b05818c95ef3ca1e5146646856c461c073169467511680876c"
    , "ed56ff6c833c07982eb7119a8f48fd363c4a9b1601cd2de736b01045c5eb8ab4f57b079\
      \403485d1c4924f0790dc10a971763337cb9f9c62226f64fff26397c79"
    , "095ee6f817b4c2cb30a5a797360a81a40ab0f9a4e25ecd672a3f58a0b5ba0687c096a6b\
      \14d2c0deb3bdefce4f61d01ae07417d502429352e27695163f7447a8c"
    , "6eff1bb21562918509c73cb990260db07c0ce34ff0e3cc4a8cb3276129fbcb300bddfe0\
      \05831350efd633909f476c45c88253276d9fd0df6ef48609e8bb7dca8"
    , "f84521c777a13b61564234bf8f8b62b3afce27fc4062b51bb5e62bdfecb23864ee6ecf0\
      \7c1d5a97c0834307c5c852d8ceb88e7c97923c0a3b496bedd4e5f88a9"
    , "b15509eaa2d09d3efd3e006ef42151b30367dc6e3aa5e44caba3fe4d3e352e65101fbdb\
      \86a96776b91946ff06f8eac594dc6ee1d3e82a42dfe1b40fef6bcc3fd"
    ]

invalidMss :: [Mnemonic]
invalidMss =
    [ "abandon abandon abandon abandon abandon abandon abandon abandon abandon\
      \ abandon abandon abandon"
    , "legal winner thank year wave sausage worth useful legal winner thank\
      \ thank"
    , "letter advice cage absurd amount doctor acoustic avoid letter advice\
      \ cage sausage"
    , "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo"
    , "abandon abandon abandon abandon abandon abandon abandon abandon abandon\
      \ abandon abandon abandon abandon abandon abandon abandon abandon abandon"
    , "legal winner thank year wave sausage worth useful legal winner thank\
      \ year wave sausage worth useful legal letter"
    , "letter advice cage absurd amount doctor acoustic avoid letter advice\
      \ cage absurd amount doctor acoustic avoid letter abandon"
    , "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo\
      \ zoo"
    , "abandon abandon abandon abandon abandon abandon abandon abandon abandon\
      \ abandon abandon abandon abandon abandon abandon abandon abandon abandon\
      \ abandon abandon abandon abandon abandon abandon"
    , "legal winner thank year wave sausage worth useful legal winner thank\
      \ year wave sausage worth useful legal winner thank year wave sausage\
      \ worth letter"
    , "letter advice cage absurd amount doctor acoustic avoid letter advice\
      \ cage absurd amount doctor acoustic avoid letter advice cage absurd\
      \ amount doctor acoustic letter"
    , "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo\
      \ zoo zoo zoo zoo zoo zoo"
    , "jelly better achieve collect unaware mountain thought cargo oxygen act\
      \ hood zoo"
    , "renew stay biology evidence goat welcome casual join adapt armor shuffle\
      \ fault little machine walk stumble urge zoo"
    , "dignity pass list indicate nasty swamp pool script soccer toe leaf photo\
      \ multiply desk host tomato cradle drill spread actor shine dismiss\
      \ champion zoo"
    , "afford alter spike radar gate glance object seek swamp infant panel\
      \ zoo"
    , "indicate race push merry suffer human cruise dwarf pole review arch keep\
      \ canvas theme poem divorce alter zoo"
    , "clutch control vehicle tonight unusual clog visa ice plunge glimpse\
      \ recipe series open hour vintage deposit universe tip job dress radar\
      \ refuse motion zoo"
    , "turtle front uncle idea crush write shrug there lottery flower risk\
      \ zoo"
    , "kiss carry display unusual confirm curtain upgrade antique rotate hello\
      \ void custom frequent obey nut hole price zoo"
    , "exile ask congress lamp submit jacket era scheme attend cousin alcohol\
      \ catch course end lucky hurt sentence oven short ball bird grab wing zoo"
    , "board flee heavy tunnel powder denial science ski answer betray cargo\
      \ zoo"
    , "board blade invite damage undo sun mimic interest slam gaze truly\
      \ inherit resist great inject rocket museum zoo"
    , "beyond stage sleep clip because twist token leaf atom beauty genius food\
      \ business side grid unable middle armed observe pair crouch tonight away\
      \ zoo"
    ]

binWordsToBS :: Serialize a => [a] -> BS.ByteString
binWordsToBS = foldr f BS.empty
  where
    f b a = a `BS.append` encode b

{- Encode mnemonic -}

toMnemonic128 :: (Word64, Word64) -> Bool
toMnemonic128 (a, b) = l == 12
  where
    bs = encode a `BS.append` encode b
    l =
        length .
        T.words . fromRight (error "Could not decode mnemonic senttence") $
        toMnemonic bs

toMnemonic160 :: (Word32, Word64, Word64) -> Bool
toMnemonic160 (a, b, c) = l == 15
  where
    bs = BS.concat [encode a, encode b, encode c]
    l =
        length .
        T.words . fromRight (error "Colud not decode mnemonic sentence") $
        toMnemonic bs

toMnemonic256 :: (Word64, Word64, Word64, Word64) -> Bool
toMnemonic256 (a, b, c, d) = l == 24
  where
    bs = BS.concat [encode a, encode b, encode c, encode d]
    l =
        length .
        T.words . fromRight (error "Could not decode mnemonic sentence") $
        toMnemonic bs

toMnemonic512 ::
    ((Word64, Word64, Word64, Word64), (Word64, Word64, Word64, Word64)) -> Bool
toMnemonic512 ((a, b, c, d), (e, f, g, h)) = l == 48
  where
    bs =
        BS.concat
            [ encode a
            , encode b
            , encode c
            , encode d
            , encode e
            , encode f
            , encode g
            , encode h
            ]
    l =
        length .
        T.words . fromRight (error "Colud not decode mnemoonic sentence") $
        toMnemonic bs

toMnemonicVar :: [Word32] -> Property
toMnemonicVar ls = not (null ls) && length ls <= 8 ==> l == wc
  where
    bs = binWordsToBS ls
    bl = BS.length bs
    cb = bl `div` 4
    wc = (cb + bl * 8) `div` 11
    l =
        length . T.words .
        fromRight (error "Could not decode mnemonic sentence") $
        toMnemonic bs

{- Encode/Decode -}

fromToMnemonic128 :: (Word64, Word64) -> Bool
fromToMnemonic128 (a, b) = bs == bs'
  where
    bs = encode a `BS.append` encode b
    bs' =
        fromRight
            (error "Colud not decode mnemonic entropy")
            (fromMnemonic =<< toMnemonic bs)

fromToMnemonic160 :: (Word32, Word64, Word64) -> Bool
fromToMnemonic160 (a, b, c) = bs == bs'
  where
    bs = BS.concat [encode a, encode b, encode c]
    bs' =
        fromRight
            (error "Could not decode mnemonic entropy")
            (fromMnemonic =<< toMnemonic bs)

fromToMnemonic256 :: (Word64, Word64, Word64, Word64) -> Bool
fromToMnemonic256 (a, b, c, d) = bs == bs'
  where
    bs = BS.concat [encode a, encode b, encode c, encode d]
    bs' =
        fromRight
            (error "Could not decode mnemonic entropy")
            (fromMnemonic =<< toMnemonic bs)

fromToMnemonic512 ::
    ((Word64, Word64, Word64, Word64), (Word64, Word64, Word64, Word64)) -> Bool
fromToMnemonic512 ((a, b, c, d), (e, f, g, h)) = bs == bs'
  where
    bs =
        BS.concat
            [ encode a
            , encode b
            , encode c
            , encode d
            , encode e
            , encode f
            , encode g
            , encode h
            ]
    bs' =
        fromRight
            (error "Could not decode mnemonic entropy")
            (fromMnemonic =<< toMnemonic bs)

fromToMnemonicVar :: [Word32] -> Property
fromToMnemonicVar ls = not (null ls) && length ls <= 8 ==> bs == bs'
  where
    bs = binWordsToBS ls
    bs' =
        fromRight
            (error "Colud not decode mnemonic entropy")
            (fromMnemonic =<< toMnemonic bs)

{- Mnemonic to seed -}

mnemonicToSeed128 :: (Word64, Word64) -> Bool
mnemonicToSeed128 (a, b) = l == 64
  where
    bs = encode a `BS.append` encode b
    seed =
        fromRight
            (error "Could not decode mnemonic seed")
            (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed160 :: (Word32, Word64, Word64) -> Bool
mnemonicToSeed160 (a, b, c) = l == 64
  where
    bs = BS.concat [encode a, encode b, encode c]
    seed =
        fromRight
            (error "Could not decode mnemonic seed")
            (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed256 :: (Word64, Word64, Word64, Word64) -> Bool
mnemonicToSeed256 (a, b, c, d) = l == 64
  where
    bs = BS.concat [encode a, encode b, encode c, encode d]
    seed =
        fromRight
            (error "Colud not decode mnemonic seed")
            (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeed512 ::
    ((Word64, Word64, Word64, Word64), (Word64, Word64, Word64, Word64)) -> Bool
mnemonicToSeed512 ((a, b, c, d), (e, f, g, h)) = l == 64
  where
    bs =
        BS.concat
            [ encode a
            , encode b
            , encode c
            , encode d
            , encode e
            , encode f
            , encode g
            , encode h
            ]
    seed =
        fromRight
            (error "Could not decode mnemonic seed")
            (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

mnemonicToSeedVar :: [Word32] -> Property
mnemonicToSeedVar ls = not (null ls) && length ls <= 16 ==> l == 64
  where
    bs = binWordsToBS ls
    seed =
        fromRight
            (error "Could not decode mnemonic seed")
            (mnemonicToSeed "" =<< toMnemonic bs)
    l = BS.length seed

{- Get bits from ByteString -}

data ByteCountGen = ByteCountGen BS.ByteString Int deriving Show

instance Arbitrary ByteCountGen where
    arbitrary = do
        bs <- arbitraryBS
        i <- choose (0, BS.length bs * 8)
        return $ ByteCountGen bs i

getBitsByteCount :: ByteCountGen -> Bool
getBitsByteCount (ByteCountGen bs i) = BS.length bits == l
  where
    (q, r) = i `quotRem` 8
    bits = getBits i bs
    l = if r == 0 then q else q + 1

getBitsEndBits :: ByteCountGen -> Bool
getBitsEndBits (ByteCountGen bs i) =
    (r == 0) || (BS.last bits .&. (0xff `shiftR` r) == 0x00)
  where
    r = i `mod` 8
    bits = getBits i bs
