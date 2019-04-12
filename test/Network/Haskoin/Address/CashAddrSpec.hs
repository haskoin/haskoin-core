{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Address.CashAddrSpec (spec) where

import           Control.Monad
import qualified Data.ByteString.Char8            as C
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                        (Text)
import           Network.Haskoin.Address
import           Network.Haskoin.Address.CashAddr
import           Network.Haskoin.Constants
import           Network.Haskoin.Util
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = do
    describe "cashaddr checksum test vectors" $ do
        it "prefix:x64nx6hz" $ do
            let mpb = cash32decode "prefix:x64nx6hz"
            mpb `shouldBe` Just ("prefix", "")
        it "p:gpf8m4h7" $ do
            let mpb = cash32decode "p:gpf8m4h7"
            mpb `shouldBe` Just ("p", "")
        it "bitcoincash:qpzry9x8gf2tvdw0s3jn54khce6mua7lcw20ayyn" $ do
            let mpb =
                    cash32decode
                        "bitcoincash:qpzry9x8gf2tvdw0s3jn54khce6mua7lcw20ayyn"
            mpb `shouldBe`
                Just
                    ( "bitcoincash"
                    , "\NULD2\DC4\199BT\182\&5\207\132e:V\215\198u\190w\223")
        it "bchtest:testnetaddress4d6njnut" $ do
            let mpb = cash32decode "bchtest:testnetaddress4d6njnut"
            mpb `shouldBe` Just ("bchtest", "^`\185\229}kG\152")
        it "bchreg:555555555555555555555555555555555555555555555udxmlmrz" $ do
            let mpb =
                    cash32decode
                        "bchreg:555555555555555555555555555555555555555555555udxmlmrz"
            mpb `shouldBe`
                Just
                    ( "bchreg"
                    , "\165)JR\148\165)JR\148\165)JR\148\165)JR\148\165)JR\148\165)J")
    describe "cashaddr to base58 translation test vectors" $ do
        it "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu" $ do
            let addr =
                    addrToString bch =<<
                    stringToAddr btc "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu"
            addr `shouldBe`
                Just "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"
        it "1KXrWXciRDZUpQwQmuM1DbwsKDLYAYsVLR" $ do
            let addr =
                    addrToString bch =<<
                    stringToAddr btc "1KXrWXciRDZUpQwQmuM1DbwsKDLYAYsVLR"
            addr `shouldBe`
                Just "bitcoincash:qr95sy3j9xwd2ap32xkykttr4cvcu7as4y0qverfuy"
        it "16w1D5WRVKJuZUsSRzdLp9w3YGcgoxDXb" $ do
            let addr =
                    addrToString bch =<<
                    stringToAddr btc "16w1D5WRVKJuZUsSRzdLp9w3YGcgoxDXb"
            addr `shouldBe`
                Just "bitcoincash:qqq3728yw0y47sqn6l2na30mcw6zm78dzqre909m2r"
        it "3CWFddi6m4ndiGyKqzYvsFYagqDLPVMTzC" $ do
            let addr =
                    addrToString bch =<<
                    stringToAddr btc "3CWFddi6m4ndiGyKqzYvsFYagqDLPVMTzC"
            addr `shouldBe`
                Just "bitcoincash:ppm2qsznhks23z7629mms6s4cwef74vcwvn0h829pq"
        it "3LDsS579y7sruadqu11beEJoTjdFiFCdX4" $ do
            let addr =
                    addrToString bch =<<
                    stringToAddr btc "3LDsS579y7sruadqu11beEJoTjdFiFCdX4"
            addr `shouldBe`
                Just "bitcoincash:pr95sy3j9xwd2ap32xkykttr4cvcu7as4yc93ky28e"
        it "31nwvkZwyPdgzjBJZXfDmSWsC4ZLKpYyUw" $ do
            let addr =
                    addrToString bch =<<
                    stringToAddr btc "31nwvkZwyPdgzjBJZXfDmSWsC4ZLKpYyUw"
            addr `shouldBe`
                Just "bitcoincash:pqq3728yw0y47sqn6l2na30mcw6zm78dzq5ucqzc37"
    describe "base58 to cashaddr translation test vectors" $ do
        it "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a" $ do
            let addr =
                    addrToString btc =<<
                    stringToAddr
                        bch
                        "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"
            addr `shouldBe` Just "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu"
        it "bitcoincash:qr95sy3j9xwd2ap32xkykttr4cvcu7as4y0qverfuy" $ do
            let addr =
                    addrToString btc =<<
                    stringToAddr
                        bch
                        "bitcoincash:qr95sy3j9xwd2ap32xkykttr4cvcu7as4y0qverfuy"
            addr `shouldBe` Just "1KXrWXciRDZUpQwQmuM1DbwsKDLYAYsVLR"
        it "bitcoincash:qqq3728yw0y47sqn6l2na30mcw6zm78dzqre909m2r" $ do
            let addr =
                    addrToString btc =<<
                    stringToAddr
                        bch
                        "bitcoincash:qqq3728yw0y47sqn6l2na30mcw6zm78dzqre909m2r"
            addr `shouldBe` Just "16w1D5WRVKJuZUsSRzdLp9w3YGcgoxDXb"
        it "bitcoincash:ppm2qsznhks23z7629mms6s4cwef74vcwvn0h829pq" $ do
            let addr =
                    addrToString btc =<<
                    stringToAddr
                        bch
                        "bitcoincash:ppm2qsznhks23z7629mms6s4cwef74vcwvn0h829pq"
            addr `shouldBe` Just "3CWFddi6m4ndiGyKqzYvsFYagqDLPVMTzC"
        it "bitcoincash:pr95sy3j9xwd2ap32xkykttr4cvcu7as4yc93ky28e" $ do
            let addr =
                    addrToString btc =<<
                    stringToAddr
                        bch
                        "bitcoincash:pr95sy3j9xwd2ap32xkykttr4cvcu7as4yc93ky28e"
            addr `shouldBe` Just "3LDsS579y7sruadqu11beEJoTjdFiFCdX4"
        it "bitcoincash:pqq3728yw0y47sqn6l2na30mcw6zm78dzq5ucqzc37" $ do
            let addr =
                    addrToString btc =<<
                    stringToAddr
                        bch
                        "bitcoincash:pqq3728yw0y47sqn6l2na30mcw6zm78dzq5ucqzc37"
            addr `shouldBe` Just "31nwvkZwyPdgzjBJZXfDmSWsC4ZLKpYyUw"
    describe "cashaddr larger test vectors" $
        forM_ (zip [0 ..] vectors) $ \(i, vec) ->
            it ("cashaddr test vector " <> show (i :: Int)) $ testCashAddr vec

{- Various utilities -}

testCashAddr :: (Int, CashVersion, Cash32, Text) -> Assertion
testCashAddr (len, typ, addr, hex) = do
    let mbs = decodeHex hex
    assertBool "Could not decode hex payload from test vector" (isJust mbs)
    let mlow = cash32decode addr
    assertBool "Could not decode low level address" (isJust mlow)
    let Just (_, lbs) = mlow
    assertEqual "Low-level payload size incorrect" len (C.length lbs - 1)
    assertEqual "Low-level payload doesn't match" bs (C.tail lbs)
    let mdec = cash32decodeType addr
    assertBool ("Could not decode test address: " <> cs addr) (isJust mdec)
    assertEqual "Length doesn't match" len (C.length pay)
    assertEqual "Version doesn't match" typ ver
    assertEqual "Payload doesn't match" bs pay
  where
    Just bs = decodeHex hex
    Just (_, ver, pay) = cash32decodeType addr

-- | All vectors starting with @pref@ had the wrong version in the spec
-- document.
vectors :: [(Int, CashVersion, Text, Text)]
vectors =
    [ ( 20
      , 0
      , "bitcoincash:qr6m7j9njldwwzlg9v7v53unlr4jkmx6eylep8ekg2"
      , "F5BF48B397DAE70BE82B3CCA4793F8EB2B6CDAC9")
    , ( 20
      , 1
      , "bchtest:pr6m7j9njldwwzlg9v7v53unlr4jkmx6eyvwc0uz5t"
      , "F5BF48B397DAE70BE82B3CCA4793F8EB2B6CDAC9")
    , ( 20
      , 1
      , "pref:pr6m7j9njldwwzlg9v7v53unlr4jkmx6ey65nvtks5"
      , "F5BF48B397DAE70BE82B3CCA4793F8EB2B6CDAC9")
    , ( 20
      , 15
      , "prefix:0r6m7j9njldwwzlg9v7v53unlr4jkmx6ey3qnjwsrf"
      , "F5BF48B397DAE70BE82B3CCA4793F8EB2B6CDAC9")
    , ( 24
      , 0
      , "bitcoincash:q9adhakpwzztepkpwp5z0dq62m6u5v5xtyj7j3h2ws4mr9g0"
      , "7ADBF6C17084BC86C1706827B41A56F5CA32865925E946EA")
    , ( 24
      , 1
      , "bchtest:p9adhakpwzztepkpwp5z0dq62m6u5v5xtyj7j3h2u94tsynr"
      , "7ADBF6C17084BC86C1706827B41A56F5CA32865925E946EA")
    , ( 24
      , 1
      , "pref:p9adhakpwzztepkpwp5z0dq62m6u5v5xtyj7j3h2khlwwk5v"
      , "7ADBF6C17084BC86C1706827B41A56F5CA32865925E946EA")
    , ( 24
      , 15
      , "prefix:09adhakpwzztepkpwp5z0dq62m6u5v5xtyj7j3h2p29kc2lp"
      , "7ADBF6C17084BC86C1706827B41A56F5CA32865925E946EA")
    , ( 28
      , 0
      , "bitcoincash:qgagf7w02x4wnz3mkwnchut2vxphjzccwxgjvvjmlsxqwkcw59jxxuz"
      , "3A84F9CF51AAE98A3BB3A78BF16A6183790B18719126325BFC0C075B")
    , ( 28
      , 1
      , "bchtest:pgagf7w02x4wnz3mkwnchut2vxphjzccwxgjvvjmlsxqwkcvs7md7wt"
      , "3A84F9CF51AAE98A3BB3A78BF16A6183790B18719126325BFC0C075B")
    , ( 28
      , 1
      , "pref:pgagf7w02x4wnz3mkwnchut2vxphjzccwxgjvvjmlsxqwkcrsr6gzkn"
      , "3A84F9CF51AAE98A3BB3A78BF16A6183790B18719126325BFC0C075B")
    , ( 28
      , 15
      , "prefix:0gagf7w02x4wnz3mkwnchut2vxphjzccwxgjvvjmlsxqwkc5djw8s9g"
      , "3A84F9CF51AAE98A3BB3A78BF16A6183790B18719126325BFC0C075B")
    , ( 32
      , 0
      , "bitcoincash:qvch8mmxy0rtfrlarg7ucrxxfzds5pamg73h7370aa87d80gyhqxq5nlegake"
      , "3173EF6623C6B48FFD1A3DCC0CC6489B0A07BB47A37F47CFEF4FE69DE825C060")
    , ( 32
      , 1
      , "bchtest:pvch8mmxy0rtfrlarg7ucrxxfzds5pamg73h7370aa87d80gyhqxq7fqng6m6"
      , "3173EF6623C6B48FFD1A3DCC0CC6489B0A07BB47A37F47CFEF4FE69DE825C060")
    , ( 32
      , 1
      , "pref:pvch8mmxy0rtfrlarg7ucrxxfzds5pamg73h7370aa87d80gyhqxq4k9m7qf9"
      , "3173EF6623C6B48FFD1A3DCC0CC6489B0A07BB47A37F47CFEF4FE69DE825C060")
    , ( 32
      , 15
      , "prefix:0vch8mmxy0rtfrlarg7ucrxxfzds5pamg73h7370aa87d80gyhqxqsh6jgp6w"
      , "3173EF6623C6B48FFD1A3DCC0CC6489B0A07BB47A37F47CFEF4FE69DE825C060")
    , ( 40
      , 0
      , "bitcoincash:qnq8zwpj8cq05n7pytfmskuk9r4gzzel8qtsvwz79zdskftrzxtar994cgutavfklv39gr3uvz"
      , "C07138323E00FA4FC122D3B85B9628EA810B3F381706385E289B0B25631197D194B5C238BEB136FB")
    , ( 40
      , 1
      , "bchtest:pnq8zwpj8cq05n7pytfmskuk9r4gzzel8qtsvwz79zdskftrzxtar994cgutavfklvmgm6ynej"
      , "C07138323E00FA4FC122D3B85B9628EA810B3F381706385E289B0B25631197D194B5C238BEB136FB")
    , ( 40
      , 1
      , "pref:pnq8zwpj8cq05n7pytfmskuk9r4gzzel8qtsvwz79zdskftrzxtar994cgutavfklv0vx5z0w3"
      , "C07138323E00FA4FC122D3B85B9628EA810B3F381706385E289B0B25631197D194B5C238BEB136FB")
    , ( 40
      , 15
      , "prefix:0nq8zwpj8cq05n7pytfmskuk9r4gzzel8qtsvwz79zdskftrzxtar994cgutavfklvwsvctzqy"
      , "C07138323E00FA4FC122D3B85B9628EA810B3F381706385E289B0B25631197D194B5C238BEB136FB")
    , ( 48
      , 0
      , "bitcoincash:qh3krj5607v3qlqh5c3wq3lrw3wnuxw0sp8dv0zugrrt5a3kj6ucysfz8kxwv2k53krr7n933jfsunqex2w82sl"
      , "E361CA9A7F99107C17A622E047E3745D3E19CF804ED63C5C40C6BA763696B98241223D8CE62AD48D863F4CB18C930E4C")
    , ( 48
      , 1
      , "bchtest:ph3krj5607v3qlqh5c3wq3lrw3wnuxw0sp8dv0zugrrt5a3kj6ucysfz8kxwv2k53krr7n933jfsunqnzf7mt6x"
      , "E361CA9A7F99107C17A622E047E3745D3E19CF804ED63C5C40C6BA763696B98241223D8CE62AD48D863F4CB18C930E4C")
    , ( 48
      , 1
      , "pref:ph3krj5607v3qlqh5c3wq3lrw3wnuxw0sp8dv0zugrrt5a3kj6ucysfz8kxwv2k53krr7n933jfsunqjntdfcwg"
      , "E361CA9A7F99107C17A622E047E3745D3E19CF804ED63C5C40C6BA763696B98241223D8CE62AD48D863F4CB18C930E4C")
    , ( 48
      , 15
      , "prefix:0h3krj5607v3qlqh5c3wq3lrw3wnuxw0sp8dv0zugrrt5a3kj6ucysfz8kxwv2k53krr7n933jfsunqakcssnmn"
      , "E361CA9A7F99107C17A622E047E3745D3E19CF804ED63C5C40C6BA763696B98241223D8CE62AD48D863F4CB18C930E4C")
    , ( 56
      , 0
      , "bitcoincash:qmvl5lzvdm6km38lgga64ek5jhdl7e3aqd9895wu04fvhlnare5937w4ywkq57juxsrhvw8ym5d8qx7sz7zz0zvcypqscw8jd03f"
      , "D9FA7C4C6EF56DC4FF423BAAE6D495DBFF663D034A72D1DC7D52CBFE7D1E6858F9D523AC0A7A5C34077638E4DD1A701BD017842789982041")
    , ( 56
      , 1
      , "bchtest:pmvl5lzvdm6km38lgga64ek5jhdl7e3aqd9895wu04fvhlnare5937w4ywkq57juxsrhvw8ym5d8qx7sz7zz0zvcypqs6kgdsg2g"
      , "D9FA7C4C6EF56DC4FF423BAAE6D495DBFF663D034A72D1DC7D52CBFE7D1E6858F9D523AC0A7A5C34077638E4DD1A701BD017842789982041")
    , ( 56
      , 1
      , "pref:pmvl5lzvdm6km38lgga64ek5jhdl7e3aqd9895wu04fvhlnare5937w4ywkq57juxsrhvw8ym5d8qx7sz7zz0zvcypqsammyqffl"
      , "D9FA7C4C6EF56DC4FF423BAAE6D495DBFF663D034A72D1DC7D52CBFE7D1E6858F9D523AC0A7A5C34077638E4DD1A701BD017842789982041")
    , ( 56
      , 15
      , "prefix:0mvl5lzvdm6km38lgga64ek5jhdl7e3aqd9895wu04fvhlnare5937w4ywkq57juxsrhvw8ym5d8qx7sz7zz0zvcypqsgjrqpnw8"
      , "D9FA7C4C6EF56DC4FF423BAAE6D495DBFF663D034A72D1DC7D52CBFE7D1E6858F9D523AC0A7A5C34077638E4DD1A701BD017842789982041")
    , ( 64
      , 0
      , "bitcoincash:qlg0x333p4238k0qrc5ej7rzfw5g8e4a4r6vvzyrcy8j3s5k0en7calvclhw46hudk5flttj6ydvjc0pv3nchp52amk97tqa5zygg96mtky5sv5w"
      , "D0F346310D5513D9E01E299978624BA883E6BDA8F4C60883C10F28C2967E67EC77ECC7EEEAEAFC6DA89FAD72D11AC961E164678B868AEEEC5F2C1DA08884175B")
    , ( 64
      , 1
      , "bchtest:plg0x333p4238k0qrc5ej7rzfw5g8e4a4r6vvzyrcy8j3s5k0en7calvclhw46hudk5flttj6ydvjc0pv3nchp52amk97tqa5zygg96mc773cwez"
      , "D0F346310D5513D9E01E299978624BA883E6BDA8F4C60883C10F28C2967E67EC77ECC7EEEAEAFC6DA89FAD72D11AC961E164678B868AEEEC5F2C1DA08884175B")
    , ( 64
      , 1
      , "pref:plg0x333p4238k0qrc5ej7rzfw5g8e4a4r6vvzyrcy8j3s5k0en7calvclhw46hudk5flttj6ydvjc0pv3nchp52amk97tqa5zygg96mg7pj3lh8"
      , "D0F346310D5513D9E01E299978624BA883E6BDA8F4C60883C10F28C2967E67EC77ECC7EEEAEAFC6DA89FAD72D11AC961E164678B868AEEEC5F2C1DA08884175B")
    , ( 64
      , 15
      , "prefix:0lg0x333p4238k0qrc5ej7rzfw5g8e4a4r6vvzyrcy8j3s5k0en7calvclhw46hudk5flttj6ydvjc0pv3nchp52amk97tqa5zygg96ms92w6845"
      , "D0F346310D5513D9E01E299978624BA883E6BDA8F4C60883C10F28C2967E67EC77ECC7EEEAEAFC6DA89FAD72D11AC961E164678B868AEEEC5F2C1DA08884175B")
    ]
