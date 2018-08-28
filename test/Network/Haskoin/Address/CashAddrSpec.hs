{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Address.CashAddrSpec (spec) where

import           Network.Haskoin.Address.CashAddr
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Test.Hspec

spec = do
  describe "cashaddr checksum test vectors" $ do
    it "prefix:x64nx6hz" $ do
      let mpb = cash32decode "prefix:x64nx6hz"
      mpb `shouldBe` Just ("prefix", "")
    it "p:gpf8m4h7" $ do
      let mpb = cash32decode "p:gpf8m4h7"
      mpb `shouldBe` Just ("p", "")
