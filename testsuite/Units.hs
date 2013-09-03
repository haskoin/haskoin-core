module Units (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Maybe
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Util

-- BIP 0032 Test Vectors
-- https://en.bitcoin.it/wiki/BIP_0032_TestVectors

m1 :: Wallet
m1 = fromJust $ createMasterWallet $ BS.pack [0..15]

tests =
    [ testGroup "Test vector 1" $
        [ testCase "Chain m" v1m1
        ] 
    ]

v1m1 = do
    assertBool "id" $ 
        walletID m1 == 0x3442193e1bb70916e914552172cd4e2dbc9df811
    assertBool "fp" $ 
        walletFP m1 == 0x3442193e
    assertBool "address" $ 
        walletAddress m1 == stringToBS "15mKKb2eos1hWa6tisdPwwDC1a5J1y9nma"
    assertBool "wif" $ 
        walletWIF m1 == 
        stringToBS "L52XzL2cMkHxqxBXRyEpnPQZGUs3uKiL3R11XbAdHigRzDozKZeW"

