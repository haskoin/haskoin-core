module Main where

import Test.Framework (defaultMain)

-- Util tests
import qualified Network.Haskoin.Util.Tests (tests)

-- Crypto tests
import qualified Network.Haskoin.Crypto.BigWord.Tests (tests)
import qualified Network.Haskoin.Crypto.Point.Tests (tests)
import qualified Network.Haskoin.Crypto.ECDSA.Tests (tests)
import qualified Network.Haskoin.Crypto.Base58.Tests (tests)
import qualified Network.Haskoin.Crypto.Base58.Units (tests)
import qualified Network.Haskoin.Crypto.Keys.Tests (tests)
import qualified Network.Haskoin.Crypto.ExtendedKeys.Tests (tests)
import qualified Network.Haskoin.Crypto.ExtendedKeys.Units (tests)
import qualified Network.Haskoin.Crypto.Hash.Tests (tests)
import qualified Network.Haskoin.Crypto.Hash.Units (tests)
import qualified Network.Haskoin.Crypto.Mnemonic.Tests (tests)
import qualified Network.Haskoin.Crypto.Mnemonic.Units (tests)
import qualified Network.Haskoin.Crypto.Bloom.Tests (tests)
import qualified Network.Haskoin.Crypto.Bloom.Units (tests)
import qualified Network.Haskoin.Crypto.Units (tests)

-- Protocol tests
import qualified Network.Haskoin.Protocol.Tests (tests)

-- Script tests
import qualified Network.Haskoin.Script.Tests (tests)
import qualified Network.Haskoin.Script.Units (tests)

-- Transaction tests
import qualified Network.Haskoin.Transaction.Tests (tests)
import qualified Network.Haskoin.Transaction.Units (tests)

-- Block tests
import qualified Network.Haskoin.Block.Tests (tests)
import qualified Network.Haskoin.Block.Units (tests)

-- Stratum tests
import qualified Network.Haskoin.Stratum.Tests (tests)
import qualified Network.Haskoin.Stratum.Units (tests)

-- Json tests
import qualified Network.Haskoin.Json.Tests (tests)

main :: IO ()
main = defaultMain
    (  Network.Haskoin.Util.Tests.tests
    ++ Network.Haskoin.Crypto.BigWord.Tests.tests 
    ++ Network.Haskoin.Crypto.Point.Tests.tests 
    ++ Network.Haskoin.Crypto.ECDSA.Tests.tests 
    ++ Network.Haskoin.Crypto.Base58.Tests.tests 
    ++ Network.Haskoin.Crypto.Base58.Units.tests 
    ++ Network.Haskoin.Crypto.Hash.Tests.tests 
    ++ Network.Haskoin.Crypto.Hash.Units.tests
    ++ Network.Haskoin.Crypto.Keys.Tests.tests 
    ++ Network.Haskoin.Crypto.ExtendedKeys.Tests.tests 
    ++ Network.Haskoin.Crypto.ExtendedKeys.Units.tests 
    ++ Network.Haskoin.Crypto.Mnemonic.Tests.tests 
    ++ Network.Haskoin.Crypto.Mnemonic.Units.tests 
    ++ Network.Haskoin.Crypto.Bloom.Tests.tests 
    ++ Network.Haskoin.Crypto.Bloom.Units.tests 
    ++ Network.Haskoin.Crypto.Units.tests
    ++ Network.Haskoin.Protocol.Tests.tests
    ++ Network.Haskoin.Script.Tests.tests
    ++ Network.Haskoin.Script.Units.tests
    ++ Network.Haskoin.Transaction.Tests.tests
    ++ Network.Haskoin.Transaction.Units.tests
    ++ Network.Haskoin.Block.Tests.tests
    ++ Network.Haskoin.Block.Units.tests
    ++ Network.Haskoin.Stratum.Tests.tests
    ++ Network.Haskoin.Stratum.Units.tests
    ++ Network.Haskoin.Json.Tests.tests
    )

