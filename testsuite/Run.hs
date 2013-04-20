module Main where

import Test.Framework 
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Bitcoin.Protocol.Tests
import Bitcoin.Protocol.BigWord.Tests 

main = defaultMain 
    (Bitcoin.Protocol.BigWord.Tests.tests ++ Bitcoin.Protocol.Tests.tests)

