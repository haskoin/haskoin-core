module Network.Haskoin.Protocol.ArbitraryMsg () where

import Test.QuickCheck

import Control.Monad
import Control.Applicative 

import qualified Data.Sequence as S (fromList)

import Network.Haskoin.Protocol.Arbitrary ()
import Network.Haskoin.Block.Arbitrary ()
import Network.Haskoin.Transaction.Arbitrary ()

import Network.Haskoin.Protocol
import Network.Haskoin.Crypto

instance Arbitrary MessageHeader where
    arbitrary = MessageHeader <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary

instance Arbitrary Message where
    arbitrary = oneof [ MVersion    <$> arbitrary
                      , return MVerAck
                      , MAddr        <$> arbitrary
                      , MInv         <$> arbitrary
                      , MGetData     <$> arbitrary
                      , MNotFound    <$> arbitrary
                      , MGetBlocks   <$> arbitrary
                      , MGetHeaders  <$> arbitrary
                      , MTx          <$> arbitrary
                      , MBlock       <$> arbitrary
                      , MMerkleBlock <$> arbitrary
                      , MHeaders     <$> arbitrary
                      , return MGetAddr
                      , MFilterLoad  <$> arbitrary
                      , MFilterAdd   <$> arbitrary
                      , return MFilterClear
                      , MPing        <$> arbitrary
                      , MPong        <$> arbitrary
                      , MAlert       <$> arbitrary
                      ]

