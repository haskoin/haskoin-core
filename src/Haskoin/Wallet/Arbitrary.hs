module Haskoin.Wallet.Arbitrary where

import Test.QuickCheck
import Haskoin.Crypto.Arbitrary
import Haskoin.Protocol.Arbitrary

import Control.Monad
import Control.Applicative

import Data.Word
import Data.Maybe

import Haskoin.Wallet
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Store
import Haskoin.Crypto

instance Arbitrary XPrvKey where
    arbitrary = XPrvKey <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> genPrvKeyC

instance Arbitrary XPubKey where
    arbitrary = deriveXPubKey <$> arbitrary

instance Arbitrary MasterKey where
    arbitrary = fromJust . makeMasterKey <$> arbitrary

instance Arbitrary AccPrvKey where
    arbitrary = do
        master <- arbitrary
        index  <- choose (0,0x7fffffff)
        return $ fromJust $ accPrvKey master index

instance Arbitrary AccPubKey where
    arbitrary = do
        master <- arbitrary
        index  <- choose (0,0x7fffffff)
        return $ fromJust $ accPubKey master index

instance Arbitrary AddrPrvKey where
    arbitrary = do
        accKey <- arbitrary
        index  <- arbitrary
        elements [ fromJust $ extPrvKey accKey index
                 , fromJust $ intPrvKey accKey index
                 ]

instance Arbitrary AddrPubKey where
    arbitrary = do
        accKey <- arbitrary
        index  <- arbitrary
        elements [ fromJust $ extPubKey accKey index
                 , fromJust $ intPubKey accKey index
                 ]

instance Arbitrary AccountData where
    arbitrary = AccountData <$> arbitrary
                            <*> arbitrary
                            <*> (choose (1,0x7fffffff))
                            <*> arbitrary
                            <*> arbitrary
                            <*> (choose (1,0x7fffffff))
                            <*> arbitrary
                            <*> (choose (1,0x7fffffff))
                            <*> (choose (1,0x7fffffff))

instance Arbitrary DBAccount where
    arbitrary = oneof [ DBAccount <$> arbitrary 
                      , DBAccountMS <$> arbitrary
                                    <*> arbitrary
                                    <*> (choose (1,16))
                                    <*> arbitrary
                      ]

instance Arbitrary DBAddress where
    arbitrary = DBAddress <$> arbitrary 
                          <*> arbitrary 
                          <*> arbitrary 
                          <*> (choose (1,0x7fffffff))
                          <*> arbitrary
                          <*> (choose (1,0x7fffffff))
                          <*> arbitrary

instance Arbitrary DBCoin where
    arbitrary = DBCoin <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> (choose (1,0x7fffffff))
                       <*> (choose (1,0x7fffffff))

instance Arbitrary DBConfig where
    arbitrary = DBConfig <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> (choose (1,0x7fffffff))
                         <*> arbitrary
                         <*> (choose (1,0x7fffffff))


