module Haskoin.Wallet.Arbitrary where

import Test.QuickCheck
import Haskoin.Crypto.Arbitrary
import Haskoin.Protocol.Arbitrary

import Control.Monad
import Control.Applicative

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

instance Arbitrary WAccount where
    arbitrary = do
        name   <- arbitrary
        index  <- arbitrary
        pos    <- choose (1,0x7fffffff)
        key    <- AccPubKey <$> arbitrary
        ext    <- arbitrary
        extC   <- choose (0,0x7fffffff)
        int    <- arbitrary
        intC   <- choose (0,0x7fffffff)
        msN    <- choose (1,16)
        msM    <- choose (1,msN)
        msKeys <- vectorOf (msN-1) arbitrary
        url    <- arbitrary
        elements [ WAccount name index pos key ext extC int intC
                 , WAccountMS name index pos key 
                        ext extC int intC msKeys msM url
                 ]

instance Arbitrary WAddr where
    arbitrary = WAddr <$> arbitrary 
                      <*> arbitrary 
                      <*> arbitrary 
                      <*> (choose (1,0x7fffffff))
                      <*> arbitrary
                      <*> (choose (1,0x7fffffff))

instance Arbitrary DBKey where
    arbitrary = oneof [ KeyConfig <$> arbitrary
                      , KeyAcc <$> choose (1,0x7fffffff)
                      , KeyAccMap <$> arbitrary
                      , KeyExtAddr <$> (choose (1,0x7fffffff)) 
                                   <*> (choose (1,0x7fffffff))
                      , KeyExtAddrMap <$> arbitrary
                      , KeyIntAddr <$> (choose (1,0x7fffffff)) 
                                   <*> (choose (1,0x7fffffff))
                      , KeyIntAddrMap <$> arbitrary
                      ]


