module Haskoin.Wallet.Arbitrary where

import Test.QuickCheck
import Haskoin.Crypto.Arbitrary
import Haskoin.Protocol.Arbitrary

import Control.Monad
import Control.Applicative

import Data.Word
import Data.Maybe
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Wallet.Keys
import Haskoin.Wallet.ScriptParser
import Haskoin.Crypto

instance Arbitrary XPrvKey where
    arbitrary = XPrvKey <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> genPrvKeyC

instance Arbitrary XPubKey where
    arbitrary = deriveXPubKey <$> arbitrary

instance Arbitrary TxSignature where
    arbitrary = liftM2 TxSignature arbitrary arbitrary

instance Arbitrary MulSig2Type where
    arbitrary = elements [ OneOfTwo, TwoOfTwo ] 

instance Arbitrary MulSig3Type where
    arbitrary = elements [ OneOfThree, TwoOfThree, ThreeOfThree ] 

instance Arbitrary SigHash where
    arbitrary = elements [ SigAll
                         , SigNone
                         , SigSingle
                         , SigAllAcp
                         , SigNoneAcp
                         , SigSingleAcp
                         ]

instance Arbitrary ScriptOutput where
    arbitrary = oneof 
        [ PayPK <$> arbitrary
        , (PayPKHash . pubKeyAddr) <$> arbitrary 
        , PayMulSig1 <$> arbitrary
        , PayMulSig2 <$> arbitrary <*> arbitrary <*> arbitrary
        , PayMulSig3 <$> arbitrary <*> arbitrary 
                     <*> arbitrary <*> arbitrary
        , (PayScriptHash . scriptAddr) <$> arbitrary
        , PayNonStd <$> arbitrary
        ]

instance Arbitrary ScriptInput where
    arbitrary = oneof
        [ SpendSig1 <$> arbitrary
        , SpendPKHash <$> arbitrary <*> arbitrary
        , SpendSig2 <$> arbitrary <*> arbitrary
        , SpendSig3 <$> arbitrary <*> arbitrary <*> arbitrary
        , SpendNonStd <$> arbitrary
        ]

instance Arbitrary ScriptHashInput where
    arbitrary = ScriptHashInput <$> arbitrary <*> arbitrary

