module Network.Haskoin.Wallet.Arbitrary () where

import Test.QuickCheck

import Data.Maybe

import Control.Monad
import Control.Applicative 

import Data.Bits (clearBit)
import qualified Data.ByteString as BS 
    ( ByteString
    , pack
    , drop
    , null
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Wallet.Types

-- Arbitrary instance for strict ByteStrings
-- TODO: Remove this if we integrate the project into Haskoin (use Util)
instance Arbitrary BS.ByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n <- choose (0, 2)
        return $ BS.drop n bs -- to give us some with non-0 offset

-- TODO: Rewrite this correctly if we integrate into Haskoin
instance Arbitrary Wallet where
    arbitrary = oneof
        [ WalletFull <$> arbitrary <*> (fromJust . makeMasterKey <$> arbitrary)
        , WalletRead <$> arbitrary <*> (getAccPubKey <$> genKey)
        ]
            
-- TODO: Rewrite this correctly if we integrate into Haskoin
instance Arbitrary Account where
    arbitrary = oneof
        [ RegularAccount <$> arbitrary <*> arbitrary <*> arbitrary <*> genKey
        , MultisigAccount <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> genKey
                          <*> arbitrary
                          <*> arbitrary
                          <*> (flip vectorOf (getAccPubKey <$> genKey) =<< choose (0,16))
        ]

-- TODO: Rewrite this correctly if we integrate into Haskoin
instance Arbitrary PaymentAddress where
    arbitrary = PaymentAddress <$> genAddr <*> arbitrary <*> arbitrary

-- TODO: Rewrite this correctly if we integrate into Haskoin
instance Arbitrary AccTx where
    arbitrary = AccTx <$> (fromInteger <$> arbitrary)
                      <*> (flip vectorOf genAddr =<< choose (1,10))
                      <*> arbitrary
                      <*> arbitrary
                      <*> (abs <$> arbitrary)

-- TODO: Remove this if we integrate into Haskoin
genKey :: Gen AccPubKey
genKey = do
    bs <- arbitrary
    deriv <- choose (0, clearBit maxBound 31)
    return $ fromJust $ do
        master <- makeMasterKey bs
        accPubKey master deriv
            
-- TODO: Remove this if we integrate into Haskoin
genAddr :: Gen Address
genAddr = oneof [ PubKeyAddress <$> (fromInteger <$> arbitrary)
                , ScriptAddress <$> (fromInteger <$> arbitrary)
                ]
