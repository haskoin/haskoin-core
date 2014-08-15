module Network.Haskoin.Wallet.Arbitrary where

import Test.QuickCheck

import Data.Maybe

import Control.Monad
import Control.Applicative 

import Data.Bits (clearBit)
import qualified Data.ByteString as BS 
    ( ByteString
    , pack
    , drop
    )

import Network.Haskoin.Wallet.Types
import Network.Haskoin.REST.Types

import Network.Haskoin.Script
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto

-- Arbitrary instance for strict ByteStrings
-- TODO: Remove this if we integrate the project into Haskoin (use Util)
instance Arbitrary BS.ByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n <- choose (0, 2)
        return $ BS.drop n bs -- to give us some with non-0 offset

-- TODO: Rewrite this correctly if we integrate into Haskoin
instance Arbitrary Wallet where
    arbitrary = Wallet <$> arbitrary <*> (fromJust . makeMasterKey <$> arbitrary)
            
-- TODO: Rewrite this correctly if we integrate into Haskoin
instance Arbitrary Account where
    arbitrary = oneof
        [ RegularAccount <$> arbitrary <*> arbitrary <*> arbitrary <*> genKey
        , MultisigAccount <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> (flip vectorOf (getAccPubKey <$> genKey) =<< choose (1,16))
        , ReadAccount <$> arbitrary <*> genKey
        , ReadMSAccount <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> (flip vectorOf (getAccPubKey <$> genKey) =<< choose (1,16))
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
                      <*> arbitrary
                      <*> (abs <$> arbitrary)

instance Arbitrary TxConfidence where
    arbitrary = elements [ TxOffline, TxDead, TxPending, TxBuilding ]

instance Arbitrary TxSource where
    arbitrary = elements [ NetworkSource, WalletSource, UnknownSource ]

instance Arbitrary SigBlob where
    arbitrary = SigBlob <$> ((flip vectorOf genDat) =<< choose (1,10))
                        <*> genTx

genDat :: Gen (OutPoint, ScriptOutput, Bool, KeyIndex)
genDat = (,,,) <$> genOutPoint
               <*> genScriptOutput
               <*> arbitrary
               <*> arbitrary

genScriptOutput :: Gen ScriptOutput
genScriptOutput = oneof [ genSimpleOutput
                        , genPaySHOutput
                        ]

genSimpleOutput :: Gen ScriptOutput
genSimpleOutput = oneof
    [ PayPK <$> (xPubKey . getAccPubKey <$> genKey)
    , (PayPKHash . pubKeyAddr) <$> (xPubKey . getAccPubKey <$> genKey)
    , genPayMulSig =<< choose (1,16)
    ]
    
genPaySHOutput :: Gen ScriptOutput
genPaySHOutput = (PayScriptHash . scriptAddr) <$> genSimpleOutput

genPayMulSig :: Int -> Gen ScriptOutput
genPayMulSig m = do
    n <- choose (m,16)
    PayMulSig <$> (vectorOf n (xPubKey . getAccPubKey <$> genKey)) <*> (return m)

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

-- TODO: Remove this if we integrate into Haskoin
genTx :: Gen Tx
genTx = Tx <$> arbitrary 
           <*> (flip vectorOf genTxIn =<< choose (0,10))
           <*> (flip vectorOf genTxOut =<< choose (0,10))
           <*> arbitrary

-- TODO: Remove this if we integrate into Haskoin
genTxIn :: Gen TxIn
genTxIn = TxIn <$> genOutPoint <*> arbitrary <*> arbitrary

-- TODO: Remove this if we integrate into Haskoin
genTxOut :: Gen TxOut
genTxOut = TxOut <$> (choose (1,2100000000000000)) <*> arbitrary

-- TODO: Remove this if we integrate into Haskoin
genOutPoint :: Gen OutPoint
genOutPoint = OutPoint <$> (fromInteger <$> arbitrary) <*> arbitrary

