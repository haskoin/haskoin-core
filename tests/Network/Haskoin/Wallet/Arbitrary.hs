module Network.Haskoin.Wallet.Arbitrary 
( RequestPair(..)
) where

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
import Network.Haskoin.Server.Types

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

instance Arbitrary WalletRequest where
    arbitrary = oneof
        [ NewWallet <$> arbitrary <*> arbitrary <*> arbitrary
        , GetWallet <$> arbitrary
        , return WalletList
        , NewAccount <$> arbitrary <*> arbitrary
        , NewMSAccount <$> arbitrary 
                       <*> arbitrary
                       <*> (choose (1,16))
                       <*> (choose (1,16))
                       <*> (flip vectorOf (getAccPubKey <$> genKey) =<< choose (1,10))
        , NewReadAccount <$> arbitrary <*> (getAccPubKey <$> genKey)
        , NewReadMSAccount <$> arbitrary 
                           <*> arbitrary 
                           <*> arbitrary 
                           <*> (flip vectorOf (getAccPubKey <$> genKey) =<< choose (1,10))
        , AddAccountKeys <$> arbitrary 
                         <*> (flip vectorOf (getAccPubKey <$> genKey) =<< choose (1,10))
        , GetAccount <$> arbitrary
        , return AccountList
        , GenAddress <$> arbitrary <*> (choose (1,maxBound))
        , AddressLabel <$> arbitrary 
                       <*> (choose (0, clearBit maxBound 31))
                       <*> arbitrary
        , AddressList <$> arbitrary
        , AddressPage <$> arbitrary 
                      <*> (abs <$> arbitrary) 
                      <*> (choose (1, maxBound))
        , TxList <$> arbitrary
        , TxPage <$> arbitrary 
                 <*> (abs <$> arbitrary)
                 <*> (choose (1, maxBound))
        , TxSend <$> arbitrary 
                 <*> (flip vectorOf (liftM2 (,) genAddr arbitrary) =<< choose (0,10))
                 <*> arbitrary
        , TxSign <$> arbitrary <*> genTx
        , GetSigBlob <$> arbitrary <*> (fromInteger <$> arbitrary)
        , SignSigBlob <$> arbitrary <*> arbitrary
        , TxGet <$> (fromInteger <$> arbitrary)
        , Balance <$> arbitrary
        , return $ Rescan Nothing
        , Rescan . Just <$> arbitrary
        ]

data RequestPair = RequestPair WalletRequest WalletResponse
    deriving (Eq, Show, Read)

instance Arbitrary RequestPair where
    arbitrary = do
        req <- arbitrary
        res <- go req
        return $ RequestPair req res
      where
        go (NewWallet _ _ _) = ResMnemonic <$> arbitrary
        go (GetWallet _) = ResWallet <$> arbitrary
        go WalletList = 
            ResWalletList <$> (flip vectorOf arbitrary =<< choose (0,10))
        go (NewAccount _ _) = ResAccount <$> arbitrary
        go (NewMSAccount _ _ _ _ _) = ResAccount <$> arbitrary
        go (NewReadAccount _ _) = ResAccount <$> arbitrary
        go (NewReadMSAccount _ _ _ _) = ResAccount <$> arbitrary
        go (AddAccountKeys _ _) = ResAccount <$> arbitrary
        go (GetAccount _) = ResAccount <$> arbitrary
        go AccountList = ResAccountList <$> arbitrary
        go (GenAddress _ _) = 
            ResAddressList <$> (flip vectorOf arbitrary =<< choose (0,10))
        go (AddressLabel _ _ _) = ResAddress <$> arbitrary
        go (AddressList _) = 
            ResAddressList <$> (flip vectorOf arbitrary =<< choose (0,10))
        go (AddressPage _ _ _) =
            ResAddressPage <$> (flip vectorOf arbitrary =<< choose (0,10)) 
                           <*> (abs <$> arbitrary)
        go (TxList _) = 
            ResAccTxList <$> (flip vectorOf arbitrary =<< choose (0,10))
        go (TxPage _ _ _) = 
            ResAccTxPage <$> (flip vectorOf arbitrary =<< choose (0,10)) 
                         <*> (abs <$> arbitrary)
        go (TxSend _ _ _) = 
            ResTxHashStatus <$> (fromInteger <$> arbitrary) <*> arbitrary
        go (TxSign _ _) = 
            ResTxHashStatus <$> (fromInteger <$> arbitrary) <*> arbitrary
        go (GetSigBlob _ _) = ResSigBlob <$> arbitrary
        go (SignSigBlob _ _) = ResTxStatus <$> genTx <*> arbitrary
        go (TxGet _) = ResTx <$> genTx
        go (Balance _) = ResBalance <$> arbitrary
        go (Rescan _) = ResRescan <$> arbitrary

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
genScriptOutput = oneof 
    [ PayPK <$> (xPubKey . getAccPubKey <$> genKey)
    , (PayPKHash . pubKeyAddr) <$> (xPubKey . getAccPubKey <$> genKey)
    , genPayMulSig =<< choose (1,16)
    , (PayScriptHash . scriptAddr) <$> genScriptOutput
    ]

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

