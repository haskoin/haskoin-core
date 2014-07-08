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
    , null
    )

import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Server.Types

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

instance Arbitrary WalletRequest where
    arbitrary = oneof
        [ NewFullWallet <$> arbitrary <*> arbitrary <*> arbitrary
        -- Not implemented yet
        -- , NewReadWallet <$> arbitrary <*> arbitrary
        , GetWallet <$> arbitrary
        , return WalletList
        , NewAccount <$> arbitrary <*> arbitrary
        , NewMSAccount <$> arbitrary 
                       <*> arbitrary
                       <*> (choose (1,16))
                       <*> (choose (1,16))
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
        , Balance <$> arbitrary
        ]

data RequestPair = RequestPair WalletRequest (Either String WalletResponse)
    deriving (Eq, Show, Read)

instance Arbitrary RequestPair where
    arbitrary = do
        req <- arbitrary
        res <- frequency
            [ (1, Left <$> arbitrary)
            , (9, Right <$> go req)  -- Generate Right more frequently
            ]
        return $ RequestPair req res
      where
        go (NewFullWallet _ _ _) = ResMnemonic <$> arbitrary
        -- Not implemented yet
        -- go (NewReadWallet _ _, _) = error "Not implemented"
        go (GetWallet _) = ResWallet <$> arbitrary
        go WalletList = 
            ResWalletList <$> (flip vectorOf arbitrary =<< choose (0,10))
        go (NewAccount _ _) = ResAccount <$> arbitrary
        go (NewMSAccount _ _ _ _ _) = ResAccount <$> arbitrary
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
            ResTxStatus <$> (fromInteger <$> arbitrary) <*> arbitrary
        go (TxSign _ _) = 
            ResTxStatus <$> (fromInteger <$> arbitrary) <*> arbitrary
        go (Balance _) = ResBalance <$> arbitrary

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

