{-|
  This package provides QuickCheck Arbitrary instances for all the protocol
  data types defined in 'Network.Haskoin.Protocol'.
-}
module Network.Haskoin.Protocol.Arbitrary () where

import Test.QuickCheck
import Network.Haskoin.Util.Arbitrary (nonEmptyBS)
import Network.Haskoin.Crypto.Arbitrary()

import Control.Monad
import Control.Applicative 

import qualified Data.Sequence as S (fromList)

import Network.Haskoin.Protocol
import Network.Haskoin.Crypto

instance Arbitrary VarInt where
    arbitrary = VarInt <$> arbitrary

instance Arbitrary VarString where
    arbitrary = VarString <$> arbitrary

instance Arbitrary NetworkAddress where
    arbitrary = do
        s <- arbitrary
        a <- liftM2 (,) arbitrary arbitrary
        p <- arbitrary
        return $ NetworkAddress s a p

instance Arbitrary InvType where
    arbitrary = elements [InvError, InvTx, InvBlock, InvMerkleBlock]

instance Arbitrary InvVector where
    arbitrary = InvVector <$> arbitrary <*> (hash256 <$> arbitrary)

instance Arbitrary Inv where
    arbitrary = Inv <$> listOf arbitrary

instance Arbitrary Version where
    arbitrary = Version <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance Arbitrary Addr where
    arbitrary = Addr <$> listOf arbitrary

instance Arbitrary Alert where
    arbitrary = Alert <$> arbitrary <*> arbitrary

instance Arbitrary BlockHeader where
    arbitrary = BlockHeader <$> arbitrary
                            <*> (hash256 <$> arbitrary)
                            <*> (hash256 <$> arbitrary)
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            
instance Arbitrary Script where
    arbitrary = do
        i <- choose (1,10)
        Script <$> (vectorOf i arbitrary)

instance Arbitrary Tx where
    arbitrary = do
        v   <- arbitrary
        tin <- do 
            l <- choose (0,10)
            vectorOf l arbitrary
        tout <- do
            l <- choose (0,10)
            vectorOf l arbitrary
        t    <- arbitrary
        return $ Tx v tin tout t

instance Arbitrary CoinbaseTx where
    arbitrary = CoinbaseTx <$> arbitrary
                           <*> (return $ OutPoint 0 0xffffffff)
                           <*> arbitrary
                           <*> (return $ 0xffffffff)
                           <*> (listOf arbitrary)
                           <*> arbitrary

instance Arbitrary TxIn where
    arbitrary = TxIn <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary TxOut where
    arbitrary = TxOut <$> (choose (0,2100000000000000))
                      <*> arbitrary

instance Arbitrary OutPoint where
    arbitrary = OutPoint <$> arbitrary
                         <*> arbitrary

instance Arbitrary Block where
    arbitrary = do
        h <- arbitrary
        c <- arbitrary
        t <- do 
            l <- choose (0,10)
            vectorOf l arbitrary
        return $ Block h c t

instance Arbitrary MerkleBlock where
    arbitrary = do
        h <- arbitrary
        ntx <- arbitrary
        hashes <- arbitrary
        c <- choose (1,10)
        flags <- vectorOf (c*8) arbitrary
        return $ MerkleBlock h ntx hashes flags

instance Arbitrary PushDataType where
    arbitrary = elements [ OPCODE, OPDATA1, OPDATA2, OPDATA4 ]

instance Arbitrary ScriptOp where
    arbitrary = oneof [ opPushData <$> nonEmptyBS
                      , return OP_0
                      , return OP_1NEGATE
                      , return OP_1
                      , return OP_2, return OP_3, return OP_4, return OP_5
                      , return OP_6, return OP_7, return OP_8, return OP_9
                      , return OP_10, return OP_11, return OP_12, return OP_13
                      , return OP_14, return OP_15, return OP_16
                      , return OP_VERIFY
                      , return OP_DUP
                      , return OP_EQUAL
                      , return OP_EQUALVERIFY
                      , return OP_HASH160
                      , return OP_CHECKSIG
                      , return OP_CHECKMULTISIG
                      , OP_PUBKEY <$> arbitrary
                      , return $ OP_INVALIDOPCODE 0xff
                      ]

instance Arbitrary GetBlocks where
    arbitrary = GetBlocks <$> arbitrary
                          <*> (listOf arbitrary)
                          <*> arbitrary

instance Arbitrary GetData where
    arbitrary = GetData <$> (listOf arbitrary)

instance Arbitrary GetHeaders where
    arbitrary = GetHeaders <$> arbitrary
                           <*> (listOf arbitrary)
                           <*> arbitrary

instance Arbitrary Headers where
    arbitrary = Headers <$> (listOf (liftM2 (,) arbitrary arbitrary))

instance Arbitrary NotFound where
    arbitrary = NotFound <$> (listOf arbitrary)

instance Arbitrary Ping where
    arbitrary = Ping <$> arbitrary

instance Arbitrary Pong where
    arbitrary = Pong <$> arbitrary

instance Arbitrary MessageCommand where
    arbitrary = elements [ MCVersion
                         , MCVerAck
                         , MCAddr
                         , MCInv
                         , MCGetData
                         , MCNotFound
                         , MCGetBlocks
                         , MCGetHeaders
                         , MCTx
                         , MCBlock
                         , MCHeaders
                         , MCGetAddr
                         , MCFilterLoad
                         , MCFilterAdd
                         , MCFilterClear
                         , MCPing
                         , MCPong
                         , MCAlert
                         ]

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
                      , MHeaders     <$> arbitrary
                      , return MGetAddr
                      , MFilterLoad  <$> arbitrary
                      , MFilterAdd   <$> arbitrary
                      , return MFilterClear
                      , MPing        <$> arbitrary
                      , MPong        <$> arbitrary
                      , MAlert       <$> arbitrary
                      ]

instance Arbitrary BloomFlags where
    arbitrary = elements [ BloomUpdateNone
                         , BloomUpdateAll
                         , BloomUpdateP2PubKeyOnly
                         , BloomUpdateMask
                         ]

instance Arbitrary BloomFilter where
    arbitrary = BloomFilter <$> (S.fromList <$> arbitrary)
                            <*> (return False)
                            <*> (return False)
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                        
instance Arbitrary FilterLoad where
    arbitrary = FilterLoad <$> arbitrary

instance Arbitrary FilterAdd where
    arbitrary = FilterAdd <$> arbitrary

