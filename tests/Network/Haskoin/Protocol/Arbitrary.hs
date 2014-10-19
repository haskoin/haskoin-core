{-|
  This package provides QuickCheck Arbitrary instances for all the protocol
  data types defined in 'Network.Haskoin.Protocol'.
-}
module Network.Haskoin.Protocol.Arbitrary () where

import Test.QuickCheck

import Control.Monad
import Control.Applicative 

import qualified Data.Sequence as S (fromList)

import Network.Socket

import Network.Haskoin.Crypto.Arbitrary()
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto

instance Arbitrary VarInt where
    arbitrary = VarInt <$> arbitrary

instance Arbitrary VarString where
    arbitrary = VarString <$> arbitrary

instance Arbitrary NetworkAddress where
    arbitrary = do
        s <- arbitrary
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        p <- arbitrary
        NetworkAddress s <$> elements
            [ SockAddrInet6 (PortNum p) 0x00000000 (a,b,c,d) 0x00000000
            , SockAddrInet (PortNum p) a
            ]

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

instance Arbitrary Reject where
    arbitrary = Reject <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RejectCode where
    arbitrary = elements [ RejectMalformed
                         , RejectInvalid
                         , RejectInvalid
                         , RejectDuplicate
                         , RejectNonStandard
                         , RejectDust
                         , RejectInsufficientFee
                         , RejectCheckpoint
                         ]

instance Arbitrary GetData where
    arbitrary = GetData <$> (listOf arbitrary)

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
                         , MCMerkleBlock
                         , MCHeaders
                         , MCGetAddr
                         , MCFilterLoad
                         , MCFilterAdd
                         , MCFilterClear
                         , MCPing
                         , MCPong
                         , MCAlert
                         ]

instance Arbitrary BloomFlags where
    arbitrary = elements [ BloomUpdateNone
                         , BloomUpdateAll
                         , BloomUpdateP2PubKeyOnly
                         ]

instance Arbitrary BloomFilter where
    arbitrary = BloomFilter <$> (S.fromList <$> arbitrary)
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                        
instance Arbitrary FilterLoad where
    arbitrary = FilterLoad <$> arbitrary

instance Arbitrary FilterAdd where
    arbitrary = FilterAdd <$> arbitrary

