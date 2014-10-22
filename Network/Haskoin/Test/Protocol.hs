{-| 
  Arbitrary types for Network.Haskoin.Protocol
-}
module Network.Haskoin.Test.Protocol
(
) where

import Test.QuickCheck

import Control.Monad
import Control.Applicative 

import qualified Data.Sequence as S (fromList)

import Network.Socket

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Crypto

import Network.Haskoin.Protocol.Types

-- | Arbitrary VarInt
newtype ArbitraryVarInt = ArbitraryVarInt VarInt
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryVarInt where
    arbitrary = ArbitraryVarInt . VarInt <$> arbitrary

-- | Arbitrary VarString
newtype ArbitraryVarString = ArbitraryVarString VarString
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryVarString where
    arbitrary = do
        ArbitraryByteString bs <- arbitrary
        return $ ArbitraryVarString $ VarString bs

-- | Arbitrary NetworkAddress
newtype ArbitraryNetworkAddress = ArbitraryNetworkAddress NetworkAddress
    deriving (Eq, Show)

instance Arbitrary ArbitraryNetworkAddress where
    arbitrary = do
        s <- arbitrary
        a <- arbitrary
        p <- arbitrary
        ArbitraryNetworkAddress . (NetworkAddress s) <$> oneof 
            [ do
                b <- arbitrary
                c <- arbitrary
                d <- arbitrary
                return $ SockAddrInet6 (PortNum p) 0 (a,b,c,d) 0
            , return $ SockAddrInet (PortNum p) a
            ]

-- | Arbitrary InvType
newtype ArbitraryInvType = ArbitraryInvType InvType
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryInvType where
    arbitrary = ArbitraryInvType <$> elements 
        [InvError, InvTx, InvBlock, InvMerkleBlock]

-- | Arbitrary InvVector
newtype ArbitraryInvVector = ArbitraryInvVector InvVector
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryInvVector where
    arbitrary = do
        ArbitraryInvType t <- arbitrary
        ArbitraryBigWord h <- arbitrary
        return $ ArbitraryInvVector $ InvVector t h
    
