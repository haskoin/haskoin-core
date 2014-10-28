{-| 
  Arbitrary types for Network.Haskoin.Protocol
-}
module Network.Haskoin.Test.Protocol
( ArbitraryVarInt(..)
, ArbitraryVarString(..)
, ArbitraryNetworkAddress(..)
, ArbitraryNetworkAddressTime(..)
, ArbitraryInvType(..)
, ArbitraryInvVector(..)
, ArbitraryInv(..)
, ArbitraryVersion(..)
, ArbitraryAddr(..)
, ArbitraryAlert(..)
, ArbitraryReject(..)
, ArbitraryRejectCode(..)
, ArbitraryGetData(..)
, ArbitraryNotFound(..)
, ArbitraryPing(..)
, ArbitraryPong(..)
, ArbitraryMessageCommand(..)
) where

import Test.QuickCheck 
    ( Arbitrary
    , arbitrary
    , elements
    , listOf1
    , oneof
    )

import Control.Applicative ((<$>))

import Data.Word (Word32)

import Network.Socket (PortNumber(..), SockAddr(..))

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Protocol

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

-- | Arbitrary NetworkAddressTime
newtype ArbitraryNetworkAddressTime 
    = ArbitraryNetworkAddressTime (Word32, NetworkAddress)

instance Arbitrary ArbitraryNetworkAddressTime where
    arbitrary = do
        w <- arbitrary
        ArbitraryNetworkAddress a <- arbitrary
        return $ ArbitraryNetworkAddressTime (w,a)

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
        h <- arbitrary
        return $ ArbitraryInvVector $ InvVector t h

-- | Arbitrary non-empty Inv
newtype ArbitraryInv = ArbitraryInv Inv
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryInv where
    arbitrary = do
        vs <- listOf1 arbitrary    
        return $ ArbitraryInv $ Inv $ map (\(ArbitraryInvVector v) -> v) vs
    
-- | Arbitrary Version
newtype ArbitraryVersion = ArbitraryVersion Version
    deriving (Eq, Show)

instance Arbitrary ArbitraryVersion where
    arbitrary = do
        v <- arbitrary
        s <- arbitrary
        t <- arbitrary
        ArbitraryNetworkAddress nr <- arbitrary
        ArbitraryNetworkAddress ns <- arbitrary
        n <- arbitrary
        ArbitraryVarString a <- arbitrary
        h <- arbitrary
        r <- arbitrary
        return $ ArbitraryVersion $ Version v s t nr ns n a h r

-- | Arbitrary non-empty Addr
newtype ArbitraryAddr = ArbitraryAddr Addr
    deriving (Eq, Show)

instance Arbitrary ArbitraryAddr where
    arbitrary = do
       vs <- listOf1 arbitrary
       return $ ArbitraryAddr $ Addr $ 
           map (\(ArbitraryNetworkAddressTime x) -> x) vs

-- | Arbitrary alert with random payload and signature. Signature is not
-- valid.
newtype ArbitraryAlert = ArbitraryAlert Alert
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAlert where
    arbitrary = do
        ArbitraryVarString p <- arbitrary
        ArbitraryVarString s <- arbitrary
        return $ ArbitraryAlert $ Alert p s

-- | Arbitrary Reject
newtype ArbitraryReject = ArbitraryReject Reject
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryReject where
    arbitrary = do
        ArbitraryMessageCommand m <- arbitrary
        ArbitraryRejectCode c <- arbitrary
        ArbitraryVarString s <- arbitrary
        return $ ArbitraryReject $ Reject m c s

-- | Arbitrary RejectCode
newtype ArbitraryRejectCode = ArbitraryRejectCode RejectCode
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryRejectCode where
    arbitrary = ArbitraryRejectCode <$> elements 
        [ RejectMalformed
        , RejectInvalid
        , RejectInvalid
        , RejectDuplicate
        , RejectNonStandard
        , RejectDust
        , RejectInsufficientFee
        , RejectCheckpoint
        ]

-- | Arbitrary non-empty GetData
newtype ArbitraryGetData = ArbitraryGetData GetData
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryGetData where
    arbitrary = do
        vs <- listOf1 arbitrary
        return $ ArbitraryGetData $ GetData $ 
            map (\(ArbitraryInvVector x) -> x) vs

-- | Arbitrary NotFound
newtype ArbitraryNotFound = ArbitraryNotFound NotFound
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryNotFound where
    arbitrary = do
        vs <- listOf1 arbitrary
        return $ ArbitraryNotFound $ NotFound $ 
            map (\(ArbitraryInvVector x) -> x) vs

-- | Arbitrary Ping
newtype ArbitraryPing = ArbitraryPing Ping
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPing where
    arbitrary = ArbitraryPing . Ping <$> arbitrary

-- | Arbitrary Pong
newtype ArbitraryPong = ArbitraryPong Pong
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPong where
    arbitrary = ArbitraryPong . Pong <$> arbitrary

-- | Arbitrary MessageCommand
newtype ArbitraryMessageCommand = ArbitraryMessageCommand MessageCommand
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryMessageCommand where
    arbitrary = ArbitraryMessageCommand <$> elements 
        [ MCVersion
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

