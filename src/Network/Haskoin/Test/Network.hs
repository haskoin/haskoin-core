{-|
Module      : Network.Haskoin.Test.Network
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX
-}
module Network.Haskoin.Test.Network where

import qualified Data.ByteString             as BS (empty, pack)
import           Data.Word                   (Word16, Word32)
import           Network.Haskoin.Network
import           Network.Haskoin.Test.Crypto
import           Network.Haskoin.Test.Util
import           Network.Socket              (SockAddr (..))
import           Test.QuickCheck

-- | Arbitrary 'VarInt'.
arbitraryVarInt :: Gen VarInt
arbitraryVarInt = VarInt <$> arbitrary

-- | Arbitrary 'VarString'.
arbitraryVarString :: Gen VarString
arbitraryVarString = VarString <$> arbitraryBS

-- | Arbitrary 'NetworkAddress'.
arbitraryNetworkAddress :: Gen NetworkAddress
arbitraryNetworkAddress = do
    s <- arbitrary
    a <- arbitrary
    p <- arbitrary
    NetworkAddress s <$> oneof
        [ do
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return $ SockAddrInet6 (fromIntegral p) 0 (a,b,c,d) 0
        , return $ SockAddrInet (fromIntegral (p :: Word16)) a
        ]

-- | Arbitrary 'NetworkAddressTime'.
arbitraryNetworkAddressTime :: Gen (Word32, NetworkAddress)
arbitraryNetworkAddressTime = (,) <$> arbitrary <*> arbitraryNetworkAddress

-- | Arbitrary 'InvType'.
arbitraryInvType :: Gen InvType
arbitraryInvType = elements [InvError, InvTx, InvBlock, InvMerkleBlock]

-- | Arbitrary 'InvVector'.
arbitraryInvVector :: Gen InvVector
arbitraryInvVector = InvVector <$> arbitraryInvType <*> arbitraryHash256

-- | Arbitrary non-empty 'Inv'.
arbitraryInv1 :: Gen Inv
arbitraryInv1 = Inv <$> listOf1 arbitraryInvVector

-- | Arbitrary 'Version'.
arbitraryVersion :: Gen Version
arbitraryVersion =
    Version <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryNetworkAddress
            <*> arbitraryNetworkAddress
            <*> arbitrary
            <*> arbitraryVarString
            <*> arbitrary
            <*> arbitrary

-- | Arbitrary non-empty 'Addr'.
arbitraryAddr1 :: Gen Addr
arbitraryAddr1 = Addr <$> listOf1 arbitraryNetworkAddressTime

-- | Arbitrary 'Alert' with random payload and signature. Signature is not
-- valid.
arbitraryAlert :: Gen Alert
arbitraryAlert = Alert <$> arbitraryVarString <*> arbitraryVarString

-- | Arbitrary 'Reject'.
arbitraryReject :: Gen Reject
arbitraryReject = do
    m <- arbitraryMessageCommand
    c <- arbitraryRejectCode
    s <- arbitraryVarString
    d <- oneof [ return BS.empty
               , BS.pack <$> vectorOf 32 arbitrary
               ]
    return $ Reject m c s d

-- | Arbitrary 'RejectCode'.
arbitraryRejectCode :: Gen RejectCode
arbitraryRejectCode =
    elements
        [ RejectMalformed
        , RejectInvalid
        , RejectInvalid
        , RejectDuplicate
        , RejectNonStandard
        , RejectDust
        , RejectInsufficientFee
        , RejectCheckpoint
        ]

-- | Arbitrary non-empty 'GetData'.
arbitraryGetData :: Gen GetData
arbitraryGetData = GetData <$> listOf1 arbitraryInvVector

-- | Arbitrary 'NotFound'.
arbitraryNotFound :: Gen NotFound
arbitraryNotFound = NotFound <$> listOf1 arbitraryInvVector

-- | Arbitrary 'Ping'.
arbitraryPing :: Gen Ping
arbitraryPing = Ping <$> arbitrary

-- | Arbitrary 'Pong'.
arbitraryPong :: Gen Pong
arbitraryPong = Pong <$> arbitrary

-- | Arbitrary bloom filter flags.
arbitraryBloomFlags :: Gen BloomFlags
arbitraryBloomFlags =
    elements
        [ BloomUpdateNone
        , BloomUpdateAll
        , BloomUpdateP2PubKeyOnly
        ]

-- | Arbitrary bloom filter with its corresponding number of elements
-- and false positive rate.
arbitraryBloomFilter :: Gen (Int, Double, BloomFilter)
arbitraryBloomFilter = do
    n     <- choose (0,100000)
    fp    <- choose (1e-8,1)
    tweak <- arbitrary
    fl    <- arbitraryBloomFlags
    return (n, fp, bloomCreate n fp tweak fl)

-- | Arbitrary 'FilterLoad'.
arbitraryFilterLoad :: Gen FilterLoad
arbitraryFilterLoad = do
    (_, _, bf) <- arbitraryBloomFilter
    return $ FilterLoad bf

-- | Arbitrary 'FilterAdd'.
arbitraryFilterAdd :: Gen FilterAdd
arbitraryFilterAdd = FilterAdd <$> arbitraryBS

-- | Arbitrary 'MessageCommand'.
arbitraryMessageCommand :: Gen MessageCommand
arbitraryMessageCommand =
    elements
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
