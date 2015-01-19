{-| 
  Arbitrary types for Network.Haskoin.Node.Message
-}
module Network.Haskoin.Test.Message
( ArbitraryMessageHeader(..)
, ArbitraryMessage(..)
) where

import Test.QuickCheck
    ( Arbitrary
    , Gen
    , arbitrary
    , oneof
    )

import Control.Applicative ((<$>))

import Network.Haskoin.Network
import Network.Haskoin.Test.Node
import Network.Haskoin.Test.Transaction
import Network.Haskoin.Test.Block

import Network.Haskoin.Node.Message

-- | Arbitrary MessageHeader
newtype ArbitraryMessageHeader = ArbitraryMessageHeader MessageHeader
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryMessageHeader where
    arbitrary = ArbitraryMessageHeader <$> do
        m <- arbitrary
        ArbitraryMessageCommand mc <- arbitrary
        p <- arbitrary
        c <- arbitrary
        return $ MessageHeader m mc p c

-- | Arbitrary Message
newtype ArbitraryMessage a = ArbitraryMessage (Message a)
    deriving (Eq, Show)

instance forall a. Network a => Arbitrary (ArbitraryMessage a) where
    arbitrary = ArbitraryMessage <$> oneof
        [ arbitrary >>= \(ArbitraryVersion x) -> return $ MVersion x
        , return MVerAck
        , arbitrary >>= \(ArbitraryAddr x) -> return $ MAddr x
        , arbitrary >>= \(ArbitraryInv x) -> return $ MInv x
        , arbitrary >>= \(ArbitraryGetData x) -> return $ MGetData x
        , arbitrary >>= \(ArbitraryNotFound x) -> return $ MNotFound x
        , arbitrary >>= \(ArbitraryGetBlocks x) -> return $ MGetBlocks x
        , arbitrary >>= \(ArbitraryGetHeaders x) -> return $ MGetHeaders x
        , (arbitrary :: Gen (ArbitraryTx a)) >>=
            \(ArbitraryTx x) -> return $ MTx x
        , (arbitrary :: Gen (ArbitraryBlock a)) >>=
            \(ArbitraryBlock x) -> return $ MBlock x
        , arbitrary >>= \(ArbitraryMerkleBlock x) -> return $ MMerkleBlock x
        , arbitrary >>= \(ArbitraryHeaders x) -> return $ MHeaders x
        , return MGetAddr
        , arbitrary >>= \(ArbitraryFilterLoad x) -> return $ MFilterLoad x
        , arbitrary >>= \(ArbitraryFilterAdd x) -> return $ MFilterAdd x
        , return MFilterClear
        , arbitrary >>= \(ArbitraryPing x) -> return $ MPing x
        , arbitrary >>= \(ArbitraryPong x) -> return $ MPong x
        , arbitrary >>= \(ArbitraryAlert x) -> return $ MAlert x
        , arbitrary >>= \(ArbitraryReject x) -> return $ MReject x
        ]

