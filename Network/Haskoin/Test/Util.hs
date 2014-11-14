module Network.Haskoin.Test.Util
( ArbitraryByteString(..)
, ArbitraryNotNullByteString(..)
, ArbitraryUTCTime(..)
) where

import Test.QuickCheck 
    ( Arbitrary
    , Gen
    , arbitrary
    , choose
    , listOf1
    )

import Data.Word (Word32)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.ByteString as BS (ByteString, pack, drop)

-- | Arbitrary strict ByteString
data ArbitraryByteString = ArbitraryByteString BS.ByteString
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n <- choose (0, 2)
        -- to give us some with non-0 offset
        return $ ArbitraryByteString $ BS.drop n bs 

-- | Arbitrary strict ByteString that is not empty
data ArbitraryNotNullByteString = ArbitraryNotNullByteString BS.ByteString
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryNotNullByteString where
    arbitrary = do
        bs <- BS.pack `fmap` (listOf1 arbitrary)
        return $ ArbitraryNotNullByteString bs

-- | Arbitrary UTCTime that generates dates after 01 Jan 1970 01:00:00 CET
newtype ArbitraryUTCTime = ArbitraryUTCTime UTCTime
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryUTCTime where
    arbitrary = do
        w <- (arbitrary :: Gen Word32)
        return $ ArbitraryUTCTime $ posixSecondsToUTCTime $ realToFrac w

