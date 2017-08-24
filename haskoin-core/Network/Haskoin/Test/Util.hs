module Network.Haskoin.Test.Util where

import qualified Data.ByteString       as BS
import           Data.Time.Clock       (UTCTime (..))
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Word             (Word32)
import           Test.QuickCheck

-- | Arbitrary strict ByteString
arbitraryBS :: Gen BS.ByteString
arbitraryBS = BS.pack `fmap` arbitrary

-- | Arbitrary strict ByteString that is not empty
arbitraryBS1 :: Gen BS.ByteString
arbitraryBS1 = BS.pack `fmap` listOf1 arbitrary

-- | Arbitrary strict ByteString of a given length
arbitraryBSn :: Int -> Gen BS.ByteString
arbitraryBSn n = BS.pack `fmap` vectorOf n arbitrary

-- | Arbitrary UTCTime that generates dates after 01 Jan 1970 01:00:00 CET
arbitraryUTCTime :: Gen UTCTime
arbitraryUTCTime = do
    w <- arbitrary :: Gen Word32
    return $ posixSecondsToUTCTime $ realToFrac w

-- | Generate a Maybe from a Gen a
arbitraryMaybe :: Gen a -> Gen (Maybe a)
arbitraryMaybe g = frequency [ (1, return Nothing)
                             , (5, Just <$> g)
                             ]

