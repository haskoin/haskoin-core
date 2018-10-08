{-|
Module      : Network.Haskoin.Test.Util
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX
-}
module Network.Haskoin.Test.Util where

import           Data.ByteString       (ByteString, pack)
import           Data.Time.Clock       (UTCTime (..))
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Word             (Word32)
import           Test.QuickCheck

-- | Arbitrary strict 'ByteString'.
arbitraryBS :: Gen ByteString
arbitraryBS = pack <$> arbitrary

-- | Arbitrary non-empty strict ByteString
arbitraryBS1 :: Gen ByteString
arbitraryBS1 = pack <$> listOf1 arbitrary

-- | Arbitrary strict ByteString of a given length
arbitraryBSn :: Int -> Gen ByteString
arbitraryBSn n = pack <$> vectorOf n arbitrary

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

