{-|
  QuickCheck Arbitrary instances for various utility data types
-}
module Network.Haskoin.Util.Arbitrary (nonEmptyBS) where

import Test.QuickCheck 
    ( Arbitrary
    , Gen
    , arbitrary
    , choose
    , oneof
    )
import qualified Data.ByteString as BS 
    ( ByteString
    , pack
    , drop
    , null
    )

import Control.Applicative

import Network.Haskoin.Util.BuildMonad (Build(..))

-- Arbitrary instance for strict ByteStrings
instance Arbitrary BS.ByteString where
    arbitrary = do
        bs <- BS.pack `fmap` arbitrary
        n <- choose (0, 2)
        return $ BS.drop n bs -- to give us some with non-0 offset

-- | Generate non-empty strict ByteStrings
nonEmptyBS :: Gen BS.ByteString
nonEmptyBS = do
    bs <- arbitrary
    return $ if BS.null bs then BS.pack [0] else bs

-- Arbitrary instance for the Build monad
instance Arbitrary a => Arbitrary (Build a) where
    arbitrary = oneof [ Complete <$> arbitrary
                      , Partial <$> arbitrary
                      , return $ Broken "Arbitrary: Broken"
                      ]

