{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
module Network.Haskoin.Types.BTC
( BTC(..)
, btc
, satoshi
, toSatoshi
, scaleBTC
)
where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData, rnf)

import Data.Fixed (Fixed, HasResolution, resolution)
import Data.Typeable (Typeable)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord64le)
import Data.Binary.Put (putWord64le)

data E8 deriving (Typeable)
instance HasResolution E8 where
    resolution _ = 100000000

-- | Data type representing a value denominated in Bitcoins.
data BTC = BTC (Fixed E8) | BTCMinusOne
    deriving (Eq, Ord, Read, Show)

instance NFData BTC where
    rnf (BTC a) = rnf a
    rnf BTCMinusOne = ()

-- | Create a new BTC amount. This function checks that the value is within the
-- bounds of 21 million.
btc :: Fixed E8 -> BTC
btc f | f > 21000000 || f < (-21000000) = 
          error $ unwords [ "Invalid BTC amount", show f ] 
      | otherwise = BTC f

-- | Create a new BTC amount from an Integer amount of satoshis.
satoshi :: Integer -> BTC
satoshi s = btc $ (fromInteger s) / 100000000

-- | Convert a BTC amount to an Integer amount of satoshis.
toSatoshi :: BTC -> Integer
toSatoshi BTCMinusOne = error "toSatoshi is not supported for BTCMinusOne"
toSatoshi (BTC a) = floor $ a * 100000000

-- | Multiply a BTC amount by an Integer value.
scaleBTC :: BTC -> Integer -> BTC
-- We allow going over 21M as long as we don't serialize it
scaleBTC (BTC a) i = BTC $ a * (fromInteger i)
scaleBTC BTCMinusOne _ = error "scaleBTC is not supported for BTCMinusOne"

instance Num BTC where
    fromInteger        = btc . fromInteger
    -- We allow going over 21M as long as we don't serialize it
    (BTC a) + (BTC b)  = BTC $ a + b
    BTCMinusOne + _    = error "(+) is not supported for BTCMinusOne"
    _ + BTCMinusOne    = error "(+) is not supported for BTCMinusOne"
    negate (BTC a)     = BTC $ negate a
    negate BTCMinusOne = error "(-) is not supported for BTCMinusOne"
    abs (BTC a)        = BTC $ abs a
    abs BTCMinusOne    = error "abs is not supported for BTCMinusOne"
    _ * _              = error "BTC multiplication is not supported"
    signum _           = error "BTC signum is not supported"

instance Bounded BTC where
    minBound = fromInteger (-21000000)
    maxBound = fromInteger 21000000

instance Binary BTC where
    get = (satoshi . toInteger) <$> getWord64le
    put BTCMinusOne = putWord64le (-1)-- This is for computing sighashes
    put b | b < 0        = error 
              "Can not serialize a negative BTC value"
          | b > 21000000 = error 
              "Can not serialize an amount greater than 21000000 BTC"
          | otherwise    = putWord64le $ fromInteger $ toSatoshi b

