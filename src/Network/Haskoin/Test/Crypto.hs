{-|
  Arbitrary types for Network.Haskoin.Crypto
-}
module Network.Haskoin.Test.Crypto where

import           Crypto.Secp256k1                 ()
import           Data.Bits                        (clearBit)
import           Data.Either                      (fromRight)
import           Data.List                        (foldl')
import           Data.Serialize                   (decode)
import           Data.Word                        (Word32)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Test.Util
import           Test.QuickCheck

arbitraryHash160 :: Gen Hash160
arbitraryHash160 =
    (fromRight (error "Could not decode Hash160") . decode) <$> arbitraryBSn 20

arbitraryHash256 :: Gen Hash256
arbitraryHash256 =
    (fromRight (error "Could not decode Hash256") . decode) <$> arbitraryBSn 32

arbitraryHash512 :: Gen Hash512
arbitraryHash512 =
    (fromRight (error "Could not decode Hash512") . decode) <$> arbitraryBSn 64

arbitraryCheckSum32 :: Gen CheckSum32
arbitraryCheckSum32 =
    (fromRight (error "Could not decode CheckSum32") . decode) <$>
    arbitraryBSn 4
