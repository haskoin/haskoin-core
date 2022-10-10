-- |
-- Stability   : experimental
-- Portability : POSIX
module Bitcoin.Util.Arbitrary.Crypto where

import Bitcoin.Crypto.Hash (
    CheckSum32,
    Hash160,
    Hash256,
    Hash512,
    checkSum32,
    ripemd160,
    sha256,
    sha512,
 )
import Bitcoin.Util.Arbitrary.Util (arbitraryBSn)
import qualified Data.ByteString.Lazy as BSL
import Test.QuickCheck (Gen)


-- | Arbitrary 160-bit hash.
arbitraryHash160 :: Gen Hash160
arbitraryHash160 =
    ripemd160 <$> arbitraryBSn 20


-- | Arbitrary 256-bit hash.
arbitraryHash256 :: Gen Hash256
arbitraryHash256 =
    sha256 <$> arbitraryBSn 32


-- | Arbitrary 512-bit hash.
arbitraryHash512 :: Gen Hash512
arbitraryHash512 =
    sha512 <$> arbitraryBSn 64


-- | Arbitrary 32-bit checksum.
arbitraryCheckSum32 :: Gen CheckSum32
arbitraryCheckSum32 =
    checkSum32 . BSL.fromStrict <$> arbitraryBSn 4
