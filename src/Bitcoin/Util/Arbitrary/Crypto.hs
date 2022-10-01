-- |
-- Stability   : experimental
-- Portability : POSIX
module Bitcoin.Util.Arbitrary.Crypto where

import Bitcoin.Crypto.Hash
import Bitcoin.Util.Arbitrary.Util
import Test.QuickCheck


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
    checkSum32 <$> arbitraryBSn 4
