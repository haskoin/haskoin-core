{-|
  Arbitrary types for Network.Haskoin.Crypto
-}
module Network.Haskoin.Test.Address where

import           Crypto.Secp256k1            ()
import           Data.Bits                   (clearBit)
import           Data.Either                 (fromRight)
import           Data.List                   (foldl')
import           Data.Serialize              (decode)
import           Data.Word                   (Word32)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Test.Crypto
import           Network.Haskoin.Test.Util
import           Test.QuickCheck

-- | Arbitrary non-witness address (can be a pubkey or script hash address)
arbitraryAddress :: Network -> Gen Address
arbitraryAddress net =
    oneof [arbitraryPubKeyAddress net, arbitraryScriptAddress net]

-- | Arbitrary public key hash address
arbitraryPubKeyAddress :: Network -> Gen Address
arbitraryPubKeyAddress net = PubKeyAddress <$> arbitraryHash160 <*> pure net

-- | Arbitrary script hash address
arbitraryScriptAddress :: Network -> Gen Address
arbitraryScriptAddress net = ScriptAddress <$> arbitraryHash160 <*> pure net
