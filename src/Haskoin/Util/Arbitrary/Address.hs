{-|
Module      : Haskoin.Test.Address
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX
-}
module Haskoin.Util.Arbitrary.Address where

import           Haskoin.Address
import           Haskoin.Util.Arbitrary.Crypto
import           Test.QuickCheck

-- | Arbitrary pay-to-public-key-hash or pay-to-script-hash address.
arbitraryAddress :: Gen Address
arbitraryAddress = oneof [arbitraryPubKeyAddress, arbitraryScriptAddress]

-- | Arbitrary pay-to-public-key-hash address.
arbitraryPubKeyAddress :: Gen Address
arbitraryPubKeyAddress = PubKeyAddress <$> arbitraryHash160

-- | Arbitrary pay-to-script-hash address.
arbitraryScriptAddress :: Gen Address
arbitraryScriptAddress = ScriptAddress <$> arbitraryHash160

-- | Arbitrary pay-to-witness public key hash
arbitraryWitnessPubKeyAddress :: Gen Address
arbitraryWitnessPubKeyAddress = WitnessPubKeyAddress <$> arbitraryHash160 

-- | Arbitrary pay-to-witness script hash
arbitraryWitnessScriptAddress :: Gen Address
arbitraryWitnessScriptAddress = WitnessPubKeyAddress <$> arbitraryHash160 
