{-|
Module      : Haskoin.Test.Address
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX
-}
module Haskoin.Test.Address where

import           Haskoin.Address
import           Haskoin.Test.Crypto
import           Test.QuickCheck

-- | Arbitrary pay-to-public-key-hash or pay-to-script-hash address.
arbitraryAddress :: Gen Address
arbitraryAddress = oneof [arbitraryPubKeyAddress, arbitraryScriptAddress]

-- | Arbitrary pay-to-public-key-hash address.
arbitraryPubKeyAddress :: Gen Address
arbitraryPubKeyAddress = p2pkhAddr <$> arbitraryHash160

-- | Arbitrary pay-to-script-hash address.
arbitraryScriptAddress :: Gen Address
arbitraryScriptAddress = p2shAddr <$> arbitraryHash160
