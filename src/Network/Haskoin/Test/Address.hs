{-|
Module      : Network.Haskoin.Test.Address
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX
-}
module Network.Haskoin.Test.Address where

import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Test.Crypto
import           Test.QuickCheck

-- | Arbitrary pay-to-public-key-hash or pay-to-script-hash address.
arbitraryAddress :: Network -> Gen Address
arbitraryAddress net =
    oneof [arbitraryPubKeyAddress net, arbitraryScriptAddress net]

-- | Arbitrary pay-to-public-key-hash address.
arbitraryPubKeyAddress :: Network -> Gen Address
arbitraryPubKeyAddress net = p2pkhAddr net <$> arbitraryHash160

-- | Arbitrary pay-to-script-hash address.
arbitraryScriptAddress :: Network -> Gen Address
arbitraryScriptAddress net = p2shAddr net <$> arbitraryHash160
