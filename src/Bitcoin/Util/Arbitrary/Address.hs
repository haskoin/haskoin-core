{-# LANGUAGE TupleSections #-}

-- |
-- Stability   : experimental
-- Portability : POSIX
module Bitcoin.Util.Arbitrary.Address where

import Bitcoin.Address
import Bitcoin.Constants
import Bitcoin.Data
import Bitcoin.Util.Arbitrary.Crypto
import Bitcoin.Util.Arbitrary.Util
import qualified Data.ByteString as B
import Test.QuickCheck


-- | Arbitrary pay-to-public-key-hash or pay-to-script-hash address.
arbitraryAddress :: Gen Address
arbitraryAddress = oneof [arbitraryPubKeyAddress, arbitraryScriptAddress]


-- | Arbitrary address including pay-to-witness
arbitraryAddressAll :: Gen Address
arbitraryAddressAll =
    oneof
        [ arbitraryPubKeyAddress
        , arbitraryScriptAddress
        , arbitraryWitnessPubKeyAddress
        , arbitraryWitnessScriptAddress
        , arbitraryWitnessAddress
        ]


-- | Arbitrary valid combination of (Network, Address)
arbitraryNetAddress :: Gen (Network, Address)
arbitraryNetAddress = do
    net <- arbitraryNetwork
    (net,) <$> arbitraryAddress


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


arbitraryWitnessAddress :: Gen Address
arbitraryWitnessAddress = do
    ver <- choose (1, 16)
    len <- choose (2, 40)
    ws <- vectorOf len arbitrary
    let bs = B.pack ws
    return $ WitnessAddress ver bs
