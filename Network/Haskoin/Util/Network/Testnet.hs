{-|
  Declaration of constant values for Testnet. This module is intended to be
  imported by Network.Haskoin.Util.Network module and not imported directly by
  other modules.
-}
module Network.Haskoin.Util.Network.Testnet where

import Data.Word (Word8,Word32)

-- | Prefix for base58 PubKey hash address
addrPrefix :: Word8
addrPrefix = 111

-- | Prefix for base58 script hash address
scriptPrefix :: Word8
scriptPrefix = 196

-- | Prefix for private key WIF format
secretPrefix :: Word8
secretPrefix = 239

-- | Prefix for extended public keys (BIP32)
extPubKeyPrefix :: Word32
extPubKeyPrefix = 0x043587cf

-- | Prefix for extended private keys (BIP32)
extSecretPrefix :: Word32
extSecretPrefix = 0x04358394

-- | Wallet database file name
walletFile :: String
walletFile = "testwalletdb"

-- | Genesis block header information
genesisHeader :: [Integer]
genesisHeader =
    -- Hash = 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
    [ 0x01
    , 0x00
    , 0x3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a
    , 1296688602
    , 486604799
    , 414098458
    ]

networkMagic :: Word32
networkMagic = 0x0b110907 

