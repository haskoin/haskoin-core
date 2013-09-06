module Haskoin.Wallet.Manager where

import Haskoin.Wallet.Keys

data Wallet =
    Wallet XPrvKey |
    WalletRead XPubKey |
    WalletMS XPrvKey XPubKey |
    WalletMSRead XPubKey XPubKey
    deriving (Eq, Show)

