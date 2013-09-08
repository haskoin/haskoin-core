module Haskoin.Wallet.Tx where

import qualified Data.ByteString as BS

import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

buildMulSig2 :: PubKey -> PubKey -> Script
buildMulSig2 k1 k2 = 
    Script [ OP_2 
           , OP_PUSHDATA $ encode' k1
           , OP_PUSHDATA $ encode' k2
           , OP_2 
           , OP_CHECKMULTISIG
           ]

buildMulSig3 :: PubKey -> PubKey -> PubKey -> Script
buildMulSig3 k1 k2 k3 = 
    Script [ OP_2 
           , OP_PUSHDATA $ encode' k1
           , OP_PUSHDATA $ encode' k2
           , OP_PUSHDATA $ encode' k3
           , OP_3 
           , OP_CHECKMULTISIG
           ]

scriptAddr :: Script -> Address
scriptAddr = ScriptAddress . hash160 . hash256BS . encode'

