module Bitcoin.Protocol.Alert 
( Alert(..)
) where

import Control.Monad
import Control.Applicative
import Bitcoin.Protocol
import Bitcoin.Protocol.VarString

data Alert = Alert {
    alertPayload   :: VarString,
    alertSignature :: VarString
} deriving (Eq, Show, Read)

instance BitcoinProtocol Alert where

    bitcoinGet = Alert <$> bitcoinGet
                       <*> bitcoinGet

    bitcoinPut (Alert p s) = do
        bitcoinPut p
        bitcoinPut s

