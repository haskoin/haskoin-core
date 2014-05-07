module Network.Haskoin.Protocol.Alert ( Alert(..) ) where

import Control.Applicative ((<$>),(<*>))
import Data.Binary (Binary, get, put)
import Network.Haskoin.Protocol.VarString

-- | Data type describing signed messages that can be sent between bitcoin
-- nodes to display important notifications to end users about the health of
-- the network.
data Alert = 
    Alert {
          -- | Alert payload. 
            alertPayload   :: !VarString
          -- | ECDSA signature of the payload
          , alertSignature :: !VarString
          } deriving (Eq, Show)

instance Binary Alert where
    get = Alert <$> get <*> get
    put (Alert p s) = put p >> put s

