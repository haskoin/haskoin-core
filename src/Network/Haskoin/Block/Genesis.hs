module Network.Haskoin.Block.Genesis where

import Network.Haskoin.Block.Types
import Network.Haskoin.Transaction.Genesis
import Network.Haskoin.Constants

genesisBlock :: Block
genesisBlock = Block genesisHeader [genesisTx]
