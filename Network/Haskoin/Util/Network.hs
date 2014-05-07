{-|
  Declaration of constant values that depend on the network type
  (for example: prodnet or testnet). The values exported from this modules
  are imported from a network-specific sub-module.
-}
module Network.Haskoin.Util.Network 
( -- $doc
  addrPrefix
, scriptPrefix
, secretPrefix
, extPubKeyPrefix
, extSecretPrefix
, walletFile
, genesisHeader
, networkMagic
) where

import Network.Haskoin.Util.Network.Prodnet
--import Network.Haskoin.Util.Network.Testnet

-- $doc
-- The network-specific values are defined in one of the sub-modules:
--
--   * @Network.Haskoin.Util.Network.Prodnet@  
--
--   * @Network.Haskoin.Util.Network.Testnet@
--
-- You must import the module for the network you are interested in and compile
-- the libraries against it.

