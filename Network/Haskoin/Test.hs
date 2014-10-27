{-|
  This package provides test types for Network.Haskoin
-}
module Network.Haskoin.Test
( 
  -- * Crypto Arbitrary instances
  ArbitraryByteString(..)
, ArbitraryNotNullByteString(..)
, ArbitraryPoint(..)
, ArbitraryInfPoint(..)
, ArbitraryPrvKey(..)
, ArbitraryPrvKeyC(..)
, ArbitraryPrvKeyU(..)
, ArbitraryPubKey(..)
, ArbitraryPubKeyC(..)
, ArbitraryPubKeyU(..)
, ArbitraryAddress(..)
, ArbitraryPubKeyAddress(..)
, ArbitraryScriptAddress(..)
, ArbitrarySignature(..)
, ArbitraryDetSignature(..)
, ArbitraryXPrvKey(..)
, ArbitraryXPubKey(..)
, ArbitraryMasterKey(..)
, ArbitraryAccPrvKey(..)
, ArbitraryAccPubKey(..)
, ArbitraryAddrPrvKey(..)
, ArbitraryAddrPubKey(..)
, ArbitraryBloomFlags(..)
, ArbitraryBloomFilter(..)
, ArbitraryFilterLoad(..)
, ArbitraryFilterAdd(..)

  -- * Protocol Arbitrary instances
, ArbitraryVarInt(..)
, ArbitraryVarString(..)
, ArbitraryNetworkAddress(..)
, ArbitraryNetworkAddressTime(..)
, ArbitraryInvType(..)
, ArbitraryInvVector(..)
, ArbitraryInv(..)
, ArbitraryVersion(..)
, ArbitraryAddr(..)
, ArbitraryAlert(..)
, ArbitraryReject(..)
, ArbitraryRejectCode(..)
, ArbitraryGetData(..)
, ArbitraryNotFound(..)
, ArbitraryPing(..)
, ArbitraryPong(..)
, ArbitraryMessageCommand(..)

  -- * Message Arbitrary instances
, ArbitraryMessageHeader(..)
, ArbitraryMessage(..)

  -- * Script Arbitrary instances
, ArbitraryScriptOp(..)
, ArbitraryScript(..)
, ArbitraryIntScriptOp(..)
, ArbitraryPushDataType(..)
, ArbitraryTxSignature(..)
, ArbitraryDetTxSignature(..)
, ArbitrarySigHash(..)
, ArbitraryValidSigHash(..)
, ArbitraryMSParam(..)
, ArbitraryScriptOutput(..)
, ArbitrarySimpleOutput(..)
, ArbitraryPKOutput(..)
, ArbitraryPKHashOutput(..)
, ArbitraryMSOutput(..)
, ArbitraryMSCOutput(..)
, ArbitrarySHOutput(..)
, ArbitraryScriptInput(..)
, ArbitrarySimpleInput(..)
, ArbitraryPKInput(..)
, ArbitraryPKHashInput(..)
, ArbitraryPKHashCInput(..)
, ArbitraryMSInput(..)
, ArbitrarySHInput(..)
, ArbitraryMulSigSHCInput(..)

  -- * Transaction Arbitrary instances
, ArbitrarySatoshi(..)
, ArbitraryTx(..)
, ArbitraryTxIn(..)
, ArbitraryTxOut(..)
, ArbitraryOutPoint(..) 
, ArbitraryCoinbaseTx(..)
, ArbitraryAddrOnlyTx(..)
, ArbitraryAddrOnlyTxIn(..)
, ArbitraryAddrOnlyTxOut(..)
, ArbitraryCoin(..)
, ArbitrarySigInput(..)
, ArbitraryPKSigInput(..)
, ArbitraryPKHashSigInput(..)
, ArbitraryMSSigInput(..)
, ArbitrarySHSigInput(..)
, ArbitrarySigningData(..)

  -- * Block Arbitrary instances
, ArbitraryBlock(..)
, ArbitraryBlockHeader(..)
, ArbitraryGetBlocks(..)
, ArbitraryGetHeaders(..)
, ArbitraryHeaders(..)
, ArbitraryMerkleBlock(..)

  -- * Stratum Arbitrary instances
, ReqRes(..)
) where

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Test.Protocol
import Network.Haskoin.Test.Message
import Network.Haskoin.Test.Script
import Network.Haskoin.Test.Transaction
import Network.Haskoin.Test.Block
import Network.Haskoin.Test.Stratum

