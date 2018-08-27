import           Network.Haskoin.Constants
import           Test.Hspec

-- Util tests
import qualified Network.Haskoin.Util.Tests                (spec)

-- Crypto tests
import qualified Network.Haskoin.Crypto.Base58.Tests       (spec)
import qualified Network.Haskoin.Crypto.Base58.Units       (spec)
import qualified Network.Haskoin.Crypto.ECDSA.Tests        (spec)
import qualified Network.Haskoin.Crypto.ExtendedKeys.Tests (spec)
import qualified Network.Haskoin.Crypto.ExtendedKeys.Units (spec)
import qualified Network.Haskoin.Crypto.Hash.Tests         (spec)
import qualified Network.Haskoin.Crypto.Hash.Units         (spec)
import qualified Network.Haskoin.Crypto.Keys.Tests         (spec)
import qualified Network.Haskoin.Crypto.Mnemonic.Tests     (spec)
import qualified Network.Haskoin.Crypto.Mnemonic.Units     (spec)
import qualified Network.Haskoin.Crypto.Units              (spec)

-- Network tests
import qualified Network.Haskoin.Network.Units             (spec)

-- Script tests
import qualified Network.Haskoin.Script.Spec               (spec)
import qualified Network.Haskoin.Script.Tests              (spec)
import qualified Network.Haskoin.Script.Units              (spec)

-- Transaction tests
import qualified Network.Haskoin.Transaction.Tests         (spec)
import qualified Network.Haskoin.Transaction.Units         (spec)

-- Block tests
import qualified Network.Haskoin.Block.Spec                (spec)
import qualified Network.Haskoin.Block.Tests               (spec)
import qualified Network.Haskoin.Block.Units               (spec)

-- Json tests
import qualified Network.Haskoin.Json.Tests                (spec)

-- Binary tests
import qualified Network.Haskoin.Cereal.Tests              (spec)

main :: IO ()
main = do
  hspec $ do
      Network.Haskoin.Script.Spec.spec btc
      Network.Haskoin.Script.Spec.spec bch
      Network.Haskoin.Block.Spec.spec bchRegTest
      Network.Haskoin.Block.Spec.spec btcRegTest
      Network.Haskoin.Block.Tests.spec btc
      Network.Haskoin.Cereal.Tests.spec btc
      Network.Haskoin.Crypto.Base58.Tests.spec btc
      Network.Haskoin.Crypto.ExtendedKeys.Tests.spec btc
      Network.Haskoin.Crypto.ExtendedKeys.Units.spec
      Network.Haskoin.Crypto.Keys.Tests.spec btc
      Network.Haskoin.Crypto.Units.spec
      Network.Haskoin.Json.Tests.spec btc
      Network.Haskoin.Network.Units.spec
      Network.Haskoin.Script.Tests.spec
      Network.Haskoin.Script.Units.spec
      Network.Haskoin.Block.Units.spec
      Network.Haskoin.Crypto.Base58.Units.spec
      Network.Haskoin.Crypto.ECDSA.Tests.spec btc
      Network.Haskoin.Crypto.Hash.Tests.spec
      Network.Haskoin.Crypto.Hash.Units.spec
      Network.Haskoin.Crypto.Mnemonic.Tests.spec
      Network.Haskoin.Crypto.Mnemonic.Units.spec
      Network.Haskoin.Transaction.Tests.spec btc
      Network.Haskoin.Transaction.Units.spec
      Network.Haskoin.Util.Tests.spec

