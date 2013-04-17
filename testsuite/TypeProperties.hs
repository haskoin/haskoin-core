import Test.QuickCheck
import QuickCheckUtils
import Data.Binary.Get
import Data.Binary.Put

import Bitcoin.Type.Version
import Bitcoin.Type.NetworkAddress
import qualified Bitcoin.Type as Bitcoin

prop_identity :: (Bitcoin.Type a, Eq a) => a -> Bool
prop_identity t = runGet Bitcoin.get (runPut . Bitcoin.put $ t) == t

