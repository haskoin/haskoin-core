import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.Binary 
import qualified Data.ByteString as BS

import Network.Haskoin.Crypto.ECDSA
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.Curve
import Network.Haskoin.Util 

vectors = 
    [
      ( 0x01
      , "Everything should be made as simple as possible, but not simpler."
      )
    , ( (curveN-1)
      , "Equations are more important to me, because politics is for the present, but an equation is something for eternity."
      )
    , ( (curveN-1)
      , "Not only is the Universe stranger than we think, it is stranger than we can think."
      )
    , ( 0x01
      , "How wonderful that we have met with a paradox. Now we have some hope of making progress."
      )
    , ( 0x69ec59eaa1f4f2e36b639716b7c30ca86d9a5375c7b38d8918bd9c0ebc80ba64
      , "Computer science is no more about computers than astronomy is about telescopes."
      )
    , ( 0x7246174ab1e92e9149c6e446fe194d072637
      , "...if you aren't, at any given time, scandalized by code you wrote five or even three years ago, you're not learning anywhere near enough"
      )
    , ( 0x056916d0f9b31dc9b637f3
      , "The question of whether computers can think is like the question of whether submarines can swim."
      )
    ]

genVector :: (Integer, String) -> (String, String, String, String, String)
genVector (k,m) = 
    ( bsToHex $ runPut' $ putPrvKey prv
    , toWIF prv
    , m
    , bsToHex sig1
    , bsToHex sig2
    )
    where prv = fromJust $ makePrvKey k
          msg = hash256 $ stringToBS m
          sig@(Signature r s) = detSignMsg msg prv
          sig1 = runPut' $ put (fromIntegral r :: Hash256) >>
                           put (fromIntegral s :: Hash256)
          sig2 = encode' sig

main :: IO ()
main = do
    print "Network.Haskoin test vectors for RFC 6979 ECDSA (secp256k1, SHA-256)"
    print $ "(PrvKey HEX, PrvKey WIF, message, R || S as HEX, sig as DER)"
    forM_ vectors $ print . genVector

