import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.Binary 
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Crypto
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Util 

vectors = 
    [ 
      ( [ (0x01,1)
        , (0x02,1)
        , (0x03,1)
        ]
      , [ ("16HGDPmTxXGokyuodGyD1f5MMdJXmeoGZr",1)
        , ("14JK5RYEwsSTDvefYC42AFj7LtFuT6TeAj",2)
        ]
      )
    ]

genVector :: ([(Integer,Word32)],[(String,Word64)]) -> String
genVector (xs,ys) = 
    bsToString $ bsToHex $ runPut' $ putScriptOps $ runScript $ fromJust $ encodeOutput out
    where os  = map (\(a,b) -> OutPoint (fromInteger a) b) xs
          tx  = fromJust $ buildPKHashTx os ys
          prv = fromJust $ makePrvKey $ 0x01
          pub = derivePubKey prv
          out = PayPKHash $ pubKeyAddr $ derivePubKey prv
          sd  = SigData tx out 1 SigAll
          tx' = fromJust $ detSignTx sd prv

main :: IO ()
main = do
    print "Haskoin test vectors for deterministic signatures"
    forM_ vectors $ pp . genVector

