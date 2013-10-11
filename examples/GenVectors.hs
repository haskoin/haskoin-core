import Control.Monad
import Control.Applicative

import Data.Either
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
      ( [ (0x02,1)
        ]
      , [ ("16HGDPmTxXGokyuodGyD1f5MMdJXmeoGZr",1)
        , ("14JK5RYEwsSTDvefYC42AFj7LtFuT6TeAj",2)
        ]
      )
    ]

genVector :: ([(Integer,Word32)],[(String,Word64)]) -> Build String
genVector (xs,ys) = do
    tx <- buildPKHashTx os ys
    sigTx <- detSignTx tx [sigi] [prv1]
    return $ bsToString $ bsToHex $ encode' sigTx
    where os  = map (\(a,b) -> OutPoint (fromInteger a) b) xs
          prv2 = fromJust $ makePrvKey $ 0x01
          prv1 = fromJust $ makePrvKey $ 0x02
          pub1 = derivePubKey prv1
          out = fromJust $ encodeOutput $ PayPKHash $ pubKeyAddr $ derivePubKey prv1
          sigi = SigInput (TxOut 1 out) (OutPoint 0x02 1) SigAll

main :: IO ()
main = do
    print "Haskoin test vectors for deterministic signatures"
    forM_ vectors $ pp . genVector

