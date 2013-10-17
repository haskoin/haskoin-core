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
      ( [ OutPoint 0x01 1
        , OutPoint 0x03 1
        , OutPoint 0x02 1
        ]
      , [ ("16HGDPmTxXGokyuodGyD1f5MMdJXmeoGZr",1)
        , ("14JK5RYEwsSTDvefYC42AFj7LtFuT6TeAj",2)
        ]
      , [ OutPoint 0x01 1
        , OutPoint 0x02 1
        ]
      , [ 0x10, 0x11 ]
      )
    ]

genVector :: ([OutPoint],[(String,Word64)],[OutPoint],[Integer]) -> Build Tx
genVector (xs,ys,zs,ps) = detSignTx tx sigi prv
    where tx = fromRight $ buildAddrTx xs ys
          prv = map (fromJust . makePrvKey) ps
          pub = map derivePubKey prv
          sps = map (encodeOutput . PayPKHash . pubKeyAddr) pub
          sigi = map (\(s,op) -> SigInput s op $ SigAll False) (zip sps zs)

main :: IO ()
main = do
    print "Haskoin test vectors for deterministic signatures"
    forM_ vectors $ pp . genVector

