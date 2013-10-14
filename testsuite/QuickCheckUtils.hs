module QuickCheckUtils where

import Test.QuickCheck

import Control.Applicative

import Data.Maybe
import Data.List

import Haskoin.Wallet
import Haskoin.Wallet.Arbitrary
import Haskoin.Script
import Haskoin.Script.Arbitrary
import Haskoin.Crypto
import Haskoin.Crypto.Arbitrary
import Haskoin.Protocol
import Haskoin.Protocol.Arbitrary
import Haskoin.Util

data PKHashSigTemplate = PKHashSigTemplate Tx [SigInput] [PrvKey]
    deriving (Eq, Show)

-- Generates data for signing a PKHash transaction
instance Arbitrary PKHashSigTemplate where
    arbitrary = do
        inCount   <- choose (0,10)
        perm      <- choose (0,max 0 $ inCount-1)
        outPoints <- vectorOf inCount arbitrary
        prvKeys   <- vectorOf inCount arbitrary
        sigHashes <- vectorOf inCount $ elements [ SigAll , SigNone
                                                 , SigAllAcp , SigNoneAcp
                                                 ]
        payTo <- choose (0,10) >>= \n -> do
            h <- (map (addrToBase58 . PubKeyAddress)) <$> vectorOf n arbitrary    
            v <- vectorOf n $ choose (1,2100000000000000)
            return $ zip h v
        let pubKeys   = map derivePubKey prvKeys
            scriptOut = map (PayPKHash . pubKeyAddr) pubKeys
            scripts   = map encodeOutput scriptOut
            sigInputs = map (\(s,o,h) -> SigInput (TxOut 1 s) o h) 
                            (zip3 scripts outPoints sigHashes)
            perInputs = (permutations sigInputs) !! perm
            perKeys   = (permutations prvKeys) !! perm
            tx        = fromRight $ buildPKHashTx outPoints payTo
        return $ PKHashSigTemplate tx perInputs perKeys

