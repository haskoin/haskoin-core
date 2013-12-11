module QuickCheckUtils where

import Test.QuickCheck

import Control.Applicative

import Data.Maybe
import Data.List

import Network.Haskoin.Wallet
import Network.Haskoin.Wallet.Arbitrary
import Network.Haskoin.Script
import Network.Haskoin.Script.Arbitrary
import Network.Haskoin.Crypto
import Network.Haskoin.Crypto.Arbitrary
import Network.Haskoin.Protocol
import Network.Haskoin.Protocol.Arbitrary
import Network.Haskoin.Util

data PKHashSigTemplate = PKHashSigTemplate Tx [SigInput] [PrvKey]
    deriving (Eq, Show)

-- Generates data for signing a PKHash transaction
instance Arbitrary PKHashSigTemplate where
    arbitrary = do
        inCount   <- choose (0,10)
        perm      <- choose (0,max 0 $ inCount-1)
        outPoints <- vectorOf inCount arbitrary
        prvKeys   <- vectorOf inCount arbitrary
        sigHashes <- vectorOf inCount arbitrary
        payTo <- choose (0,10) >>= \n -> do
            h <- (map (addrToBase58 . PubKeyAddress)) <$> vectorOf n arbitrary    
            v <- vectorOf n $ choose (1,2100000000000000)
            return $ zip h v
        let pubKeys   = map derivePubKey prvKeys
            scriptOut = map (PayPKHash . pubKeyAddr) pubKeys
            scripts   = map encodeOutput scriptOut
            sigInputs = map (\(s,o,h) -> SigInput s o h) 
                            (zip3 scripts outPoints sigHashes)
            perInputs = (permutations sigInputs) !! perm
            perKeys   = (permutations prvKeys) !! perm
            tx        = fromRight $ buildAddrTx outPoints payTo
        return $ PKHashSigTemplate tx perInputs perKeys

