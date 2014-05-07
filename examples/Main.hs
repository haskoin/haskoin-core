import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)

import Data.Maybe (fromJust)

-- For serializing/de-serializing interface
import Data.Binary (encode, decodeOrFail)
import qualified Data.ByteString as BS

import Network.Haskoin.Crypto
import Network.Haskoin.Util

main :: IO ()
main = do

    -- Build an Integer from a random source
    rnd <- bsToInteger <$> (devURandom 32)

    -- Test if Integer rnd is in the range [1, N-1]
    let vld  = isValidPrvKey rnd
        -- Build a private key from a random Integer
        -- Will fail if rnd is not in the range [1, N-1]
        prv  = fromJust $ makePrvKey rnd
        -- Derive the public key from a private key
        pub  = derivePubKey prv
        -- Compute the bitcoin address from the public key
        addr = addrToBase58 $ pubKeyAddr pub
        -- Serialize the private key to WIF format
        wif  = toWIF prv
        -- Deserialize a private key from WIF format
        prv' = fromWIF wif

    -- Serialize and de-serialize a public key
    -- See Data.Binary for more details
    let pubBin = encode pub
        pub'   = case decodeOrFail pubBin of
            (Left  (_, _, err)) -> error err
            (Right (_, _, res)) -> res :: PubKey

    -- Create a message in ByteString format
    let msg  = BS.pack [1,3,3,7]
        -- Compute two rounds of SHA-256
        hash = doubleHash256 msg
        -- Deterministically sign messages. Both signatures here are equal
        dSig1 = detSignMsg hash prv
        dSig2 = detSignMsg hash prv
        -- Verify a signature
        dVer  = verifySig hash dSig1 pub

    -- Initialize a PRNG environment for creating signatures
    withSource devURandom $ do 

        -- Generate private keys derived from the internal PRNG
        prv1 <- genPrvKey
        prv2 <- genPrvKey

        -- Signatures are signed with nonces derived from the internal PRNG
        sig1 <- signMsg hash prv
        sig2 <- signMsg hash prv

        -- Verify signatures
        let ver1 = verifySig hash sig1 pub
            ver2 = verifySig hash sig2 pub

        -- Serialize and de-serialize a signature
        -- See Data.Binary for more details
        let sigBin = encode sig1
            -- Deserialize a signature
            sig1'  = case decodeOrFail sigBin of
                (Left  (_, _, err)) -> error err
                (Right (_, _, res)) -> res :: Signature

        -- Print some results
        liftIO $ do
            print $ "Deterministic Signature 1: " ++ (show dSig1)
            print $ "Deterministic Signature 2: " ++ (show dSig2)
            print $ "Random Signature 1: " ++ (show sig1)
            print $ "Random Signature 2: " ++ (show sig2)
            print $ "Signature verification: " 
                ++ (show dVer) ++" " ++ (show ver1) ++ " " ++ (show ver2)


