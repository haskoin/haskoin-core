{-|
  Arbitrary types for Network.Haskoin.Transaction
-}
module Network.Haskoin.Test.Transaction
( ArbitrarySatoshi(..)
, ArbitraryTx(..)
, ArbitraryTxHash(..)
, ArbitraryTxIn(..)
, ArbitraryTxOut(..)
, ArbitraryOutPoint(..)
, ArbitraryAddrOnlyTx(..)
, ArbitraryAddrOnlyTxIn(..)
, ArbitraryAddrOnlyTxOut(..)
, ArbitrarySigInput(..)
, ArbitraryPKSigInput(..)
, ArbitraryPKHashSigInput(..)
, ArbitraryMSSigInput(..)
, ArbitrarySHSigInput(..)
, ArbitrarySigningData(..)
, ArbitraryPartialTxs(..)
) where

import Test.QuickCheck
    ( Arbitrary
    , arbitrary
    , vectorOf
    , oneof
    , choose
    , elements
    )

import Control.Monad (forM)

import Data.Word (Word64)
import Data.List (permutations, nubBy, nub)
import qualified Data.ByteString as BS (empty)

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Test.Script

import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Constants
import Network.Haskoin.Util

newtype ArbitraryTxHash = ArbitraryTxHash TxHash
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryTxHash where
    arbitrary = do
        ArbitraryHash256 h <- arbitrary
        return $ ArbitraryTxHash $ TxHash h

-- | Arbitrary amount of Satoshi as Word64 (Between 1 and 21e14)
newtype ArbitrarySatoshi = ArbitrarySatoshi Word64
    deriving (Eq, Show, Read)

instance Arbitrary ArbitrarySatoshi where
    arbitrary = ArbitrarySatoshi <$> choose (1, maxSatoshi)

instance Coin ArbitrarySatoshi where
    coinValue (ArbitrarySatoshi v) = v

-- | Arbitrary OutPoint
newtype ArbitraryOutPoint = ArbitraryOutPoint OutPoint
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryOutPoint where
    arbitrary = do
        op <- do
            ArbitraryTxHash tx <- arbitrary
            i  <- arbitrary
            return $ OutPoint tx i
        return $ ArbitraryOutPoint op

-- | Arbitrary TxOut
newtype ArbitraryTxOut = ArbitraryTxOut TxOut
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryTxOut where
    arbitrary = do
        ArbitrarySatoshi v <- arbitrary
        ArbitraryScriptOutput out <- arbitrary
        return $ ArbitraryTxOut $ TxOut v $ encodeOutputBS out

-- | Arbitrary TxIn
newtype ArbitraryTxIn = ArbitraryTxIn TxIn
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryTxIn where
    arbitrary = do
        ArbitraryOutPoint o <- arbitrary
        ArbitraryScriptInput inp <- arbitrary
        s <- arbitrary
        return $ ArbitraryTxIn $ TxIn o (encodeInputBS inp) s

-- | Arbitrary Tx
newtype ArbitraryTx = ArbitraryTx Tx
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryTx where
    arbitrary = do
        v <- arbitrary
        ni <- choose (0,5)
        no <- choose (0,5)
        inps <- vectorOf ni $ arbitrary >>= \(ArbitraryTxIn i) -> return i
        outs <- vectorOf no $ arbitrary >>= \(ArbitraryTxOut o) -> return o
        let uniqueInps = nubBy (\a b -> prevOutput a == prevOutput b) inps
        t <- arbitrary
        return $ ArbitraryTx $ createTx v uniqueInps outs t

-- | Arbitrary Tx containing only inputs of type SpendPKHash, SpendScriptHash
-- (multisig) and outputs of type PayPKHash and PaySH. Only compressed
-- public keys are used.
newtype ArbitraryAddrOnlyTx = ArbitraryAddrOnlyTx Tx
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAddrOnlyTx where
    arbitrary = do
        v <- arbitrary
        ni <- choose (0,5)
        no <- choose (0,5)
        inps <- vectorOf ni $
            arbitrary >>= \(ArbitraryAddrOnlyTxIn i) -> return i
        outs <- vectorOf no $
            arbitrary >>= \(ArbitraryAddrOnlyTxOut o) -> return o
        t <- arbitrary
        return $ ArbitraryAddrOnlyTx $ createTx v inps outs t

-- | Arbitrary TxIn that can only be of type SpendPKHash or
-- SpendScriptHash (multisig). Only compressed public keys are used.
newtype ArbitraryAddrOnlyTxIn = ArbitraryAddrOnlyTxIn TxIn
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAddrOnlyTxIn where
    arbitrary = do
        ArbitraryOutPoint o <- arbitrary
        inp <- oneof
            [ arbitrary >>= \(ArbitraryPKHashCInput i) -> return i
            , arbitrary >>= \(ArbitraryMulSigSHCInput i) -> return i
            ]
        s <- arbitrary
        return $ ArbitraryAddrOnlyTxIn $ TxIn o (encodeInputBS inp) s

-- | Arbitrary TxOut that can only be of type PayPKHash or PaySH
newtype ArbitraryAddrOnlyTxOut = ArbitraryAddrOnlyTxOut TxOut
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryAddrOnlyTxOut where
    arbitrary = do
        ArbitrarySatoshi v <- arbitrary
        out <- oneof
            [ arbitrary >>= \(ArbitraryPKHashOutput o) -> return o
            , arbitrary >>= \(ArbitrarySHOutput o) -> return o
            ]
        return $ ArbitraryAddrOnlyTxOut $ TxOut v $ encodeOutputBS out

-- | Arbitrary SigInput with the corresponding private keys used
-- to generate the ScriptOutput or RedeemScript
data ArbitrarySigInput = ArbitrarySigInput SigInput [PrvKey]
    deriving (Eq, Show, Read)

instance Arbitrary ArbitrarySigInput where
    arbitrary = do
        (si, ks) <- oneof
            [ arbitrary >>= \(ArbitraryPKSigInput si k) -> return (si, [k])
            , arbitrary >>= \(ArbitraryPKHashSigInput si k) -> return (si, [k])
            , arbitrary >>= \(ArbitraryMSSigInput si ks) -> return (si, ks)
            , arbitrary >>= \(ArbitrarySHSigInput si ks) -> return (si, ks)
            ]
        return $ ArbitrarySigInput si ks

-- | Arbitrary SigInput with a ScriptOutput of type PayPK
data ArbitraryPKSigInput = ArbitraryPKSigInput SigInput PrvKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPKSigInput where
    arbitrary = do
        ArbitraryPrvKey k <- arbitrary
        let out = PayPK $ derivePubKey k
        ArbitraryOutPoint op <- arbitrary
        ArbitraryValidSigHash sh <- arbitrary
        return $ ArbitraryPKSigInput (SigInput out op sh Nothing) k

-- | Arbitrary SigInput with a ScriptOutput of type PayPKHash
data ArbitraryPKHashSigInput = ArbitraryPKHashSigInput SigInput PrvKey
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPKHashSigInput where
    arbitrary = do
        ArbitraryPrvKey k <- arbitrary
        let out = PayPKHash $ pubKeyAddr $ derivePubKey k
        ArbitraryOutPoint op <- arbitrary
        ArbitraryValidSigHash sh <- arbitrary
        return $ ArbitraryPKHashSigInput (SigInput out op sh Nothing) k

-- | Arbitrary SigInput with a ScriptOutput of type PayMulSig
data ArbitraryMSSigInput = ArbitraryMSSigInput SigInput [PrvKey]
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryMSSigInput where
    arbitrary = do
        ArbitraryMSParam m n <- arbitrary
        ks <- map (\(ArbitraryPrvKey k) -> k) <$> vectorOf n arbitrary
        let out = PayMulSig (map derivePubKey ks) m
        ArbitraryOutPoint op <- arbitrary
        ArbitraryValidSigHash sh <- arbitrary
        perm <- choose (0,n-1)
        let ksPerm = take m $ permutations ks !! perm
        return $ ArbitraryMSSigInput (SigInput out op sh Nothing) ksPerm

-- | Arbitrary SigInput with  ScriptOutput of type PaySH and a RedeemScript
data ArbitrarySHSigInput = ArbitrarySHSigInput SigInput [PrvKey]
    deriving (Eq, Show, Read)

instance Arbitrary ArbitrarySHSigInput where
    arbitrary = do
        (rdm, ks, op, sh) <- oneof
            [ a <$> arbitrary, b <$> arbitrary, c <$> arbitrary ]
        let out = PayScriptHash $ scriptAddr rdm
        return $ ArbitrarySHSigInput (SigInput out op sh $ Just rdm) ks
      where
        a (ArbitraryPKSigInput (SigInput o op sh _) k) = (o, [k], op, sh)
        b (ArbitraryPKHashSigInput (SigInput o op sh _) k) = (o, [k], op, sh)
        c (ArbitraryMSSigInput (SigInput o op sh _) ks) = (o, ks, op, sh)

-- | Arbitrary Tx (empty TxIn), SigInputs and PrvKeys that can be passed to
-- signTx or detSignTx to fully sign the Tx.
data ArbitrarySigningData = ArbitrarySigningData Tx [SigInput] [PrvKey]
    deriving (Eq, Show, Read)

instance Arbitrary ArbitrarySigningData where
    arbitrary = do
        v <- arbitrary
        ni <- choose (1,5)
        no <- choose (1,5)
        sigis <- map f <$> vectorOf ni arbitrary
        let uSigis = nubBy (\(a,_) (b,_) -> sigDataOP a == sigDataOP b) sigis
        inps <- forM uSigis $ \(s,_) -> do
            sq <- arbitrary
            return $ TxIn (sigDataOP s) BS.empty sq
        outs <- map (\(ArbitraryTxOut o) -> o) <$> vectorOf no arbitrary
        l <- arbitrary
        perm <- choose (0, length inps - 1)
        let tx   = createTx v (permutations inps !! perm) outs l
            keys = concat $ map snd uSigis
        return $ ArbitrarySigningData tx (map fst uSigis) keys
      where
        f (ArbitrarySigInput s ks) = (s,ks)

data ArbitraryPartialTxs =
    ArbitraryPartialTxs [Tx] [(ScriptOutput, OutPoint, Int, Int)]
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryPartialTxs where
    arbitrary = do
        tx <- arbitraryEmptyTx
        res <- forM (map prevOutput $ txIn tx) $ \op -> do
            (so, rdmM, prvs, m, n) <- arbitraryData
            txs <- mapM (singleSig so rdmM tx op) prvs
            return (txs, (so, op, m, n))
        return $ ArbitraryPartialTxs (concat $ map fst res) (map snd res)
      where
        singleSig so rdmM tx op prv = do
            ArbitraryValidSigHash sh <- arbitrary
            let sigi = SigInput so op sh rdmM
            return $ fromRight $ signTx tx [sigi] [prv]
        arbitraryData = do
            ArbitraryMSParam m n <- arbitrary
            nPrv <- choose (m,n)
            keys <- vectorOf n $
                (\(ArbitraryPubKey k p) -> (k, p)) <$> arbitrary
            perm <- choose (0, length keys - 1)
            let pubKeys = map snd keys
                prvKeys = take nPrv $ permutations (map fst keys) !! perm
            let so = PayMulSig pubKeys m
            elements [ (so, Nothing, prvKeys, m, n)
                     , (PayScriptHash $ scriptAddr so, Just so, prvKeys, m, n)
                     ]
        arbitraryEmptyTx = do
            v <- arbitrary
            no <- choose (1,5)
            ni <- choose (1,5)
            outs <- vectorOf no $ (\(ArbitraryTxOut o) -> o) <$> arbitrary
            ops <- vectorOf ni $ (\(ArbitraryOutPoint op) -> op) <$> arbitrary
            t <- arbitrary
            s <- arbitrary
            return $ createTx
                v (map (\op -> TxIn op BS.empty s) (nub ops)) outs t


