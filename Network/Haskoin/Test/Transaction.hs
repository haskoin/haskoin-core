{-| 
  Arbitrary types for Network.Haskoin.Transaction
-}
module Network.Haskoin.Test.Transaction
( ArbitrarySatoshi(..)
, ArbitraryTx(..)
, ArbitraryTxIn(..)
, ArbitraryTxOut(..)
, ArbitraryOutPoint(..) 
, ArbitraryCoinbaseTx(..)
, ArbitraryAddrOnlyTx(..)
, ArbitraryAddrOnlyTxIn(..)
, ArbitraryAddrOnlyTxOut(..)
, ArbitraryCoin(..)
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
    , Gen
    , arbitrary
    , vectorOf
    , oneof
    , choose
    , elements
    )

import Control.Monad (forM)
import Control.Applicative ((<$>),(<*>))

import Data.Word (Word64)
import Data.List (permutations, nubBy, nub)
import qualified Data.ByteString as BS (empty)

import Network.Haskoin.Test.Crypto
import Network.Haskoin.Test.Script

import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Network
import Network.Haskoin.Util

-- | Arbitrary amount of Satoshi as Word64 (Between 1 and 21e14)
newtype ArbitrarySatoshi a = ArbitrarySatoshi Word64
    deriving (Eq, Show, Read)

instance forall a. Network a => Arbitrary (ArbitrarySatoshi a) where
    arbitrary = ArbitrarySatoshi <$> choose (1, maxSatoshi (undefined :: a))

-- | Arbitrary OutPoint
newtype ArbitraryOutPoint = ArbitraryOutPoint OutPoint
    deriving (Eq, Show, Read)

instance Arbitrary ArbitraryOutPoint where
    arbitrary = ArbitraryOutPoint <$> (OutPoint <$> arbitrary <*> arbitrary)

-- | Arbitrary TxOut
newtype ArbitraryTxOut a = ArbitraryTxOut TxOut
    deriving (Eq, Show, Read)

instance forall a. Network a => Arbitrary (ArbitraryTxOut a) where
    arbitrary = do
        ArbitrarySatoshi v <- (arbitrary :: Gen (ArbitrarySatoshi a))
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
newtype ArbitraryTx a = ArbitraryTx Tx
    deriving (Eq, Show, Read)

instance forall a. Network a => Arbitrary (ArbitraryTx a) where
    arbitrary = do
        v <- arbitrary
        ni <- choose (0,5)
        no <- choose (0,5)
        inps <- vectorOf ni $ do
            ArbitraryTxIn i <- arbitrary
            return i
        outs <- vectorOf no $ do
            ArbitraryTxOut o <- arbitrary :: Gen (ArbitraryTxOut a)
            return o
        let uniqueInps = nubBy (\a b -> prevOutput a == prevOutput b) inps
        t <- arbitrary
        return $ ArbitraryTx $ Tx v uniqueInps outs t

-- | Arbitrary CoinbaseTx
newtype ArbitraryCoinbaseTx a = ArbitraryCoinbaseTx CoinbaseTx
    deriving (Eq, Show, Read)

instance forall a. Network a => Arbitrary (ArbitraryCoinbaseTx a) where
    arbitrary = do
        v <- arbitrary
        ArbitraryOutPoint op <- arbitrary
        ArbitraryByteString d <- arbitrary
        s <- arbitrary
        no <- choose (0,5)
        outs <- vectorOf no $ do
            ArbitraryTxOut o <- arbitrary :: (Gen (ArbitraryTxOut a))
            return o
        t <- arbitrary
        return $ ArbitraryCoinbaseTx $ CoinbaseTx v op d s outs t

-- | Arbitrary Tx containing only inputs of type SpendPKHash, SpendScriptHash
-- (multisig) and outputs of type PayPKHash and PaySH. Only compressed
-- public keys are used.
newtype ArbitraryAddrOnlyTx a = ArbitraryAddrOnlyTx Tx
    deriving (Eq, Show, Read)

instance forall a. Network a => Arbitrary (ArbitraryAddrOnlyTx a) where
    arbitrary = do
        v <- arbitrary
        ni <- choose (0,5)
        no <- choose (0,5)
        inps <- vectorOf ni $ do
            ArbitraryAddrOnlyTxIn i <- arbitrary
            return i
        outs <- vectorOf no $ do
            ArbitraryAddrOnlyTxOut o <- arbitrary
                :: Gen (ArbitraryAddrOnlyTxOut a)
            return o
        t <- arbitrary
        return $ ArbitraryAddrOnlyTx $ Tx v inps outs t

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
newtype ArbitraryAddrOnlyTxOut a = ArbitraryAddrOnlyTxOut TxOut
    deriving (Eq, Show, Read)

instance forall a. Network a => Arbitrary (ArbitraryAddrOnlyTxOut a) where
    arbitrary = do
        ArbitrarySatoshi v <- (arbitrary :: Gen (ArbitrarySatoshi a))
        out <- oneof
            [ arbitrary >>= \(ArbitraryPKHashOutput o) -> return o
            , arbitrary >>= \(ArbitrarySHOutput o) -> return o
            ]
        return $ ArbitraryAddrOnlyTxOut $ TxOut v $ encodeOutputBS out

-- | Arbitrary Coin
newtype ArbitraryCoin a = ArbitraryCoin (Coin a)
    deriving (Eq, Show, Read)

instance forall a. Network a => Arbitrary (ArbitraryCoin a) where
    arbitrary = do
        ArbitrarySatoshi v <- arbitrary :: Gen (ArbitrarySatoshi a)
        ArbitraryScriptOutput out <- arbitrary
        ArbitraryOutPoint op <- arbitrary
        rdm <- oneof 
            [ arbitrary >>= \(ArbitrarySimpleOutput r) -> return $ Just r
            , return Nothing
            ]
        return $ ArbitraryCoin $ Coin v out op rdm
            
-- | Arbitrary SigInput with the corresponding private keys used
-- to generate the ScriptOutput or RedeemScript
data ArbitrarySigInput a = ArbitrarySigInput (SigInput a) [PrvKey]
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitrarySigInput a) where
    arbitrary = do
        (si, ks) <- oneof
            [ arbitrary >>= \(ArbitraryPKSigInput si k) -> return (si, [k])
            , arbitrary >>= \(ArbitraryPKHashSigInput si k) -> return (si, [k])
            , arbitrary >>= \(ArbitraryMSSigInput si ks) -> return (si, ks)
            , arbitrary >>= \(ArbitrarySHSigInput si ks) -> return (si, ks)
            ]
        return $ ArbitrarySigInput si ks

-- | Arbitrary SigInput with a ScriptOutput of type PayPK
data ArbitraryPKSigInput a = ArbitraryPKSigInput (SigInput a) PrvKey
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryPKSigInput a) where
    arbitrary = do
        ArbitraryPrvKey k <- arbitrary
        let out = PayPK $ derivePubKey k
        ArbitraryOutPoint op <- arbitrary
        ArbitraryValidSigHash sh <- arbitrary
        return $ ArbitraryPKSigInput (SigInput out op sh Nothing) k

-- | Arbitrary SigInput with a ScriptOutput of type PayPKHash
data ArbitraryPKHashSigInput a = ArbitraryPKHashSigInput (SigInput a) PrvKey
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryPKHashSigInput a) where
    arbitrary = do
        ArbitraryPrvKey k <- arbitrary
        let out = PayPKHash $ pubKeyAddr $ derivePubKey k
        ArbitraryOutPoint op <- arbitrary
        ArbitraryValidSigHash sh <- arbitrary
        return $ ArbitraryPKHashSigInput (SigInput out op sh Nothing) k

-- | Arbitrary SigInput with a ScriptOutput of type PayMulSig
data ArbitraryMSSigInput a = ArbitraryMSSigInput (SigInput a) [PrvKey]
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitraryMSSigInput a) where
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
data ArbitrarySHSigInput a = ArbitrarySHSigInput (SigInput a) [PrvKey]
    deriving (Eq, Show, Read)

instance Arbitrary (ArbitrarySHSigInput a) where
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
data ArbitrarySigningData a = ArbitrarySigningData Tx [SigInput a] [PrvKey]
    deriving (Eq, Show, Read)

instance forall a. Network a => Arbitrary (ArbitrarySigningData a) where
    arbitrary = do
        v <- arbitrary
        ni <- choose (1,5)
        no <- choose (1,5)
        sigis <- map f <$> vectorOf ni arbitrary 
        let uSigis = nubBy (\(a,_) (b,_) -> sigDataOP a == sigDataOP b) sigis
        inps <- forM uSigis $ \(s,_) -> do
            sq <- arbitrary
            return $ TxIn (sigDataOP s) BS.empty sq
        outs <- map (\(ArbitraryTxOut o) -> o) <$>
            vectorOf no (arbitrary :: Gen (ArbitraryTxOut a))
        l <- arbitrary
        perm <- choose (0, length inps - 1)
        let tx   = Tx v (permutations inps !! perm) outs l
            keys = concat $ map snd uSigis
        return $ ArbitrarySigningData tx (map fst uSigis) keys
      where
        f (ArbitrarySigInput s ks) = (s,ks)

data ArbitraryPartialTxs a = 
    ArbitraryPartialTxs [Tx] [(ScriptOutput a, OutPoint, Int, Int)]
    deriving (Eq, Show, Read)

instance forall a. Network a => Arbitrary (ArbitraryPartialTxs a) where
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
            return $ fst $ fromRight $ detSignTx tx [sigi] [prv]
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
            outs <- vectorOf no $ (\(ArbitraryTxOut o) -> o) <$>
                (arbitrary :: Gen (ArbitraryTxOut a))
            ops <- vectorOf ni $ (\(ArbitraryOutPoint op) -> op) <$> arbitrary
            t <- arbitrary
            s <- arbitrary
            return $ Tx v (map (\op -> TxIn op BS.empty s) (nub ops)) outs t


