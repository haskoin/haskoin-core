{-|
  Arbitrary types for Network.Haskoin.Transaction
-}
module Network.Haskoin.Test.Transaction where

import           Control.Monad
import qualified Data.ByteString             as BS
import           Data.List                   (nub, nubBy, permutations)
import           Data.Word                   (Word64)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Script
import           Network.Haskoin.Test.Crypto
import           Network.Haskoin.Test.Script
import           Network.Haskoin.Test.Util
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Test.QuickCheck

newtype TestCoin = TestCoin { getTestCoin :: Word64 }
    deriving (Eq, Show)

instance Coin TestCoin where
    coinValue = getTestCoin

arbitraryTxHash :: Gen TxHash
arbitraryTxHash = TxHash <$> arbitraryHash256

-- | Arbitrary amount of Satoshi as Word64 (Between 1 and 21e14)
arbitrarySatoshi :: Gen TestCoin
arbitrarySatoshi = TestCoin <$> choose (1, maxSatoshi)

-- | Arbitrary OutPoint
arbitraryOutPoint :: Gen OutPoint
arbitraryOutPoint = OutPoint <$> arbitraryTxHash <*> arbitrary

-- | Arbitrary TxOut
arbitraryTxOut :: Gen TxOut
arbitraryTxOut =
    TxOut <$> (getTestCoin <$> arbitrarySatoshi)
          <*> (encodeOutputBS <$> arbitraryScriptOutput)

-- | Arbitrary Witness
arbitraryWitness :: Gen Witness
arbitraryWitness = do
    n <- choose (0, 5)
    ws <- vectorOf n arbitraryBS
    return $ Witness ws

-- | Arbitrary TxIn without Witness
arbitraryTxIn :: Gen TxIn
arbitraryTxIn =
    TxIn <$> arbitraryOutPoint
         <*> (encodeInputBS <$> arbitraryScriptInput)
         <*> return (Witness [])
         <*> arbitrary

-- | Arbitrary TxIn with Witness
arbitraryTxInWitness :: Gen TxIn
arbitraryTxInWitness =
    TxIn <$> arbitraryOutPoint
         <*> (encodeInputBS <$> arbitraryScriptInput)
         <*> arbitraryWitness
         <*> arbitrary

-- | Arbitrary Tx
arbitraryTx :: Gen Tx
arbitraryTx = do
    v    <- arbitrary
    ni   <- choose (0,5)
    no   <- choose (0,5)
    inps <- vectorOf ni arbitraryTxInWitness
    outs <- vectorOf no arbitraryTxOut
    let uniqueInps = nubBy (\a b -> prevOutput a == prevOutput b) inps
    t    <- arbitrary
    return $ createTx v uniqueInps outs t

-- | Arbitrary Tx containing only inputs of type SpendPKHash, SpendScriptHash
-- (multisig) and outputs of type PayPKHash and PaySH. Only compressed
-- public keys are used.
arbitraryAddrOnlyTx :: Gen Tx
arbitraryAddrOnlyTx = do
    v    <- arbitrary
    ni   <- choose (0,5)
    no   <- choose (0,5)
    inps <- vectorOf ni arbitraryAddrOnlyTxIn
    outs <- vectorOf no arbitraryAddrOnlyTxOut
    t    <- arbitrary
    return $ createTx v inps outs t

-- | Arbitrary TxIn that can only be of type SpendPKHash or
-- SpendScriptHash (multisig). Only compressed public keys are used.
arbitraryAddrOnlyTxIn :: Gen TxIn
arbitraryAddrOnlyTxIn = do
    o   <- arbitraryOutPoint
    inp <- oneof [ arbitraryPKHashCInput, arbitraryMulSigSHCInput ]
    s   <- arbitrary
    return $ TxIn o (encodeInputBS inp) (Witness []) s

-- | Arbitrary TxOut that can only be of type PayPKHash or PaySH
arbitraryAddrOnlyTxOut :: Gen TxOut
arbitraryAddrOnlyTxOut = do
    v <- getTestCoin <$> arbitrarySatoshi
    out <- oneof [ arbitraryPKHashOutput, arbitrarySHOutput ]
    return $ TxOut v $ encodeOutputBS out

-- | Arbitrary SigInput with the corresponding private keys used
-- to generate the ScriptOutput or RedeemScript
arbitrarySigInput :: Gen (SigInput, [PrvKey])
arbitrarySigInput =
    oneof
        [ arbitraryPKSigInput >>= \(si, k) -> return (si, [k])
        , arbitraryPKHashSigInput  >>= \(si, k) -> return (si, [k])
        , arbitraryMSSigInput
        , arbitrarySHSigInput
        ]

-- | Arbitrary SigInput with a ScriptOutput of type PayPK
arbitraryPKSigInput :: Gen (SigInput, PrvKey)
arbitraryPKSigInput = do
    k <- arbitraryPrvKey
    let out = PayPK $ derivePubKey k
    op <- arbitraryOutPoint
    sh <- arbitraryValidSigHash
    return (SigInput out op sh Nothing, k)

-- | Arbitrary SigInput with a ScriptOutput of type PayPKHash
arbitraryPKHashSigInput :: Gen (SigInput, PrvKey)
arbitraryPKHashSigInput = do
    k <- arbitraryPrvKey
    let out = PayPKHash $ pubKeyAddr $ derivePubKey k
    op <- arbitraryOutPoint
    sh <- arbitraryValidSigHash
    return (SigInput out op sh Nothing, k)

-- | Arbitrary SigInput with a ScriptOutput of type PayMulSig
arbitraryMSSigInput :: Gen (SigInput, [PrvKey])
arbitraryMSSigInput = do
    (m,n) <- arbitraryMSParam
    ks    <- vectorOf n arbitraryPrvKey
    let out = PayMulSig (map derivePubKey ks) m
    op <- arbitraryOutPoint
    sh <- arbitraryValidSigHash
    perm <- choose (0,n-1)
    let ksPerm = take m $ permutations ks !! perm
    return (SigInput out op sh Nothing, ksPerm)

-- | Arbitrary SigInput with  ScriptOutput of type PaySH and a RedeemScript
arbitrarySHSigInput :: Gen (SigInput, [PrvKey])
arbitrarySHSigInput = do
    (SigInput rdm op sh _, ks) <- oneof
        [ f <$> arbitraryPKSigInput
        , f <$> arbitraryPKHashSigInput
        , arbitraryMSSigInput
        ]
    let out = PayScriptHash $ scriptAddr rdm
    return (SigInput out op sh $ Just rdm, ks)
  where
    f (si, k) = (si, [k])

-- | Arbitrary Tx (empty TxIn), SigInputs and PrvKeys that can be passed to
-- signTx or detSignTx to fully sign the Tx.
arbitrarySigningData :: Gen (Tx, [SigInput], [PrvKey])
arbitrarySigningData = do
    v  <- arbitrary
    ni <- choose (1,5)
    no <- choose (1,5)
    sigis <- vectorOf ni arbitrarySigInput
    let uSigis = nubBy (\(a,_) (b,_) -> sigDataOP a == sigDataOP b) sigis
    inps <- forM uSigis $ \(s,_) -> do
        sq <- arbitrary
        return $ TxIn (sigDataOP s) BS.empty (Witness []) sq
    outs <- vectorOf no arbitraryTxOut
    l    <- arbitrary
    perm <- choose (0, length inps - 1)
    let tx   = createTx v (permutations inps !! perm) outs l
        keys = concatMap snd uSigis
    return (tx, map fst uSigis, keys)

arbitraryEmptyTx :: Gen Tx
arbitraryEmptyTx = do
    v    <- arbitrary
    no   <- choose (1,5)
    ni   <- choose (1,5)
    outs <- vectorOf no arbitraryTxOut
    ops  <- vectorOf ni arbitraryOutPoint
    t    <- arbitrary
    s    <- arbitrary
    return $ createTx v (map (\op -> TxIn op BS.empty (Witness []) s) (nub ops)) outs t

arbitraryPartialTxs :: Gen ([Tx], [(ScriptOutput, OutPoint, Int, Int)])
arbitraryPartialTxs = do
    tx <- arbitraryEmptyTx
    res <- forM (map prevOutput $ txIn tx) $ \op -> do
        (so, rdmM, prvs, m, n) <- arbitraryData
        txs <- mapM (singleSig so rdmM tx op) prvs
        return (txs, (so, op, m, n))
    return (concatMap fst res, map snd res)
  where
    singleSig so rdmM tx op prv = do
        sh <- arbitraryValidSigHash
        let sigi = SigInput so op sh rdmM
        return $ fromRight $ signTx tx [sigi] [prv]
    arbitraryData = do
        (m,n) <- arbitraryMSParam
        nPrv  <- choose (m,n)
        keys  <- vectorOf n arbitraryPubKey
        perm <- choose (0, length keys - 1)
        let pubKeys = map snd keys
            prvKeys = take nPrv $ permutations (map fst keys) !! perm
        let so = PayMulSig pubKeys m
        elements [ (so, Nothing, prvKeys, m, n)
                 , (PayScriptHash $ scriptAddr so, Just so, prvKeys, m, n)
                 ]
