{-|
Module      : Network.Haskoin.Test.Transaction
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX
-}
module Network.Haskoin.Test.Transaction where
import           Control.Monad
import qualified Data.ByteString             as BS
import           Data.Either                 (fromRight)
import           Data.List                   (nub, nubBy, permutations)
import           Data.Word                   (Word64)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys.Common
import           Network.Haskoin.Script
import           Network.Haskoin.Test.Crypto
import           Network.Haskoin.Test.Keys
import           Network.Haskoin.Test.Script
import           Network.Haskoin.Test.Util
import           Network.Haskoin.Transaction
import           Test.QuickCheck

-- | Wrapped coin value for testing.
newtype TestCoin = TestCoin { getTestCoin :: Word64 }
    deriving (Eq, Show)

instance Coin TestCoin where
    coinValue = getTestCoin

-- | Arbitrary transaction hash (for non-existent transaction).
arbitraryTxHash :: Gen TxHash
arbitraryTxHash = TxHash <$> arbitraryHash256

-- | Arbitrary amount of Satoshi as 'Word64' (Between 1 and 21e14)
arbitrarySatoshi :: Network -> Gen TestCoin
arbitrarySatoshi net = TestCoin <$> choose (1, getMaxSatoshi net)

-- | Arbitrary 'OutPoint'.
arbitraryOutPoint :: Gen OutPoint
arbitraryOutPoint = OutPoint <$> arbitraryTxHash <*> arbitrary

-- | Arbitrary 'TxOut'.
arbitraryTxOut :: Network -> Gen TxOut
arbitraryTxOut net =
    TxOut <$> (getTestCoin <$> arbitrarySatoshi net)
          <*> (encodeOutputBS <$> arbitraryScriptOutput net)

-- | Arbitrary 'TxIn'.
arbitraryTxIn :: Network -> Gen TxIn
arbitraryTxIn net =
    TxIn <$> arbitraryOutPoint
         <*> (encodeInputBS <$> arbitraryScriptInput net)
         <*> arbitrary

-- | Arbitrary transaction. Can be regular or with witnesses.
arbitraryTx :: Network -> Gen Tx
arbitraryTx net = oneof [arbitraryLegacyTx net, arbitraryWitnessTx net]

-- | Arbitrary regular transaction.
arbitraryLegacyTx :: Network -> Gen Tx
arbitraryLegacyTx net = arbitraryWLTx net False

-- | Arbitrary witness transaction (witness data is fake).
arbitraryWitnessTx :: Network -> Gen Tx
arbitraryWitnessTx net = arbitraryWLTx net True

-- | Arbitrary witness or legacy transaction.
arbitraryWLTx :: Network -> Bool -> Gen Tx
arbitraryWLTx net wit = do
    ni <- choose (0, 5)
    no <-
        if wit
            then choose (0, 5)
            else choose
                     ( if ni == 0
                           then 2
                           else 0
                     , 5 -- avoid witness case
                      )
    inps <- vectorOf ni (arbitraryTxIn net)
    outs <- vectorOf no (arbitraryTxOut net)
    let uniqueInps = nubBy (\a b -> prevOutput a == prevOutput b) inps
    w <- if wit then vectorOf (length uniqueInps) (listOf arbitraryBS) else return []
    Tx <$> arbitrary <*> pure uniqueInps <*> pure outs <*> pure w <*> arbitrary

-- | Arbitrary transaction containing only inputs of type 'SpendPKHash',
-- 'SpendScriptHash' (multisig) and outputs of type 'PayPKHash' and 'PaySH'.
-- Only compressed public keys are used.
arbitraryAddrOnlyTx :: Network -> Gen Tx
arbitraryAddrOnlyTx net = do
    ni <- choose (0, 5)
    no <- choose (0, 5)
    inps <- vectorOf ni (arbitraryAddrOnlyTxIn net)
    outs <- vectorOf no (arbitraryAddrOnlyTxOut net)
    Tx <$> arbitrary <*> pure inps <*> pure outs <*> pure [] <*> arbitrary

-- | Like 'arbitraryAddrOnlyTx' without empty signatures in the inputs.
arbitraryAddrOnlyTxFull :: Network -> Gen Tx
arbitraryAddrOnlyTxFull net = do
    ni <- choose (0, 5)
    no <- choose (0, 5)
    inps <- vectorOf ni (arbitraryAddrOnlyTxInFull net)
    outs <- vectorOf no (arbitraryAddrOnlyTxOut net)
    Tx <$> arbitrary <*> pure inps <*> pure outs <*> pure [] <*> arbitrary

-- | Arbitrary TxIn that can only be of type 'SpendPKHash' or 'SpendScriptHash'
-- (multisig). Only compressed public keys are used.
arbitraryAddrOnlyTxIn :: Network -> Gen TxIn
arbitraryAddrOnlyTxIn net = do
    inp <- oneof [arbitraryPKHashInput net, arbitraryMulSigSHInput net]
    TxIn <$> arbitraryOutPoint <*> pure (encodeInputBS inp) <*> arbitrary

-- | like 'arbitraryAddrOnlyTxIn' with no empty signatures.
arbitraryAddrOnlyTxInFull :: Network -> Gen TxIn
arbitraryAddrOnlyTxInFull net = do
    inp <-
        oneof [arbitraryPKHashInputFullC net, arbitraryMulSigSHInputFullC net]
    TxIn <$> arbitraryOutPoint <*> pure (encodeInputBS inp) <*> arbitrary

-- | Arbitrary 'TxOut' that can only be of type 'PayPKHash' or 'PaySH'.
arbitraryAddrOnlyTxOut :: Network -> Gen TxOut
arbitraryAddrOnlyTxOut net = do
    v <- getTestCoin <$> arbitrarySatoshi net
    out <- oneof [arbitraryPKHashOutput, arbitrarySHOutput]
    return $ TxOut v $ encodeOutputBS out

-- | Arbitrary 'SigInput' with the corresponding private keys used
-- to generate the 'ScriptOutput' or 'RedeemScript'.
arbitrarySigInput :: Network -> Gen (SigInput, [SecKeyI])
arbitrarySigInput net =
    oneof
        [ arbitraryPKSigInput net >>= \(si, k) -> return (si, [k])
        , arbitraryPKHashSigInput  net >>= \(si, k) -> return (si, [k])
        , arbitraryMSSigInput net
        , arbitrarySHSigInput net
        ]

-- | Arbitrary 'SigInput' with a 'ScriptOutput' of type 'PayPK'.
arbitraryPKSigInput :: Network -> Gen (SigInput, SecKeyI)
arbitraryPKSigInput net = arbitraryAnyInput net False

-- | Arbitrary 'SigInput' with a 'ScriptOutput' of type 'PayPKHash'.
arbitraryPKHashSigInput :: Network -> Gen (SigInput, SecKeyI)
arbitraryPKHashSigInput net = arbitraryAnyInput net True

-- | Arbitrary 'SigInput'.
arbitraryAnyInput :: Network -> Bool -> Gen (SigInput, SecKeyI)
arbitraryAnyInput net pkh = do
    (k, p) <- arbitraryKeyPair
    let out | pkh = PayPKHash $ getAddrHash160 $ pubKeyAddr p
            | otherwise = PayPK p
    (val, op, sh) <- arbitraryInputStuff net
    return (SigInput out val op sh Nothing, k)

-- | Arbitrary value, out point and sighash for an input.
arbitraryInputStuff :: Network -> Gen (Word64, OutPoint, SigHash)
arbitraryInputStuff net = do
    val <- getTestCoin <$> arbitrarySatoshi net
    op <- arbitraryOutPoint
    sh <- arbitraryValidSigHash net
    return (val, op, sh)

-- | Arbitrary 'SigInput' with a 'ScriptOutput' of type 'PayMulSig'.
arbitraryMSSigInput :: Network -> Gen (SigInput, [SecKeyI])
arbitraryMSSigInput net = do
    (m, n) <- arbitraryMSParam
    ks <- vectorOf n arbitraryKeyPair
    let out = PayMulSig (map snd ks) m
    (val, op, sh) <- arbitraryInputStuff net
    perm <- choose (0, n - 1)
    let ksPerm = map fst $ take m $ permutations ks !! perm
    return (SigInput out val op sh Nothing, ksPerm)

-- | Arbitrary 'SigInput' with 'ScriptOutput' of type 'PaySH' and a
-- 'RedeemScript'.
arbitrarySHSigInput :: Network -> Gen (SigInput, [SecKeyI])
arbitrarySHSigInput net = do
    (SigInput rdm val op sh _, ks) <- oneof
        [ f <$> arbitraryPKSigInput net
        , f <$> arbitraryPKHashSigInput net
        , arbitraryMSSigInput net
        ]
    let out = PayScriptHash $ getAddrHash160 $ payToScriptAddress rdm
    return (SigInput out val op sh $ Just rdm, ks)
  where
    f (si, k) = (si, [k])

-- | Arbitrary 'Tx' (empty 'TxIn'), 'SigInputs' and private keys that can be
-- passed to 'signTx' or 'detSignTx' to fully sign the 'Tx'.
arbitrarySigningData :: Network -> Gen (Tx, [SigInput], [SecKeyI])
arbitrarySigningData net = do
    v <- arbitrary
    ni <- choose (1, 5)
    no <- choose (1, 5)
    sigis <- vectorOf ni (arbitrarySigInput net)
    let uSigis = nubBy (\(a, _) (b, _) -> sigInputOP a == sigInputOP b) sigis
    inps <- forM uSigis $ \(s, _) -> TxIn (sigInputOP s) BS.empty <$> arbitrary
    outs <- vectorOf no (arbitraryTxOut net)
    l <- arbitrary
    perm <- choose (0, length inps - 1)
    let tx = Tx v (permutations inps !! perm) outs [] l
        keys = concatMap snd uSigis
    return (tx, map fst uSigis, keys)

-- | Arbitrary transaction with empty inputs.
arbitraryEmptyTx :: Network -> Gen Tx
arbitraryEmptyTx net = do
    v    <- arbitrary
    no   <- choose (1,5)
    ni   <- choose (1,5)
    outs <- vectorOf no (arbitraryTxOut net)
    ops  <- vectorOf ni arbitraryOutPoint
    t    <- arbitrary
    s    <- arbitrary
    return $ Tx v (map (\op -> TxIn op BS.empty s) (nub ops)) outs [] t

-- | Arbitrary partially-signed transactions.
arbitraryPartialTxs ::
       Network -> Gen ([Tx], [(ScriptOutput, Word64, OutPoint, Int, Int)])
arbitraryPartialTxs net = do
    tx <- arbitraryEmptyTx net
    res <-
        forM (map prevOutput $ txIn tx) $ \op -> do
            (so, val, rdmM, prvs, m, n) <- arbitraryData
            txs <- mapM (singleSig so val rdmM tx op . secKeyData) prvs
            return (txs, (so, val, op, m, n))
    return (concatMap fst res, map snd res)
  where
    singleSig so val rdmM tx op prv = do
        sh <- arbitraryValidSigHash net
        let sigi = SigInput so val op sh rdmM
        return . fromRight (error "Colud not decode transaction") $
            signTx net tx [sigi] [prv]
    arbitraryData = do
        (m, n) <- arbitraryMSParam
        val <- getTestCoin <$> arbitrarySatoshi net
        nPrv <- choose (m, n)
        keys <- vectorOf n arbitraryKeyPair
        perm <- choose (0, length keys - 1)
        let pubKeys = map snd keys
            prvKeys = take nPrv $ permutations (map fst keys) !! perm
        let so = PayMulSig pubKeys m
        elements
            [ (so, val, Nothing, prvKeys, m, n)
            , ( PayScriptHash $ getAddrHash160 $ payToScriptAddress so
              , val
              , Just so
              , prvKeys
              , m
              , n)
            ]
