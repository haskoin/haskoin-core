module Network.Haskoin.Transaction.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Word (Word64)
import qualified Data.ByteString as BS (length)

import Network.Haskoin.Test
import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Transaction tests"
        [ testProperty "decode . encode Txid" decEncTxid 
        ]
    , testGroup "Building Transactions"
        [ testProperty "building address tx" testBuildAddrTx
        , testProperty "testing guessTxSize function" testGuessSize
        , testProperty "testing chooseCoins function" testChooseCoins
        , testProperty "testing chooseMSCoins function" testChooseMSCoins
        ]
    , testGroup "Signing Transactions"
        [ testProperty "Sign and validate transactions" testDetSignTx
        , testProperty "Merge partially signed transactions" testMergeTx
        ]
    ]

{- Transaction Tests -}

decEncTxid :: TxHash -> Bool
decEncTxid h = decodeTxHashLE (encodeTxHashLE h) == Just h

{- Building Transactions -}

testBuildAddrTx :: ArbitraryAddress -> ArbitrarySatoshi -> Bool
testBuildAddrTx (ArbitraryAddress a) (ArbitrarySatoshi v) = case a of
    x@(PubKeyAddress _) -> Right (PayPKHash x) == out
    x@(ScriptAddress _) -> Right (PayScriptHash x) == out
  where 
    tx  = buildAddrTx [] [(addrToBase58 a,v)]
    out = decodeOutputBS $ scriptOutput $ txOut (fromRight tx) !! 0

testGuessSize :: ArbitraryAddrOnlyTx -> Bool
testGuessSize (ArbitraryAddrOnlyTx tx) =
    -- We compute an upper bound but it should be close enough to the real size
    -- We give 2 bytes of slack on every signature (1 on r and 1 on s)
    guess >= len && guess <= len + 2*delta
  where 
    delta    = pki + (sum $ map fst msi)
    guess    = guessTxSize pki msi pkout msout
    len      = BS.length $ encode' tx
    ins      = map f $ txIn tx
    f i      = fromRight $ decodeInputBS $ scriptInput i
    pki      = length $ filter isSpendPKHash ins
    msi      = concat $ map shData ins
    shData (ScriptHashInput _ (PayMulSig keys r)) = [(r,length keys)]
    shData _ = []
    out      = map (fromRight . decodeOutputBS . scriptOutput) $ txOut tx
    pkout    = length $ filter isPayPKHash out
    msout    = length $ filter isPayScriptHash out

testChooseCoins :: Word64 -> Word64 -> [ArbitrarySatoshi] -> Bool
testChooseCoins target kbfee coins = case chooseCoins target kbfee True coins of
    Right (chosen, change) ->
        let outSum = sum $ map coinValue chosen
            fee    = getFee kbfee (length chosen) 
        in outSum == target + change + fee
    Left _ -> 
        let fee = getFee kbfee (length coins) 
        in target == 0 || s < target || s < target + fee
  where 
    s  = sum $ map coinValue coins

testChooseMSCoins :: Word64 -> Word64 
                  -> ArbitraryMSParam -> [ArbitrarySatoshi] -> Bool
testChooseMSCoins target kbfee (ArbitraryMSParam m n) coins = 
    case chooseMSCoins target kbfee (m,n) True coins of
        Right (chosen,change) ->
            let outSum = sum $ map coinValue chosen
                fee    = getMSFee kbfee (m,n) (length chosen) 
            in outSum == target + change + fee
        Left _ -> 
            let fee = getMSFee kbfee (m,n) (length coins) 
            in target == 0 || s < target + fee
  where 
    s  = sum $ map coinValue coins

{- Signing Transactions -}

testDetSignTx :: ArbitrarySigningData -> Bool
testDetSignTx (ArbitrarySigningData tx sigis prv) = 
    (not $ verifyStdTx tx verData)
        && (not $ verifyStdTx txSigP verData)
        && verifyStdTx txSigC verData
  where
    txSigP  = fromRight $ detSignTx tx sigis (tail prv)
    txSigC  = fromRight $ detSignTx txSigP sigis [head prv]
    verData = map (\(SigInput s o _ _) -> (s,o)) sigis

testMergeTx :: ArbitraryPartialTxs -> Bool
testMergeTx (ArbitraryPartialTxs txs os) = and 
    [ isRight mergeRes
    , length (txIn mergedTx) == length os
    , if enoughSigs then isValid else not isValid
    -- Signature count == min (length txs) (sum required signatures)
    , sum (map snd sigMap) == min (length txs) (sum (map fst sigMap))
    ]
  where
    outs = map (\(so, op, _, _) -> (so, op)) os
    mergeRes = mergeTxs txs outs
    mergedTx = fromRight mergeRes
    isValid = verifyStdTx mergedTx outs
    enoughSigs = and $ map (\(m,c) -> c >= m) sigMap
    sigMap = map (\((_,_,m,_), inp) -> (m, sigCnt inp)) $ zip os $ txIn mergedTx
    sigCnt inp = case decodeInputBS $ scriptInput inp of
        Right (RegularInput (SpendMulSig sigs)) -> length sigs
        Right (ScriptHashInput (SpendMulSig sigs) _) -> length sigs
        _ -> error "Invalid input script type"

