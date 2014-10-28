module Network.Haskoin.Transaction.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Word (Word64)
import qualified Data.ByteString as BS (length)

import Network.Haskoin.Test.Transaction
import Network.Haskoin.Test.Script
import Network.Haskoin.Test.Crypto

import Network.Haskoin.Transaction.Builder
import Network.Haskoin.Transaction.Types
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

testChooseCoins :: Word64 -> Word64 -> [ArbitraryCoin] -> Bool
testChooseCoins target kbfee acoins = case chooseCoins target kbfee xs of
    Right (chosen,change) ->
        let outSum = sum $ map coinValue chosen
            fee    = getFee kbfee (length chosen) 
        in outSum == target + change + fee
    Left _ -> 
        let fee = getFee kbfee (length xs) 
        in target == 0 || s < target || s < target + fee
  where 
    xs = map (\(ArbitraryCoin x) -> x) acoins
    s  = sum $ map coinValue xs

testChooseMSCoins :: Word64 -> Word64 
                  -> ArbitraryMSParam -> [ArbitraryCoin] -> Bool
testChooseMSCoins target kbfee (ArbitraryMSParam m n) acoins = 
    case chooseMSCoins target kbfee (m,n) xs of
        Right (chosen,change) ->
            let outSum = sum $ map coinValue chosen
                fee    = getMSFee kbfee (m,n) (length chosen) 
            in outSum == target + change + fee
        Left _ -> 
            let fee = getMSFee kbfee (m,n) (length xs) 
            in target == 0 || s < target + fee
  where 
    xs = map (\(ArbitraryCoin x) -> x) acoins
    s  = sum $ map coinValue xs

{- Signing Transactions -}

testDetSignTx :: ArbitrarySigningData -> Bool
testDetSignTx (ArbitrarySigningData tx sigis prv) = 
    (not $ verifyStdTx tx verData)
        && (not statP) && (not $ verifyStdTx txSigP verData)
        && statC && verifyStdTx txSigC verData
  where
    (txSigP, statP) = fromRight $ detSignTx tx sigis (tail prv)
    (txSigC, statC) = fromRight $ detSignTx txSigP sigis [head prv]
    verData         = map (\(SigInput s o _ _) -> (s,o)) sigis


