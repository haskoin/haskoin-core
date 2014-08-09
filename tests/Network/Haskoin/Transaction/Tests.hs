module Network.Haskoin.Transaction.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Word (Word64)
import qualified Data.ByteString as BS (length)

import Network.Haskoin.Transaction.Arbitrary
import Network.Haskoin.Transaction.Builder
import Network.Haskoin.Protocol
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Building Transactions"
        [ testProperty "building address tx" testBuildAddrTx
        , testProperty "testing guessTxSize function" testGuessSize
        , testProperty "testing chooseCoins function" testChooseCoins
        , testProperty "testing chooseMSCoins function" testChooseMSCoins
        ]
    -- TODO: Turn these tests into unit tests as they are very heavy 
    , testGroup "Signing Transactions"
        [ testProperty "Sign and validate PKHash transactions" testSignTx
        , testProperty "Sign and validate Multisig transactions" testSignMS
        ]
    ]

{- Building Transactions -}

testBuildAddrTx :: [OutPoint] -> Address -> Word64 -> Bool
testBuildAddrTx os a v 
    | v <= 2100000000000000 = isRight tx && case a of
        x@(PubKeyAddress _) -> Right (PayPKHash x) == out
        x@(ScriptAddress _) -> Right (PayScriptHash x) == out
    | otherwise = isLeft tx
    where tx  = buildAddrTx os [(addrToBase58 a,v)]
          out = decodeOutputBS $ scriptOutput $ txOut (fromRight tx) !! 0

testGuessSize :: SpendAddrTx -> Bool
testGuessSize (SpendAddrTx tx) =
    -- We compute an upper bound but it should be close enough to the real size
    -- We give 2 bytes of slack on every signature (1 on r and 1 on s)
    guess >= len && guess - 2*delta <= len
    where delta   = pki + (sum $ map fst msi)
          guess   = guessTxSize pki msi pkout msout
          len     = BS.length $ encode' tx
          ins     = map f $ txIn tx
          f i     = fromRight $ decodeInputBS $ scriptInput i
          pki     = length $ filter isSpendPKHash ins
          msi     = concat $ map shData ins
          shData (ScriptHashInput _ (PayMulSig keys r)) = [(r,length keys)]
          shData _ = []
          out      = map (fromRight . decodeOutputBS . scriptOutput) $ txOut tx
          pkout    = length $ filter isPayPKHash out
          msout    = length $ filter isPayScriptHash out

testChooseCoins :: Word64 -> Word64 -> [Coin] -> Bool
testChooseCoins target kbfee xs = case chooseCoins target kbfee xs of
    Right (chosen,change) ->
        let outSum = sum $ map coinValue chosen
            fee    = getFee kbfee (length chosen) 
        in outSum == target + change + fee
    Left _ -> 
        let fee = getFee kbfee (length xs) 
        in target == 0 || s < target || s < target + fee
    where s = sum $ map coinValue xs

testChooseMSCoins :: Word64 -> Word64 -> MSParam -> [Coin] -> Bool
testChooseMSCoins target kbfee (MSParam m n) xs = 
    case chooseMSCoins target kbfee (m,n) xs of
        Right (chosen,change) ->
            let outSum = sum $ map coinValue chosen
                fee    = getMSFee kbfee (m,n) (length chosen) 
            in outSum == target + change + fee
        Left _ -> 
            let fee = getMSFee kbfee (m,n) (length xs) 
            in target == 0 || s < target + fee
    where s = sum $ map coinValue xs

{- Signing Transactions -}

testSignTx :: PKHashSigTemplate -> Bool
testSignTx (PKHashSigTemplate tx sigi prv)
    | null $ txIn tx = (isLeft res) && (isLeft resP)
    | otherwise = (not $ verifyStdTx tx verData)
                      && isRight res && stat
                      && verifyStdTx txSig verData
                      && isRight resP && (not statP)
                      && (not $ verifyStdTx txSigP verData)
                      && isRight resC && statC
                      && verifyStdTx txSigC verData
    where res             = detSignTx tx sigi prv
          (txSig, stat)   = fromRight res
          resP            = detSignTx tx sigi (tail prv)
          (txSigP, statP) = fromRight resP
          resC            = detSignTx txSigP sigi [head prv]
          (txSigC, statC) = fromRight resC
          verData = map (\(SigInput s o _ _) -> (s,o)) sigi
         
testSignMS :: MulSigTemplate -> Bool
testSignMS (MulSigTemplate tx sigi prv)
    | null $ txIn tx = (isLeft res) && (isLeft resP)
    | otherwise = (not $ verifyStdTx tx verData)
                      && isRight res && stat
                      && verifyStdTx txSig verData
                      && isRight resP && (not statP)
                      && (not $ verifyStdTx txSigP verData)
                      && isRight resC && statC
                      && verifyStdTx txSigC verData
    where res             = detSignTx tx sigi prv
          (txSig, stat)   = fromRight res
          resP            = detSignTx tx sigi (tail prv)
          (txSigP, statP) = fromRight resP
          resC            = detSignTx txSigP sigi [head prv]
          (txSigC, statC) = fromRight resC
          verData = map (\(SigInput s o _ _) -> (s,o)) sigi

