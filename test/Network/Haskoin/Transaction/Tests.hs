module Network.Haskoin.Transaction.Tests (spec) where

import qualified Data.ByteString             as BS
import           Data.Either                 (fromRight, isRight)
import           Data.Maybe
import           Data.Serialize              (encode)
import           Data.String                 (fromString)
import           Data.String.Conversions     (cs)
import           Data.Word                   (Word64)
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Script
import           Network.Haskoin.Test
import           Network.Haskoin.Transaction
import           Test.Hspec
import           Test.QuickCheck

spec :: Network -> Spec
spec net =
    describe "transaction" $ do
        it "decode and encode txid" $
            property $
            forAll arbitraryTxHash $ \h -> hexToTxHash (txHashToHex h) == Just h
        it "from string transaction id" $
            property $
            forAll arbitraryTxHash $ \h -> fromString (cs $ txHashToHex h) == h
        it "building address tx" $
            property $
            forAll (arbitraryAddress net) $
            forAll (arbitrarySatoshi net) . testBuildAddrTx net
        it "guess transaction size" $
            property $ forAll (arbitraryAddrOnlyTxFull net) (testGuessSize net)
        it "choose coins" $
            property $ forAll (listOf (arbitrarySatoshi net)) testChooseCoins
        it "choose multisig coins" $
            property $
            forAll arbitraryMSParam $
            forAll (listOf (arbitrarySatoshi net)) . testChooseMSCoins
        it "sign and validate transaction" $
            property $ forAll (arbitrarySigningData net) (testDetSignTx net)
        it "merge partially signed transactions" $
            property $ forAll (arbitraryPartialTxs net) (testMergeTx net)

{- Building Transactions -}

testBuildAddrTx :: Network -> Address -> TestCoin -> Bool
testBuildAddrTx net a (TestCoin v) =
    case a of
        PubKeyAddress h net -> Right (PayPKHash h) == out
        ScriptAddress h net -> Right (PayScriptHash h) == out
  where
    tx =
        buildAddrTx
            net
            []
            [ ( fromMaybe
                    (error "Could not convert address to string")
                    (addrToString a)
              , v)
            ]
    out =
        decodeOutputBS $
        scriptOutput $
        head $ txOut (fromRight (error "Could not build transaction") tx)

testGuessSize :: Network -> Tx -> Bool
testGuessSize net tx
    -- We compute an upper bound but it should be close enough to the real size
    -- We give 2 bytes of slack on every signature (1 on r and 1 on s)
 = guess >= len && guess <= len + 2 * delta
  where
    delta = pki + sum (map fst msi)
    guess = guessTxSize pki msi pkout msout
    len = BS.length $ encode tx
    ins = map f $ txIn tx
    f i =
        fromRight (error "Could not decode input") $
        decodeInputBS net $ scriptInput i
    pki = length $ filter isSpendPKHash ins
    msi = concatMap shData ins
    shData (ScriptHashInput _ (PayMulSig keys r)) = [(r, length keys)]
    shData _                                      = []
    out =
        map
            (fromRight (error "Could not decode transaction output") .
             decodeOutputBS . scriptOutput) $
        txOut tx
    pkout = length $ filter isPayPKHash out
    msout = length $ filter isPayScriptHash out

testChooseCoins :: [TestCoin] -> Word64 -> Word64 -> Int -> Property
testChooseCoins coins target byteFee nOut = nOut >= 0 ==>
    case chooseCoins target byteFee nOut True coins of
        Right (chosen, change) ->
            let outSum = sum $ map coinValue chosen
                fee    = guessTxFee byteFee nOut (length chosen)
            in outSum == target + change + fee
        Left _ ->
            let fee = guessTxFee byteFee nOut (length coins)
            in target == 0 || s < target + fee
  where
    s  = sum $ map coinValue coins

testChooseMSCoins :: (Int, Int) -> [TestCoin]
                  -> Word64 -> Word64 -> Int -> Property
testChooseMSCoins (m, n) coins target byteFee nOut = nOut >= 0 ==>
    case chooseMSCoins target byteFee (m,n) nOut True coins of
        Right (chosen,change) ->
            let outSum = sum $ map coinValue chosen
                fee    = guessMSTxFee byteFee (m,n) nOut (length chosen)
            in outSum == target + change + fee
        Left _ ->
            let fee = guessMSTxFee byteFee (m,n) nOut (length coins)
            in target == 0 || s < target + fee
  where
    s = sum $ map coinValue coins

{- Signing Transactions -}

testDetSignTx :: Network -> (Tx, [SigInput], [PrvKey]) -> Bool
testDetSignTx net  (tx, sigis, prv) =
    not (verifyStdTx net tx verData) &&
    not (verifyStdTx net txSigP verData) && verifyStdTx net txSigC verData
  where
    txSigP =
        fromRight (error "Could not decode transaction") $
        signTx net tx sigis (tail prv)
    txSigC =
        fromRight (error "Could not decode transaction") $
        signTx net txSigP sigis [head prv]
    verData = map (\(SigInput s v o _ _) -> (s, v, o)) sigis

testMergeTx :: Network -> ([Tx], [(ScriptOutput, Word64, OutPoint, Int, Int)]) -> Bool
testMergeTx net (txs, os) = and
    [ isRight mergeRes
    , length (txIn mergedTx) == length os
    , if enoughSigs then isValid else not isValid
    -- Signature count == min (length txs) (sum required signatures)
    , sum (map snd sigMap) == min (length txs) (sum (map fst sigMap))
    ]
  where
    outs = map (\(so, val, op, _, _) -> (so, val, op)) os
    mergeRes = mergeTxs net txs outs
    mergedTx = fromRight (error "Could not merge") mergeRes
    isValid = verifyStdTx net mergedTx outs
    enoughSigs = all (\(m,c) -> c >= m) sigMap
    sigMap = map (\((_,_,_,m,_), inp) -> (m, sigCnt inp)) $ zip os $ txIn mergedTx
    sigCnt inp = case decodeInputBS net $ scriptInput inp of
        Right (RegularInput (SpendMulSig sigs)) -> length sigs
        Right (ScriptHashInput (SpendMulSig sigs) _) -> length sigs
        _ -> error "Invalid input script type"

