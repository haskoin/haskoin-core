module Network.Haskoin.Wallet.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad (liftM2)
import Control.Applicative ((<$>))

import Data.Word (Word32, Word64)
import Data.Bits ((.&.))
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS (length)

import QuickCheckUtils
import Network.Haskoin.Wallet.Arbitrary

import Network.Haskoin.Wallet
import Network.Haskoin.Wallet.TxBuilder
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Util
import Network.Haskoin.Util.BuildMonad

tests :: [Test]
tests = 
    [ testGroup "HDW Extended Keys"
        [ testProperty "prvSubKey(k,c)*G = pubSubKey(k*G,c)" subkeyTest
        , testProperty "decode . encode prvKey" binXPrvKey
        , testProperty "decode . encode pubKey" binXPubKey
        , testProperty "fromB58 . toB58 prvKey" b58PrvKey
        , testProperty "fromB58 . toB58 pubKey" b58PubKey
        ]
    , testGroup "HDW Extended Key Manager"
        [ testProperty "decode . encode masterKey" decEncMaster
        , testProperty "decode . encode prvAccKey" decEncPrvAcc
        , testProperty "decode . encode pubAccKey" decEncPubAcc
        ]
    , testGroup "Building Transactions"
        [ testProperty "building address tx" testBuildAddrTx
        , testProperty "testing guessTxSize function" testGuessSize
        , testProperty "testing chooseCoins function" testChooseCoins
        , testProperty "testing chooseMSCoins function" testChooseMSCoins
        ]
    , testGroup "Signing Transactions"
        [ testProperty "Check signed transaction status" testSignTxBuild
        , testProperty "Sign and validate transactions" testSignTxValidate
        ]
    ]

{- HDW Extended Keys -}

subkeyTest :: XPrvKey -> Word32 -> Bool
subkeyTest k i = fromJust $ liftM2 (==) 
    (deriveXPubKey <$> prvSubKey k i') (pubSubKey (deriveXPubKey k) i')
    where i' = fromIntegral $ i .&. 0x7fffffff -- make it a public derivation

binXPrvKey :: XPrvKey -> Bool
binXPrvKey k = (decode' $ encode' k) == k

binXPubKey :: XPubKey -> Bool
binXPubKey k = (decode' $ encode' k) == k

b58PrvKey :: XPrvKey -> Bool
b58PrvKey k = (fromJust $ xPrvImport $ xPrvExport k) == k

b58PubKey :: XPubKey -> Bool
b58PubKey k = (fromJust $ xPubImport $ xPubExport k) == k

{- HDW Extended Key Manager -}

decEncMaster :: MasterKey -> Bool
decEncMaster k = (fromJust $ loadMasterKey $ decode' bs) == k
    where bs = encode' $ masterKey k

decEncPrvAcc :: AccPrvKey -> Bool
decEncPrvAcc k = (fromJust $ loadPrvAcc $ decode' bs) == k
    where bs = encode' $ getAccPrvKey k

decEncPubAcc :: AccPubKey -> Bool
decEncPubAcc k = (fromJust $ loadPubAcc $ decode' bs) == k
    where bs = encode' $ getAccPubKey k

{- Building Transactions -}

testBuildAddrTx :: [OutPoint] -> Address -> Word64 -> Bool
testBuildAddrTx os a v 
    | v <= 2100000000000000 = isRight tx && case a of
        x@(PubKeyAddress _) -> Right (PayPKHash x) == out
        x@(ScriptAddress _) -> Right (PayScriptHash x) == out
    | otherwise = isLeft tx
    where tx  = buildAddrTx os [(addrToBase58 a,v)]
          out = decodeOutput $ scriptOutput $ txOut (fromRight tx) !! 0

testGuessSize :: RegularTx -> Bool
testGuessSize (RegularTx tx) =
    -- We compute an upper bound but it should be close enough to the real size
    -- We give 3 bytes of slack on every signature (1 on r and 2 on s)
    guess >= len && guess - 3*delta <= len
    where delta = pki + (sum $ map fst msi)
          guess = guessTxSize pki msi pkout msout
          len = BS.length $ encode' tx
          rIns = map (decodeInput . scriptInput) $ txIn tx
          mIns = map (decodeScriptHash . scriptInput) $ txIn tx
          pki = length $ filter (isSpendPKHash . fromRight) $ 
                    filter isRight rIns
          msi = concat $ map (shData . fromRight) $ filter isRight mIns
          shData (ScriptHashInput _ (PayMulSig keys r)) = [(r,length keys)]
          shData _ = []
          out  = map (fromRight . decodeOutput . scriptOutput) $ txOut tx
          pkout = length $ filter isPayPKHash out
          msout = length $ filter isPayScriptHash out

testChooseCoins :: Word64 -> Word64 -> [Coin] -> Bool
testChooseCoins target kbfee xs = case chooseCoins target kbfee xs of
    Right (chosen,change) ->
        let outSum = sum $ map (outValue . coinTxOut) chosen
            fee    = getFee kbfee (length chosen) 
        in outSum == target + change + fee
    Left _ -> 
        let fee = getFee kbfee (length xs) 
        in target == 0 || s < target || s < target + fee
    where s = sum $ map (outValue . coinTxOut) xs

testChooseMSCoins :: Word64 -> Word64 -> MSParam -> [Coin] -> Bool
testChooseMSCoins target kbfee (MSParam m n) xs = 
    case chooseMSCoins target kbfee (m,n) xs of
        Right (chosen,change) ->
            let outSum = sum $ map (outValue . coinTxOut) chosen
                fee    = getMSFee kbfee (m,n) (length chosen) 
            in outSum == target + change + fee
        Left _ -> 
            let fee = getMSFee kbfee (m,n) (length xs) 
            in target == 0 || s < target + fee
    where s = sum $ map (outValue . coinTxOut) xs

{- Signing Transactions -}

testSignTxBuild :: PKHashSigTemplate -> Bool
testSignTxBuild (PKHashSigTemplate tx sigi prv)
    | null $ txIn tx = isBroken txSig && isBroken txSigP
    | otherwise = isComplete txSig && isPartial txSigP
    where txSig = detSignTx tx sigi prv
          txSigP = detSignTx tx (tail sigi) (tail prv)
         
testSignTxValidate :: PKHashSigTemplate -> Bool
testSignTxValidate (PKHashSigTemplate tx sigi prv) =
    case detSignTx tx sigi prv of
        (Broken _)    -> True
        (Partial btx)  -> not $ verifyTx btx $ map f sigi
        (Complete btx) -> verifyTx btx $ map f sigi
    where f si = (sigDataOut si, sigDataOP si)

-- todo: test p2sh transactions


