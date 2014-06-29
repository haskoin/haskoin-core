{-|
Module providing Bitcoin script execution.
-}
module Network.Haskoin.Script.Exec
(
  -- * Execution
  execScript
  -- * Integral stack conversion
, toStackVal
, fromStackVal
, StackVal
) where
import Network.Haskoin.Script ( Script, ScriptOp(..), txSigHash, decodeSig, TxSignature(..) )
import Network.Haskoin.Crypto ( hash160BS, hash256BS, verifySig, PubKey )
import Network.Haskoin.Protocol ( Tx(..), TxIn(..) )
import Network.Haskoin.Util ( decode', encode', bsToInteger, eitherToMaybe )
import Network.Haskoin.Script.StatementTree( Statement(..), parseScript )

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe ( fromMaybe )
import Data.List ( genericSplitAt )
import Data.Bits ( testBit, clearBit, setBit )
import Data.Word ( Word8 )

import qualified Data.ByteString as BS

-- | Convenience type for values on the stack
type StackVal = BS.ByteString
-- | Convenience type for the stack
type Stack = [ StackVal ]

-- | Conversion function of integers to stack byte-strings.  Important
-- due to the meaning of the most significant bit.  See
-- <https://en.bitcoin.it/wiki/Script>
toStackVal :: Integer 
           -> StackVal
toStackVal i = 
  let absEnc = BS.reverse . encode' $ abs i
      msb = BS.last absEnc in
  if i >= 0 -- Need to ensure ms-bit is 0
    then if testBit msb 7
           then absEnc `BS.snoc` zeroByte
           else absEnc
    else -- i < 0
      if testBit msb 7
        then absEnc `BS.snoc` negByte
        else BS.init absEnc `BS.snoc` setBit msb 7
  where zeroByte =  0 :: Word8
        negByte  =  0x80 :: Word8

-- | Inverse of toStackVal
fromStackVal :: StackVal
             -> Integer
fromStackVal v = 
  let msb = BS.last v in
  if testBit msb 7
    then negate $ bsToInteger $ BS.reverse $ BS.init v `BS.snoc` clearBit msb 7
    else bsToInteger . BS.reverse $ v

-- | Data for the reader monad carrying the extra information required
-- to verify signatures
data ExecInfo = ExecInfo { transaction :: Tx
                          , script      :: Script
                          , inputNum   :: Int }

-- | Script execution monad.
type OpExec = StateT Stack ( ReaderT ExecInfo Maybe )

-- | Runs the script monad.  Nothing if there has been a failure.  Also
-- returns the remaining stack.
runOpExec :: Stack -> ExecInfo -> OpExec a -> Maybe ( a, Stack )
runOpExec s e o = runReaderT ( runStateT o s ) e

-- | Lifts simply written functions into the monad.
stackLift :: ( Stack -> Stack ) -> OpExec ()
stackLift f = do
  s <- get
  put $ f s

-- | Lifts simply written functions with the possibility of failure
-- into the monad.
maybeStackLift :: ( Stack -> Maybe Stack ) -> OpExec ()
maybeStackLift f = do
  s <- get
  case f s of
    Nothing -> lift . lift $ Nothing
    Just ns -> put ns

opPush :: StackVal -> Stack -> Stack
opPush v s = v:s

opPushM :: StackVal -> OpExec ()
opPushM v = stackLift $ opPush v

   
opNop :: Stack -> Stack
opNop = id

opNopM :: OpExec ()
opNopM = stackLift opNop

opVerify :: Stack -> Maybe Stack
opVerify (t:rs)  = if fromStackVal t == 0 then Nothing else Just rs
opVerify [] = Nothing

opVerifyM :: OpExec ()
opVerifyM = maybeStackLift opVerify
  

opDup :: Stack -> Maybe Stack
opDup s@(t:_) = Just $ t:s
opDup [] = Nothing

opDupM :: OpExec ()
opDupM = maybeStackLift opDup

opEqual :: Stack -> Maybe Stack
opEqual (x1:x2:xs) = Just $ if x1 == x2 then toStackVal 1: xs else toStackVal 0 : xs
opEqual (_:[]) = Nothing 
opEqual [] = Nothing

opEqualM :: OpExec ()
opEqualM = maybeStackLift opEqual

opEqualVerifyM :: OpExec ()
opEqualVerifyM = opEqualM >> opVerifyM

opHash160 :: Stack -> Maybe Stack
opHash160 (x:xs) = Just $ ( hash160BS . hash256BS $ x ) : xs
opHash160 [] = Nothing

opHash160M :: OpExec ()
opHash160M = maybeStackLift opHash160

-- | A wrapper around 'verifySig' which handles grabbing the hash type
verifySigWithType :: Tx -> Script -> Int -> TxSignature -> PubKey -> Bool
verifySigWithType tx out i txSig pubKey = 
  let h = txSigHash tx out i ( sigHashType txSig ) in
  verifySig h ( txSignature txSig ) pubKey

opCheckSig :: Tx -> Script -> Int -> Stack -> Maybe Stack
opCheckSig _ _ _ [] = Nothing
opCheckSig _ _ _ (_:[]) = Nothing
opCheckSig tx out i (p:s:ss) =
   let pubKey = decode' p :: PubKey
       txSig = decodeSig s
   in
       case txSig of
         Left _ -> Nothing
         Right t -> Just $ if verifySigWithType tx out i t pubKey
                             then toStackVal 1 : ss
                             else toStackVal 0 : ss

opCheckSigM :: OpExec ()
opCheckSigM = do
   execInfo <- ask
   let t = transaction execInfo
       s = script execInfo
       i = inputNum execInfo
   maybeStackLift $ opCheckSig t s i

opCheckMultiSig :: Tx -> Script -> Int -> Stack -> Maybe Stack
opCheckMultiSig _ _ _ [] = Nothing
opCheckMultiSig _ _ _ (_:[]) = Nothing
opCheckMultiSig tx out i (nKeys:ss) = do
  let ( pubKeysBS, rest ) = genericSplitAt ( fromStackVal nKeys ) ss
  nSig <- if null rest then Nothing else Just . fromStackVal . head $ rest
  let ( sigsBS, leftOverPlus ) = genericSplitAt nSig $ tail rest
  -- Check that we have that extra thing on the stack (due to old bug).
  leftOver <- if null leftOverPlus then Nothing else Just $ tail leftOverPlus

  let pubKeys = map ( Just . decode' ) pubKeysBS
      sigs =  map ( eitherToMaybe . decodeSig ) sigsBS
      results = (liftM2 . liftM2) ( verifySigWithType tx out i ) sigs pubKeys
      nVerified = length $ filter ( fromMaybe False ) results
  return $ if fromIntegral nVerified >= nSig
             then toStackVal 1 : leftOver
             else toStackVal 0 : leftOver

opCheckMultiSigM :: OpExec ()
opCheckMultiSigM = do
   execInfo <- ask
   let t = transaction execInfo
       s = script execInfo
       i = inputNum execInfo
   maybeStackLift $ opCheckMultiSig t s i
      
opReturn :: Stack -> Maybe Stack
opReturn _ = Nothing

opReturnM :: OpExec ()
opReturnM = maybeStackLift opReturn

opFcn :: ScriptOp -> OpExec ()
opFcn op = case op of
  OP_PUSHDATA d _ -> opPushM d
  OP_0 -> opPushM ( toStackVal 0 )
  OP_1NEGATE -> opPushM ( toStackVal (-1) )
  OP_1  -> opPushM ( toStackVal 1 )
  OP_2  -> opPushM ( toStackVal 2 )
  OP_3  -> opPushM ( toStackVal 3 )
  OP_4  -> opPushM ( toStackVal 4 )
  OP_5  -> opPushM ( toStackVal 5 )
  OP_6  -> opPushM ( toStackVal 6 )
  OP_7  -> opPushM ( toStackVal 7 )
  OP_8  -> opPushM ( toStackVal 8 )
  OP_9  -> opPushM ( toStackVal 9 )
  OP_10 -> opPushM ( toStackVal 10 )
  OP_11 -> opPushM ( toStackVal 11 )
  OP_12 -> opPushM ( toStackVal 12 )
  OP_13 -> opPushM ( toStackVal 13 )
  OP_14 -> opPushM ( toStackVal 14 )
  OP_15 -> opPushM ( toStackVal 15 )
  OP_16 -> opPushM ( toStackVal 16 )
  OP_VERIFY -> opVerifyM
  OP_DUP -> opDupM
  OP_EQUAL -> opEqualM
  OP_EQUALVERIFY -> opEqualVerifyM
  OP_HASH160 -> opHash160M
  OP_CHECKSIG -> opCheckSigM
  OP_CHECKMULTISIG -> opCheckMultiSigM
  OP_NOP -> opNopM
  _ -> opReturnM

-- | if statement monad
ifM :: OpExec () -- ^ Action when True
    -> OpExec () -- ^ Action when False
    -> OpExec ()
ifM i e = do
  s <- get
  case s of
    [] -> e
    _  -> do put $ tail s
             let z = head s
             if 0 == fromStackVal z then e else i

-- | Returns the condition based on the top value of the stack
checkResult :: OpExec Bool 
checkResult = do
  s <- get
  return $ case s of
    x:_ -> fromStackVal x /= 0
    _   -> False

-- | Converts a single statement to a 'OpExec' monad
statementFcn :: Statement ScriptOp -> OpExec ()
statementFcn s = case s of
   SingletonStatement x -> opFcn x
   IfStatement x1 x2 -> ifM ( scriptM x1 ) ( scriptM x2 )

-- | Converts a full script to a 'OpExec' monad
scriptM :: [ Statement ScriptOp ] -> OpExec ()
scriptM s = forM_ s statementFcn

-- | Executes a transaction script
execScript :: Tx      -- ^ The spending transaction with the input script
           -> Script  -- ^ The output script we are trying to satisfy
           -> Int     -- ^ The index in the spending transaction of the input
           -> Bool
execScript tx s i = let sigScript = parseScript .
                                    decode' .
                                    scriptInput $ txIn tx !! i 
                        fullScript = sigScript ++ parseScript s
                        scriptAction = scriptM fullScript
                        e = ExecInfo { transaction = tx
                                     , script = s
                                     , inputNum = i }
                     in
   case runOpExec [] e ( scriptAction >> checkResult ) of
        Just ( x, _ ) -> x
        _ -> False
