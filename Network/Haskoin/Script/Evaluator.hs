{-# LANGUAGE LambdaCase #-}
{-|

Module providing Bitcoin script evaluation.  See
<https://github.com/bitcoin/bitcoin/blob/master/src/script.cpp>
EvalScript and <https://en.bitcoin.it/wiki/Script>

-}
module Network.Haskoin.Script.Evaluator
( 
-- * Script evaluation
  verifySpend
, evalScript
, SigCheck
-- * Evaluation data types
, Program
, Stack
-- * Helper functions
, encodeInt
, decodeInt
, encodeBool
, decodeBool
, runStack
, checkStack
, dumpScript
, dumpStack
, execScript
) where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Control.Applicative ((<$>), (<*>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.List (intercalate)
import Data.Bits (shiftR, shiftL, testBit, setBit, clearBit)
import Data.Int (Int64)
import Data.Word (Word8, Word64)
import Data.Either ( rights )
import Data.Maybe ( mapMaybe, isJust )

import Network.Haskoin.Crypto
import Network.Haskoin.Script.Types
import Network.Haskoin.Script.SigHash( TxSignature(..), decodeSig, txSigHash )
import Network.Haskoin.Util ( bsToHex, decode', decodeToMaybe )
import Network.Haskoin.Protocol( Tx(..), TxIn(..) )

import Data.Binary (encode, decode)


maxScriptSize :: Int
maxScriptSize = 10000

maxScriptElementSize :: Int
maxScriptElementSize = 520

maxStackSize :: Int
maxStackSize = 1000

maxOpcodes :: Int
maxOpcodes = 200

data EvalError =
    EvalError String
    | ProgramError String Program
    | StackError ScriptOp
    | DisabledOp ScriptOp

instance Error EvalError where
    noMsg = EvalError "Evaluation Error"
    strMsg s = EvalError $ noMsg ++ " " ++ s

instance Show EvalError where
    show (EvalError m) = m
    show (ProgramError m prog) = m ++ " - program: " ++ show prog
    show (StackError op) = show op ++ ": Stack Error"
    show (DisabledOp op) = show op ++ ": disabled"

type StackValue = [Word8]
type AltStack = [StackValue]
type Stack = [StackValue]
type HashOps = [ScriptOp] -- the code that is verified by OP_CHECKSIG

-- | Defines the type of function required by script evaluating
-- functions to check transaction signatures.
type SigCheck = [ScriptOp] -> TxSignature -> PubKey -> Bool

-- | Data type of the evaluation state.
data Program = Program {
    stack        :: Stack,
    altStack     :: AltStack,
    hashOps      :: HashOps,
    sigCheck     :: SigCheck,
    opCount      :: Int
}

dumpOp :: ScriptOp -> String
dumpOp (OP_PUSHDATA payload optype) =
  "OP_PUSHDATA(" ++ show optype ++ ")" ++
  " 0x" ++ bsToHex payload
dumpOp op = show op

dumpList :: [String] -> String
dumpList xs = "[" ++ intercalate "," xs ++ "]"

dumpScript :: [ScriptOp] -> String
dumpScript script = dumpList $ map dumpOp script

dumpStack :: Stack -> String
dumpStack s = dumpList $ map (bsToHex . BS.pack) s

instance Show Program where
    show p = " stack: " ++ dumpStack (stack p)

type ProgramState = ErrorT EvalError Identity
type IfStack = [ Bool ]

-- | Monad of actions independent of conditional statements.
type ProgramTransition = StateT Program ProgramState
-- | Monad of actions which taking if statements into account.
-- Separate state type from ProgramTransition for type safety
type ConditionalProgramTransition a = StateT IfStack ProgramTransition a

evalProgramTransition :: ProgramTransition a -> Program -> Either EvalError a
evalProgramTransition m s = runIdentity . runErrorT $ evalStateT m s

evalConditionalProgram :: ConditionalProgramTransition a -- ^ Program monad
                       -> [ Bool ]                       -- ^ Initial if state stack
                       -> Program                        -- ^ Initial computation data
                       -> Either EvalError a
evalConditionalProgram m s = evalProgramTransition ( evalStateT m s )

--------------------------------------------------------------------------------
-- Error utils

programError :: String -> ProgramTransition a
programError s = get >>= throwError . ProgramError s

disabled :: ScriptOp -> ProgramTransition ()
disabled op = throwError . DisabledOp $ op

--------------------------------------------------------------------------------
-- Type Conversions

-- | Encoding function for the stack value format of integers.  Most
-- significant bit defines sign.
encodeInt :: Int64 -> StackValue
encodeInt i = prefix $ encode' (fromIntegral $ abs i) []
    where encode' :: Word64 -> StackValue -> StackValue
          encode' 0 bytes = bytes
          encode' j bytes = fromIntegral j:encode' (j `shiftR` 8) bytes
          prefix :: StackValue -> StackValue
          prefix [] = []
          prefix xs | testBit (last xs) 7 = prefix $ xs ++ [0]
                    | i < 0 = init xs ++ [setBit (last xs) 7]
                    | otherwise = xs

-- | Inverse of `encodeInt`.
decodeInt :: StackValue -> Int64
decodeInt bytes = sign' (decodeW bytes)
    where decodeW [] = 0
          decodeW [x] = fromIntegral $ clearBit x 7
          decodeW (x:xs) = fromIntegral x + decodeW xs `shiftL` 8
          sign' i | null bytes = 0
                  | testBit (last bytes) 7 = -i
                  | otherwise = i

-- | Conversion of StackValue to Bool (true if non-zero).
decodeBool :: StackValue -> Bool
decodeBool v = decodeInt v /= 0

encodeBool :: Bool -> StackValue
encodeBool True = [1]
encodeBool False = []

constValue :: ScriptOp -> Maybe StackValue
constValue op = case op of
    OP_0  -> Just $ encodeInt 0
    OP_1  -> Just $ encodeInt 1
    OP_2  -> Just $ encodeInt 2
    OP_3  -> Just $ encodeInt 3
    OP_4  -> Just $ encodeInt 4
    OP_5  -> Just $ encodeInt 5
    OP_6  -> Just $ encodeInt 6
    OP_7  -> Just $ encodeInt 7
    OP_8  -> Just $ encodeInt 8
    OP_9  -> Just $ encodeInt 9
    OP_10 -> Just $ encodeInt 10
    OP_11 -> Just $ encodeInt 11
    OP_12 -> Just $ encodeInt 12
    OP_13 -> Just $ encodeInt 13
    OP_14 -> Just $ encodeInt 14
    OP_15 -> Just $ encodeInt 15
    OP_16 -> Just $ encodeInt 16
    OP_1NEGATE -> Just $ encodeInt $ -1
    (OP_PUSHDATA string _) -> Just $ BS.unpack string
    _ -> Nothing


-- | Check if OpCode is constant
isConstant :: ScriptOp -> Bool
isConstant = isJust . constValue

-- | Check if OpCode is disabled
isDisabled :: ScriptOp -> Bool
isDisabled op = op `elem` [ OP_CAT
                          , OP_SUBSTR
                          , OP_LEFT
                          , OP_RIGHT
                          , OP_INVERT
                          , OP_AND
                          , OP_OR
                          , OP_XOR
                          , OP_2MUL
                          , OP_2DIV
                          , OP_MUL
                          , OP_DIV
                          , OP_MOD
                          , OP_LSHIFT
                          , OP_RSHIFT
                          , OP_VER
                          , OP_VERIF
                          , OP_VERNOTIF ]

-- | Check if OpCode counts towards opcount limit

countOp :: ScriptOp -> Bool
countOp op | isConstant op     = False
           | op == OP_RESERVED = False
           | otherwise         = True

popInt :: ProgramTransition Int64
popInt = popStack >>= \sv ->
    if length sv > 4 then
        programError $ "integer > nMaxNumSize: " ++ show (length sv)
    else
        return $ decodeInt sv

pushInt :: Int64 -> ProgramTransition ()
pushInt = pushStack . encodeInt

popBool :: ProgramTransition Bool
popBool = decodeBool <$> popStack

pushBool :: Bool -> ProgramTransition ()
pushBool = pushStack . encodeBool

opToSv :: StackValue -> BS.ByteString
opToSv = BS.pack

bsToSv :: BS.ByteString -> StackValue
bsToSv = BS.unpack

--------------------------------------------------------------------------------
-- Stack Primitives

getStack :: ProgramTransition Stack
getStack = stack <$> get

getCond :: ConditionalProgramTransition [Bool]
getCond = get

popCond :: ConditionalProgramTransition Bool
popCond = get >>= \condStack -> case condStack of
    [] -> lift $ programError "popCond: empty condStack"
    (c:cs) -> put cs >> return c

pushCond :: Bool -> ConditionalProgramTransition ()
pushCond c = get >>= \s ->
    put (c:s)

flipCond :: ConditionalProgramTransition ()
flipCond = popCond >>= pushCond . not

withStack :: ProgramTransition Stack
withStack = getStack >>= \case
    [] -> stackError
    s  -> return s

putStack :: Stack -> ProgramTransition ()
putStack st = modify $ \p -> p { stack = st }

prependStack :: Stack -> ProgramTransition ()
prependStack s = getStack >>= \s' -> putStack $ s ++ s'

checkPushData :: ScriptOp -> ProgramTransition ()
checkPushData (OP_PUSHDATA v _) | BS.length v > fromIntegral maxScriptElementSize
                                  = programError "OP_PUSHDATA > maxScriptElementSize"
                                | otherwise = return ()
checkPushData _ = return ()

checkStackSize :: ProgramTransition ()
checkStackSize = do n <- length <$> stack <$> get
                    m <- length <$> altStack <$> get
                    when ((n + m) > fromIntegral maxStackSize) $
                         programError "stack > maxStackSize"

pushStack :: StackValue -> ProgramTransition ()
pushStack v = getStack >>= \s -> putStack (v:s)

popStack :: ProgramTransition StackValue
popStack = withStack >>= \(s:ss) -> putStack ss >> return s

popStackN :: Integer -> ProgramTransition [StackValue]
popStackN 0 = return []
popStackN n = (:) <$> popStack <*> popStackN (n - 1)

pickStack :: Bool -> Int -> ProgramTransition ()
pickStack remove n = do
    st <- getStack

    when (n < 0) $
        programError "pickStack: n < 0"
    when (n > length st - 1) $
        programError "pickStack: n > size"

    let v = st !! n
    when remove $ putStack $ take n st ++ drop (n+1) st
    pushStack v

getHashOps :: ProgramTransition HashOps
getHashOps = hashOps <$> get

-- | Function to track the verified OPs signed by OP_CHECK(MULTI) sig.
-- Dependent on the sequence of `OP_CODESEPARATOR`
dropHashOpsSeparatedCode :: ProgramTransition ()
dropHashOpsSeparatedCode = modify $ \p ->
   let tryDrop = dropWhile ( /= OP_CODESEPARATOR ) $ hashOps p in
   case tryDrop of
     -- If no OP_CODESEPARATOR, take the whole script.  This case is
     -- possible when there is no OP_CODESEPARATOR in scriptPubKey but
     -- one exists in scriptSig
     [] -> p
     _  -> p { hashOps = tail tryDrop }

-- | Filters out `OP_CODESEPARATOR` from the output script used by
-- OP_CHECK(MULTI)SIG
preparedHashOps :: ProgramTransition HashOps
preparedHashOps = filter ( /= OP_CODESEPARATOR ) <$> getHashOps

-- | Removes any PUSHDATA that contains the signatures.  Used in
-- CHECK(MULTI)SIG so that signatures can be contained in output
-- scripts.  See FindAndDelete() in Bitcoin Core.
findAndDelete :: [ StackValue ] -> [ ScriptOp ] -> [ ScriptOp ]
findAndDelete [] ops = ops
findAndDelete (s:ss) ops = let pushOp = opPushData . opToSv $ s in
  findAndDelete ss $ filter ( /= pushOp ) ops

checkMultiSig :: SigCheck -- ^ Signature checking function
              -> [ StackValue ] -- ^ PubKeys
              -> [ StackValue ] -- ^ Signatures
              -> [ ScriptOp ]   -- ^ CODESEPARATOR'd hashops
              -> Bool
checkMultiSig f encPubKeys encSigs hOps =
  let pubKeys = mapMaybe ( decodeToMaybe . opToSv ) encPubKeys
      sigs = rights $ map ( decodeSig . opToSv ) encSigs
      cleanHashOps = findAndDelete encSigs hOps
  in (length sigs == length encSigs) && -- check for bad signatures
     orderedSatisfy (f cleanHashOps) sigs pubKeys

-- | Tests whether a function is satisfied for every a with some b "in
-- order".  By "in order" we mean, if a pair satisfies the function,
-- any other satisfying pair must be deeper in each list.  Designed to
-- return as soon as the result is known to minimize expensive
-- function calls.  Used in checkMultiSig to verify signature/pubKey
-- pairs with a values as signatures and b values as pubkeys
orderedSatisfy :: ( a -> b -> Bool )
                    -> [ a ]
                    -> [ b ]
                    -> Bool
orderedSatisfy _ [] _ = True
orderedSatisfy _ (_:_) [] = False
orderedSatisfy f x@(a:as) y@(b:bs) | length x > length y = False
                                   | f a b     = orderedSatisfy f as bs
                                   | otherwise = orderedSatisfy f x bs

tStack1 :: (StackValue -> Stack) -> ProgramTransition ()
tStack1 f = f <$> popStack >>= prependStack

tStack2 :: (StackValue -> StackValue -> Stack) -> ProgramTransition ()
tStack2 f = f <$> popStack <*> popStack >>= prependStack

tStack3 :: (StackValue -> StackValue -> StackValue -> Stack) -> ProgramTransition ()
tStack3 f = f <$> popStack <*> popStack <*> popStack >>= prependStack

tStack4 :: (StackValue -> StackValue -> StackValue -> StackValue -> Stack)
            -> ProgramTransition ()
tStack4 f = f <$> popStack <*> popStack <*> popStack <*> popStack
              >>= prependStack

tStack6 :: (StackValue -> StackValue -> StackValue ->
            StackValue -> StackValue -> StackValue -> Stack) -> ProgramTransition ()
tStack6 f = f <$> popStack <*> popStack <*> popStack
              <*> popStack <*> popStack <*> popStack >>= prependStack

arith1 :: (Int64 -> Int64) -> ProgramTransition ()
arith1 f = do
    i <- popInt
    pushStack $ encodeInt (f i)

arith2 :: (Int64 -> Int64 -> Int64) -> ProgramTransition ()
arith2 f = do
    i <- popInt
    j <- popInt
    pushStack $ encodeInt (f i j)

stackError :: ProgramTransition a
stackError = programError "stack error"

-- AltStack Primitives

pushAltStack :: StackValue -> ProgramTransition ()
pushAltStack op = modify $ \p -> p { altStack = op:altStack p }

popAltStack :: ProgramTransition StackValue
popAltStack = get >>= \p -> case altStack p of
    a:as -> put p { altStack = as } >> return a
    []   -> programError "popAltStack: empty stack"


incrementOpCount :: Int -> ProgramTransition ()
incrementOpCount i | i > maxOpcodes = programError "reached opcode limit"
                   | otherwise      = modify $ \p -> p { opCount = i + 1 }

-- Instruction Evaluation
eval :: ScriptOp -> ProgramTransition ()
eval OP_NOP     = return ()
eval OP_NOP1    = return ()
eval OP_NOP2    = return ()
eval OP_NOP3    = return ()
eval OP_NOP4    = return ()
eval OP_NOP5    = return ()
eval OP_NOP6    = return ()
eval OP_NOP7    = return ()
eval OP_NOP8    = return ()
eval OP_NOP9    = return ()
eval OP_NOP10   = return ()

eval OP_VERIFY = decodeBool <$> popStack >>= \case
    False -> programError "OP_VERIFY failed"
    True  -> return ()

eval OP_RETURN = programError "explicit OP_RETURN"

-- Stack

eval OP_TOALTSTACK = popStack >>= pushAltStack
eval OP_FROMALTSTACK = popAltStack >>= pushStack
eval OP_IFDUP   = tStack1 $ \a -> if decodeBool a then [a, a] else [a]
eval OP_DEPTH   = getStack >>= pushStack . encodeInt . fromIntegral . length
eval OP_DROP    = void popStack
eval OP_DUP     = tStack1 $ \a -> [a, a]
eval OP_NIP     = tStack2 $ \a _ -> [a]
eval OP_OVER    = tStack2 $ \a b -> [b, a, b]
eval OP_PICK    = popInt >>= (pickStack False . fromIntegral)
eval OP_ROLL    = popInt >>= (pickStack True . fromIntegral)
eval OP_ROT     = tStack3 $ \a b c -> [c, a, b]
eval OP_SWAP    = tStack2 $ \a b -> [b, a]
eval OP_TUCK    = tStack2 $ \a b -> [a, b, a]
eval OP_2DROP   = tStack2 $ \_ _ -> []
eval OP_2DUP    = tStack2 $ \a b -> [a, b, a, b]
eval OP_3DUP    = tStack3 $ \a b c -> [a, b, c, a, b, c]
eval OP_2OVER   = tStack4 $ \a b c d -> [c, d, a, b, c, d]
eval OP_2ROT    = tStack6 $ \a b c d e f -> [e, f, a, b, c, d]
eval OP_2SWAP   = tStack4 $ \a b c d -> [c, d, a, b]

-- Splice

eval OP_SIZE   = (fromIntegral . length <$> popStack) >>= pushStack . encodeInt

-- Bitwise Logic

eval OP_EQUAL   = tStack2 $ \a b -> [encodeBool (a == b)]
eval OP_EQUALVERIFY = eval OP_EQUAL >> eval OP_VERIFY

-- Arithmetic

eval OP_1ADD    = arith1 (+1)
eval OP_1SUB    = arith1 (subtract 1)
eval OP_NEGATE  = arith1 negate
eval OP_ABS     = arith1 abs
eval OP_NOT         = arith1 $ \case 0 -> 1; _ -> 0
eval OP_0NOTEQUAL   = arith1 $ \case 0 -> 0; _ -> 1
eval OP_ADD     = arith2 (+)
eval OP_SUB     = arith2 $ flip (-)
eval OP_BOOLAND     = (&&) <$> popBool <*> popBool >>= pushBool
eval OP_BOOLOR      = (||) <$> popBool <*> popBool >>= pushBool
eval OP_NUMEQUAL    = (==) <$> popInt <*> popInt >>= pushBool
eval OP_NUMEQUALVERIFY = eval OP_NUMEQUAL >> eval OP_VERIFY
eval OP_NUMNOTEQUAL         = (/=) <$> popInt <*> popInt >>= pushBool
eval OP_LESSTHAN            = (>)  <$> popInt <*> popInt >>= pushBool
eval OP_GREATERTHAN         = (<)  <$> popInt <*> popInt >>= pushBool
eval OP_LESSTHANOREQUAL     = (>=) <$> popInt <*> popInt >>= pushBool
eval OP_GREATERTHANOREQUAL  = (<=) <$> popInt <*> popInt >>= pushBool
eval OP_MIN     = min <$> popInt <*> popInt >>= pushInt
eval OP_MAX     = max <$> popInt <*> popInt >>= pushInt
eval OP_WITHIN  = within <$> popInt <*> popInt <*> popInt >>= pushBool
                  where within y x a = (x <= a) && (a < y)

eval OP_RIPEMD160 = tStack1 $ return . bsToSv . hash160BS . opToSv
eval OP_SHA1 = tStack1 $ return . bsToSv . hashSha1BS . opToSv

eval OP_SHA256 = tStack1 $ return . bsToSv . hash256BS . opToSv
eval OP_HASH160 = tStack1 $ return . bsToSv . hash160BS . hash256BS . opToSv
eval OP_HASH256 = tStack1 $ return . bsToSv . doubleHash256BS  . opToSv
eval OP_CODESEPARATOR = dropHashOpsSeparatedCode
eval OP_CHECKSIG = do
  pubKey <- popStack
  sig <- popStack
  checker <- sigCheck <$> get
  hOps <- preparedHashOps
  pushBool $ checkMultiSig checker [ pubKey ] [ sig ] hOps -- Reuse checkMultiSig code

eval OP_CHECKMULTISIG =
    do pubKeys <- popInt >>= popStackN . fromIntegral
       sigs <- popInt >>= popStackN . fromIntegral
       void popStack -- spec bug
       checker <- sigCheck <$> get
       hOps <- preparedHashOps
       pushBool $ checkMultiSig checker pubKeys sigs hOps
       modify $ \p -> p { opCount = opCount p + length pubKeys }

eval OP_CHECKSIGVERIFY      = eval OP_CHECKSIG      >> eval OP_VERIFY
eval OP_CHECKMULTISIGVERIFY = eval OP_CHECKMULTISIG >> eval OP_VERIFY

eval op = case constValue op of
    Just sv -> pushStack sv
    Nothing -> programError $ "unexpected op " ++ show op

--------------------------------------------------------------------------------
-- | Based on the IfStack, returns whether the script is within an
-- evaluating if-branch.
getExec :: ConditionalProgramTransition Bool
getExec = and <$> getCond

-- | Converts a `ScriptOp` to a program monad.
conditionalEval :: ScriptOp -> ConditionalProgramTransition ()
conditionalEval scrpOp = do
   -- lift $ checkOpEnabled scrpOp
   lift $ checkPushData scrpOp

   e  <- getExec
   eval' e scrpOp

   when (countOp scrpOp) $ lift $ join $ incrementOpCount <$> opCount <$> get

   lift checkStackSize

   where
     eval' :: Bool -> ScriptOp -> ConditionalProgramTransition ()

     eval' True  OP_IF      = lift popStack >>= pushCond . decodeBool
     eval' True  OP_NOTIF   = lift popStack >>= pushCond . not . decodeBool
     eval' True  OP_ELSE    = flipCond
     eval' True  OP_ENDIF   = void popCond
     eval' True  op = lift $ eval op

     eval' False OP_IF    = pushCond False
     eval' False OP_NOTIF = pushCond False
     eval' False OP_ELSE  = flipCond
     eval' False OP_ENDIF = void popCond
     eval' False OP_CODESEPARATOR = lift $ eval OP_CODESEPARATOR
     eval' False OP_VER = return ()
     eval' False op | isDisabled op = lift $ disabled op
                    | otherwise = return ()

-- | Builds a Script evaluation monad.
evalAll :: [ ScriptOp ] -> ConditionalProgramTransition ()
evalAll ops = do mapM_ conditionalEval ops
                 cond <- getCond
                 unless (null cond) (lift $ programError "ifStack not empty")


checkPushOnly :: [ ScriptOp ] -> ConditionalProgramTransition ()
checkPushOnly ops
      | not (all checkPushOp ops) = lift $ programError "only push ops allowed"
      | otherwise = return ()
      where checkPushOp op = case constValue op of
                                  Just _ -> True
                                  Nothing -> False

checkStack :: Stack -> Bool
checkStack (x:_) = decodeBool x
checkStack []  = False


isPayToScriptHash :: [ ScriptOp ] -> Bool
isPayToScriptHash [OP_HASH160, OP_PUSHDATA bytes OPCODE, OP_EQUAL]
                    = BS.length bytes == 20
isPayToScriptHash _ = False

stackToScriptOps :: StackValue -> [ ScriptOp ]
stackToScriptOps sv = scriptOps $ decode $ BSL.pack sv

--
-- exported functions

execScript :: Script -- ^ scriptSig ( redeemScript )
           -> Script -- ^ scriptPubKey
           -> SigCheck -- ^ signature verification Function
           -> Either EvalError Program
execScript scriptSig scriptPubKey sigCheckFcn =
  let sigOps = scriptOps scriptSig
      pubKeyOps = scriptOps scriptPubKey
      emptyProgram = Program {
        stack = [],
        altStack = [],
        hashOps = pubKeyOps,
        sigCheck = sigCheckFcn,
        opCount = 0
      }


      checkSig | isPayToScriptHash pubKeyOps = checkPushOnly sigOps
               | otherwise = return ()

      checkKey | BSL.length (encode scriptPubKey) > fromIntegral maxScriptSize
                 = lift $ programError "pubKey > maxScriptSize"
               | otherwise = return ()


      redeemEval = checkSig >> evalAll sigOps >> lift (stack <$> get)
      pubKeyEval = checkKey >> evalAll pubKeyOps >> lift get
      p2shEval [] = lift $ programError "PayToScriptHash: no script on stack"
      p2shEval (sv:_) = evalAll (stackToScriptOps sv) >> lift get

      in do s <- evalConditionalProgram redeemEval [] emptyProgram
            p <- evalConditionalProgram pubKeyEval [] emptyProgram { stack = s }
            if ( checkStack . runStack $ p ) &&  ( isPayToScriptHash pubKeyOps ) && ( not . null $ s )
                then evalConditionalProgram (p2shEval s) [] emptyProgram { stack = drop 1 s,
                                                                           hashOps = stackToScriptOps $ head s }
                else return p

evalScript :: Script -> Script -> SigCheck -> Bool
evalScript scriptSig scriptPubKey sigCheckFcn =
              case execScript scriptSig scriptPubKey sigCheckFcn of
                  Left _ -> False
                  Right p -> checkStack . runStack $ p

runStack :: Program -> Stack
runStack = stack

-- | A wrapper around 'verifySig' which handles grabbing the hash type
verifySigWithType :: Tx -> Int -> [ ScriptOp ] -> TxSignature -> PubKey -> Bool
verifySigWithType tx i outOps txSig pubKey = 
  let outScript = Script outOps
      h = txSigHash tx outScript i ( sigHashType txSig ) in
  verifySig h ( txSignature txSig ) pubKey

-- | Uses `evalScript` to check that the input script of a spending
-- transaction satisfies the output script.
verifySpend :: Tx     -- ^ The spending transaction
            -> Int    -- ^ The input index
            -> Script -- ^ The output script we are spending
            -> Bool
verifySpend tx i outscript =
  let scriptSig = decode' . scriptInput $ txIn tx !! i
      verifyFcn = verifySigWithType tx i
  in
  evalScript scriptSig outscript verifyFcn
