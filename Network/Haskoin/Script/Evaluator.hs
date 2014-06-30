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

import Data.List (intercalate)
import Data.Bits (shiftR, shiftL, testBit, setBit, clearBit)
import Data.Int (Int64)
import Data.Word (Word8, Word64)
import Data.Either ( rights )

import Network.Haskoin.Crypto
import Network.Haskoin.Script.Types
import Network.Haskoin.Script.SigHash( TxSignature(..), decodeSig, txSigHash )
import Network.Haskoin.Util ( bsToHex, decode' )
import Network.Haskoin.Protocol( Tx(..), TxIn(..) )

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
    show (ProgramError m prog) = m ++ " - program: " ++ (show prog)
    show (StackError op) = (show op) ++ ": Stack Error"
    show (DisabledOp op) = (show op) ++ ": disabled"

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
    sigCheck     :: SigCheck
}

dumpOp :: ScriptOp -> String
dumpOp (OP_PUSHDATA payload optype) =
  "OP_PUSHDATA(" ++ (show optype) ++ ")" ++
  " 0x" ++ (bsToHex payload)
dumpOp op = show op

dumpList :: [String] -> String
dumpList xs = "[" ++ (intercalate "," xs) ++ "]"

dumpScript :: [ScriptOp] -> String
dumpScript script = dumpList $ map dumpOp script

dumpStack :: Stack -> String
dumpStack s = dumpList $ map (bsToHex . BS.pack) s

instance Show Program where
    show p = " stack: " ++ (dumpStack $ stack p)

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
evalConditionalProgram m s p = evalProgramTransition ( evalStateT m s ) p

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
encodeInt i = prefix $ encode (fromIntegral $ abs i) []
    where encode :: Word64 -> StackValue -> StackValue
          encode 0 bytes = bytes
          encode j bytes = (fromIntegral j):(encode (j `shiftR` 8) bytes)
          prefix :: StackValue -> StackValue
          prefix [] = []
          prefix xs | testBit (last xs) 7 = prefix $ xs ++ [0]
                    | i < 0 = (init xs) ++ [setBit (last xs) 7]
                    | otherwise = xs

-- | Inverse of `encodeInt`.
decodeInt :: StackValue -> Int64
decodeInt bytes = sign' (decodeW bytes)
    where decodeW [] = 0
          decodeW [x] = fromIntegral $ clearBit x 7
          decodeW (x:xs) = (fromIntegral x) + (decodeW xs) `shiftL` 8
          sign' i | bytes == [] = 0
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

-- | A trivial `SigCheck` always reporting False
rejectSignature :: SigCheck
rejectSignature _ _ _ = False

popInt :: ProgramTransition Int64
popInt = popStack >>= \sv ->
    if (length sv) > 4 then
        programError $ "integer > nMaxNumSize: " ++ show (length sv)
    else
        return $ decodeInt sv

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
putStack stack = modify $ \p -> p { stack = stack }

prependStack :: Stack -> ProgramTransition ()
prependStack s = getStack >>= \s' -> putStack $ s ++ s'

pushStack :: StackValue -> ProgramTransition ()
pushStack v = getStack >>= \s -> putStack (v:s)

popStack :: ProgramTransition StackValue
popStack = withStack >>= \(s:ss) -> putStack ss >> return s

popStackN :: Integer -> ProgramTransition [StackValue]
popStackN 0 = return []
popStackN n = (:) <$> popStack <*> (popStackN (n - 1))

peekStack :: ProgramTransition StackValue
peekStack = withStack >>= \(s:_) -> return s

pickStack :: Bool -> Int -> ProgramTransition ()
pickStack remove n = do
    stack <- getStack

    when (n < 0) $
        programError "pickStack: n < 0"
    when (n > (length stack) - 1) $
        programError "pickStack: n > size"

    let v = stack !! n
    when remove $ putStack $ (take n stack) ++ (drop (n+1) stack)
    pushStack v

getHashOps :: ProgramTransition HashOps
getHashOps = hashOps <$> get

-- | Function to track the verified OPs signed by OP_CHECK(MULTI) sig.
-- Dependent on the sequence of `OP_CODESEPARATOR`
dropHashOpsSeparatedCode :: ProgramTransition ()
dropHashOpsSeparatedCode = modify $ \p ->
   p { hashOps = tail . ( dropWhile ( /= OP_CODESEPARATOR ) ) $ hashOps p }

-- | Filters out `OP_CODESEPARATOR` from the output script used by
-- OP_CHECK(MULTI)SIG
preparedHashOps :: ProgramTransition HashOps
preparedHashOps = filter ( /= OP_CODESEPARATOR ) <$> getHashOps

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

tStack1L :: (StackValue -> a) -> (b -> StackValue) ->
            (a -> b) -> ProgramTransition ()
tStack1L p q f = tStack1 $ return . q . f . p


tStack2L :: (StackValue -> a) -> (b -> StackValue) ->
            (a -> a -> b) -> ProgramTransition ()
tStack2L p q f = tStack2 $ \a b -> return $ q $ f (p a) (p b)

tStack3L :: (StackValue -> a) -> (b -> StackValue) ->
            (a -> a -> a -> b) -> ProgramTransition ()
tStack3L p q f = tStack3 $ \a b c -> return $ q $ f (p a) (p b) (p c)

arith1 :: (Int64 -> Int64) -> ProgramTransition ()
arith1 f = do
    i <- popInt
    pushStack $ encodeInt (f i)

arith2 :: (Int64 -> Int64 -> Int64) -> ProgramTransition ()
arith2 f = do
    i <- popInt
    j <- popInt
    pushStack $ encodeInt (f i j)

bool2 :: (Bool -> Bool -> Bool) -> ProgramTransition ()
bool2 = tStack2L decodeBool encodeBool

stackError :: ProgramTransition a
stackError = programError "stack error"

-- AltStack Primitives

pushAltStack :: StackValue -> ProgramTransition ()
pushAltStack op = modify $ \p -> p { altStack = op:(altStack p) }

popAltStack :: ProgramTransition StackValue
popAltStack = get >>= \p -> case altStack p of
    a:as -> put p { altStack = as } >> return a
    []   -> programError "popAltStack: empty stack"

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
eval OP_EQUALVERIFY = (eval OP_EQUAL) >> (eval OP_VERIFY)

-- Arithmetic

eval OP_1ADD    = arith1 (+1)
eval OP_1SUB    = arith1 (subtract 1)
eval OP_NEGATE  = arith1 negate
eval OP_ABS     = arith1 abs
eval OP_NOT         = arith1 $ \case 0 -> 1; _ -> 0
eval OP_0NOTEQUAL   = arith1 $ \case 0 -> 0; _ -> 1
eval OP_ADD     = arith2 (+)
eval OP_SUB     = arith2 $ flip (-)
eval OP_BOOLAND     = bool2 (&&)
eval OP_BOOLOR      = bool2 (||)
eval OP_NUMEQUAL    = tStack2L decodeInt encodeBool (==)
eval OP_NUMEQUALVERIFY = eval OP_NUMEQUAL >> eval OP_VERIFY
eval OP_NUMNOTEQUAL         = tStack2L decodeInt encodeBool (/=)
eval OP_LESSTHAN            = tStack2L decodeInt encodeBool (>)
eval OP_GREATERTHAN         = tStack2L decodeInt encodeBool (<)
eval OP_LESSTHANOREQUAL     = tStack2L decodeInt encodeBool (>=)
eval OP_GREATERTHANOREQUAL  = tStack2L decodeInt encodeBool (<=)
eval OP_MIN     = tStack2L decodeInt encodeInt min
eval OP_MAX     = tStack2L decodeInt encodeInt max
eval OP_WITHIN  = tStack3L decodeInt encodeBool $ \y x a -> (x <= a) && (a < y)

eval OP_RIPEMD160 = tStack1 $ return . bsToSv . hash160BS . opToSv
eval OP_SHA1 = tStack1 $ return . bsToSv . hashSha1BS . opToSv

eval OP_SHA256 = tStack1 $ return . bsToSv . hash256BS . opToSv
eval OP_HASH160 = tStack1 $ return . bsToSv . hash160BS . hash256BS . opToSv
eval OP_HASH256 = tStack1 $ return . bsToSv . doubleHash256BS  . opToSv
eval OP_CODESEPARATOR = dropHashOpsSeparatedCode
eval OP_CHECKSIG = (join $ f <$> popStack <*> popStack) >>= pushStack . encodeBool
    where f :: StackValue -> StackValue -> ProgramTransition Bool
          f key sig = do c <- sigCheck <$> get
                         h <- preparedHashOps
                         case decodeSig $ opToSv sig  of
                           Left e -> programError $  "Invalid signature: " ++ e
                           Right s -> return $ c h s ( decode' $ opToSv key )

eval OP_CHECKMULTISIG =
    do pubKeys <- map ( decode' . opToSv ) <$> (popInt >>= popStackN . fromIntegral)
       sigs <- rights . map ( decodeSig . opToSv ) <$> (popInt >>= popStackN . fromIntegral)
       void popStack -- spec bug
       f <- sigCheck <$> get
       h <- preparedHashOps
       pushBool $ checkMultiSigVerify ( f h ) sigs pubKeys
    where checkMultiSigVerify g s keys = let results = liftM2 g s keys
                                             count = length . ( filter id ) $ results
                                           in count >= length s

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
   e  <- getExec
   eval' e scrpOp

   where
     eval' :: Bool -> ScriptOp -> ConditionalProgramTransition ()

     eval' True  OP_IF      = ( lift $ popStack ) >>= pushCond . decodeBool
     eval' True  OP_NOTIF   = ( lift $ popStack ) >>= pushCond . not . decodeBool
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
        sigCheck = sigCheckFcn }

      redeemEval = -- (checkPushOnly sigOps) >>  -- FIXME: disabled
                   (evalAll sigOps) >> (lift $ stack <$> get)
      pubKeyEval = evalAll pubKeyOps >> (lift $ get)

      in do s <- evalConditionalProgram redeemEval [] emptyProgram
            evalConditionalProgram pubKeyEval [] emptyProgram { stack = s }

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
