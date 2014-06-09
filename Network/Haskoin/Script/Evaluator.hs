{-# LANGUAGE LambdaCase #-}
module Network.Haskoin.Script.Evaluator
( Program
, Stack
, evalScript
, runProgram
, runStack
) where

import Debug.Trace (trace)

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Control.Applicative ((<$>), (<*>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Word (Word8)

import Data.Binary.Put (runPut)


import Network.Haskoin.Crypto
import Network.Haskoin.Protocol
import Network.Haskoin.Script.Types


-- see https://github.com/bitcoin/bitcoin/blob/master/src/script.cpp EvalScript
-- see https://en.bitcoin.it/wiki/Script

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
type Instructions = [ScriptOp]
type AltStack = [StackValue]
type Stack = [StackValue]
type HashOps = [ScriptOp] -- the code that is verified by OP_CHECKSIG

-- type Program = (Instructions, Stack, AltStack, HashCode)

data Program = Program {
    instructions :: Instructions,
    stack        :: Stack,
    altStack     :: AltStack,
    hashOps      :: HashOps,
    sigCheck     :: SigCheck
}

instance Show Program where
    show p = "script: " ++ (show $ instructions p) ++
             " stack: " ++ (show $ stack p)

type ProgramState = ErrorT EvalError Identity

type ProgramTransition a = StateT Program ProgramState a

type SigCheck = PubKey -> [ScriptOp] -> Bool


--------------------------------------------------------------------------------
-- Error utils

programError :: String -> ProgramTransition a
programError s = get >>= throwError . ProgramError s


--------------------------------------------------------------------------------
-- Type Conversions

isConstant :: ScriptOp -> Bool
isConstant op = case op of
    (OP_PUSHDATA _ _) -> True
    OP_0  -> True
    OP_1  -> True
    OP_2  -> True
    OP_4  -> True
    OP_5  -> True
    OP_7  -> True
    OP_8  -> True
    OP_9  -> True
    OP_10 -> True
    OP_11 -> True
    OP_12 -> True
    OP_13 -> True
    OP_14 -> True
    OP_15 -> True
    OP_16 -> True
    OP_1NEGATE -> True
    _ -> False


rejectSignature :: SigCheck
rejectSignature _ _ = False

isDisabled :: ScriptOp -> Bool
isDisabled op = case runProgram [op] rejectSignature of
    Left (DisabledOp _) -> True
    _ -> False

intToSv :: Int -> StackValue
intToSv i = undefined

svToInt :: StackValue -> Int
svToInt sv = undefined

svSize :: StackValue -> Int
svSize = undefined

-- see CastToBool
-- TODO probably incomplete
svToBool :: StackValue -> Bool
svToBool sv = undefined

boolToSv :: Bool -> StackValue
boolToSv sv = undefined

svTrue :: StackValue
svTrue = undefined

svFalse :: StackValue
svFalse = undefined



-- see CastToBignum
-- https://github.com/piotrnar/gocoin/blob/master/btc/stack.go#L56
-- https://github.com/bitcoin/bitcoin/blob/master/src/bignum.h



opToSv :: StackValue -> BS.ByteString
opToSv op = undefined

bsToSv :: BS.ByteString -> StackValue
bsToSv op = undefined



opSize :: ScriptOp -> Int
opSize op = undefined -- TODO


stackValue :: ScriptOp -> StackValue
stackValue = undefined

--------------------------------------------------------------------------------
-- Script Primitives

getOp :: ProgramTransition ScriptOp
getOp = instructions <$> get >>= \case
    [] -> programError "getOp: empty script"
    (i:_) -> return i

popOp :: ProgramTransition ScriptOp
popOp = get >>= \prog -> case instructions prog of
    [] -> programError "popOp: empty script"
    (i:is) -> put prog { instructions = is } >> return i

-- Stack Primitives

getStack :: ProgramTransition Stack
getStack = stack <$> get

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

peekStack :: ProgramTransition StackValue
peekStack = withStack >>= \(s:ss) -> return s

pickStack :: Bool -> Int -> ProgramTransition ()
pickStack remove n = do
    stack <- getStack

    when (n < 0) $
        programError "pickStack: n < 0"
    when (n > (length stack)) $
        programError "pickStack: n > size"

    let v = stack !! n
    when remove $ putStack $ (take (n-1) stack) ++ (drop n stack)
    pushStack v


pushHashOp :: ScriptOp -> ProgramTransition ()
pushHashOp op = modify $ \p -> p { hashOps = op:(hashOps p) }

getHashOps :: ProgramTransition HashOps
getHashOps = hashOps <$> get

clearHashOps :: ProgramTransition ()
clearHashOps = modify $ \p -> p { hashOps = [] }

-- transformStack :: (Stack -> Stack) -> ProgramTransition ()
-- transformStack f = (getStack >>= putStack . f)


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



arith1 :: (Int -> Int) -> ProgramTransition ()
arith1 = tStack1L svToInt intToSv

arith2 :: (Int -> Int -> Int) -> ProgramTransition ()
arith2 = tStack2L svToInt intToSv

bool2 :: (Bool -> Bool -> Bool) -> ProgramTransition ()
bool2 = tStack2L svToBool boolToSv


stackError :: ProgramTransition a
-- stackError = getOp >>= throwError . StackError
stackError = programError "stack error"

disabled :: ProgramTransition ()
disabled = getOp >>= throwError . DisabledOp

-- AltStack Primitives

pushAltStack :: StackValue -> ProgramTransition ()
pushAltStack op = modify $ \p -> p { altStack = op:(altStack p) }

popAltStack :: ProgramTransition StackValue
popAltStack = get >>= \p -> case altStack p of
    a:as -> put p { altStack = as } >> return a
    []   -> programError "popAltStack: empty stack"



-- Instruction Evaluation

eval :: ScriptOp -> ProgramTransition ()

-- Flow Control

-- TODO check nested conditionals

evalIf :: Bool -> ProgramTransition ()
evalIf cond = case cond of
    True -> popOp >> evalUntil OP_ELSE >> popOp >> skipUntil OP_ENDIF
    False -> popOp >> skipUntil OP_ELSE >> popOp >> evalUntil OP_ENDIF
    where
        doUntil stop evalOps = do
            op <- getOp
            unless (op == stop) $ do
                when evalOps $ (eval op)
                void popOp
                doUntil stop evalOps

        skipUntil stop = doUntil stop False
        evalUntil stop = doUntil stop True


eval OP_NOP     = return ()
eval OP_IF      = popStack >>= evalIf . svToBool
eval OP_NOTIF   = popStack >>= evalIf . not . svToBool
eval OP_ELSE    = programError "OP_ELSE outside OP_IF"
eval OP_ENDIF   = programError "OP_ENDIF outside OP_IF"

eval OP_VERIFY = svToBool <$> popStack >>= \case
    False -> programError "OP_VERIFY failed"
    True  -> return ()

eval OP_RETURN = programError "explicit OP_RETURN"



-- Stack

eval OP_TOALTSTACK = popStack >>= pushAltStack
eval OP_FROMALTSTACK = popAltStack >>= pushStack
eval OP_IFDUP   = tStack1 $ \a -> if svToBool a then [a, a] else []
eval OP_DEPTH   = getStack >>= pushStack . intToSv . length
eval OP_DROP    = void popStack
eval OP_DUP     = tStack1 $ \a -> [a, a]
eval OP_NIP     = tStack2 $ \a b -> [a]
eval OP_OVER    = tStack2 $ \a b -> [a, b, a]
eval OP_PICK    = svToInt <$> popStack >>= (pickStack False)
eval OP_ROLL    = svToInt <$> popStack >>= (pickStack True)
eval OP_ROT     = tStack3 $ \a b c -> [c, b, a]
eval OP_SWAP    = tStack2 $ \a b -> [b, a]
eval OP_TUCK    = tStack2 $ \a b -> [b, a, b]
eval OP_2DROP   = tStack2 $ \a b -> []
eval OP_2DUP    = tStack2 $ \a b -> [a, b, a, b]
eval OP_3DUP    = tStack3 $ \a b c -> [a, b, c, a, b, c]
eval OP_2OVER   = tStack4 $ \a b c d -> [a, b, c, d, a, b]
eval OP_2ROT    = tStack6 $ \a b c d e f -> [c, d, e, f, a, b]
eval OP_2SWAP   = tStack4 $ \a b c d -> [c, d, a, b]

-- Splice

eval OP_CAT     = disabled
eval OP_SUBSTR  = disabled
eval OP_LEFT    = disabled
eval OP_RIGHT   = disabled
eval OP_SIZE    = (svSize <$> popStack) >>= pushStack . intToSv

-- Bitwise Logic

eval OP_INVERT  = disabled
eval OP_AND     = disabled
eval OP_OR      = disabled
eval OP_XOR     = disabled
eval OP_EQUAL   = tStack2 $ \a b -> if a == b then [svTrue] else [svFalse]
eval OP_EQUALVERIFY = (eval OP_EQUAL) >> (eval OP_VERIFY)

-- Arithmetic

eval OP_1ADD    = arith1 (+1)
eval OP_1SUB    = arith1 (subtract 1)
eval OP_2MUL    = disabled
eval OP_2DIV    = disabled
eval OP_NEGATE  = arith1 negate
eval OP_ABS     = arith1 abs
eval OP_NOT         = arith1 $ \case 0 -> 1; _ -> 0
eval OP_0NOTEQUAL   = arith1 $ \case 0 -> 0; _ -> 1
eval OP_ADD     = arith2 (+)
eval OP_SUB     = arith2 (-)
eval OP_MUL     = disabled
eval OP_DIV     = disabled
eval OP_MOD     = disabled
eval OP_LSHIFT  = disabled
eval OP_RSHIFT  = disabled
eval OP_BOOLAND     = bool2 (&&)
eval OP_BOOLOR      = bool2 (||)
eval OP_NUMEQUAL    = bool2 (==)
eval OP_NUMEQUALVERIFY = eval OP_NUMEQUAL >> eval OP_VERIFY
eval OP_NUMNOTEQUAL         = tStack2L svToInt boolToSv (/=)
eval OP_LESSTHAN            = tStack2L svToInt boolToSv (<)
eval OP_GREATERTHAN         = tStack2L svToInt boolToSv (>)
eval OP_LESSTHANOREQUAL     = tStack2L svToInt boolToSv (<=)
eval OP_GREATERTHANOREQUAL  = tStack2L svToInt boolToSv (>=)
eval OP_MIN     = tStack2L svToInt intToSv min
eval OP_MAX     = tStack2L svToInt intToSv max
eval OP_WITHIN  = tStack3L svToInt boolToSv $
    \a x y -> (x <= a) && (a < y)

eval OP_RIPEMD160 = tStack1 $ return . bsToSv . hash160BS . opToSv
eval OP_SHA1 = undefined  -- TODO: add sha160 to Network.Haskoin.Crypto.Hash
-- eval OP_SHA1 = tStack1 $ return . bsToSv . hashSha160BS . opToSv

eval OP_SHA256 = tStack1 $ return . bsToSv . hash256BS . opToSv
eval OP_HASH160 = tStack1 $ return . bsToSv . hash160BS . hash256BS . opToSv
eval OP_HASH256 = tStack1 $ return . bsToSv . doubleHash256BS  . opToSv
eval OP_CODESEPARATOR = clearHashOps
eval OP_CHECKSIG = undefined
eval OP_CHECKMULTISIG = popStack >>= checkMultiSig . svToInt
    where  checkMultiSig 0 = return ()
           checkMultiSig x = eval OP_CHECKSIG >> checkMultiSig (x - 1)

eval OP_CHECKSIGVERIFY      = eval OP_CHECKSIG      >> eval OP_VERIFY
eval OP_CHECKMULTISIGVERIFY = eval OP_CHECKMULTISIG >> eval OP_VERIFY

eval op | isConstant op = pushStack $ stackValue op
        | otherwise     = programError $ "unknown op " ++ show op

--------------------------------------------------------------------------------

evalAll :: ProgramTransition ()
evalAll = do
    instructions <$> get >>= \case
        [] -> return ()
        (op:ops) -> do
            eval op
            popOp >>= pushHashOp
            evalAll

-- exported functions

runProgram :: [ScriptOp] -> SigCheck -> Either EvalError ((), Program)
runProgram i sigCheck =
    runIdentity . runErrorT . runStateT evalAll $ Program {
        instructions = i,
        stack = [],
        altStack = [],
        hashOps = [],
        sigCheck = sigCheck
    }

evalScript :: Script -> SigCheck -> Bool
evalScript script sigCheck = case runProgram (scriptOps script) sigCheck of
    Left _ -> False
    Right ((), prog) -> case stack prog of
        (x:_)  -> svToBool x
        []     -> False

runStack :: Program -> Stack
runStack = stack

dumpStack :: [ScriptOp] -> IO ()
dumpStack instructions =
    case runProgram instructions rejectSignature of
        Left e -> putStrLn $ "error " ++ show e
        Right ((), prog) -> putStrLn $ show $ stack prog
