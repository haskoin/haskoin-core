{-# LANGUAGE LambdaCase #-}
module Network.Haskoin.Script.Evaluator
( Program
, Stack
, evalScript
, encodeInt
, decodeInt
, encodeBool
, decodeBool
, runProgram
, runStack
) where

import Debug.Trace (trace, traceM)

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Control.Applicative ((<$>), (<*>))

import qualified Data.ByteString as BS

import Data.Bits (shiftR, shiftL, testBit, setBit, clearBit)
import Data.Int (Int64)
import Data.Word (Word8, Word64)

import Network.Haskoin.Crypto
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
    condStack    :: [Bool],
    hashOps      :: HashOps,
    sigCheck     :: SigCheck
}

instance Show Program where
    show p = "script: " ++ (show $ instructions p) ++
             " stack: " ++ (show $ stack p) ++
             " condStack: " ++ (show $ condStack p)

type ProgramState = ErrorT EvalError Identity

type ProgramTransition a = StateT Program ProgramState a

type SigCheck = PubKey -> [ScriptOp] -> Bool


--------------------------------------------------------------------------------
-- Error utils

programError :: String -> ProgramTransition a
programError s = get >>= throwError . ProgramError s


--------------------------------------------------------------------------------
-- Type Conversions


encodeInt :: Int64 -> StackValue
encodeInt i = prefix $ reverse $ encode (fromIntegral $ abs i) []
    where encode :: Word64 -> StackValue -> StackValue
          encode 0 bytes = bytes
          encode j bytes = (fromIntegral j):(encode (j `shiftR` 8) bytes)
          prefix [] = []
          prefix (x:xs) | testBit x 7 = prefix (0:x:xs)
                        | i < 0       = (setBit x 7):xs
                        | otherwise   = (x:xs)


decodeInt :: StackValue -> Int64
decodeInt [] = 0
decodeInt (x:xs) | testBit x 7 = -(decode 0 ((clearBit x 7):xs))
                 | otherwise   = decode 0 (x:xs)
    where decode :: Int64 -> StackValue -> Int64
          decode s [] = s
          decode s (y:ys) = decode ((s `shiftL` 8) + (fromIntegral y)) ys


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

rejectSignature :: SigCheck
rejectSignature _ _ = False

isDisabled :: ScriptOp -> Bool
isDisabled op = case runProgram [op] rejectSignature of
    Left (DisabledOp _) -> True
    _ -> False

popInt :: ProgramTransition Int64
popInt = popStack >>= \sv ->
    if (length sv) > 4 then
        programError $ "integer > nMaxNumSize: " ++ show (length sv)
    else
        return $ decodeInt sv


-- see CastToBignum
-- https://github.com/piotrnar/gocoin/blob/master/btc/stack.go#L56
-- https://github.com/bitcoin/bitcoin/blob/master/src/bignum.h


opToSv :: StackValue -> BS.ByteString
opToSv = BS.pack

bsToSv :: BS.ByteString -> StackValue
bsToSv = BS.unpack



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

getCond :: ProgramTransition [Bool]
getCond = condStack <$> get

popCond :: ProgramTransition Bool
popCond = get >>= \prog -> case condStack prog of
    [] -> programError "popCond: empty condStack"
    (c:cs) -> put prog { condStack = cs } >> return c

pushCond :: Bool -> ProgramTransition ()
pushCond c = get >>= \prog ->
    put prog { condStack = (c:condStack prog) }

flipCond :: ProgramTransition ()
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




dumpState :: String -> ProgramTransition ()
dumpState message = do
    traceM message
    get >>= \s -> traceM $ show s

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

eval OP_IF      = popStack >>= pushCond . decodeBool
eval OP_NOTIF   = popStack >>= pushCond . not . decodeBool
eval OP_ELSE    = flipCond
eval OP_ENDIF   = void popCond

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
eval OP_OVER    = tStack2 $ \a b -> [a, b, a]
eval OP_PICK    = decodeInt <$> popStack >>= (pickStack False . fromIntegral)
eval OP_ROLL    = decodeInt <$> popStack >>= (pickStack True . fromIntegral)
eval OP_ROT     = tStack3 $ \a b c -> [b, c, a]
eval OP_SWAP    = tStack2 $ \a b -> [b, a]
eval OP_TUCK    = tStack2 $ \a b -> [b, a, b]
eval OP_2DROP   = tStack2 $ \_ _ -> []
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
eval OP_SIZE    = (fromIntegral . length <$> popStack) >>= pushStack . encodeInt

-- Bitwise Logic

eval OP_INVERT  = disabled
eval OP_AND     = disabled
eval OP_OR      = disabled
eval OP_XOR     = disabled
eval OP_EQUAL   = tStack2 $ \a b -> [encodeBool (a == b)]
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
eval OP_NUMNOTEQUAL         = tStack2L decodeInt encodeBool (/=)
eval OP_LESSTHAN            = tStack2L decodeInt encodeBool (<)
eval OP_GREATERTHAN         = tStack2L decodeInt encodeBool (>)
eval OP_LESSTHANOREQUAL     = tStack2L decodeInt encodeBool (<=)
eval OP_GREATERTHANOREQUAL  = tStack2L decodeInt encodeBool (>=)
eval OP_MIN     = tStack2L decodeInt encodeInt min
eval OP_MAX     = tStack2L decodeInt encodeInt max
eval OP_WITHIN  = tStack3L decodeInt encodeBool $
    \a x y -> (x <= a) && (a < y)

eval OP_RIPEMD160 = tStack1 $ return . bsToSv . hash160BS . opToSv
eval OP_SHA1 = undefined  -- TODO: add sha160 to Network.Haskoin.Crypto.Hash
-- eval OP_SHA1 = tStack1 $ return . bsToSv . hashSha160BS . opToSv

eval OP_SHA256 = tStack1 $ return . bsToSv . hash256BS . opToSv
eval OP_HASH160 = tStack1 $ return . bsToSv . hash160BS . hash256BS . opToSv
eval OP_HASH256 = tStack1 $ return . bsToSv . doubleHash256BS  . opToSv
eval OP_CODESEPARATOR = clearHashOps
eval OP_CHECKSIG = undefined
eval OP_CHECKMULTISIG = popStack >>= checkMultiSig . decodeInt
    where  checkMultiSig 0 = return ()
           checkMultiSig x = eval OP_CHECKSIG >> checkMultiSig (x - 1)

eval OP_CHECKSIGVERIFY      = eval OP_CHECKSIG      >> eval OP_VERIFY
eval OP_CHECKMULTISIGVERIFY = eval OP_CHECKMULTISIG >> eval OP_VERIFY

eval op = case constValue op of
    Just sv -> pushStack sv
    Nothing -> programError $ "unknown op " ++ show op

--------------------------------------------------------------------------------

evalAll :: ProgramTransition ()
evalAll = instructions <$> get >>= \case
            [] -> return ()
            (op:_) -> do
                -- dumpState "evalAll"
                condStack <- getCond
                let ex = all id condStack

                if all id condStack
                    then (getOp >>= eval)
                    else (getOp >>= evalFalse)

                void popOp
                evalAll

          where evalFalse OP_IF = pushCond False
                evalFalse OP_ELSE = flipCond
                evalFalse OP_NOTIF = pushCond False
                evalFalse OP_ENDIF = void popCond
                evalFalse _ = return ()

-- exported functions

runProgram :: [ScriptOp] -> SigCheck -> Either EvalError Program
runProgram i sigCheck =
    snd <$> (runIdentity . runErrorT . runStateT evalAll $ Program {
        instructions = i,
        stack = [],
        altStack = [],
        hashOps = [],
        condStack = [],
        sigCheck = sigCheck
    })

evalScript :: Script -> SigCheck -> Bool
evalScript script sigCheck = case runProgram (scriptOps script) sigCheck of
    Left _ -> False
    Right prog -> case stack prog of
        (x:_)  -> decodeBool x
        []     -> False

runStack :: Program -> Stack
runStack = stack
