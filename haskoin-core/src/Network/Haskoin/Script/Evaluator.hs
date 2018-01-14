{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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
, Flag
-- * Evaluation data types
, ProgramData
, Stack
-- * Helper functions
, encodeInt
, decodeInt
, decodeFullInt
, cltvEncodeInt
, cltvDecodeInt
, encodeBool
, decodeBool
, runStack
, checkStack
, dumpScript
, dumpStack
, execScript
) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bits                         (clearBit, setBit, shiftL,
                                                    shiftR, testBit, (.&.))
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import           Data.Either                       (rights)
import           Data.Int                          (Int64)
import           Data.Maybe                        (isJust, mapMaybe)
import           Data.Serialize                    (decode, encode)
import           Data.String.Conversions           (cs)
import           Data.Word                         (Word32, Word64, Word8)
import           Network.Haskoin.Crypto
import           Network.Haskoin.Script.SigHash
import           Network.Haskoin.Script.Types
import           Network.Haskoin.Transaction.Types
import           Network.Haskoin.Util

maxScriptSize :: Int
maxScriptSize = 10000

maxScriptElementSize :: Int
maxScriptElementSize = 520

maxStackSize :: Int
maxStackSize = 1000

maxOpcodes :: Int
maxOpcodes = 200

maxKeysMultisig :: Int
maxKeysMultisig = 20

data Flag = P2SH
          | STRICTENC
          | DERSIG
          | LOW_S
          | NULLDUMMY
          | SIGPUSHONLY
          | MINIMALDATA
          | DISCOURAGE_UPGRADABLE_NOPS
     deriving ( Show, Read, Eq )

type FlagSet = [ Flag ]

data EvalError =
    EvalError String
    | ProgramError String ProgramData
    | StackError ScriptOp
    | DisabledOp ScriptOp

instance Show EvalError where
    show (EvalError m)         = m
    show (ProgramError m prog) = m ++ " - ProgramData: " ++ show prog
    show (StackError op)       = show op ++ ": Stack Error"
    show (DisabledOp op)       = show op ++ ": disabled"

type StackValue = [Word8]
type AltStack = [StackValue]
type Stack = [StackValue]
type HashOps = [ScriptOp] -- the code that is verified by OP_CHECKSIG

-- | Defines the type of function required by script evaluating
-- functions to check transaction signatures.
type SigCheck = [ScriptOp] -> TxSignature -> PubKey -> Bool

-- | Data type of the evaluation state.
data ProgramData = ProgramData {
    stack    :: Stack,
    altStack :: AltStack,
    hashOps  :: HashOps,
    sigCheck :: SigCheck,
    opCount  :: Int
}

dumpOp :: ScriptOp -> ByteString
dumpOp (OP_PUSHDATA payload optype) = mconcat
  [ "OP_PUSHDATA(", cs (show optype), ")", " 0x", encodeHex payload ]
dumpOp op = cs $ show op

dumpList :: [ByteString] -> ByteString
dumpList xs = mconcat [ "[", BS.intercalate "," xs, "]" ]

dumpScript :: [ScriptOp] -> ByteString
dumpScript script = dumpList $ map dumpOp script

dumpStack :: Stack -> ByteString
dumpStack s = dumpList $ map (encodeHex . BS.pack) s

-- TODO: Test
instance Show ProgramData where
    show p = "stack: " ++ cs (dumpStack $ stack p)

type ProgramState = ExceptT EvalError Identity
type IfStack = [Bool]

-- | Monad of actions independent of conditional statements.
type StackOperation = ReaderT FlagSet ( StateT ProgramData ProgramState )
-- | Monad of actions which taking if statements into account.
-- Separate state type from StackOperation for type safety
type Program a = StateT IfStack StackOperation a

evalStackOperation :: StackOperation a -> ProgramData -> FlagSet -> Either EvalError a
evalStackOperation m s f = runIdentity . runExceptT $ evalStateT ( runReaderT m f ) s

evalProgram :: Program a                      -- ^ ProgramData monad
            -> [ Bool ]                       -- ^ Initial if state stack
            -> ProgramData                    -- ^ Initial computation data
            -> FlagSet                        -- ^ Evaluation Flags
            -> Either EvalError a
evalProgram m s = evalStackOperation ( evalStateT m s )

--------------------------------------------------------------------------------
-- Error utils

programError :: String -> StackOperation a
programError s = get >>= throwError . ProgramError s

disabled :: ScriptOp -> StackOperation ()
disabled = throwError . DisabledOp

--------------------------------------------------------------------------------
-- Type Conversions

-- | Encoding function for the stack value format of integers.  Most
-- significant bit defines sign.
-- Note that this function will encode any Int64 into a StackValue,
-- thus producing stack-encoded integers which are not valid numeric
-- opcodes, as they exceed 4 bytes in length.
encodeInt :: Int64 -> StackValue
encodeInt i = prefix $ encod (fromIntegral $ abs i) []
    where encod :: Word64 -> StackValue -> StackValue
          encod 0 bytes = bytes
          encod j bytes = fromIntegral j:encod (j `shiftR` 8) bytes
          prefix :: StackValue -> StackValue
          prefix [] = []
          prefix xs | testBit (last xs) 7 = prefix $ xs ++ [0]
                    | i < 0 = init xs ++ [setBit (last xs) 7]
                    | otherwise = xs

-- | Decode an Int64 from the stack value integer format.
-- Inverse of `encodeInt`.
-- Note that only integers decoded by 'decodeInt' are valid
-- numeric opcodes (numeric opcodes can only be up to 4 bytes in size).
-- However, in the case of eg. CHECKLOCKTIMEVERIFY, we need to
-- be able to encode and decode stack integers up to
-- (maxBound :: Word32), which are 5 bytes.
decodeFullInt :: StackValue -> Maybe Int64
decodeFullInt bytes
    | length bytes > 8 = Nothing
    | otherwise = Just $ sign' (decodeW bytes)
        where decodeW []     = 0
              decodeW [x]    = fromIntegral $ clearBit x 7
              decodeW (x:xs) = fromIntegral x + decodeW xs `shiftL` 8
              sign' i | null bytes = 0
                    | testBit (last bytes) 7 = -i
                    | otherwise = i

-- | Used for decoding numeric opcodes. Will not return
-- an integer that takes up more than
-- 4 bytes on the stack (the size limit for numeric opcodes).
-- The naming is kept for backwards compatibility.
decodeInt :: StackValue -> Maybe Int64
decodeInt bytes | length bytes > 4 = Nothing
                | otherwise = decodeFullInt bytes

-- | Decode the integer argument to OP_CHECKLOCKTIMEVERIFY (CLTV)
-- from a stack value.
-- The full uint32 range is needed in order to represent timestamps
-- for use with CLTV. Reference:
-- https://github.com/bitcoin/bips/blob/master/bip-0065.mediawiki#Detailed_Specification
cltvDecodeInt :: StackValue -> Maybe Word32
cltvDecodeInt bytes
    | length bytes > 5 = Nothing
    | otherwise = decodeFullInt bytes >>= uint32Bounds
  where
    uint32Bounds :: Int64 -> Maybe Word32
    uint32Bounds i64
        | i64 < 0 || i64 > fromIntegral (maxBound :: Word32) = Nothing
        | otherwise = Just $ fromIntegral i64

-- | Helper function for encoding the argument to OP_CHECKLOCKTIMEVERIFY
cltvEncodeInt :: Word32 -> StackValue
cltvEncodeInt = encodeInt . fromIntegral

-- | Conversion of StackValue to Bool (true if non-zero).
decodeBool :: StackValue -> Bool
decodeBool []        = False
decodeBool [0x00]    = False
decodeBool [0x80]    = False
decodeBool (0x00:vs) = decodeBool vs
decodeBool _         = True

encodeBool :: Bool -> StackValue
encodeBool True  = [1]
encodeBool False = []

constValue :: ScriptOp -> Maybe StackValue
constValue op = case op of
    OP_0                   -> Just $ encodeInt 0
    OP_1                   -> Just $ encodeInt 1
    OP_2                   -> Just $ encodeInt 2
    OP_3                   -> Just $ encodeInt 3
    OP_4                   -> Just $ encodeInt 4
    OP_5                   -> Just $ encodeInt 5
    OP_6                   -> Just $ encodeInt 6
    OP_7                   -> Just $ encodeInt 7
    OP_8                   -> Just $ encodeInt 8
    OP_9                   -> Just $ encodeInt 9
    OP_10                  -> Just $ encodeInt 10
    OP_11                  -> Just $ encodeInt 11
    OP_12                  -> Just $ encodeInt 12
    OP_13                  -> Just $ encodeInt 13
    OP_14                  -> Just $ encodeInt 14
    OP_15                  -> Just $ encodeInt 15
    OP_16                  -> Just $ encodeInt 16
    OP_1NEGATE             -> Just $ encodeInt $ -1
    (OP_PUSHDATA string _) -> Just $ BS.unpack string
    _                      -> Nothing


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

popInt :: StackOperation Int64
popInt = minimalStackValEnforcer >> decodeInt <$> popStack >>= \case
    Nothing -> programError "popInt: data > nMaxNumSize"
    Just i -> return i

pushInt :: Int64 -> StackOperation ()
pushInt = pushStack . encodeInt

popBool :: StackOperation Bool
popBool = decodeBool <$> popStack

pushBool :: Bool -> StackOperation ()
pushBool = pushStack . encodeBool

opToSv :: StackValue -> ByteString
opToSv = BS.pack

bsToSv :: ByteString -> StackValue
bsToSv = BS.unpack

--------------------------------------------------------------------------------
-- Stack Primitives

getStack :: StackOperation Stack
getStack = stack <$> get

getCond :: Program [Bool]
getCond = get

popCond :: Program Bool
popCond = get >>= \case
    []     -> lift $ programError "popCond: empty condStack"
    (x:xs) -> put xs >> return x

pushCond :: Bool -> Program ()
pushCond c = get >>= \s ->
    put (c:s)

flipCond :: Program ()
flipCond = popCond >>= pushCond . not

withStack :: StackOperation Stack
withStack = getStack >>= \case
    [] -> stackError
    s  -> return s

putStack :: Stack -> StackOperation ()
putStack st = modify $ \p -> p { stack = st }

prependStack :: Stack -> StackOperation ()
prependStack s = getStack >>= \s' -> putStack $ s ++ s'

checkPushData :: ScriptOp -> StackOperation ()
checkPushData (OP_PUSHDATA v _) | BS.length v > fromIntegral maxScriptElementSize
                                  = programError "OP_PUSHDATA > maxScriptElementSize"
                                | otherwise = return ()
checkPushData _ = return ()

checkStackSize :: StackOperation ()
checkStackSize = do n <- length . stack <$> get
                    m <- length . altStack <$> get
                    when ((n + m) > fromIntegral maxStackSize) $
                         programError "stack > maxStackSize"

pushStack :: StackValue -> StackOperation ()
pushStack v = getStack >>= \s -> putStack (v:s)

popStack :: StackOperation StackValue
popStack = withStack >>= \(s:ss) -> putStack ss >> return s

popStackN :: Integer -> StackOperation [StackValue]
popStackN n | n < 0     = programError "popStackN: negative argument"
            | n == 0    = return []
            | otherwise = (:) <$> popStack <*> popStackN (n - 1)

pickStack :: Bool -> Int -> StackOperation ()
pickStack remove n = do
    st <- getStack

    when (n < 0) $
        programError "pickStack: n < 0"
    when (n > length st - 1) $
        programError "pickStack: n > size"

    let v = st !! n
    when remove $ putStack $ take n st ++ drop (n+1) st
    pushStack v

getHashOps :: StackOperation HashOps
getHashOps = hashOps <$> get

-- | Function to track the verified OPs signed by OP_CHECK(MULTI) sig.
-- Dependent on the sequence of `OP_CODESEPARATOR`
dropHashOpsSeparatedCode :: StackOperation ()
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
preparedHashOps :: StackOperation HashOps
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
  let pubKeys = mapMaybe (eitherToMaybe . decode . opToSv) encPubKeys
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

tStack1 :: (StackValue -> Stack) -> StackOperation ()
tStack1 f = f <$> popStack >>= prependStack

tStack2 :: (StackValue -> StackValue -> Stack) -> StackOperation ()
tStack2 f = f <$> popStack <*> popStack >>= prependStack

tStack3 :: (StackValue -> StackValue -> StackValue -> Stack) -> StackOperation ()
tStack3 f = f <$> popStack <*> popStack <*> popStack >>= prependStack

tStack4 :: (StackValue -> StackValue -> StackValue -> StackValue -> Stack)
            -> StackOperation ()
tStack4 f = f <$> popStack <*> popStack <*> popStack <*> popStack
              >>= prependStack

tStack6 :: (StackValue -> StackValue -> StackValue ->
            StackValue -> StackValue -> StackValue -> Stack) -> StackOperation ()
tStack6 f = f <$> popStack <*> popStack <*> popStack
              <*> popStack <*> popStack <*> popStack >>= prependStack

arith1 :: (Int64 -> Int64) -> StackOperation ()
arith1 f = do
    i <- popInt
    pushStack $ encodeInt (f i)

arith2 :: (Int64 -> Int64 -> Int64) -> StackOperation ()
arith2 f = do
    i <- popInt
    j <- popInt
    pushStack $ encodeInt (f i j)

stackError :: StackOperation a
stackError = programError "stack error"

-- AltStack Primitives

pushAltStack :: StackValue -> StackOperation ()
pushAltStack op = modify $ \p -> p { altStack = op:altStack p }

popAltStack :: StackOperation StackValue
popAltStack = get >>= \p -> case altStack p of
    a:as -> put p { altStack = as } >> return a
    []   -> programError "popAltStack: empty stack"


incrementOpCount :: Int -> StackOperation ()
incrementOpCount i | i > maxOpcodes = programError "reached opcode limit"
                   | otherwise      = modify $ \p -> p { opCount = i + 1 }

nopDiscourager :: StackOperation ()
nopDiscourager = do
    flgs <- ask
    when (DISCOURAGE_UPGRADABLE_NOPS `elem` flgs) $
        programError "Discouraged OP used."

-- Instruction Evaluation
eval :: ScriptOp -> StackOperation ()
eval OP_NOP     = return ()
eval OP_NOP1    = void nopDiscourager
eval OP_NOP2    = void nopDiscourager
eval OP_NOP3    = void nopDiscourager
eval OP_NOP4    = void nopDiscourager
eval OP_NOP5    = void nopDiscourager
eval OP_NOP6    = void nopDiscourager
eval OP_NOP7    = void nopDiscourager
eval OP_NOP8    = void nopDiscourager
eval OP_NOP9    = void nopDiscourager
eval OP_NOP10   = void nopDiscourager

eval OP_VERIFY = popBool >>= \case
    True  -> return ()
    False -> programError "OP_VERIFY failed"

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

eval OP_SIZE   = (fromIntegral . length . head <$> withStack) >>= pushInt

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
eval OP_BOOLAND     = (&&) <$> ((0 /=) <$> popInt)
                           <*> ((0 /=) <$> popInt) >>= pushBool
eval OP_BOOLOR      = (||) <$> ((0 /=) <$> popInt)
                           <*> ((0 /=) <$> popInt) >>= pushBool
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

eval OP_RIPEMD160 = tStack1 $ return . bsToSv . encode . hash160 . opToSv
eval OP_SHA1 = tStack1 $ return . bsToSv . encode . hashSHA1 . opToSv

eval OP_SHA256 = tStack1 $ return . bsToSv . encode . hash256 . opToSv
eval OP_HASH160 = tStack1 $
    return . bsToSv . encode . addressHash . opToSv
eval OP_HASH256 = tStack1 $
    return . bsToSv . encode . doubleHash256  . opToSv
eval OP_CODESEPARATOR = dropHashOpsSeparatedCode
eval OP_CHECKSIG = do
    pubKey <- popStack
    sig <- popStack
    checker <- sigCheck <$> get
    hOps <- preparedHashOps
    -- Reuse checkMultiSig code
    pushBool $ checkMultiSig checker [ pubKey ] [ sig ] hOps

eval OP_CHECKMULTISIG =
    do nPubKeys <- fromIntegral <$> popInt
       when (nPubKeys < 0 || nPubKeys > maxKeysMultisig)
            $ programError $ "nPubKeys outside range: " ++ show nPubKeys
       pubKeys <- popStackN $ toInteger nPubKeys

       nSigs <- fromIntegral <$> popInt
       when (nSigs < 0 || nSigs > nPubKeys)
            $ programError $ "nSigs outside range: " ++ show nSigs
       sigs <- popStackN $ toInteger nSigs

       nullDummyEnforcer
       void popStack -- spec bug
       checker <- sigCheck <$> get
       hOps <- preparedHashOps
       pushBool $ checkMultiSig checker pubKeys sigs hOps
       modify $ \p -> p { opCount = opCount p + length pubKeys }

eval OP_CHECKSIGVERIFY      = eval OP_CHECKSIG      >> eval OP_VERIFY
eval OP_CHECKMULTISIGVERIFY = eval OP_CHECKMULTISIG >> eval OP_VERIFY

eval op = case constValue op of
    Just sv -> minimalPushEnforcer op >> pushStack sv
    Nothing -> programError $ "unexpected op " ++ show op

minimalPushEnforcer :: ScriptOp -> StackOperation ()
minimalPushEnforcer op = do
    flgs <- ask
    when (MINIMALDATA `elem` flgs) $
        unless (checkMinimalPush op) $
            programError $ "Non-minimal data: " ++ show op

checkMinimalPush :: ScriptOp -> Bool -- Putting in a maybe monad to avoid elif chain
checkMinimalPush ( OP_PUSHDATA payload optype ) =
  let l = BS.length payload
      v = head (BS.unpack payload)
  in not $
     BS.null payload                     -- Check if could have used OP_0
     || (l == 1 && v <= 16 && v >= 1)   -- Could have used OP_{1,..,16}
     || (l == 1 && v == 0x81)           -- Could have used OP_1NEGATE
     || (l <= 75 && optype /= OPCODE)   -- Could have used direct push
     || (l <= 255 && l > 75 && optype /= OPDATA1)
     || (l > 255 && l <= 65535 && optype /= OPDATA2)
checkMinimalPush _ = True

-- | Checks the top of the stack for a minimal numeric representation
-- if flagged to do so
minimalStackValEnforcer :: StackOperation ()
minimalStackValEnforcer = do
    flgs <- ask
    s <- getStack
    let topStack = if null s then [] else head s
    when (MINIMALDATA `elem` flgs || null topStack) $
        unless (checkMinimalNumRep topStack) $
            programError $ "Non-minimal stack value: " ++ show topStack

-- | Checks if a stack value is the minimal numeric representation of
-- the integer to which it decoes.  Based on CScriptNum from Bitcoin
-- Core.
checkMinimalNumRep :: StackValue -> Bool
checkMinimalNumRep [] = True
checkMinimalNumRep s =
    let msb = last s
        l = length s in
    not $
         -- If the MSB except sign bit is zero, then nonMinimal
         ( msb .&. 0x7f == 0 )
         -- With the exception of when a new byte is forced by a filled last bit
      && ( l <= 1 || ( s !! (l-2) ) .&. 0x80 == 0 )

nullDummyEnforcer :: StackOperation ()
nullDummyEnforcer = do
    flgs <- ask
    topStack <- getStack >>= headOrError
    when ((NULLDUMMY `elem` flgs) && (not . null $ topStack)) $
        programError "Non-null dummy stack in multi-sig"
  where
    headOrError s =
        if null s
        then programError "Empty stack where dummy op should be."
        else return $ head s

--------------------------------------------------------------------------------
-- | Based on the IfStack, returns whether the script is within an
-- evaluating if-branch.
getExec :: Program Bool
getExec = and <$> getCond

-- | Converts a `ScriptOp` to a ProgramData monad.
conditionalEval :: ScriptOp -> Program ()
conditionalEval scrpOp = do
   -- lift $ checkOpEnabled scrpOp
   lift $ checkPushData scrpOp

   e  <- getExec
   eval' e scrpOp

   when (countOp scrpOp) $ lift $ join $ incrementOpCount . opCount <$> get

   lift checkStackSize

   where
     eval' :: Bool -> ScriptOp -> Program ()

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
evalOps :: [ ScriptOp ] -> Program ()
evalOps ops = do mapM_ conditionalEval ops
                 cond <- getCond
                 unless (null cond) (lift $ programError "ifStack not empty")


checkPushOnly :: [ ScriptOp ] -> Program ()
checkPushOnly ops
      | not (all checkPushOp ops) = lift $ programError "only push ops allowed"
      | otherwise = return ()
      where checkPushOp op = case constValue op of
                                  Just _  -> True
                                  Nothing -> False

checkStack :: Stack -> Bool
checkStack (x:_) = decodeBool x
checkStack []    = False


isPayToScriptHash :: [ ScriptOp ] -> [ Flag ]  -> Bool
isPayToScriptHash [OP_HASH160, OP_PUSHDATA bytes OPCODE, OP_EQUAL] flgs
                    = ( P2SH `elem` flgs ) && ( BS.length bytes == 20 )
isPayToScriptHash _ _ = False

stackToScriptOps :: StackValue -> [ ScriptOp ]
stackToScriptOps sv = case decode $ BS.pack sv of
    Left _  -> []  -- Maybe should propogate the error some how
    Right s -> scriptOps s

-- exported functions

execScript :: Script -- ^ scriptSig ( redeemScript )
           -> Script -- ^ scriptPubKey
           -> SigCheck -- ^ signature verification Function
           -> [ Flag ] -- ^ Evaluation flags
           -> Either EvalError ProgramData
execScript scriptSig scriptPubKey sigCheckFcn flags =
  let sigOps = scriptOps scriptSig
      pubKeyOps = scriptOps scriptPubKey
      initData = ProgramData {
        stack = [],
        altStack = [],
        hashOps = pubKeyOps,
        sigCheck = sigCheckFcn,
        opCount = 0
      }


      checkSig | isPayToScriptHash pubKeyOps flags = checkPushOnly sigOps
               | SIGPUSHONLY `elem` flags = checkPushOnly sigOps
               | otherwise = return ()

      checkKey
          | BS.length (encode scriptPubKey) > fromIntegral maxScriptSize =
                lift $ programError "pubKey > maxScriptSize"
          | otherwise = return ()


      redeemEval = checkSig >> evalOps sigOps >> lift (stack <$> get)
      pubKeyEval = checkKey >> evalOps pubKeyOps >> lift get

      in do s <- evalProgram redeemEval [] initData flags
            p <- evalProgram pubKeyEval [] initData { stack = s } flags
            if not (null s)
                   && isPayToScriptHash pubKeyOps flags
                   && checkStack (runStack p)
              then evalProgram (evalP2SH s) [] initData { stack = drop 1 s,
                      hashOps = stackToScriptOps $ head s } flags
              else return p


-- | Evaluates a P2SH style script from its serialization in the stack
evalP2SH :: Stack -> Program ProgramData
evalP2SH []     = lift $ programError "PayToScriptHash: no script on stack"
evalP2SH (sv:_) = evalOps (stackToScriptOps sv) >> lift get

evalScript :: Script -> Script -> SigCheck -> [ Flag ] -> Bool
evalScript scriptSig scriptPubKey sigCheckFcn flags =
              case execScript scriptSig scriptPubKey sigCheckFcn flags of
                  Left _  -> False
                  Right p -> checkStack . runStack $ p

runStack :: ProgramData -> Stack
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
            -> [Flag] -- ^ Evaluation flags
            -> Bool
verifySpend tx i outscript flags =
  let scriptSig = either err id . decode . scriptInput $ txIn tx !! i
      verifyFcn = verifySigWithType tx i
      err e = error $ "Could not decode scriptInput in verifySpend: " ++ e
  in evalScript scriptSig outscript verifyFcn flags

