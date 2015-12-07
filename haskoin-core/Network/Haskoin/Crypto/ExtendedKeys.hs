{-# LANGUAGE GADTs #-}
module Network.Haskoin.Crypto.ExtendedKeys
( XPubKey(..)
, XPrvKey(..)
, ChainCode
, KeyIndex
, DerivationException(..)
, makeXPrvKey
, deriveXPubKey
, prvSubKey
, pubSubKey
, hardSubKey
, xPrvIsHard
, xPubIsHard
, xPrvChild
, xPubChild
, xPubID
, xPrvID
, xPubFP
, xPrvFP
, xPubAddr
, xPubExport
, xPrvExport
, xPubImport
, xPrvImport
, xPrvWif
  -- Helpers
, prvSubKeys
, pubSubKeys
, hardSubKeys
, deriveAddr
, deriveAddrs
, deriveMSAddr
, deriveMSAddrs
, cycleIndex
  -- Derivation paths
, DerivPathI(..)
, HardOrGeneric
, GenericOrSoft
, DerivPath
, HardPath
, SoftPath
, Bip32PathIndex (..)
, derivePath
, derivePubPath
, toHard
, toSoft
, toGeneric
, (++/)
, pathToStr

  -- Derivation path parsing
, XKey(..)
, ParsedPath(..)
, parsePath
, parseHard
, parseSoft
, applyPath

, derivePathAddr
, derivePathAddrs
, derivePathMSAddr
, derivePathMSAddrs
, concatBip32Segments
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (mzero, guard, unless, (<=<))
import Control.Exception (Exception, throw)

import qualified Crypto.Secp256k1 as EC

import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON, withText)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8, getWord32be)
import Data.Binary.Put (Put, putWord8, putWord32be)
import Data.Word (Word8, Word32)
import Data.Bits (setBit, testBit, clearBit)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (append, take)

import Text.Read (readPrec, parens, lexP, pfail)
import qualified Text.Read as Read (Lexeme(Ident, String))

import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Script.Parser
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Crypto.Base58
import Data.List (foldl')

{- See BIP32 for details: https://en.bitcoin.it/wiki/BIP_0032 -}

-- | A derivation exception is thrown in the very unlikely event that a
-- derivation is invalid.
data DerivationException = DerivationException String
    deriving (Eq, Read, Show, Typeable)

instance Exception DerivationException

type ChainCode = Hash256
type KeyIndex = Word32

-- | Data type representing an extended BIP32 private key. An extended key
-- is a node in a tree of key derivations. It has a depth in the tree, a
-- parent node and an index to differentiate it from other siblings.
data XPrvKey = XPrvKey
    { xPrvDepth  :: !Word8     -- ^ Depth in the tree of key derivations.
    , xPrvParent :: !Word32    -- ^ Fingerprint of the parent key.
    , xPrvIndex  :: !KeyIndex  -- ^ Key derivation index.
    , xPrvChain  :: !ChainCode -- ^ Chain code.
    , xPrvKey    :: !PrvKeyC   -- ^ The private key of this extended key node.
    } deriving (Eq)

-- TODO: Test
instance Show XPrvKey where
    showsPrec d k = showParen (d > 10) $
        showString "XPrvKey " . shows (xPrvExport k)

-- TODO: Test
instance Read XPrvKey where
    readPrec = parens $ do
        Read.Ident "XPrvKey" <- lexP
        Read.String str <- lexP
        maybe pfail return $ xPrvImport $ cs str

-- TODO: Test
instance IsString XPrvKey where
    fromString =
        fromMaybe e . xPrvImport . cs
      where
        e = error "Could not decode extended private key"

instance NFData XPrvKey where
    rnf (XPrvKey d p i c k) =
        rnf d `seq` rnf p `seq` rnf i `seq` rnf c `seq` rnf k

instance ToJSON XPrvKey where
    toJSON = String . cs . xPrvExport

instance FromJSON XPrvKey where
    parseJSON = withText "xprvkey" $ maybe mzero return . xPrvImport . cs

-- | Data type representing an extended BIP32 public key.
data XPubKey = XPubKey
    { xPubDepth  :: !Word8     -- ^ Depth in the tree of key derivations.
    , xPubParent :: !Word32    -- ^ Fingerprint of the parent key.
    , xPubIndex  :: !KeyIndex  -- ^ Key derivation index.
    , xPubChain  :: !ChainCode -- ^ Chain code.
    , xPubKey    :: !PubKeyC   -- ^ The public key of this extended key node.
    } deriving (Eq)

-- TODO: Test
instance Show XPubKey where
    showsPrec d k = showParen (d > 10) $
        showString "XPubKey " . shows (xPubExport k)

-- TODO: Test
instance Read XPubKey where
    readPrec = parens $ do
        Read.Ident "XPubKey" <- lexP
        Read.String str <- lexP
        maybe pfail return $ xPubImport $ cs str

-- TODO: Test
instance IsString XPubKey where
    fromString =
        fromMaybe e . xPubImport . cs
      where
        e = error "Could not import extended public key"

instance NFData XPubKey where
    rnf (XPubKey d p i c k) =
        rnf d `seq` rnf p `seq` rnf i `seq` rnf c `seq` rnf k

instance ToJSON XPubKey where
    toJSON = String . cs . xPubExport

instance FromJSON XPubKey where
    parseJSON = withText "xpubkey" $ maybe mzero return . xPubImport . cs

-- | Build a BIP32 compatible extended private key from a bytestring. This will
-- produce a root node (depth=0 and parent=0).
makeXPrvKey :: ByteString -> XPrvKey
makeXPrvKey bs =
    XPrvKey 0 0 0 c k
  where
    (p, c) = split512 $ hmac512 "Bitcoin seed" bs
    k     = fromMaybe err $ makePrvKeyC <$> EC.secKey (getHash256 p)
    err   = throw $ DerivationException "Invalid seed"

-- | Derive an extended public key from an extended private key. This function
-- will preserve the depth, parent, index and chaincode fields of the extended
-- private keys.
deriveXPubKey :: XPrvKey -> XPubKey
deriveXPubKey (XPrvKey d p i c k) = XPubKey d p i c (derivePubKey k)

-- | Compute a private, soft child key derivation. A private soft derivation
-- will allow the equivalent extended public key to derive the public key for
-- this child. Given a parent key /m/ and a derivation index /i/, this function
-- will compute m\/i\/.
--
-- Soft derivations allow for more flexibility such as read-only wallets.
-- However, care must be taken not the leak both the parent extended public key
-- and one of the extended child private keys as this would compromise the
-- extended parent private key.
prvSubKey :: XPrvKey  -- ^ Extended parent private key
          -> KeyIndex -- ^ Child derivation index
          -> XPrvKey  -- ^ Extended child private key
prvSubKey xkey child
    | child >= 0 && child < 0x80000000 =
        XPrvKey (xPrvDepth xkey + 1) (xPrvFP xkey) child c k
    | otherwise = error "Invalid child derivation index"
  where
    pK     = xPubKey $ deriveXPubKey xkey
    msg    = BS.append (encode' pK) (encode' child)
    (a, c) = split512 $ hmac512 (encode' $ xPrvChain xkey) msg
    k      = fromMaybe err $ tweakPrvKeyC (xPrvKey xkey) a
    err    = throw $ DerivationException "Invalid prvSubKey derivation"

-- | Compute a public, soft child key derivation. Given a parent key /M/
-- and a derivation index /i/, this function will compute M\/i\/.
pubSubKey :: XPubKey  -- ^ Extended Parent public key
          -> KeyIndex -- ^ Child derivation index
          -> XPubKey  -- ^ Extended child public key
pubSubKey xKey child
    | child >= 0 && child < 0x80000000 =
        XPubKey (xPubDepth xKey + 1) (xPubFP xKey) child c pK
    | otherwise = error "Invalid child derivation index"
  where
    msg    = BS.append (encode' $ xPubKey xKey) (encode' child)
    (a, c) = split512 $ hmac512 (encode' $ xPubChain xKey) msg
    pK     = fromMaybe err $ tweakPubKeyC (xPubKey xKey) a
    err    = throw $ DerivationException "Invalid pubSubKey derivation"

-- | Compute a hard child key derivation. Hard derivations can only be computed
-- for private keys. Hard derivations do not allow the parent public key to
-- derive the child public keys. However, they are safer as a breach of the
-- parent public key and child private keys does not lead to a breach of the
-- parent private key. Given a parent key /m/ and a derivation index /i/, this
-- function will compute m\/i'\/.
hardSubKey :: XPrvKey  -- ^ Extended Parent private key
           -> KeyIndex -- ^ Child derivation index
           -> XPrvKey  -- ^ Extended child private key
hardSubKey xkey child
    | child >= 0 && child < 0x80000000 =
        XPrvKey (xPrvDepth xkey + 1) (xPrvFP xkey) i c k
    | otherwise = error "Invalid child derivation index"
  where
    i      = setBit child 31
    msg    = BS.append (bsPadPrvKey $ xPrvKey xkey) (encode' i)
    (a, c) = split512 $ hmac512 (encode' $ xPrvChain xkey) msg
    k      = fromMaybe err $ tweakPrvKeyC (xPrvKey xkey) a
    err    = throw $ DerivationException "Invalid hardSubKey derivation"

-- | Returns True if the extended private key was derived through a hard
-- derivation.
xPrvIsHard :: XPrvKey -> Bool
xPrvIsHard k = testBit (xPrvIndex k) 31

-- | Returns True if the extended public key was derived through a hard
-- derivation.
xPubIsHard :: XPubKey -> Bool
xPubIsHard k = testBit (xPubIndex k) 31

-- | Returns the derivation index of this extended private key without the hard
-- bit set.
xPrvChild :: XPrvKey -> KeyIndex
xPrvChild k = clearBit (xPrvIndex k) 31

-- | Returns the derivation index of this extended public key without the hard
-- bit set.
xPubChild :: XPubKey -> KeyIndex
xPubChild k = clearBit (xPubIndex k) 31

-- | Computes the key identifier of an extended private key.
xPrvID :: XPrvKey -> Hash160
xPrvID = xPubID . deriveXPubKey

-- | Computes the key identifier of an extended public key.
xPubID :: XPubKey -> Hash160
xPubID = hash160 . getHash256 . hash256 . encode' . xPubKey

-- | Computes the key fingerprint of an extended private key.
xPrvFP :: XPrvKey -> Word32
xPrvFP = decode' . BS.take 4 . getHash160 . xPrvID

-- | Computes the key fingerprint of an extended public key.
xPubFP :: XPubKey -> Word32
xPubFP = decode' . BS.take 4 . getHash160 . xPubID

-- | Computer the 'Address' of an extended public key.
xPubAddr :: XPubKey -> Address
xPubAddr = pubKeyAddr . xPubKey

-- | Exports an extended private key to the BIP32 key export format (base 58).
xPrvExport :: XPrvKey -> ByteString
xPrvExport = encodeBase58Check . encode'

-- | Exports an extended public key to the BIP32 key export format (base 58).
xPubExport :: XPubKey -> ByteString
xPubExport = encodeBase58Check . encode'

-- | Decodes a BIP32 encoded extended private key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPrvImport :: ByteString -> Maybe XPrvKey
xPrvImport = decodeToMaybe <=< decodeBase58Check

-- | Decodes a BIP32 encoded extended public key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPubImport :: ByteString -> Maybe XPubKey
xPubImport = decodeToMaybe <=< decodeBase58Check

-- | Export an extended private key to WIF (Wallet Import Format).
xPrvWif :: XPrvKey -> ByteString
xPrvWif = toWif . xPrvKey

instance Binary XPrvKey where

    get = do
        ver <- getWord32be
        unless (ver == extSecretPrefix) $ fail $
            "Get: Invalid version for extended private key"
        dep <- getWord8
        par <- getWord32be
        idx <- getWord32be
        chn <- get
        prv <- getPadPrvKey
        return $ XPrvKey dep par idx chn prv

    put k = do
        putWord32be  extSecretPrefix
        putWord8     $ xPrvDepth k
        putWord32be  $ xPrvParent k
        putWord32be  $ xPrvIndex k
        put          $ xPrvChain k
        putPadPrvKey $ xPrvKey k

instance Binary XPubKey where

    get = do
        ver <- getWord32be
        unless (ver == extPubKeyPrefix) $ fail $
            "Get: Invalid version for extended public key"
        dep <- getWord8
        par <- getWord32be
        idx <- getWord32be
        chn <- get
        pub <- get
        return $ XPubKey dep par idx chn pub

    put k = do
        putWord32be extPubKeyPrefix
        putWord8    $ xPubDepth k
        putWord32be $ xPubParent k
        putWord32be $ xPubIndex k
        put         $ xPubChain k
        put $ xPubKey k

{- Derivation helpers -}

-- | Cyclic list of all private soft child key derivations of a parent key
-- starting from an offset index.
prvSubKeys :: XPrvKey -> KeyIndex -> [(XPrvKey, KeyIndex)]
prvSubKeys k = map (\i -> (prvSubKey k i, i)) . cycleIndex

-- | Cyclic list of all public soft child key derivations of a parent key
-- starting from an offset index.
pubSubKeys :: XPubKey -> KeyIndex -> [(XPubKey, KeyIndex)]
pubSubKeys k = map (\i -> (pubSubKey k i, i)) . cycleIndex

-- | Cyclic list of all hard child key derivations of a parent key starting
-- from an offset index.
hardSubKeys :: XPrvKey -> KeyIndex -> [(XPrvKey, KeyIndex)]
hardSubKeys k = map (\i -> (hardSubKey k i, i)) . cycleIndex

-- | Derive an address from a public key and an index. The derivation type
-- is a public, soft derivation.
deriveAddr :: XPubKey -> KeyIndex -> (Address, PubKeyC)
deriveAddr k i =
    (xPubAddr key, xPubKey key)
  where
    key = pubSubKey k i

-- | Cyclic list of all addresses derived from a public key starting from an
-- offset index. The derivation types are public, soft derivations.
deriveAddrs :: XPubKey -> KeyIndex -> [(Address, PubKeyC, KeyIndex)]
deriveAddrs k =
    map f . cycleIndex
  where
    f i = let (a, key) = deriveAddr k i in (a, key, i)

-- | Derive a multisig address from a list of public keys, the number of
-- required signatures (m) and a derivation index. The derivation type is a
-- public, soft derivation.
deriveMSAddr :: [XPubKey] -> Int -> KeyIndex -> (Address, RedeemScript)
deriveMSAddr keys m i =
    (scriptAddr rdm, rdm)
  where
    rdm = sortMulSig $ PayMulSig k m
    k   = map (toPubKeyG . xPubKey . flip pubSubKey i) keys

-- | Cyclic list of all multisig addresses derived from a list of public keys,
-- a number of required signatures (m) and starting from an offset index. The
-- derivation type is a public, soft derivation.
deriveMSAddrs :: [XPubKey] -> Int -> KeyIndex
              -> [(Address, RedeemScript, KeyIndex)]
deriveMSAddrs keys m =
    map f . cycleIndex
  where
    f i = let (a, rdm) = deriveMSAddr keys m i in (a, rdm, i)

cycleIndex :: KeyIndex -> [KeyIndex]
cycleIndex i
    | i == 0         = cycle [0..0x7fffffff]
    | i < 0x80000000 = cycle $ [i..0x7fffffff] ++ [0..(i-1)]
    | otherwise      = error $ "cycleIndex: invalid index " ++ (show i)

{- Derivation Paths -}

data Hard
data Generic
data Soft

type HardPath = DerivPathI Hard
type DerivPath = DerivPathI Generic
type SoftPath = DerivPathI Soft

class HardOrGeneric a
instance HardOrGeneric Hard
instance HardOrGeneric Generic

class GenericOrSoft a
instance GenericOrSoft Generic
instance GenericOrSoft Soft

-- | Data type representing a derivation path. Two constructors are provided
-- for specifying soft or hard derivations. The path /0/1'/2 for example can be
-- expressed as Deriv :/ 0 :| 1 :/ 2. The HardOrGeneric and GenericOrSoft type
-- classes are used to constrain the valid values for the phantom type t. If
-- you mix hard (:|) and soft (:/) paths, the only valid type for t is Generic.
-- Otherwise, t can be Hard if you only have hard derivation or Soft if you
-- only have soft derivations.
--
-- Using this type is as easy as writing the required derivation like in these
-- example:
-- Deriv :/ 0 :/ 1 :/ 2 :: SoftPath
-- Deriv :| 0 :| 1 :| 2 :: HardPath
-- Deriv :| 0 :/ 1 :/ 2 :: DerivPath
data DerivPathI t where
    (:|)  :: HardOrGeneric t => !(DerivPathI t) -> !KeyIndex -> DerivPathI t
    (:/)  :: GenericOrSoft t => !(DerivPathI t) -> !KeyIndex -> DerivPathI t
    Deriv :: DerivPathI t

instance NFData (DerivPathI t) where
    rnf p = case p of
        next :| i -> rnf i `seq` rnf next
        next :/ i -> rnf i `seq` rnf next
        Deriv     -> ()

instance Eq (DerivPathI t) where
    (nextA :| iA) == (nextB :| iB) = iA == iB && nextA == nextB
    (nextA :/ iA) == (nextB :/ iB) = iA == iB && nextA == nextB
    Deriv         == Deriv         = True
    _             == _             = False

-- TODO: Test
pathToStr :: DerivPathI t -> String
pathToStr p =
    case p of
        next :| i -> concat [ pathToStr next, "/", show i, "'" ]
        next :/ i -> concat [ pathToStr next, "/", show i ]
        Deriv     -> ""

toHard :: DerivPathI t -> Maybe HardPath
toHard p = case p of
    next :| i -> (:| i) <$> toHard next
    Deriv     -> Just Deriv
    _         -> Nothing

toSoft :: DerivPathI t -> Maybe SoftPath
toSoft p = case p of
    next :/ i -> (:/ i) <$> toSoft next
    Deriv     -> Just Deriv
    _         -> Nothing

toGeneric :: DerivPathI t -> DerivPath
toGeneric p = case p of
    next :/ i -> (toGeneric next) :/ i
    next :| i -> (toGeneric next) :| i
    Deriv     -> Deriv

-- | Append two derivation paths together. The result will be a mixed
-- derivation path.
(++/) :: DerivPathI t1 -> DerivPathI t2 -> DerivPath
(++/) p1 p2 =
    go id (toGeneric p2) $ toGeneric p1
  where
    go f p = case p of
        next :/ i -> go (f . (:/ i)) $ toGeneric next
        next :| i -> go (f . (:| i)) $ toGeneric next
        _ -> f

-- | Derive a private key from a derivation path
derivePath :: DerivPathI t -> XPrvKey -> XPrvKey
derivePath path key =
    go id path $ key
  where
    -- Build the full derivation function starting from the end
    go f p = case p of
        next :| i -> go (f . flip hardSubKey i) next
        next :/ i -> go (f . flip prvSubKey i) next
        _ -> f

-- | Derive a public key from a soft derivation path
derivePubPath :: SoftPath -> XPubKey -> XPubKey
derivePubPath path key =
    go id path $ key
  where
    -- Build the full derivation function starting from the end
    go f p = case p of
        next :/ i -> go (f . flip pubSubKey i) next
        _ -> f

-- TODO: Test
instance Show DerivPath where
    showsPrec d p = showParen (d > 10) $
        showString "DerivPath " . shows (pathToStr p)

-- TODO: Test
instance Show HardPath where
    showsPrec d p = showParen (d > 10) $
        showString "HardPath " . shows (pathToStr p)

-- TODO: Test
instance Show SoftPath where
    showsPrec d p = showParen (d > 10) $
        showString "SoftPath " . shows (pathToStr p)

-- TODO: Test
instance Read DerivPath where
    readPrec = parens $ do
        Read.Ident "DerivPath" <- lexP
        Read.String str <- lexP
        maybe pfail (return . getParsedPath) $ parsePath str

-- TODO: Test
instance Read HardPath where
    readPrec = parens $ do
        Read.Ident "HardPath" <- lexP
        Read.String str <- lexP
        maybe pfail return $ parseHard str

-- TODO: Test
instance Read SoftPath where
    readPrec = parens $ do
        Read.Ident "SoftPath" <- lexP
        Read.String str <- lexP
        maybe pfail return $ parseSoft str

-- TODO: Test
instance IsString ParsedPath where
    fromString =
        fromMaybe e . parsePath
      where
        e = error "Could not parse derivation path"

-- TODO: Test
instance IsString DerivPath where
    fromString =
        getParsedPath . fromMaybe e . parsePath
      where
        e = error "Could not parse derivation path"

-- TODO: Test
instance IsString HardPath where
    fromString =
        fromMaybe e . parseHard
      where
        e = error "Could not parse hard derivation path"

-- TODO: Test
instance IsString SoftPath where
    fromString =
        fromMaybe e . parseSoft
      where
        e = error "Could not parse soft derivation path"

instance FromJSON ParsedPath where
    parseJSON = withText "ParsedPathPath" $ \str -> case parsePath $ cs str of
        Just p -> return p
        _      -> mzero

instance FromJSON DerivPath where
    parseJSON = withText "DerivPath" $ \str -> case parsePath $ cs str of
        Just p -> return $ getParsedPath p
        _      -> mzero

instance FromJSON HardPath where
    parseJSON = withText "HardPath" $ \str -> case parseHard $ cs str of
        Just p -> return p
        _      -> mzero

instance FromJSON SoftPath where
    parseJSON = withText "SoftPath" $ \str -> case parseSoft $ cs str of
        Just p -> return p
        _      -> mzero

instance ToJSON (DerivPathI t) where
    toJSON = String . cs . pathToStr

instance ToJSON ParsedPath where
    toJSON (ParsedPrv p)   = String . cs . ("m" ++) . pathToStr $ p
    toJSON (ParsedPub p)   = String . cs . ("M" ++) . pathToStr $ p
    toJSON (ParsedEmpty p) = String . cs . ("" ++) . pathToStr $ p

{- Parsing derivation paths of the form m/1/2'/3 or M/1/2'/3 -}

data ParsedPath = ParsedPrv   { getParsedPath :: !DerivPath }
                | ParsedPub   { getParsedPath :: !DerivPath }
                | ParsedEmpty { getParsedPath :: !DerivPath }
  deriving (Read, Show, Eq)
-- | Parse derivation path string for extended key.
-- Forms: “m/0'/2”, “M/2/3/4”.
parsePath :: String -> Maybe ParsedPath
parsePath str = do
    res <- concatBip32Segments <$> mapM parseBip32PathIndex xs
    case x of
        "m" -> Just $ ParsedPrv res
        "M" -> Just $ ParsedPub res
        ""  -> Just $ ParsedEmpty res
        _   -> Nothing
  where
    (x : xs) = splitOn "/" str
                

concatBip32Segments :: [Bip32PathIndex] -> DerivPath
concatBip32Segments xs = foldl' appendBip32Segment Deriv xs


appendBip32Segment :: DerivPath -> Bip32PathIndex  -> DerivPath
appendBip32Segment d (Bip32SoftIndex i) = d :/ i 
appendBip32Segment d (Bip32HardIndex i) = d :| i 


parseBip32PathIndex :: String -> Maybe Bip32PathIndex
parseBip32PathIndex segment = case reads segment of
    [(i, "" )] -> guard (is31Bit i) >> ( return $ Bip32SoftIndex i )
    [(i, "'")] -> guard (is31Bit i) >> ( return $ Bip32HardIndex i )
    _ -> Nothing


data Bip32PathIndex = Bip32HardIndex KeyIndex | Bip32SoftIndex KeyIndex
  deriving (Read,Show,Eq)

is31Bit :: (Integral a) => a -> Bool
is31Bit i = (i >=0 && i < 0x80000000) 


-- Helper function to parse a hard path
parseHard :: String -> Maybe HardPath
parseHard = toHard . getParsedPath <=< parsePath

-- Helper function to parse a soft path
parseSoft :: String -> Maybe SoftPath
parseSoft = toSoft . getParsedPath <=< parsePath

data XKey = XPrv { getXPrvKey :: !XPrvKey }
          | XPub { getXPubKey :: !XPubKey }
    deriving (Eq, Show)

-- | Apply a parsed path to a private key to derive the new key defined in the
-- path. If the path starts with m/, a private key will be returned and if the
-- path starts with M/, a public key will be returned.
applyPath :: ParsedPath -> XKey -> Either String XKey
applyPath path key = case (path, key) of
    (ParsedPrv _, XPrv k) -> return $ XPrv $ derivPrvF k
    (ParsedPrv _, XPub _) -> Left "applyPath: Invalid public key"
    (ParsedPub _, XPrv k) -> return $ XPub $ deriveXPubKey $ derivPrvF k
    (ParsedPub _, XPub k) -> derivPubFE >>= \f -> return $ XPub $ f k
    -- For empty parsed paths, we take a hint from the provided key
    (ParsedEmpty _, XPrv k) -> return $ XPrv $ derivPrvF k
    (ParsedEmpty _, XPub k) -> derivPubFE >>= \f -> return $ XPub $ f k
  where
    derivPrvF  = goPrv id $ getParsedPath path
    derivPubFE = goPubE id $ getParsedPath path
    -- Build the full private derivation function starting from the end
    goPrv f p = case p of
        next :| i -> goPrv (f . flip hardSubKey i) next
        next :/ i -> goPrv (f . flip prvSubKey i) next
        Deriv     -> f
    -- Build the full public derivation function starting from the end
    goPubE f p = case p of
        next :/ i -> goPubE (f . flip pubSubKey i) next
        Deriv     -> Right f
        _         -> Left "applyPath: Invalid hard derivation"

{- Helpers for derivation paths and addresses -}

-- | Derive an address from a given parent path.
derivePathAddr :: XPubKey -> SoftPath -> KeyIndex -> (Address, PubKeyC)
derivePathAddr key path i = deriveAddr (derivePubPath path key) i

-- | Cyclic list of all addresses derived from a given parent path and starting
-- from the given offset index.
derivePathAddrs :: XPubKey -> SoftPath -> KeyIndex
                -> [(Address, PubKeyC, KeyIndex)]
derivePathAddrs key path i = deriveAddrs (derivePubPath path key) i

-- | Derive a multisig address from a given parent path. The number of required
-- signatures (m in m of n) is also needed.
derivePathMSAddr :: [XPubKey] -> SoftPath -> Int -> KeyIndex
                 -> (Address, RedeemScript)
derivePathMSAddr keys path m i =
    deriveMSAddr (map (derivePubPath path) keys) m i

-- | Cyclic list of all multisig addresses derived from a given parent path and
-- starting from the given offset index. The number of required signatures
-- (m in m of n) is also needed.
derivePathMSAddrs :: [XPubKey] -> SoftPath -> Int -> KeyIndex
                  -> [(Address, RedeemScript, KeyIndex)]
derivePathMSAddrs keys path m i =
    deriveMSAddrs (map (derivePubPath path) keys) m i

{- Utilities for extended keys -}

-- De-serialize HDW-specific private key
getPadPrvKey :: Get PrvKeyC
getPadPrvKey = do
    pad <- getWord8
    unless (pad == 0x00) $ fail $
        "Private key must be padded with 0x00"
    prvKeyGetMonad makePrvKeyC -- Compressed version

-- Serialize HDW-specific private key
putPadPrvKey :: PrvKeyC -> Put
putPadPrvKey p = putWord8 0x00 >> prvKeyPutMonad p

bsPadPrvKey :: PrvKeyC -> ByteString
bsPadPrvKey = runPut' . putPadPrvKey

