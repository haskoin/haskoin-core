{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Haskoin.Keys.Extended
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX

BIP-32 extended keys.
-}
module Network.Haskoin.Keys.Extended
    (
      -- * Extended Keys
      XPubKey(..)
    , XPrvKey(..)
    , ChainCode
    , KeyIndex
    , Fingerprint
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
    , xPubToJSON
    , xPubFromJSON
    , xPrvExport
    , xPrvToJSON
    , xPrvFromJSON
    , xPubImport
    , xPrvImport
    , xPrvWif
    , putXPrvKey
    , putXPubKey
    , getXPrvKey
    , getXPubKey

      -- ** Helper Functions
    , prvSubKeys
    , pubSubKeys
    , hardSubKeys
    , deriveAddr
    , deriveAddrs
    , deriveMSAddr
    , deriveMSAddrs
    , cycleIndex

      -- ** Derivation Paths
    , DerivPathI(..)
    , AnyDeriv, HardDeriv, SoftDeriv
    , HardOrAny
    , AnyOrSoft
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
    , listToPath
    , pathToList

      -- *** Derivation Path Parser
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

import           Control.Applicative
import           Control.Exception              (Exception, throw)
import           Control.Monad                  (guard, mzero, unless, (<=<))
import           Crypto.Secp256k1
import           Data.Aeson                     as A (FromJSON, ToJSON,
                                                      Value (String), parseJSON,
                                                      toJSON, withText)
import           Data.Aeson.Types               (Parser)
import           Data.Bits                      (clearBit, setBit, testBit)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import           Data.Either                    (fromRight)
import           Data.List                      (foldl')
import           Data.List.Split                (splitOn)
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    ((<>))
import           Data.Serialize                 as S (Serialize, decode, encode,
                                                      get, put)
import           Data.Serialize.Get             (Get, getWord32be, getWord8,
                                                 runGet)
import           Data.Serialize.Put             (Putter, putWord32be, putWord8,
                                                 runPut)
import           Data.String                    (IsString, fromString)
import           Data.String.Conversions        (cs)
import           Data.Typeable                  (Typeable)
import           Data.Word                      (Word32, Word8)
import           GHC.Generics                   (Generic)
import           Network.Haskoin.Address
import           Network.Haskoin.Address.Base58
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto.Hash
import           Network.Haskoin.Keys.Common
import           Network.Haskoin.Script
import           Network.Haskoin.Util
import           Text.Read                      as R
import           Text.Read.Lex


-- | A derivation exception is thrown in the very unlikely event that a
-- derivation is invalid.
newtype DerivationException = DerivationException String
    deriving (Eq, Read, Show, Typeable)

instance Exception DerivationException

-- | Chain code as specified in BIP-32.
type ChainCode = Hash256

-- | Index of key as specified in BIP-32.
type KeyIndex = Word32

-- | Fingerprint of parent
type Fingerprint = Word32

-- | Data type representing an extended BIP32 private key. An extended key
-- is a node in a tree of key derivations. It has a depth in the tree, a
-- parent node and an index to differentiate it from other siblings.
data XPrvKey = XPrvKey
    { xPrvDepth  :: !Word8       -- ^ depth in the tree
    , xPrvParent :: !Fingerprint -- ^ fingerprint of parent
    , xPrvIndex  :: !KeyIndex    -- ^ derivation index
    , xPrvChain  :: !ChainCode   -- ^ chain code
    , xPrvKey    :: !SecKey      -- ^ private key of this node
    } deriving (Generic, Eq, Show, Read)

xPrvToJSON :: Network -> XPrvKey -> Value
xPrvToJSON net = A.String . xPrvExport net

-- | Data type representing an extended BIP32 public key.
data XPubKey = XPubKey
    { xPubDepth  :: !Word8     -- ^ depth in the tree
    , xPubParent :: !Fingerprint    -- ^ fingerprint of parent
    , xPubIndex  :: !KeyIndex  -- ^ derivation index
    , xPubChain  :: !ChainCode -- ^ chain code
    , xPubKey    :: !PubKey    -- ^ public key of this node
    } deriving (Generic, Eq, Show, Read)


-- | Decode an extended public key from a JSON string
xPubFromJSON :: Network -> Value -> Parser XPubKey
xPubFromJSON net =
    withText "xpub" $ \t ->
        case xPubImport net t of
            Nothing -> fail "could not read xpub"
            Just x  -> return x

-- | Get JSON 'Value' from 'XPubKey'.
xPubToJSON :: Network -> XPubKey -> Value
xPubToJSON net = A.String . xPubExport net

-- | Decode an extended private key from a JSON string
xPrvFromJSON :: Network -> Value -> Parser XPrvKey
xPrvFromJSON net =
    withText "xprv" $ \t ->
        case xPrvImport net t of
            Nothing -> fail "could not read xprv"
            Just x  -> return x

-- | Build a BIP32 compatible extended private key from a bytestring. This will
-- produce a root node (@depth=0@ and @parent=0@).
makeXPrvKey :: ByteString -> XPrvKey
makeXPrvKey bs =
    XPrvKey 0 0 0 c k
  where
    (p, c) = split512 $ hmac512 "Bitcoin seed" bs
    k     = fromMaybe err (secKey (encode p))
    err   = throw $ DerivationException "Invalid seed"

-- | Derive an extended public key from an extended private key. This function
-- will preserve the depth, parent, index and chaincode fields of the extended
-- private keys.
deriveXPubKey :: XPrvKey -> XPubKey
deriveXPubKey (XPrvKey d p i c k) = XPubKey d p i c (derivePubKey k)

-- | Compute a private, soft child key derivation. A private soft derivation
-- will allow the equivalent extended public key to derive the public key for
-- this child. Given a parent key /m/ and a derivation index /i/, this function
-- will compute /m\/i/.
--
-- Soft derivations allow for more flexibility such as read-only wallets.
-- However, care must be taken not the leak both the parent extended public key
-- and one of the extended child private keys as this would compromise the
-- extended parent private key.
prvSubKey :: XPrvKey  -- ^ extended parent private key
          -> KeyIndex -- ^ child derivation index
          -> XPrvKey  -- ^ extended child private key
prvSubKey xkey child
    | child >= 0 && child < 0x80000000 =
        XPrvKey (xPrvDepth xkey + 1) (xPrvFP xkey) child c k
    | otherwise = error "Invalid child derivation index"
  where
    pK = xPubKey $ deriveXPubKey xkey
    m = B.append (exportPubKey True pK) (encode child)
    (a, c) = split512 $ hmac512 (encode $ xPrvChain xkey) m
    k = fromMaybe err $ tweakSecKey (xPrvKey xkey) a
    err = throw $ DerivationException "Invalid prvSubKey derivation"

-- | Compute a public, soft child key derivation. Given a parent key /M/
-- and a derivation index /i/, this function will compute /M\/i/.
pubSubKey :: XPubKey  -- ^ extended parent public key
          -> KeyIndex -- ^ child derivation index
          -> XPubKey  -- ^ extended child public key
pubSubKey xKey child
    | child >= 0 && child < 0x80000000 =
        XPubKey (xPubDepth xKey + 1) (xPubFP xKey) child c pK
    | otherwise = error "Invalid child derivation index"
  where
    m      = B.append (exportPubKey True (xPubKey xKey)) (encode child)
    (a, c) = split512 $ hmac512 (encode $ xPubChain xKey) m
    pK     = fromMaybe err $ tweakPubKey (xPubKey xKey) a
    err    = throw $ DerivationException "Invalid pubSubKey derivation"

-- | Compute a hard child key derivation. Hard derivations can only be computed
-- for private keys. Hard derivations do not allow the parent public key to
-- derive the child public keys. However, they are safer as a breach of the
-- parent public key and child private keys does not lead to a breach of the
-- parent private key. Given a parent key /m/ and a derivation index /i/, this
-- function will compute /m\/i'/.
hardSubKey :: XPrvKey  -- ^ extended parent private key
           -> KeyIndex -- ^ child derivation index
           -> XPrvKey  -- ^ extended child private key
hardSubKey xkey child
    | child >= 0 && child < 0x80000000 =
        XPrvKey (xPrvDepth xkey + 1) (xPrvFP xkey) i c k
    | otherwise = error "Invalid child derivation index"
  where
    i      = setBit child 31
    m      = B.append (bsPadPrvKey $ xPrvKey xkey) (encode i)
    (a, c) = split512 $ hmac512 (encode $ xPrvChain xkey) m
    k      = fromMaybe err $ tweakSecKey (xPrvKey xkey) a
    err    = throw $ DerivationException "Invalid hardSubKey derivation"

-- | Returns true if the extended private key was derived through a hard
-- derivation.
xPrvIsHard :: XPrvKey -> Bool
xPrvIsHard k = testBit (xPrvIndex k) 31

-- | Returns true if the extended public key was derived through a hard
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
xPubID = ripemd160 . encode . sha256 . exportPubKey True . xPubKey

-- | Computes the key fingerprint of an extended private key.
xPrvFP :: XPrvKey -> Fingerprint
xPrvFP =
    fromRight err . decode . B.take 4 . encode . xPrvID
  where
    err = error "Could not decode xPrvFP"

-- | Computes the key fingerprint of an extended public key.
xPubFP :: XPubKey -> Fingerprint
xPubFP =
    fromRight err . decode . B.take 4 . encode . xPubID
  where
    err = error "Could not decode xPubFP"

-- | Computer the 'Address' of an extended public key.
xPubAddr :: XPubKey -> Address
xPubAddr xkey = pubKeyAddr (wrapPubKey True (xPubKey xkey))

-- | Exports an extended private key to the BIP32 key export format ('Base58').
xPrvExport :: Network -> XPrvKey -> Base58
xPrvExport net = encodeBase58Check . runPut . putXPrvKey net

-- | Exports an extended public key to the BIP32 key export format ('Base58').
xPubExport :: Network -> XPubKey -> Base58
xPubExport net = encodeBase58Check . runPut . putXPubKey net

-- | Decodes a BIP32 encoded extended private key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPrvImport :: Network -> Base58 -> Maybe XPrvKey
xPrvImport net = eitherToMaybe . runGet (getXPrvKey net) <=< decodeBase58Check

-- | Decodes a BIP32 encoded extended public key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPubImport :: Network -> Base58 -> Maybe XPubKey
xPubImport net = eitherToMaybe . runGet (getXPubKey net) <=< decodeBase58Check

-- | Export an extended private key to WIF (Wallet Import Format).
xPrvWif :: Network -> XPrvKey -> Base58
xPrvWif net xkey = toWif net (wrapSecKey True (xPrvKey xkey))

-- | Parse a binary extended private key.
getXPrvKey :: Network -> Get XPrvKey
getXPrvKey net = do
        ver <- getWord32be
        unless (ver == getExtSecretPrefix net) $ fail
            "Get: Invalid version for extended private key"
        XPrvKey <$> getWord8
                <*> getWord32be
                <*> getWord32be
                <*> S.get
                <*> getPadPrvKey

-- | Serialize an extended private key.
putXPrvKey :: Network -> Putter XPrvKey
putXPrvKey net k = do
        putWord32be  $ getExtSecretPrefix net
        putWord8     $ xPrvDepth k
        putWord32be  $ xPrvParent k
        putWord32be  $ xPrvIndex k
        put          $ xPrvChain k
        putPadPrvKey $ xPrvKey k

-- | Parse a binary extended public key.
getXPubKey :: Network -> Get XPubKey
getXPubKey net = do
        ver <- getWord32be
        unless (ver == getExtPubKeyPrefix net) $ fail
            "Get: Invalid version for extended public key"
        XPubKey <$> getWord8
                <*> getWord32be
                <*> getWord32be
                <*> S.get
                <*> (pubKeyPoint <$> S.get)

-- | Serialize an extended public key.
putXPubKey :: Network -> Putter XPubKey
putXPubKey net k = do
        putWord32be $ getExtPubKeyPrefix net
        putWord8    $ xPubDepth k
        putWord32be $ xPubParent k
        putWord32be $ xPubIndex k
        put         $ xPubChain k
        put         $ wrapPubKey True (xPubKey k)

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
deriveAddr :: XPubKey -> KeyIndex -> (Address, PubKey)
deriveAddr k i =
    (xPubAddr key, xPubKey key)
  where
    key = pubSubKey k i

-- | Cyclic list of all addresses derived from a public key starting from an
-- offset index. The derivation types are public, soft derivations.
deriveAddrs :: XPubKey -> KeyIndex -> [(Address, PubKey, KeyIndex)]
deriveAddrs k =
    map f . cycleIndex
  where
    f i = let (a, key) = deriveAddr k i in (a, key, i)

-- | Derive a multisig address from a list of public keys, the number of
-- required signatures /m/ and a derivation index. The derivation type is a
-- public, soft derivation.
deriveMSAddr :: [XPubKey] -> Int -> KeyIndex -> (Address, RedeemScript)
deriveMSAddr keys m i = (payToScriptAddress rdm, rdm)
  where
    rdm = sortMulSig $ PayMulSig k m
    k = map (wrapPubKey True . xPubKey . flip pubSubKey i) keys

-- | Cyclic list of all multisig addresses derived from a list of public keys,
-- a number of required signatures /m/ and starting from an offset index. The
-- derivation type is a public, soft derivation.
deriveMSAddrs :: [XPubKey] -> Int -> KeyIndex
              -> [(Address, RedeemScript, KeyIndex)]
deriveMSAddrs keys m = map f . cycleIndex
  where
    f i =
        let (a, rdm) = deriveMSAddr keys m i
         in (a, rdm, i)

-- | Helper function to go through derivation indices.
cycleIndex :: KeyIndex -> [KeyIndex]
cycleIndex i
    | i == 0         = cycle [0..0x7fffffff]
    | i < 0x80000000 = cycle $ [i..0x7fffffff] ++ [0..(i-1)]
    | otherwise      = error $ "cycleIndex: invalid index " ++ show i

{- Derivation Paths -}

-- | Phantom type signaling a hardened derivation path that can only be computed
-- from private extended key.
data HardDeriv

-- | Phantom type signaling no knowledge about derivation path: can be hardened or not.
data AnyDeriv

-- | Phantom type signaling derivation path including only non-hardened paths
-- that can be computed from an extended public key.
data SoftDeriv

-- | Hardened derivation path. Can be computed from extended private key only.
type HardPath = DerivPathI HardDeriv

-- | Any derivation path.
type DerivPath = DerivPathI AnyDeriv

-- | Non-hardened derivation path can be computed from extended public key.
type SoftPath = DerivPathI SoftDeriv

-- | Helper class to perform validations on a hardened derivation path.
class HardOrAny a
instance HardOrAny HardDeriv
instance HardOrAny AnyDeriv

-- | Helper class to perform validations on a non-hardened derivation path.
class AnyOrSoft a
instance AnyOrSoft AnyDeriv
instance AnyOrSoft SoftDeriv

-- | Data type representing a derivation path. Two constructors are provided
-- for specifying soft or hard derivations. The path /\/0\/1'\/2/ for example can be
-- expressed as @'Deriv' :\/ 0 :| 1 :\/ 2@. The 'HardOrAny' and 'AnyOrSoft' type
-- classes are used to constrain the valid values for the phantom type /t/. If
-- you mix hard '(:|)' and soft '(:\/)' paths, the only valid type for /t/ is 'AnyDeriv'.
-- Otherwise, /t/ can be 'HardDeriv' if you only have hard derivation or 'SoftDeriv'
-- if you only have soft derivations.
--
-- Using this type is as easy as writing the required derivation like in these
-- example:
--
-- > Deriv :/ 0 :/ 1 :/ 2 :: SoftPath
-- > Deriv :| 0 :| 1 :| 2 :: HardPath
-- > Deriv :| 0 :/ 1 :/ 2 :: DerivPath
data DerivPathI t where
    (:|)  :: HardOrAny t => !(DerivPathI t) -> !KeyIndex -> DerivPathI t
    (:/)  :: AnyOrSoft t => !(DerivPathI t) -> !KeyIndex -> DerivPathI t
    Deriv :: DerivPathI t

instance Eq (DerivPathI t) where
    (nextA :| iA) == (nextB :| iB) = iA == iB && nextA == nextB
    (nextA :/ iA) == (nextB :/ iB) = iA == iB && nextA == nextB
    Deriv         == Deriv         = True
    _             == _             = False

instance Serialize DerivPath where
    get = listToPath <$> S.get
    put = put . pathToList

-- | Get a list of derivation indices from a derivation path.
pathToList :: DerivPathI t -> [KeyIndex]
pathToList =
    reverse . go
  where
    go (next :| i) = setBit i 31 : go next
    go (next :/ i) = i : go next
    go _           = []

-- | Convert a list of derivation indices to a derivation path.
listToPath :: [KeyIndex] -> DerivPath
listToPath =
    go . reverse
  where
    go (i:is)
        | testBit i 31 = go is :| clearBit i 31
        | otherwise    = go is :/ i
    go [] = Deriv

-- | Convert a derivation path to a human-readable string.
pathToStr :: DerivPathI t -> String
pathToStr p =
    case p of
        next :| i -> concat [ pathToStr next, "/", show i, "'" ]
        next :/ i -> concat [ pathToStr next, "/", show i ]
        Deriv     -> ""

-- | Turn a derivation path into a hard derivation path. Will fail if the path
-- contains soft derivations.
toHard :: DerivPathI t -> Maybe HardPath
toHard p = case p of
    next :| i -> (:| i) <$> toHard next
    Deriv     -> Just Deriv
    _         -> Nothing

-- | Turn a derivation path into a soft derivation path. Will fail if the path
-- has hard derivations.
toSoft :: DerivPathI t -> Maybe SoftPath
toSoft p = case p of
    next :/ i -> (:/ i) <$> toSoft next
    Deriv     -> Just Deriv
    _         -> Nothing

-- | Make a derivation path generic.
toGeneric :: DerivPathI t -> DerivPath
toGeneric p = case p of
    next :/ i -> toGeneric next :/ i
    next :| i -> toGeneric next :| i
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
        _         -> f

-- | Derive a private key from a derivation path
derivePath :: DerivPathI t -> XPrvKey -> XPrvKey
derivePath = go id
  where
    -- Build the full derivation function starting from the end
    go f p = case p of
        next :| i -> go (f . flip hardSubKey i) next
        next :/ i -> go (f . flip prvSubKey i) next
        _         -> f

-- | Derive a public key from a soft derivation path
derivePubPath :: SoftPath -> XPubKey -> XPubKey
derivePubPath = go id
  where
    -- Build the full derivation function starting from the end
    go f p = case p of
        next :/ i -> go (f . flip pubSubKey i) next
        _         -> f

instance Show DerivPath where
    showsPrec d p = showParen (d > 10) $
        showString "DerivPath " . shows (pathToStr p)

instance Read DerivPath where
    readPrec = parens $ do
        R.Ident "DerivPath" <- lexP
        R.String str <- lexP
        maybe pfail return $ getParsedPath <$> parsePath str

instance Show HardPath where
    showsPrec d p = showParen (d > 10) $
        showString "HardPath " . shows (pathToStr p)

instance Read HardPath where
    readPrec = parens $ do
        R.Ident "HardPath" <- lexP
        R.String str <- lexP
        maybe pfail return $ parseHard str

instance Show SoftPath where
    showsPrec d p = showParen (d > 10) $
        showString "SoftPath " . shows (pathToStr p)

instance Read SoftPath where
    readPrec = parens $ do
        R.Ident "SoftPath" <- lexP
        R.String str <- lexP
        maybe pfail return $ parseSoft str

instance IsString ParsedPath where
    fromString =
        fromMaybe e . parsePath
      where
        e = error "Could not parse derivation path"

instance IsString DerivPath where
    fromString =
        getParsedPath . fromMaybe e . parsePath
      where
        e = error "Could not parse derivation path"

instance IsString HardPath where
    fromString =
        fromMaybe e . parseHard
      where
        e = error "Could not parse hard derivation path"

instance IsString SoftPath where
    fromString =
        fromMaybe e . parseSoft
      where
        e = error "Could not parse soft derivation path"

instance FromJSON ParsedPath where
    parseJSON = withText "ParsedPath" $ \str -> case parsePath $ cs str of
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
    toJSON = A.String . cs . pathToStr

instance ToJSON ParsedPath where
    toJSON (ParsedPrv p)   = A.String . cs . ("m" ++) . pathToStr $ p
    toJSON (ParsedPub p)   = A.String . cs . ("M" ++) . pathToStr $ p
    toJSON (ParsedEmpty p) = A.String . cs . ("" ++) . pathToStr $ p

{- Parsing derivation paths of the form m/1/2'/3 or M/1/2'/3 -}

-- | Type for parsing derivation paths of the form /m\/1\/2'\/3/ or
-- /M\/1\/2'\/3/.
data ParsedPath = ParsedPrv   { getParsedPath :: !DerivPath }
                | ParsedPub   { getParsedPath :: !DerivPath }
                | ParsedEmpty { getParsedPath :: !DerivPath }
  deriving Eq

instance Show ParsedPath where
    showsPrec d p = showParen (d > 10) $ showString "ParsedPath " . shows f
      where
        f =
            case p of
                ParsedPrv d'   -> "m" <> pathToStr d'
                ParsedPub d'   -> "M" <> pathToStr d'
                ParsedEmpty d' -> pathToStr d'

instance Read ParsedPath where
    readPrec = parens $ do
        R.Ident "ParsedPath" <- lexP
        R.String str <- lexP
        maybe pfail return $ parsePath str

-- | Parse derivation path string for extended key.
-- Forms: /m\/0'\/2/, /M\/2\/3\/4/.
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

-- | Concatenate derivation path indices into a derivation path.
concatBip32Segments :: [Bip32PathIndex] -> DerivPath
concatBip32Segments = foldl' appendBip32Segment Deriv

-- | Append an extra derivation path index element into an existing path.
appendBip32Segment :: DerivPath -> Bip32PathIndex  -> DerivPath
appendBip32Segment d (Bip32SoftIndex i) = d :/ i
appendBip32Segment d (Bip32HardIndex i) = d :| i

-- | Parse a BIP32 derivation path index element from a string.
parseBip32PathIndex :: String -> Maybe Bip32PathIndex
parseBip32PathIndex segment = case reads segment of
    [(i, "" )] -> guard (is31Bit i) >> return (Bip32SoftIndex i)
    [(i, "'")] -> guard (is31Bit i) >> return (Bip32HardIndex i)
    _          -> Nothing

-- | Type for BIP32 path index element.
data Bip32PathIndex = Bip32HardIndex KeyIndex
                    | Bip32SoftIndex KeyIndex
  deriving Eq

instance Show Bip32PathIndex where
    showsPrec d (Bip32HardIndex i) = showParen (d > 10) $
        showString "Bip32HardIndex " . shows i
    showsPrec d (Bip32SoftIndex i) = showParen (d > 10) $
        showString "Bip32SoftIndex " . shows i

instance Read Bip32PathIndex where
    readPrec = h <|> s
      where
        h =
            parens $ do
                R.Ident "Bip32HardIndex" <- lexP
                R.Number n <- lexP
                maybe pfail return $
                    Bip32HardIndex . fromIntegral <$> numberToInteger n
        s =
            parens $ do
                R.Ident "Bip32SoftIndex" <- lexP
                R.Number n <- lexP
                maybe pfail return $
                    Bip32SoftIndex . fromIntegral <$> numberToInteger n

-- | Test whether the number could be a valid BIP32 derivation index.
is31Bit :: (Integral a) => a -> Bool
is31Bit i = i >= 0 && i < 0x80000000

-- | Helper function to parse a hard path.
parseHard :: String -> Maybe HardPath
parseHard = toHard . getParsedPath <=< parsePath

-- | Helper function to parse a soft path.
parseSoft :: String -> Maybe SoftPath
parseSoft = toSoft . getParsedPath <=< parsePath

-- | Data type representing a private or public key with its respective network.
data XKey
    = XPrv { getXKeyPrv :: !XPrvKey
           , getXKeyNet :: !Network }
    | XPub { getXKeyPub :: !XPubKey
           , getXKeyNet :: !Network }
    deriving (Eq, Show)

-- | Apply a parsed path to an extended key to derive the new key defined in the
-- path. If the path starts with /m/, a private key will be returned and if the
-- path starts with /M/, a public key will be returned. Private derivations on a
-- public key, and public derivations with a hard segment, return an error
-- value.
applyPath :: ParsedPath -> XKey -> Either String XKey
applyPath path key =
    case (path, key) of
        (ParsedPrv _, XPrv k n) -> return $ XPrv (derivPrvF k) n
        (ParsedPrv _, XPub {}) -> Left "applyPath: Invalid public key"
        (ParsedPub _, XPrv k n) -> return $ XPub (deriveXPubKey (derivPrvF k)) n
        (ParsedPub _, XPub k n) -> derivPubFE >>= \f -> return $ XPub (f k) n
    -- For empty parsed paths, we take a hint from the provided key
        (ParsedEmpty _, XPrv k n) -> return $ XPrv (derivPrvF k) n
        (ParsedEmpty _, XPub k n) -> derivPubFE >>= \f -> return $ XPub (f k) n
  where
    derivPrvF = goPrv id $ getParsedPath path
    derivPubFE = goPubE id $ getParsedPath path
    -- Build the full private derivation function starting from the end
    goPrv f p =
        case p of
            next :| i -> goPrv (f . flip hardSubKey i) next
            next :/ i -> goPrv (f . flip prvSubKey i) next
            Deriv     -> f
    -- Build the full public derivation function starting from the end
    goPubE f p =
        case p of
            next :/ i -> goPubE (f . flip pubSubKey i) next
            Deriv     -> Right f
            _         -> Left "applyPath: Invalid hard derivation"

{- Helpers for derivation paths and addresses -}

-- | Derive an address from a given parent path.
derivePathAddr :: XPubKey -> SoftPath -> KeyIndex -> (Address, PubKey)
derivePathAddr key path = deriveAddr (derivePubPath path key)

-- | Cyclic list of all addresses derived from a given parent path and starting
-- from the given offset index.
derivePathAddrs ::
       XPubKey -> SoftPath -> KeyIndex -> [(Address, PubKey, KeyIndex)]
derivePathAddrs key path = deriveAddrs (derivePubPath path key)

-- | Derive a multisig address from a given parent path. The number of required
-- signatures (m in m of n) is also needed.
derivePathMSAddr ::
       [XPubKey]
    -> SoftPath
    -> Int
    -> KeyIndex
    -> (Address, RedeemScript)
derivePathMSAddr keys path =
    deriveMSAddr $ map (derivePubPath path) keys

-- | Cyclic list of all multisig addresses derived from a given parent path and
-- starting from the given offset index. The number of required signatures
-- (m in m of n) is also needed.
derivePathMSAddrs ::
       [XPubKey]
    -> SoftPath
    -> Int
    -> KeyIndex
    -> [(Address, RedeemScript, KeyIndex)]
derivePathMSAddrs keys path =
    deriveMSAddrs $ map (derivePubPath path) keys

{- Utilities for extended keys -}

-- | De-serialize HDW-specific private key.
getPadPrvKey :: Get SecKey
getPadPrvKey = do
    pad <- getWord8
    unless (pad == 0x00) $ fail "Private key must be padded with 0x00"
    secKeyGet

-- | Serialize HDW-specific private key.
putPadPrvKey :: Putter SecKey
putPadPrvKey p = putWord8 0x00 >> secKeyPut p

bsPadPrvKey :: SecKey -> ByteString
bsPadPrvKey = runPut . putPadPrvKey
