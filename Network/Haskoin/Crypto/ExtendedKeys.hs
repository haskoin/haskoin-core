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
  -- Custom derivations
, DerivPathI(..)
, HardOrMixed
, MixedOrSoft
, DerivPath
, HardPath
, SoftPath
, parsePath
, parseHard
, parseSoft
, toHard
, toSoft
, derivePath
, derivePubPath
, derivePathE
, derivePathAddr
, derivePathAddrs
, derivePathMSAddr
, derivePathMSAddrs
) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData, rnf)
import Control.Monad (mzero, guard, unless, (<=<))
import Control.Exception (Exception, throw)

import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON, withText)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8, getWord32be)
import Data.Binary.Put (Put, putWord8, putWord32be)
import Data.Word (Word8, Word32)
import Data.Bits (shiftR, setBit, testBit, clearBit)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Typeable (Typeable)
import Data.Text (pack, unpack)
import qualified Data.ByteString as BS (ByteString, append)

import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Script.Parser
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.Curve
import Network.Haskoin.Crypto.Point

{- See BIP32 for details: https://en.bitcoin.it/wiki/BIP_0032 -}

-- | A derivation exception is thrown in the very unlikely event that a
-- derivation is invalid.
data DerivationException = DerivationException String
    deriving (Eq, Read, Show, Typeable)

instance Exception DerivationException

type ChainCode = Word256
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
    } deriving (Eq, Show, Read)

instance NFData XPrvKey where
    rnf (XPrvKey d p i c k) =
        rnf d `seq` rnf p `seq` rnf i `seq` rnf c `seq` rnf k

instance ToJSON XPrvKey where
    toJSON = String . pack . xPrvExport

instance FromJSON XPrvKey where
    parseJSON = withText "xprvkey" $ maybe mzero return . xPrvImport . unpack

-- | Data type representing an extended BIP32 public key.
data XPubKey = XPubKey
    { xPubDepth  :: !Word8     -- ^ Depth in the tree of key derivations.
    , xPubParent :: !Word32    -- ^ Fingerprint of the parent key.
    , xPubIndex  :: !KeyIndex  -- ^ Key derivation index.
    , xPubChain  :: !ChainCode -- ^ Chain code.
    , xPubKey    :: !PubKeyC   -- ^ The public key of this extended key node.
    } deriving (Eq, Show, Read)

instance NFData XPubKey where
    rnf (XPubKey d p i c k) =
        rnf d `seq` rnf p `seq` rnf i `seq` rnf c `seq` rnf k

instance ToJSON XPubKey where
    toJSON = String . pack . xPubExport

instance FromJSON XPubKey where
    parseJSON = withText "xpubkey" $ maybe mzero return . xPubImport . unpack

-- | Build a BIP32 compatible extended private key from a bytestring. This will
-- produce a root node (depth=0 and parent=0).
makeXPrvKey :: BS.ByteString -> XPrvKey
makeXPrvKey bs = 
    XPrvKey 0 0 0 c pk
  where 
    (p,c) = split512 $ hmac512 "Bitcoin seed" bs
    pk    = fromMaybe err $ makePrvKeyC $ fromIntegral p
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
    pK    = xPubKey $ deriveXPubKey xkey
    msg   = BS.append (encode' pK) (encode' child)
    (a,c) = split512 $ hmac512 (encode' $ xPrvChain xkey) msg
    k     = addPrvKeys (xPrvKey xkey) a

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
    msg   = BS.append (encode' $ xPubKey xKey) (encode' child)
    (a,c) = split512 $ hmac512 (encode' $ xPubChain xKey) msg
    pK    = addPubKeys (xPubKey xKey) a

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
    i     = setBit child 31
    msg   = BS.append (bsPadPrvKey $ xPrvKey xkey) (encode' i)
    (a,c) = split512 $ hmac512 (encode' $ xPrvChain xkey) msg
    k     = addPrvKeys (xPrvKey xkey) a

-- Add two private keys together. One of the keys is defined by a Word256. The
-- functions can only be called on compressed private keys and throws an
-- exception if the Word256 is smaller than the order of the curve N.
addPrvKeys :: PrvKeyC -> Word256 -> PrvKeyC
addPrvKeys key i
    | toInteger i < curveN = fromMaybe err $ makePrvKeyC $ toInteger r
    | otherwise = err
  where
    r   = (prvKeyFieldN key) + (fromIntegral i :: FieldN) 
    err = throw $ DerivationException "Invalid derivation"

-- Add a public key to a private key defined by its Word256 value. This will
-- transform the private key into a public key and add the respective public
-- key points together. This function only works for compressed keys and throws
-- an exception if the private key value is >= than the order of the curve N.
addPubKeys :: PubKeyC -> Word256 -> PubKeyC
addPubKeys pub i
    | toInteger i < curveN =
        if isInfPoint pt2 then err else makePubKeyC pt2
    | otherwise = err
  where
    pt1 = mulPoint (fromIntegral i :: FieldN) curveG
    pt2 = addPoint (pubKeyPoint pub) pt1
    err = throw $ DerivationException "Invalid derivation"

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
xPrvID :: XPrvKey -> Word160
xPrvID = xPubID . deriveXPubKey

-- | Computes the key identifier of an extended public key.
xPubID :: XPubKey -> Word160
xPubID = hash160 . hash256BS . encode' . xPubKey 

-- | Computes the key fingerprint of an extended private key.
xPrvFP :: XPrvKey -> Word32
xPrvFP = fromIntegral . (`shiftR` 128) . xPrvID

-- | Computes the key fingerprint of an extended public key.
xPubFP :: XPubKey -> Word32
xPubFP = fromIntegral . (`shiftR` 128) . xPubID

-- | Computer the 'Address' of an extended public key.
xPubAddr :: XPubKey -> Address
xPubAddr = pubKeyAddr . xPubKey

-- | Exports an extended private key to the BIP32 key export format (base 58).
xPrvExport :: XPrvKey -> String
xPrvExport = bsToString . encodeBase58Check . encode' 

-- | Exports an extended public key to the BIP32 key export format (base 58).
xPubExport :: XPubKey -> String
xPubExport = bsToString . encodeBase58Check . encode'

-- | Decodes a BIP32 encoded extended private key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPrvImport :: String -> Maybe XPrvKey
xPrvImport str = decodeToMaybe =<< (decodeBase58Check $ stringToBS str)

-- | Decodes a BIP32 encoded extended public key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPubImport :: String -> Maybe XPubKey
xPubImport str = decodeToMaybe =<< (decodeBase58Check $ stringToBS str)

-- | Export an extended private key to WIF (Wallet Import Format).
xPrvWif :: XPrvKey -> String
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
deriveAddr :: XPubKey -> KeyIndex -> Address
deriveAddr k = xPubAddr . pubSubKey k

-- | Cyclic list of all addresses derived from a public key starting from an
-- offset index. The derivation types are public, soft derivations.
deriveAddrs :: XPubKey -> KeyIndex -> [(Address, KeyIndex)]
deriveAddrs k = map (\i -> (deriveAddr k i, i)) . cycleIndex

-- | Derive a multisig address from a list of public keys, the number of
-- required signatures (m) and a derivation index. The derivation type is a
-- public, soft derivation.
deriveMSAddr :: [XPubKey] -> Int -> KeyIndex -> Address
deriveMSAddr keys m i = 
    scriptAddr $ sortMulSig $ PayMulSig (map (toPubKeyG . xPubKey) k) m
  where
    k = map (flip pubSubKey i) keys

-- | Cyclic list of all multisig addresses derived from a list of public keys,
-- a number of required signatures (m) and starting from an offset index. The
-- derivation type is a public, soft derivation.
deriveMSAddrs :: [XPubKey] -> Int -> KeyIndex -> [(Address, KeyIndex)]
deriveMSAddrs keys m = map (\i -> (deriveMSAddr keys m i, i)) . cycleIndex

cycleIndex :: KeyIndex -> [KeyIndex]
cycleIndex i
    | i == 0         = cycle [0..0x7fffffff]
    | i < 0x80000000 = cycle $ [i..0x7fffffff] ++ [0..(i-1)]
    | otherwise      = error $ "cycleIndex: invalid index " ++ (show i)

{- Custom derivations -}

data Hard
data Mixed
data Soft

type HardPath = DerivPathI Hard
type DerivPath = DerivPathI Mixed
type SoftPath = DerivPathI Soft

class HardOrMixed a 
instance HardOrMixed Hard
instance HardOrMixed Mixed

class MixedOrSoft a 
instance MixedOrSoft Mixed
instance MixedOrSoft Soft

data DerivPathI t where
    (:|) :: HardOrMixed t => !HardPath -> !KeyIndex -> DerivPathI t
    (:/) :: MixedOrSoft t => !(DerivPathI t) -> !KeyIndex -> DerivPathI t
    Deriv :: DerivPathI t
    DerivPrv :: DerivPathI t
    DerivPub :: DerivPathI t

instance Show (DerivPathI t) where
    show (next :| i) = unwords [ show next, ":|", show i ]
    show (next :/ i) = unwords [ show next, ":/", show i ]
    show Deriv = "Deriv"
    show DerivPrv = "DerivPrv"
    show DerivPub = "DerivPub"

instance Eq (DerivPathI t) where
    (nextA :| iA) == (nextB :| iB) = iA == iB && nextA == nextB
    (nextA :/ iA) == (nextB :/ iB) = iA == iB && nextA == nextB
    Deriv         == Deriv         = True
    DerivPrv      == DerivPrv      = True
    DerivPub      == DerivPub      = True
    _             == _             = False

instance IsString DerivPath where
    fromString str = case parsePath str of
        Just p -> p
        _ -> error "Invalid derivation path"

instance IsString HardPath where
    fromString str = case parseHard str of
        Just p -> p
        _ -> error "Invalid hard derivation path"

instance IsString SoftPath where
    fromString str = case parseSoft str of
        Just p -> p
        _ -> error "Invalid soft derivation path"

instance FromJSON DerivPath where
    parseJSON = withText "DerivPath" $ \str -> case parsePath $ unpack str of
        Just p -> return p
        _      -> mzero

instance FromJSON HardPath where
    parseJSON = withText "HardPath" $ \str -> case parseHard $ unpack str of
        Just p -> return p
        _      -> mzero

instance FromJSON SoftPath where
    parseJSON = withText "SoftPath" $ \str -> case parseSoft $ unpack str of
        Just p -> return p
        _      -> mzero

instance ToJSON (DerivPathI t) where
    toJSON = String . pack . go 
      where
        go :: DerivPathI t -> String
        go p = case p of
            next :| i -> concat [ go next, "/", show i, "'" ]
            next :/ i -> concat [ go next, "/", show i ]
            Deriv     -> ""
            DerivPrv  -> "m"
            DerivPub  -> "M"

-- | Parse derivation path string for extended key.
-- Forms: “m/0'/2”, “M/2/3/4”.
parsePath :: String -> Maybe DerivPath
parsePath str = do
    ds <- reverse <$> mapM f xs
    let (s,h) = break fst ds
    -- No soft derivations in the hard branch
    guard $ null $ filter (not . fst) h
    hPath <- pHard $ map snd h
    pSoft hPath $ map snd s
  where
    (x:xs) = splitOn "/" str
    f deriv = case reads deriv of
        [(i, "" )] -> (,) False <$> g i
        [(i, "'")] -> (,) True <$> g i
        _ -> Nothing
    g i = guard (i >=0 && i < 0x80000000) >> return i
    pSoft h (i:is) = (:/ i) <$> pSoft h is
    pSoft h [] = return h
    pHard :: HardOrMixed t => [KeyIndex] -> Maybe (DerivPathI t)
    pHard (i:is) = (:| i) <$> pHard is
    pHard [] = pEnd
    pEnd :: Maybe (DerivPathI t)
    pEnd = case x of
        ""  -> Just Deriv
        "m" -> Just DerivPrv
        "M" -> Just DerivPub
        _   -> Nothing

parseHard :: String -> Maybe HardPath
parseHard = toHard <=< parsePath

parseSoft :: String -> Maybe SoftPath
parseSoft = toSoft <=< parsePath

toHard :: DerivPath -> Maybe HardPath
toHard p = case p of
    _ :/ _    -> Nothing
    next :| i -> Just $ next :| i
    Deriv     -> Just Deriv
    DerivPrv  -> Just DerivPrv
    DerivPub  -> Just DerivPub

toSoft :: DerivPath -> Maybe SoftPath
toSoft p = case p of
    _ :| _    -> Nothing
    next :/ i -> (:/ i) <$> toSoft next
    Deriv     -> Just Deriv
    DerivPrv  -> Just DerivPrv
    DerivPub  -> Just DerivPub

-- | Derive a private key from a derivation path
derivePath :: DerivPathI t -> XPrvKey -> XPrvKey
derivePath path key = 
    go id path $ key
  where
    -- Build the full derivation function starting from the end
    go :: (XPrvKey -> XPrvKey) -> DerivPathI t -> (XPrvKey -> XPrvKey)
    go f p = case p of
        next :| i -> go (f . flip hardSubKey i) next
        next :/ i -> go (f . flip prvSubKey i) next 
        _         -> f

-- | Derive a public key from a soft derivation path
derivePubPath :: SoftPath -> XPubKey -> XPubKey
derivePubPath path key = 
    go id path $ key
  where
    -- Build the full derivation function starting from the end
    go f p = case p of
        next :/ i -> go (f . flip pubSubKey i) next 
        _         -> f

-- | Derive a key from a derivation path and return either a private or public
-- key depending on the initial derivation constructor. If you parsed a string
-- as m/ you will get a private key and if you parsed a string as M/ you will
-- get a public key. If you used the neutral derivation constructor `Deriv`, a
-- private key will be returned. 
derivePathE :: DerivPathI t -> XPrvKey -> Either XPubKey XPrvKey
derivePathE path key = 
    go id path $ key
  where
    -- Build the full derivation function starting from the end
    go :: (XPrvKey -> XPrvKey) 
       -> DerivPathI t 
       -> (XPrvKey -> Either XPubKey XPrvKey) 
    go f p = case p of
        next :| i -> go (f . flip hardSubKey i) next
        next :/ i -> go (f . flip prvSubKey i) next 
        -- Derive a public key as the last function
        DerivPub  -> Left . deriveXPubKey . f
        _         -> Right . f

-- | Derive an address from a given parent path.
derivePathAddr :: XPubKey -> SoftPath -> KeyIndex -> Address
derivePathAddr key path i = deriveAddr (derivePubPath path key) i

-- | Cyclic list of all addresses derived from a given parent path and starting
-- from the given offset index.
derivePathAddrs :: XPubKey -> SoftPath -> KeyIndex -> [(Address, KeyIndex)]
derivePathAddrs key path i = deriveAddrs (derivePubPath path key) i

-- | Derive a multisig address from a given parent path. The number of required
-- signatures (m in m of n) is also needed.
derivePathMSAddr :: [XPubKey] -> SoftPath -> Int -> KeyIndex -> Address
derivePathMSAddr keys path m i = 
    deriveMSAddr (map (derivePubPath path) keys) m i

-- | Cyclic list of all multisig addresses derived from a given parent path and
-- starting from the given offset index. The number of required signatures
-- (m in m of n) is also needed.
derivePathMSAddrs :: [XPubKey] -> SoftPath -> Int -> KeyIndex 
                  -> [(Address, KeyIndex)]
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

bsPadPrvKey :: PrvKeyC -> BS.ByteString
bsPadPrvKey = runPut' . putPadPrvKey 

