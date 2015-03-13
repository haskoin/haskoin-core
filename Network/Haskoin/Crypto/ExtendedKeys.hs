module Network.Haskoin.Crypto.ExtendedKeys
( XPubKey(..)
, XPrvKey(..)
, XKey(..)
, ChainCode
, KeyIndex
, DerivationException(..)
, makeXPrvKey
, deriveXPubKey
, prvSubKey
, pubSubKey
, primeSubKey
, xPrvIsPrime
, xPubIsPrime
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
, primeSubKeys
, deriveAddr
, deriveAddrs
, deriveMSAddr
, deriveMSAddrs
, cycleIndex
  -- Custom derivations
, DerivPath(..)
, parsePath
, derivePath
, derivePubPath
) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData, rnf)
import Control.Monad (mzero, guard, unless)
import Control.Exception (Exception, throw)

import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON, withText)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8, getWord32be)
import Data.Binary.Put (Put, putWord8, putWord32be)
import Data.Word (Word8, Word32)
import Data.Bits (shiftR, setBit, testBit, clearBit)
import Data.List (intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Typeable (Typeable)
import qualified Data.Text as T (pack, unpack)
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
    toJSON = String . T.pack . xPrvExport

instance FromJSON XPrvKey where
    parseJSON = withText "xprvkey" $ maybe mzero return . xPrvImport . T.unpack

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
    toJSON = String . T.pack . xPubExport

instance FromJSON XPubKey where
    parseJSON = withText "xpubkey" $ maybe mzero return . xPubImport . T.unpack

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

-- | Compute a private, non-prime child key derivation. A private non-prime
-- derivation will allow the equivalent extended public key to derive the
-- public key for this child. Given a parent key /m/ and a derivation index /i/,
-- this function will compute m\/i\/. 
--
-- Non-prime derivations allow for more flexibility such as read-only wallets.
-- However, care must be taken not the leak both the parent extended public
-- key and one of the extended child private keys as this would compromise the
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

-- | Compute a public, non-prime child key derivation. Given a parent key /M/
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

-- | Compute a prime child key derivation. Prime derivations can only be
-- computed for private keys. Prime derivations do not allow the parent 
-- public key to derive the child public keys. However, they are safer as
-- a breach of the parent public key and child private keys does not lead
-- to a breach of the parent private key. Given a parent key /m/ and a
-- derivation index /i/, this function will compute m\/i'\/.
primeSubKey :: XPrvKey  -- ^ Extended Parent private key
            -> KeyIndex -- ^ Child derivation index
            -> XPrvKey  -- ^ Extended child private key
primeSubKey xkey child
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

-- | Returns True if the extended private key was derived through a prime
-- derivation.
xPrvIsPrime :: XPrvKey -> Bool
xPrvIsPrime k = testBit (xPrvIndex k) 31

-- | Returns True if the extended public key was derived through a prime
-- derivation.
xPubIsPrime :: XPubKey -> Bool
xPubIsPrime k = testBit (xPubIndex k) 31

-- | Returns the derivation index of this extended private key without the
-- prime bit set.
xPrvChild :: XPrvKey -> KeyIndex
xPrvChild k = clearBit (xPrvIndex k) 31

-- | Returns the derivation index of this extended public key without the prime
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

-- | Cyclic list of all private non-prime child key derivations of a parent key
-- starting from an offset index.
prvSubKeys :: XPrvKey -> KeyIndex -> [(XPrvKey, KeyIndex)]
prvSubKeys k = map (\i -> (prvSubKey k i, i)) . cycleIndex

-- | Cyclic list of all public non-prime child key derivations of a parent key
-- starting from an offset index.
pubSubKeys :: XPubKey -> KeyIndex -> [(XPubKey, KeyIndex)]
pubSubKeys k = map (\i -> (pubSubKey k i, i)) . cycleIndex

-- | Cyclic list of all prime child key derivations of a parent key starting
-- from an offset index.
primeSubKeys :: XPrvKey -> KeyIndex -> [(XPrvKey, KeyIndex)]
primeSubKeys k = map (\i -> (primeSubKey k i, i)) . cycleIndex

-- | Derive an address from a public key and an index. The derivation type
-- is a public, non-prime derivation.
deriveAddr :: XPubKey -> KeyIndex -> Address
deriveAddr k = xPubAddr . pubSubKey k

-- | Cyclic list of all addresses derived from a public key starting from an
-- offset index. The derivation types are public, non-prime derivations.
deriveAddrs :: XPubKey -> KeyIndex -> [(Address, KeyIndex)]
deriveAddrs k = map (\i -> (deriveAddr k i, i)) . cycleIndex

-- | Derive a multisig address from a list of public keys, the number of
-- required signatures (m) and a derivation index. The derivation type is a
-- public, non-prime derivation.
deriveMSAddr :: [XPubKey] -> Int -> KeyIndex -> Address
deriveMSAddr keys m i = 
    scriptAddr $ sortMulSig $ PayMulSig (map (toPubKeyG . xPubKey) k) m
  where
    k = map (flip pubSubKey i) keys

-- | Cyclic list of all multisig addresses derived from a list of public keys,
-- a number of required signatures (m) and starting from an offset index. The
-- derivation type is a public, non-prime derivation.
deriveMSAddrs :: [XPubKey] -> Int -> KeyIndex -> [(Address, KeyIndex)]
deriveMSAddrs keys m = map (\i -> (deriveMSAddr keys m i, i)) . cycleIndex

cycleIndex :: KeyIndex -> [KeyIndex]
cycleIndex i
    | i == 0         = cycle [0..0x7fffffff]
    | i < 0x80000000 = cycle $ [i..0x7fffffff] ++ [0..(i-1)]
    | otherwise      = error $ "cycleIndex: invalid index " ++ (show i)

{- Custom derivations -}

-- | Any extended key.
data XKey = XKeyPrv { xKeyPrv :: XPrvKey }
          | XKeyPub { xKeyPub :: XPubKey }
          deriving (Eq, Show)

-- | Derivation path.
data DerivPath
    = DerivPrv [(KeyIndex, Bool)]
    | DerivPub [(KeyIndex, Bool)]
    deriving (Eq, Show, Read)

instance IsString DerivPath where
    fromString = fromMaybe (error "Invalid derivation path") . parsePath

instance FromJSON DerivPath where
    parseJSON = withText "DerivPath" $ maybe mzero return . parsePath . T.unpack

instance ToJSON DerivPath where
    toJSON dp = case dp of
        DerivPrv path      -> g $ "m" : map f path
        DerivPub path      -> g $ "M" : map f path
      where
        f (i, p) = show i ++ if p then "'" else ""
        g        = String . T.pack . concat . intersperse "/"


-- | Parse derivation path string for extended key.
-- Forms: “m/0'/2”, “M/2/3/4”.
parsePath :: String -> Maybe DerivPath
parsePath s = case x of
    "m" -> DerivPrv <$> mapM f xs
    "M" -> DerivPub <$> mapM f xs
    _ -> Nothing
  where
    (x : xs) = splitOn "/" s
    f y = case reads y of
        [(n, "" )] -> flip (,) False <$> v n
        [(n, "'")] -> flip (,) True  <$> v n
        _ -> Nothing
    v z = guard (z >= 0 && z < 2 ^ (31 :: Int)) >> return (fromInteger z)

-- | Derive private key into private or public key using derivation path.
derivePath :: DerivPath -> XPrvKey -> XKey
derivePath (DerivPrv path) xprv =
    XKeyPrv $ derivePrvPath path xprv
derivePath (DerivPub path) xprv =
    XKeyPub $ deriveXPubKey $ derivePrvPath path xprv

-- | Derive private key using derivation path components.
derivePrvPath :: [(KeyIndex, Bool)] -> XPrvKey -> XPrvKey
derivePrvPath path xprv = 
    foldl f xprv path 
  where
    f xkey (i, p) = if p then primeSubKey xkey i else prvSubKey xkey i

-- | Derive public key using derivation path.
derivePubPath :: DerivPath -> XPubKey -> Maybe XPubKey
derivePubPath (DerivPub path) xpub = do
    guard $ null $ filter snd path
    return $ foldl pubSubKey xpub $ map fst path
derivePubPath _ _ = Nothing

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

