module Network.Haskoin.Crypto.ExtendedKeys
( XPubKey(..)
, XPrvKey(..)
, XKey(..)
, ChainCode
, DerivPath(..)
, makeXPrvKey
, deriveXPubKey
, prvSubKey
, pubSubKey
, primeSubKey
, prvSubKeys
, pubSubKeys
, parsePath
, derivePath
, derivePubPath
, primeSubKeys
, mulSigSubKey
, mulSigSubKeys
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
, cycleIndex
, cycleIndex'
) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData, rnf)
import Control.Monad (mzero, guard, unless, liftM2, foldM)

import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON, withText)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8, getWord32be)
import Data.Binary.Put (Put, putWord8, putWord32be)
import Data.Word (Word8, Word32)
import Data.Bits (shiftR, setBit, testBit, clearBit)
import Data.List (intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromJust)
import Data.String (IsString, fromString)
import qualified Data.Text as T (pack, unpack)
import qualified Data.ByteString as BS (ByteString, append)

import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Crypto.Base58
import Network.Haskoin.Crypto.BigWord
import Network.Haskoin.Crypto.Curve
import Network.Haskoin.Crypto.Point

{- See BIP32 for details: https://en.bitcoin.it/wiki/BIP_0032 -}

type ChainCode = Word256

-- | Data type representing an extended BIP32 private key. An extended key
-- is a node in a tree of key derivations. It has a depth in the tree, a 
-- parent node and an index to differentiate it from other siblings.
data XPrvKey = XPrvKey
    { xPrvDepth  :: !Word8     -- ^ Depth in the tree of key derivations.
    , xPrvParent :: !Word32    -- ^ Fingerprint of the parent key.
    , xPrvIndex  :: !Word32    -- ^ Key derivation index.
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
    , xPubIndex  :: !Word32    -- ^ Key derivation index.
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

-- | Any extended key.
data XKey = XKeyPrv { xKeyPrv :: XPrvKey }
          | XKeyPub { xKeyPub :: XPubKey }
          deriving (Eq, Show)

-- | Derivation path.
data DerivPath
    = DerivPrv [(Word32, Bool)]
    | DerivPub [(Word32, Bool)]
    | DerivNonPrime [Word32]
    deriving (Eq, Show, Read)

instance IsString DerivPath where
    fromString = fromJust . parsePath

instance FromJSON DerivPath where
    parseJSON = withText "DerivPath" $ maybe mzero return . parsePath . T.unpack

instance ToJSON DerivPath where
    toJSON dp = case dp of
        DerivPrv path      -> g $ "m" : map f path
        DerivPub path      -> g $ "M" : map f path
        DerivNonPrime path -> g $ "M" : map show path
      where
        f (i, p) = show i ++ if p then "'" else ""
        g        = String . T.pack . concat . intersperse "/"

-- | Build a BIP32 compatible extended private key from a bytestring. This will
-- produce a root node (depth=0 and parent=0).
makeXPrvKey :: BS.ByteString -> Maybe XPrvKey
makeXPrvKey bs = do
    pk' <- makePrvKeyC $ fromIntegral pk
    return $ XPrvKey 0 0 0 c pk'
    where (pk,c) = split512 $ hmac512 (stringToBS "Bitcoin seed") bs

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
prvSubKey :: XPrvKey       -- ^ Extended parent private key
          -> Word32        -- ^ Child derivation index
          -> Maybe XPrvKey -- ^ Extended child private key 
prvSubKey xkey child = guardIndex child >> do
    k <- addPrvKeys (xPrvKey xkey) a
    return $ XPrvKey (xPrvDepth xkey + 1) (xPrvFP xkey) child c k
    where pK    = xPubKey $ deriveXPubKey xkey
          msg   = BS.append (encode' pK) (encode' child)
          (a,c) = split512 $ hmac512 (encode' $ xPrvChain xkey) msg

-- | Compute a public, non-prime child key derivation. Given a parent key /M/
-- and a derivation index /i/, this function will compute M\/i\/. 
pubSubKey :: XPubKey       -- ^ Extended Parent public key
          -> Word32        -- ^ Child derivation index
          -> Maybe XPubKey -- ^ Extended child public key
pubSubKey xKey child = guardIndex child >> do
    pK <- addPubKeys (xPubKey xKey) a
    return $ XPubKey (xPubDepth xKey + 1) (xPubFP xKey) child c pK
    where msg   = BS.append (encode' $ xPubKey xKey) (encode' child)
          (a,c) = split512 $ hmac512 (encode' $ xPubChain xKey) msg

-- | Compute a prime child key derivation. Prime derivations can only be
-- computed for private keys. Prime derivations do not allow the parent 
-- public key to derive the child public keys. However, they are safer as
-- a breach of the parent public key and child private keys does not lead
-- to a breach of the parent private key. Given a parent key /m/ and a
-- derivation index /i/, this function will compute m\/i'\/.
primeSubKey :: XPrvKey       -- ^ Extended Parent private key
            -> Word32        -- ^ Child derivation index
            -> Maybe XPrvKey -- ^ Extended child private key
primeSubKey xkey child = guardIndex child >> do
    k  <- addPrvKeys (xPrvKey xkey) a
    return $ XPrvKey (xPrvDepth xkey + 1) (xPrvFP xkey) i c k
    where i     = setBit child 31
          msg   = BS.append (bsPadPrvKey $ xPrvKey xkey) (encode' i)
          (a,c) = split512 $ hmac512 (encode' $ xPrvChain xkey) msg

-- Add two private keys together. One of the keys is defined by a Word256.
-- The functions fails on uncompressed private keys and return Nothing if the
-- Word256 is smaller than the order of the curve N.
addPrvKeys :: PrvKeyC -> Word256 -> Maybe PrvKeyC
addPrvKeys key i
    | toInteger i < curveN =
        let r = (prvKeyFieldN key) + (fromIntegral i :: FieldN) 
            in makePrvKeyC $ toInteger r
    | otherwise = Nothing

-- Add a public key to a private key defined by its Word256 value. This will
-- transform the private key into a public key and add the respective public
-- key points together. This function fails for uncompressed keys and returns
-- Nothing if the private key value is >= than the order of the curve N.
addPubKeys :: PubKeyC -> Word256 -> Maybe PubKeyC
addPubKeys pub i
    | toInteger i < curveN =
        let pt1 = mulPoint (fromIntegral i :: FieldN) curveG
            pt2 = addPoint (pubKeyPoint pub) pt1
            in if isInfPoint pt2 then Nothing
                                 else Just $ makePubKeyC pt2
    | otherwise = Nothing


-- | Cyclic list of all private non-prime child key derivations of a parent key
-- starting from an offset index.
prvSubKeys :: XPrvKey -> Word32 -> [(XPrvKey,Word32)]
prvSubKeys k i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (prvSubKey k j) (return j)

-- | Cyclic list of all public non-prime child key derivations of a parent key
-- starting from an offset index.
pubSubKeys :: XPubKey -> Word32 -> [(XPubKey,Word32)]
pubSubKeys k i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (pubSubKey k j) (return j)

-- | Cyclic list of all prime child key derivations of a parent key starting
-- from an offset index.
primeSubKeys :: XPrvKey -> Word32 -> [(XPrvKey,Word32)]
primeSubKeys k i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (primeSubKey k j) (return j)

-- | Compute a public, non-prime subkey derivation for all of the parent public
-- keys in the input. This function will succeed only if the child key
-- derivations for all the parent keys are valid. 
--
-- This function is intended to be used in the context of multisignature
-- accounts. Parties exchanging their master public keys to create a
-- multisignature account can then individually generate all the receiving
-- multisignature addresses without further communication.
mulSigSubKey :: [XPubKey]       -- ^ List of extended parent public keys
             -> Word32          -- ^ Child key derivation index
             -> Maybe [XPubKey] -- ^ List of extended child public keys
mulSigSubKey pubs i = mapM (flip pubSubKey i) pubs

-- | Cyclic list of all public, non-prime multisig key derivations of a list
-- of parent keys starting from an offset index.
mulSigSubKeys :: [XPubKey] -> Word32 -> [([XPubKey],Word32)]
mulSigSubKeys pubs i = mapMaybe f $ cycleIndex i
    where f j = liftM2 (,) (mulSigSubKey pubs j) (return j)

cycleIndex :: Word32 -> [Word32]
cycleIndex i
    | i == 0         = cycle [0..0x7fffffff]
    | i < 0x80000000 = cycle $ [i..0x7fffffff] ++ [0..(i-1)]
    | otherwise      = error $ "cycleIndex: invalid index " ++ (show i)

-- Cycle in reverse
cycleIndex' :: Word32 -> [Word32]
cycleIndex' i
    | i == 0          = cycle $ 0 : [0x7fffffff,0x7ffffffe..1]
    | i == 0x7fffffff = cycle [0x7fffffff,0x7ffffffe..0]
    | i == 0x7ffffffe = cycle $ [0x7ffffffe,0x7ffffffd..0] ++ [0x7fffffff]
    | i < 0x80000000  = cycle $ [i,(i-1)..0] ++ [0x7fffffff,0x7ffffffe..(i+1)]
    | otherwise       = error $ "cycleIndex: invalid index " ++ (show i)

guardIndex :: Word32 -> Maybe ()
guardIndex child = guard $ child >= 0 && child < 0x80000000

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
xPrvChild :: XPrvKey -> Word32
xPrvChild k = clearBit (xPrvIndex k) 31

-- | Returns the derivation index of this extended public key without the prime
-- bit set.
xPubChild :: XPubKey -> Word32
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

-- | Parse derivation path string for extended key.
-- Forms: “m/0'/2”, “M/2/3/4”.
parsePath :: String -> Maybe DerivPath
parsePath s = case x of
    "m" -> DerivPrv <$> mapM f xs
    "M" -> mapM f xs >>= \paths -> if null $ filter snd paths
        then return $ DerivNonPrime $ map fst paths
        else return $ DerivPub paths
    _ -> Nothing
  where
    (x : xs) = splitOn "/" s
    f y = case reads y of
        [(n, "" )] -> flip (,) False <$> v n
        [(n, "'")] -> flip (,) True  <$> v n
        _ -> Nothing
    v z = guard (z >= 0 && z < 2 ^ (31 :: Int)) >> return (fromInteger z)

-- | Derive private key into private or public key using derivation path.
derivePath :: DerivPath -> XPrvKey -> Maybe XKey
derivePath (DerivPrv path) xprv =
    XKeyPrv <$> derivePrvPath path xprv
derivePath (DerivPub path) xprv =
    (XKeyPub . deriveXPubKey) <$> derivePrvPath path xprv
derivePath (DerivNonPrime path) xprv =
    (XKeyPub . deriveXPubKey) <$> foldM prvSubKey xprv path

-- | Derive private key using derivation path components.
derivePrvPath :: [(Word32, Bool)] -> XPrvKey -> Maybe XPrvKey
derivePrvPath path xprv = foldM f xprv path where
    f xp (i, p) = if p then primeSubKey xp i else prvSubKey xp i

-- | Derive public key using derivation path.
derivePubPath :: DerivPath -> XPubKey -> Maybe XPubKey
derivePubPath (DerivNonPrime path) xpub = foldM pubSubKey xpub path
derivePubPath _ _ = Nothing

