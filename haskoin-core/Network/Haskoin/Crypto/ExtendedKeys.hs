{-# LANGUAGE ScopedTypeVariables #-}

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
  -- Custom derivations -- todo, can we whittle down some of these exports?
, XKeyChildIndex
, XKeySoftIndex (..) 
, XKeyHardIndex (..)
-- , StartDerivPath (..)
, DerivPath (..)
, HardPath (..)
, SoftPath (..)
, Bip32Path (..)
, Bip32XKey (..)
-- , parsePath
-- , parseHard
-- , parseSoft
-- , (++/), (++|)
, deriveHardPrvPath 
, deriveSoftPrvPath
, derivePubPath 
, derivePathE -- deprecate? (un-evocative name)
, derivePath 
, deriveBip32Path


, derivePathAddr
, derivePathAddrs
, derivePathMSAddr
, derivePathMSAddrs

, hardPlusSoft
, softPlusSoft
, incrementHardPathEnd
-- , addPubPriv
-- , addPrivPriv
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (mzero, unless, (<=<))
import Control.Exception (Exception, throw)
import qualified Crypto.Secp256k1 as EC
import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON, withText)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8, getWord32be)
import Data.Binary.Put (Put, putWord8, putWord32be)
import Data.Word (Word8, Word32)
import Data.Bits (setBit, testBit, clearBit)
-- import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Data.Typeable (Typeable)
import Data.Text (unpack)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (ByteString, append, take)

import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Script.Parser
import Network.Haskoin.Crypto.Keys
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Crypto.Base58
import Data.List (foldl')
-- import Network.Haskoin.Crypto.ExtendedKeyHelpers
import Network.Haskoin.Crypto.BigWord (Word31, bigWordParser    )
import Data.Monoid (Endo(..), (<>))
import Text.Parsec
-- import Safe

{- See BIP32 for details: https://en.bitcoin.it/wiki/BIP_0032 -}

-- | A derivation exception is thrown in the very unlikely event that a
-- derivation is invalid.
data DerivationException = DerivationException String
    deriving (Eq, Read, Show, Typeable)

instance Exception DerivationException

type ChainCode = Hash256

type XKeyChildIndex = Word31 -- can we make this Word31 ?
type KeyIndex = Word32

-- | Data type representing an extended BIP32 private key. An extended key
-- is a node in a tree of key derivations. It has a depth in the tree, a
-- parent node and an index to differentiate it from other siblings.
data XPrvKey = XPrvKey
    { xPrvDepth  :: !Word8     -- ^ Depth in the tree of key derivations.
    , xPrvParent :: !Word32    -- ^ Fingerprint of the parent key. (same for corresponding xprv and xpub keys)
    , xPrvIndex  :: !KeyIndex  -- ^ Key derivation index.
    , xPrvChain  :: !ChainCode -- ^ Chain code. (same for corresponding xprv and xpub keys)
    , xPrvKey    :: !PrvKeyC   -- ^ The private key of this extended key node.
    } deriving (Eq, Show, Read)

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
    , xPubParent :: !Word32    -- ^ Fingerprint of the parent key. (same for corresponding xprv and xpub keys)
    , xPubIndex  :: !KeyIndex  -- ^ Key derivation index.
    , xPubChain  :: !ChainCode -- ^ Chain code. (same for corresponding xprv and xpub keys)
    , xPubKey    :: !PubKeyC   -- ^ The public key of this extended key node.
    } deriving (Eq, Show, Read)

instance NFData XPubKey where
    rnf (XPubKey d p i c k) =
        rnf d `seq` rnf p `seq` rnf i `seq` rnf c `seq` rnf k

instance ToJSON XPubKey where
    toJSON = String . cs . xPubExport

instance FromJSON XPubKey where
    parseJSON = withText "xpubkey" $ maybe mzero return . xPubImport . cs

-- | Build a BIP32 compatible extended private key from a bytestring. This will
-- produce a root node (depth=0 and parent=0).
makeXPrvKey :: BS.ByteString -> XPrvKey
makeXPrvKey bs =
    XPrvKey 0 0 0 c pk
  where
    (p,c) = split512 . hmac512 "Bitcoin seed" $ bs
    pk    = fromMaybe err $ makePrvKeyC <$> EC.secKey (getHash256 p)
    err   = throw $ DerivationException "Invalid seed"

-- | Derive an extended public key from an extended private key. This function
-- will preserve the depth, parent, index and chaincode fields of the extended
-- private keys.
deriveXPubKey :: XPrvKey -> XPubKey
deriveXPubKey (XPrvKey d p i c k) = XPubKey d p i c (derivePubKey k)


-- | deprecate in favor of xPrvKeyDeriveChild ?
-- todo: rename prvSoftSubKey 
prvSubKey :: XPrvKey  -- ^ Extended parent private key
          -> XKeySoftIndex -- ^ Child derivation index
          -> XPrvKey  -- ^ Extended child private key
prvSubKey xkey child' = (appEndo . deriveXPrvKeyEndo $ child' ) xkey



hardSubKey :: XPrvKey  -- ^ Extended Parent private key
            -> XKeyHardIndex -- ^ Child derivation index
            -> XPrvKey  -- ^ Extended child private key
hardSubKey xkey child' = (appEndo . deriveXPrvKeyEndo $ child' ) xkey


-- todo: rename pubSoftSubKey 
-- | deprecate in favor of xPrvKeyDeriveChild ?
pubSubKey :: XPubKey  -- ^ Extended Parent public key
          -> XKeySoftIndex -- ^ Child derivation index
          -> XPubKey  -- ^ Extended child public key
pubSubKey xkey child' = (appEndo . deriveXPubKeyEndo $ child' ) xkey

-- | Returns True if the extended private key was derived through a hard
-- derivation.
xPrvIsHard :: XPrvKey -> Bool
xPrvIsHard k = (`testBit` 31) . xPrvIndex $ k

-- | Returns True if the extended public key was derived through a hard
-- derivation.
xPubIsHard :: XPubKey -> Bool
xPubIsHard k = (`testBit` 31) . xPubIndex $ k

-- | Returns the derivation index of this extended private key without the hard
-- bit set.
xPrvChild :: XPrvKey -> KeyIndex
xPrvChild k = (`clearBit` 31) . xPrvIndex $ k -- only needed if Hard? 

-- | Returns the derivation index of this extended public key without the hard
-- bit set.
xPubChild :: XPubKey -> KeyIndex
xPubChild k = (`clearBit` 31) . xPubIndex $ k -- only needed if hard? 

-- | Computes the key identifier of an extended private key.
-- | ID is same for public and private keys
xPrvID :: XPrvKey -> Hash160
xPrvID = xPubID . deriveXPubKey

-- | Computes the key identifier of an extended public key.
xPubID :: XPubKey -> Hash160
xPubID = hash160 . getHash256 . hash256 . encode' . xPubKey

-- | Computes the key fingerprint of an extended private key.
-- | Fingerprint is same for public and private keys
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
prvSubKeys :: XPrvKey -> XKeySoftIndex -> [(XPrvKey, XKeySoftIndex)]
prvSubKeys k = map (\i -> (prvSubKey k i, i)) . cycleSoftIndex

-- | Cyclic list of all public soft child key derivations of a parent key
-- starting from an offset index.
pubSubKeys :: XPubKey -> XKeySoftIndex -> [(XPubKey, XKeySoftIndex)]
pubSubKeys k = map (\i -> (pubSubKey k i, i)) . cycleSoftIndex

-- | Cyclic list of all hard child key derivations of a parent key starting
-- from an offset index.
hardSubKeys :: XPrvKey -> XKeyHardIndex -> [(XPrvKey, XKeyHardIndex)]
hardSubKeys k = map (\i -> (hardSubKey k i, i)) . cycleHardIndex

-- | Derive an address from a public key and an index. The derivation type
-- is a public, soft derivation.
deriveAddr :: XPubKey -> XKeySoftIndex -> (Address, PubKeyC)
deriveAddr k i =
    (xPubAddr key, xPubKey key)
  where
    key = pubSubKey k i

-- | Cyclic list of all addresses derived from a public key starting from an
-- offset index. The derivation types are public, soft derivations.
deriveAddrs :: XPubKey -> XKeySoftIndex -> [(Address, PubKeyC, XKeySoftIndex)]
deriveAddrs k =
    map f . cycleSoftIndex
  where
    f i = let (a, key) = deriveAddr k i in (a, key, i)

-- | Derive a multisig address from a list of public keys, the number of
-- required signatures (m) and a derivation index. The derivation type is a
-- public, soft derivation.
deriveMSAddr :: [XPubKey] -> Int -> XKeySoftIndex -> (Address, RedeemScript)
deriveMSAddr keys m i =
    (scriptAddr rdm, rdm)
  where
    rdm = sortMulSig $ PayMulSig k m
    k   = map (toPubKeyG . xPubKey . flip pubSubKey i) keys

-- | Cyclic list of all multisig addresses derived from a list of public keys,
-- a number of required signatures (m) and starting from an offset index. The
-- derivation type is a public, soft derivation.
deriveMSAddrs :: [XPubKey] -> Int -> XKeySoftIndex
              -> [(Address, RedeemScript, XKeySoftIndex)]
deriveMSAddrs keys m =
    map f . cycleSoftIndex
  where
    f i = let (a, rdm) = deriveMSAddr keys m i in (a, rdm, i)


cycleIndex :: XKeyChildIndex -> [XKeyChildIndex]
cycleIndex i
    | i == 0         = cycle [0..maxBound]
    | i <= maxBound = cycle $ [i..maxBound] ++ [0..(i-1)]
    | otherwise      = error $ "cycleIndex: invalid index " ++ (show i)

cycleHardIndex :: XKeyHardIndex -> [XKeyHardIndex]
cycleHardIndex (XKeyHardIndex ci) = map XKeyHardIndex . cycleIndex $ ci
cycleSoftIndex :: XKeySoftIndex -> [XKeySoftIndex]
cycleSoftIndex (XKeySoftIndex ci) = map XKeySoftIndex . cycleIndex $ ci

newtype XKeyHardIndex = XKeyHardIndex XKeyChildIndex
  deriving (Read,Show,Eq,Ord)

instance Enum XKeyHardIndex where 
  toEnum i = XKeyHardIndex . toEnum $ i
  fromEnum (XKeyHardIndex ci) = fromEnum ci 

instance Bounded XKeyHardIndex where
  minBound = XKeyHardIndex minBound
  maxBound = XKeyHardIndex maxBound

newtype XKeySoftIndex = XKeySoftIndex XKeyChildIndex
  deriving (Read,Show,Eq,Ord)

instance Enum XKeySoftIndex where 
  toEnum i = XKeySoftIndex . toEnum $ i
  fromEnum (XKeySoftIndex ci) = fromEnum ci 

instance Bounded XKeySoftIndex where
  minBound = XKeySoftIndex minBound
  maxBound = XKeySoftIndex maxBound

class XPrvKeyEndoDerivable x where
  deriveXPrvKeyEndo :: x -> Endo XPrvKey

instance NFData XKeySoftIndex where 
  rnf ( XKeySoftIndex i ) = rnf i
    
instance NFData XKeyHardIndex where 
  rnf ( XKeyHardIndex i ) = rnf i

-- | Compute a hard child key derivation. Hard derivations can only be computed
-- for private keys. Hard derivations do not allow the parent public key to
-- derive the child public keys. However, they are safer as a breach of the
-- parent public key and child private keys does not lead to a breach of the
-- parent private key. Given a parent key /m/ and a derivation index /i/, this
-- function will compute m\/i'\/.\
instance XPrvKeyEndoDerivable XKeyHardIndex
  where deriveXPrvKeyEndo xk@(XKeyHardIndex child') =
          Endo $ \xkey ->
                    let i     = (`setBit` 31) . fromIntegral . toInteger $ child'
                        parentXPrivKey = xkey 
                        parentChainCode = encode' . xPrvChain $ xkey
                        parentFP = xPrvFP xkey
                        depth = xPrvDepth xkey + 1
                        parentPrivKey = bsPadPrvKey . xPrvKey $ parentXPrivKey
                        (a,c) = split512 . hmac512 parentChainCode . BS.append parentPrivKey . encode' $ i
                        k      = fromMaybe err $ tweakPrvKeyC (xPrvKey xkey) a
                        err    = throw $ DerivationException $ "Invalid deriveXPrvKeyEndo derivation: " ++ show xk
                    in  XPrvKey depth parentFP i c k

    
-- | Compute a private, soft child key derivation. A private soft derivation
-- will allow the equivalent extended public key to derive the public key for
-- this child. Given a parent key /m/ and a derivation index /i/, this function
-- will compute m\/i\/.
--
-- Soft derivations allow for more flexibility such as read-only wallets.
-- However, care must be taken not the leak both the parent extended public key
-- and one of the extended child private keys as this would compromise the
-- extended parent private key.
instance XPrvKeyEndoDerivable XKeySoftIndex
  where deriveXPrvKeyEndo xk@(XKeySoftIndex child') = Endo $ \xkey ->
                    let i     = fromIntegral . toInteger $ child' 
                        parentXPubKey = deriveXPubKey xkey 
                        parentChainCode = encode' . xPubChain $ parentXPubKey
                        parentFP = xPubFP parentXPubKey
                        depth = (xPubDepth parentXPubKey + 1)
                        parentPubKey = encode' . xPubKey $ parentXPubKey
                        (a,c) = split512 . hmac512 parentChainCode . BS.append parentPubKey . encode' $ i
                        k      = fromMaybe err $ tweakPrvKeyC (xPrvKey xkey) a
                        err    = throw $ DerivationException $ "Invalid deriveXPrvKeyEndo derivation" ++ show xk
                    in XPrvKey depth parentFP i c k

class XPubKeyEndoDerivable x where
  deriveXPubKeyEndo :: x -> Endo XPubKey

-- | Compute a public, soft child key derivation. Given a parent key /M/
-- and a derivation index /i/, this function will compute M\/i\/.
instance XPubKeyEndoDerivable XKeySoftIndex
  where deriveXPubKeyEndo xk@(XKeySoftIndex child') = Endo $ \xkey ->
                    let i = fromIntegral . toInteger $ child' 
                        parentXPubKey = xkey
                        parentChainCode = encode' . xPubChain $ parentXPubKey
                        parentFP = xPubFP parentXPubKey
                        depth = (xPubDepth parentXPubKey + 1)
                        parentPubKey = encode' . xPubKey $ parentXPubKey
                        (a,c) = split512 . hmac512 parentChainCode . BS.append parentPubKey . encode' $ i
                        pK    = fromMaybe err $ tweakPubKeyC (xPubKey xkey) a
                        err    = throw $ DerivationException $ "Invalid deriveXPubKeyEndo derivation" ++ show xk
                    in XPubKey depth parentFP i c pK

-- account for the private m and public M at the beginning of bip32 path
data DerivPath =  Bip32Prvm Bip32Path  | Bip32PubM Bip32Path
  deriving (Read,Show,Eq,Ord)

data Bip32Path = Bip32Hard HardPath | Bip32Soft SoftPath 
  deriving (Read,Show,Eq,Ord)

data Bip32XKey = Bip32PrvK XPrvKey | Bip32PubK XPubKey 
  deriving (Read,Show,Eq)

-- todo: skeptical about strictness.  test with and without and compare times.  if doesn't make any difference, leave non strict.
data SoftPath = XKeyEmptyPath
                    | (://) !XKeySoftIndex !SoftPath
  deriving (Read,Show,Eq,Ord)

data HardPath = (:/|) !XKeySoftIndex !HardPath
                | (:|/) !XKeyHardIndex !SoftPath
                | (:||) !XKeyHardIndex !HardPath
  deriving (Read,Show,Eq,Ord)

incrementSoftIndex (XKeySoftIndex i) = XKeySoftIndex $ i + 1
incrementHardIndex (XKeyHardIndex i) = XKeyHardIndex $ i + 1

incrementHardPathEnd :: HardPath -> HardPath
incrementHardPathEnd ( (:/|) softIndex hardPath )         = softIndex                       :/| incrementHardPathEnd hardPath
incrementHardPathEnd ( (:||) hardIndex hardPath )         = hardIndex                       :|| incrementHardPathEnd hardPath
incrementHardPathEnd ( (:|/) hardIndex XKeyEmptyPath )    = (incrementHardIndex hardIndex ) :|/ XKeyEmptyPath
incrementHardPathEnd ( (:|/) hardIndex nonEmptySoftPath ) = hardIndex                       :|/ incrementSoftPathEnd XKeyEmptyPath nonEmptySoftPath


incrementSoftPathEnd :: SoftPath -> SoftPath -> SoftPath
incrementSoftPathEnd def XKeyEmptyPath            = def
incrementSoftPathEnd def ( (://) softIndex XKeyEmptyPath) = incrementSoftIndex softIndex :// XKeyEmptyPath
incrementSoftPathEnd def ( (://) softIndex softPath)             = softIndex :// incrementSoftPathEnd def softPath

modifySoftEndOfSoftPath :: (XKeySoftIndex -> XKeySoftIndex) -> SoftPath -> Either String SoftPath
modifySoftEndOfSoftPath f XKeyEmptyPath = Left "modifySoftEndOfSoftPath, empty soft path" 
modifySoftEndOfSoftPath f ( (://) i XKeyEmptyPath ) = Right $ (f i) :// XKeyEmptyPath
modifySoftEndOfSoftPath f ( (://) i p ) = (i ://) <$> modifySoftEndOfSoftPath f p

instance NFData HardPath where
    rnf ( (:/|) index path ) = rnf index `seq` rnf path
    rnf ( (:|/) index path ) = rnf index `seq` rnf path
    rnf ( (:||) index path ) = rnf index `seq` rnf path
    
instance NFData SoftPath where
    rnf ( (://) index path ) = rnf index `seq` rnf path

instance NFData DerivPath where
    rnf ( Bip32Prvm p ) = rnf p
    rnf ( Bip32PubM p ) = rnf p

instance NFData Bip32Path where
    rnf ( Bip32Hard p ) = rnf p
    rnf ( Bip32Soft p ) = rnf p

{-
instance Monoid XKeySoftPath
  where mempty = XKeyEmptyPath
        mappend  XKeyEmptyPath xksp = xksp
        mappend  xksp XKeyEmptyPath = xksp
        mappend  (xkiA :// xkpA) xkpB = xkiA :// (xkpA `mappend` xkpB)
-}

hardPlusSoft :: HardPath -> SoftPath -> HardPath
hardPlusSoft hardPath softPath = case hardPath of 
   (:/|) i p -> (:/|) i (hardPlusSoft p softPath)
   (:|/) i p -> (:|/) i (softPlusSoft p softPath)
   (:||) i p -> (:||) i (hardPlusSoft p softPath)

softPlusSoft :: SoftPath -> SoftPath -> SoftPath
softPlusSoft soft1 XKeyEmptyPath = soft1
softPlusSoft XKeyEmptyPath soft2 = soft2
softPlusSoft ( soft1 ) ( (://) index2 path2 ) = soft1 `softPlusSoft` (index2 :// XKeyEmptyPath) `softPlusSoft` path2
  


instance XPrvKeyEndoDerivable HardPath where
  deriveXPrvKeyEndo (index :|/ path) =  (deriveXPrvKeyEndo path) <> (deriveXPrvKeyEndo index)
  deriveXPrvKeyEndo (index :/| path) =  (deriveXPrvKeyEndo path) <> (deriveXPrvKeyEndo index)
  deriveXPrvKeyEndo (index :|| path) =  (deriveXPrvKeyEndo path) <> (deriveXPrvKeyEndo index)

instance XPrvKeyEndoDerivable SoftPath where
  deriveXPrvKeyEndo XKeyEmptyPath = mempty
  deriveXPrvKeyEndo (index :// path) =  (deriveXPrvKeyEndo path) <> (deriveXPrvKeyEndo index)

instance XPubKeyEndoDerivable SoftPath where
  deriveXPubKeyEndo XKeyEmptyPath = mempty
  deriveXPubKeyEndo (index :// path) = (deriveXPubKeyEndo path) <> (deriveXPubKeyEndo index)

{-
data StartDerivPath where
    Deriv :: StartDerivPath -- neither m nor M, interpret as m (private)
    DerivPrv :: StartDerivPath -- m (private)
    DerivPub :: StartDerivPath -- M (public)
-}
-- hmmmmmm

-- pathToStr :: DerivPath -> String
-- pathToStr = toHaskoinString

instance ToHaskoinString DerivPath where
    toHaskoinString x = case x of
        Bip32Prvm (Bip32Soft XKeyEmptyPath) -> "m"
        Bip32Prvm p -> "m" ++ "/" ++ toHaskoinString p
        Bip32PubM (Bip32Soft XKeyEmptyPath) -> "M"
        Bip32PubM p -> "M" ++ "/" ++ toHaskoinString p
instance IsString DerivPath where
    fromString = derivFromString "Could not read DerivPath: " 
instance FromHaskoinString DerivPath where
    fromHaskoinString = eitherToMaybe . parsePath
-- | Parse derivation path string for extended key.
-- Forms: “m/0'/2”, “M/2/3/4”.
-- wip, needs beginning of path
-- parsePath = parseDerivPath
parsePath :: String -> Either ParseError DerivPath
parsePath x = parse derivPathParser "derivPathParser" x
derivPathParser :: Parsec String () DerivPath
derivPathParser = do
    dp <- try ( Bip32Prvm <$> 
                 ( ( try $ ( string "m/" >> bip32PathParser )) 
                   <|> ( (try $ string "m") >> return (Bip32Soft XKeyEmptyPath) ) )
              ) 
      <|> try ( Bip32PubM <$> 
                ( ( try $ string "M/" >> bip32PathParser) 
                  <|> ( (try $ string "M") >> return (Bip32Soft XKeyEmptyPath) ) )
              )
    eof
    return dp

instance ToHaskoinString Bip32XKey where
    toHaskoinString x = case x of 
      Bip32PrvK xpvk -> toHaskoinString xpvk
      Bip32PubK xpbk -> toHaskoinString xpbk

instance ToHaskoinString Bip32Path where
    toHaskoinString x = case x of 
        Bip32Hard p -> toHaskoinString p 
        Bip32Soft p -> toHaskoinString p
instance IsString Bip32Path where
    fromString = derivFromString "Could not read Bip32Path: " 
instance FromHaskoinString Bip32Path where
    fromHaskoinString = eitherToMaybe . parseBip32Path
parseBip32Path :: String -> Either ParseError Bip32Path
parseBip32Path x = parse bip32PathParser  "bip32PathParser" x
bip32PathParser :: Parsec String () Bip32Path
bip32PathParser = try ( Bip32Soft <$> softPathParser) <|> try ( Bip32Hard <$> hardPathParser)
  



instance ToHaskoinString HardPath where
    toHaskoinString x =  case x of 
      (:|/) hardIndex softPath -> case softPath of
                XKeyEmptyPath -> toHaskoinString hardIndex
                nonEmptySoftPath -> toHaskoinString hardIndex ++ "/" ++ toHaskoinString nonEmptySoftPath
      (:/|) softIndex hardPath -> toHaskoinString softIndex ++ "/" ++ toHaskoinString hardPath
      (:||) hardIndex hardPath -> toHaskoinString hardIndex ++ "/" ++ toHaskoinString hardPath       
instance IsString HardPath where
    fromString = derivFromString "Could not read HardPath: " 
instance FromHaskoinString HardPath where
    fromHaskoinString = eitherToMaybe . parseHard
parseHard :: String -> Either ParseError HardPath
parseHard x = parse hardPathParser "hardPathParser" x
hardPathParser :: Parsec String () HardPath
hardPathParser = do
      hardPath <- choice [ try hardThenSoft, try softThenHard, try hardThenHard, try oneHardIndex ] 
      return hardPath
  where hardThenSoft, softThenHard, hardThenHard :: Parsec String () HardPath
        hardThenSoft = do
            i <- hardIndexParser            
            string "/"
            p <- softPathParser
            return $ i :|/ p
        softThenHard = do
            i <- softIndexParser
            string "/"
            p <- hardPathParser
            return $ i :/| p
        hardThenHard = do
            i <- hardIndexParser
            string "/"
            p <- hardPathParser
            return $ i :|| p
        oneHardIndex = do
            i <- hardIndexParser
            return $ i :|/ XKeyEmptyPath


instance ToHaskoinString XKeyHardIndex where
    toHaskoinString (XKeyHardIndex i) = (show . toInteger $ i) ++ "'" 
instance IsString XKeyHardIndex where
    fromString = derivFromString "Could not read XKeyHardIndex: " 
instance FromHaskoinString XKeyHardIndex where
    fromHaskoinString = eitherToMaybe . parseHardIndex 
parseHardIndex :: String -> Either ParseError XKeyHardIndex
parseHardIndex x = parse hardIndexParser "hardIndexParser" x
hardIndexParser :: Parsec String () XKeyHardIndex
hardIndexParser = do
    i <- bigWordParser
    string "'"
    return $ XKeyHardIndex i



instance ToHaskoinString XKeySoftIndex where
    toHaskoinString (XKeySoftIndex i) = show . toInteger $ i
instance IsString XKeySoftIndex where
    fromString = derivFromString "Could not read XKeySoftIndex: " 
instance FromHaskoinString XKeySoftIndex where
    fromHaskoinString = eitherToMaybe . parseSoftIndex 
parseSoftIndex :: String -> Either ParseError XKeySoftIndex
parseSoftIndex x = parse softIndexParser "softIndexParser" x
softIndexParser :: Parsec String () XKeySoftIndex
softIndexParser = do
    i <- bigWordParser
    return $ XKeySoftIndex i

instance ToHaskoinString SoftPath where
    toHaskoinString x = case x of 
          XKeyEmptyPath -> ""
          (://) softIndex XKeyEmptyPath -> toHaskoinString softIndex
          (://) softIndex nonEmpty      -> toHaskoinString softIndex ++ "/" ++ toHaskoinString nonEmpty 
instance IsString SoftPath where
    fromString = derivFromString "Could not read SoftPath: " 
instance FromHaskoinString SoftPath where
    fromHaskoinString = eitherToMaybe . parseSoft 
parseSoft :: String -> Either ParseError SoftPath
parseSoft x = parse softPathParser "softPathParser" x
softPathParser :: Parsec String () SoftPath
softPathParser = do
  softPath <- try more <|> try oneSoftIndex <|> try ( return XKeyEmptyPath )    
  eof <|> ( try $ string "/" >> eof ) -- optional trailing slash
  return softPath
  where more :: Parsec String () SoftPath
        more = do 
            i <- softIndexParser
            string "/"
            p <- softPathParser
            return $ i :// p
        oneSoftIndex = do
            i <- softIndexParser
            return $ i :// XKeyEmptyPath



-- TODO: Test
instance ToHaskoinString XPubKey where
    toHaskoinString = cs . xPubExport
instance FromHaskoinString XPubKey where
    fromHaskoinString = xPubImport . cs
instance IsString XPubKey where
    fromString = derivFromString "Could not import extended public key: " 

-- TODO: Test
instance ToHaskoinString XPrvKey where
    toHaskoinString = cs . xPrvExport
instance FromHaskoinString XPrvKey where
    fromHaskoinString = xPrvImport . cs
instance IsString XPrvKey where
    fromString = derivFromString "Could not decode extended private key: " 


instance FromJSON DerivPath where
    parseJSON = withText "DerivPath" $ \str -> case fromHaskoinString . unpack $ str of
        Just p -> return p
        _      -> mzero

instance FromJSON HardPath where
    parseJSON = withText "HardPath" $ \str -> case fromHaskoinString . unpack $ str of 
        Just p -> return p
        _      -> mzero

instance FromJSON SoftPath where
    parseJSON = withText "SoftPath" $ \str -> case fromHaskoinString . unpack $ str of 
        Just p -> return p
        _      -> mzero

instance ToJSON SoftPath where
  toJSON = String . cs . toHaskoinString -- toHaskoinString
instance ToJSON HardPath where
  toJSON = String . cs . toHaskoinString
instance ToJSON DerivPath where
  toJSON = String . cs . toHaskoinString






-- | Derive a private key from a hard derivation path 
deriveSoftPrvPath :: SoftPath -> XPrvKey -> XPrvKey
deriveSoftPrvPath path xkey = ( appEndo . deriveXPrvKeyEndo $ path ) xkey

-- | Derive a private key from a hard derivation path 
deriveHardPrvPath :: HardPath -> XPrvKey -> XPrvKey
deriveHardPrvPath path xkey = ( appEndo . deriveXPrvKeyEndo $ path ) xkey

-- | Derive a public key from a soft derivation path 
derivePubPath :: SoftPath -> XPubKey -> XPubKey
derivePubPath path xkey = ( appEndo . deriveXPubKeyEndo $ path ) xkey

-- for backwards compatibility, so wallet package compiles
derivePath :: XPrvKeyEndoDerivable x => x -> XPrvKey -> XPrvKey 
derivePath path xpvk = ( appEndo . deriveXPrvKeyEndo $ path ) $ xpvk

-- | Derive a key from a derivation path and return either a private or public
-- key depending on the initial derivation constructor. If you parsed a string
-- with m/ you will get a private key and if you parsed a string starting with M/ you will
-- get a public key, assuming the derivation is possible.  Impossible derivations will fail 
-- with a Left error message.  Example impossible derivations: 
-- m/1/2/3 (private key result) starting with pub key. Or M/1'/2'/3' (a hard path) starting with pub key.
deriveBip32Path, derivePathE :: DerivPath -> Bip32XKey -> Either String Bip32XKey 
deriveBip32Path dp xkey = 
  let dXPrivPath :: XPrvKeyEndoDerivable x => x -> XPrvKey -> XPrvKey 
      dXPrivPath path xpvk = ( appEndo . deriveXPrvKeyEndo $ path ) $ xpvk
      dXPPubPath :: XPubKeyEndoDerivable x => x -> XPubKey -> XPubKey
      dXPPubPath path xpbk = ( appEndo . deriveXPubKeyEndo $ path ) $ xpbk
  in case xkey of 
    Bip32PubK xpbk -> 
      case dp of
        Bip32PubM (Bip32Soft path) -> Right . Bip32PubK . dXPPubPath path $ xpbk
        Bip32PubM (Bip32Hard path) -> Left $ "deriveBip32Path, can't derive hard path starting with public key for path: " ++ show dp ++ ", xkey: " ++ show xkey
        Bip32Prvm (Bip32Soft path) -> Left $ "deriveBip32Path, can't derive private key starting with public key for path: " ++ show dp ++ ", xkey: " ++ show xkey
        Bip32Prvm (Bip32Hard path) -> Left $ "deriveBip32Path, can't derive private key starting with public key for path: " ++ show dp ++ ", xkey: " ++ show xkey
    Bip32PrvK xpvk -> 
      case dp of
        Bip32PubM (Bip32Soft path) -> Right . Bip32PubK . deriveXPubKey . dXPrivPath path $ xpvk
        Bip32PubM (Bip32Hard path) -> Right . Bip32PubK . deriveXPubKey . dXPrivPath path $ xpvk
        Bip32Prvm (Bip32Soft path) -> Right . Bip32PrvK . dXPrivPath path $ xpvk
        Bip32Prvm (Bip32Hard path) -> Right . Bip32PrvK . dXPrivPath path $ xpvk
derivePathE = deriveBip32Path

-- | Derive an address from a given parent path.
derivePathAddr :: XPubKey -> SoftPath -> XKeySoftIndex -> (Address, PubKeyC)
derivePathAddr key path i = deriveAddr (derivePubPath path key) i

-- | Cyclic list of all addresses derived from a given parent path and starting
-- from the given offset index.
derivePathAddrs :: XPubKey -> SoftPath -> XKeySoftIndex
                -> [(Address, PubKeyC, XKeySoftIndex)]
derivePathAddrs key path i = deriveAddrs (derivePubPath path key) i

-- | Derive a multisig address from a given parent path. The number of required
-- signatures (m in m of n) is also needed.
derivePathMSAddr :: [XPubKey] -> SoftPath -> Int -> XKeySoftIndex
                 -> (Address, RedeemScript)
derivePathMSAddr keys path m i =
    deriveMSAddr (map (derivePubPath path) keys) m i

-- | Cyclic list of all multisig addresses derived from a given parent path and
-- starting from the given offset index. The number of required signatures
-- (m in m of n) is also needed.
derivePathMSAddrs :: [XPubKey] -> SoftPath -> Int -> XKeySoftIndex
                  -> [(Address, RedeemScript, XKeySoftIndex)]
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

