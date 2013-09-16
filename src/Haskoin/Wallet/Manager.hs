module Haskoin.Wallet.Manager
( Wallet(..)
, Account(..)
, AccInfo(..)
) where

import Control.Monad
import Control.Applicative
import qualified Control.Monad.State as S

import Data.Word
import Data.Maybe
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS

import Haskoin.Wallet.Tx
import Haskoin.Wallet.Keys
import Haskoin.Crypto
import Haskoin.Util

type KeyIndex = Word32

data AccInfo = AccInfo
    { accName   :: String
    , extOffset :: KeyIndex
    , intOffset :: KeyIndex
    , accLabels :: Map.Map KeyIndex String
    } deriving (Eq, Show)

-- Standard inter-operability accounts
data Account = 
    Account
        { accInfo :: AccInfo
        , accKey  :: XPubKey
        } |
    AccMulSig2
        { accInfo :: AccInfo
        , accKey  :: XPubKey
        , othKey  :: XPubKey
        } |
    AccMulSig3
        { accInfo :: AccInfo
        , accKey  :: XPubKey
        , othKey1 :: XPubKey
        , othKey2 :: XPubKey
        } deriving (Eq, Show)

data Wallet = 
    -- m/
    MasterWallet
        { walletKey    :: XPrvKey
        , walletOffset :: KeyIndex
        , walletAccs   :: Map.Map String Account
        } |
    -- m/i'/ 
    AccWallet 
        { walletKey   :: XPrvKey
        , walletParID :: Hash160
        , walletAcc   :: Account 
        } |
    -- M/i'/
    PubAccWallet 
        { walletParID :: Hash160
        , walletAcc   :: Account 
        } deriving (Eq, Show)

accIndex :: Account -> KeyIndex
accIndex = xPubIndex . accKey

accParent :: Account -> Word32
accParent = xPubParent . accKey

accDepth :: Account -> Word8
accDepth = xPubDepth . accKey

isMasterWallet :: Wallet -> Bool
isMasterWallet (MasterWallet _ _ _) = True
isMasterWallet _                    = False

newMasterWallet :: BS.ByteString -> Maybe Wallet
newMasterWallet bs = do
    prv <- makeXPrvKey bs
    return $ MasterWallet prv 0 Map.empty

type WalletManager m a = S.StateT Wallet m a

withWallet :: Monad m => Wallet -> WalletManager m a -> m a
withWallet s m = S.evalStateT m s

getWallet :: Monad m => WalletManager m Wallet
getWallet = S.get

putWallet :: Monad m => Wallet -> WalletManager m ()
putWallet s = S.put s

getAccount :: Monad m => String -> WalletManager m (Maybe Account)
getAccount name = do
    (MasterWallet _ _ accMap) <- getWallet
    return $ Map.lookup name accMap

lastAddr :: Monad m => String -> WalletManager m (Maybe Address)
lastAddr name = do
    acc <- getAccount name
    return $ acc >>= lastAccAddr

newAccount :: Monad m => String -> WalletManager m (Maybe Account)
newAccount name = do
    (MasterWallet k offset accMap) <- getWallet
    case nextAccount k (offset+1) name of
        (Just (acc,i)) -> do
            putWallet $ MasterWallet k i (Map.insert name acc accMap)
            return $ Just acc
        Nothing -> return Nothing

newAccMulSig2 :: Monad m => String -> XPubKey 
              -> WalletManager m (Maybe Account)
newAccMulSig2 name pub1 = do
    (MasterWallet k offset accMap) <- getWallet
    case nextAccMulSig2 k pub1 (offset+1) name of
        (Just (acc,i)) -> do
            putWallet $ MasterWallet k i (Map.insert name acc accMap)
            return $ Just acc
        Nothing -> return Nothing

newAccMulSig3 :: Monad m => String -> XPubKey -> XPubKey 
              -> WalletManager m (Maybe Account)
newAccMulSig3 name pub1 pub2 = do
    (MasterWallet k offset accMap) <- getWallet
    case nextAccMulSig3 k pub1 pub2 (offset+1) name of
        (Just (acc,i)) -> do
            putWallet $ MasterWallet k i (Map.insert name acc accMap)
            return $ Just acc
        Nothing -> return Nothing

{- Helper functions -}

lastAccAddr :: Account -> Maybe Address
lastAccAddr acc = case acc of
    (Account info key) -> do
        (p,_) <- nextIndex (pubSubKey key) (extOffset info)
        return $ xPubAddr p
    (AccMulSig2 info key k1) -> do
        (p1,p2,_) <- nextIndex2 (pubSubKey key) 
                                (pubSubKey k1) 
                                (extOffset info)
        return $ scriptAddr $ buildMulSig2 (xPubKey p1) (xPubKey p2)
    (AccMulSig3 info key k1 k2) -> do
        (p1,p2,p3,_) <- nextIndex3 (pubSubKey key) 
                                   (pubSubKey k1) 
                                   (pubSubKey k2) 
                                   (extOffset info)
        return $ scriptAddr $ buildMulSig3 (xPubKey p1) (xPubKey p2) (xPubKey p3)

nextAccount :: XPrvKey -> KeyIndex -> String -> Maybe (Account, KeyIndex)
nextAccount key idx name = do
    (accKey,accIdx) <- nextIndex (primeSubKey key) idx
    let accPub = deriveXPubKey accKey
    -- If this fails, the account is invalid and we must try the next one
    fromMaybe (nextAccount key (accIdx+1) name) $ do
        extKey     <- pubSubKey accPub 0
        intKey     <- pubSubKey accPub 1
        (_,extOff) <- nextIndex (pubSubKey extKey) 0
        (_,intOff) <- nextIndex (pubSubKey intKey) 0
        let info   = AccInfo name extOff intOff Map.empty
        return $ Just (Account info accPub, accIdx)

nextAccMulSig2 :: XPrvKey -> XPubKey -> KeyIndex -> String 
               -> Maybe (Account, KeyIndex)
nextAccMulSig2 key pub1 idx name = do
    (accKey,accIdx) <- nextIndex (primeSubKey key) idx
    let accPub = deriveXPubKey accKey
    extKey1 <- pubSubKey pub1 0
    intKey1 <- pubSubKey pub1 1
    fromMaybe (nextAccMulSig2 key pub1 (accIdx+1) name) $ do
        extKey     <- pubSubKey accPub 0
        intKey     <- pubSubKey accPub 1
        (_,_,extOff) <- nextIndex2 (pubSubKey extKey) (pubSubKey extKey1) 0
        (_,_,intOff) <- nextIndex2 (pubSubKey intKey) (pubSubKey intKey1) 0
        let info = AccInfo name extOff intOff Map.empty
        return $ Just (AccMulSig2 info accPub pub1, accIdx)

nextAccMulSig3 :: XPrvKey -> XPubKey -> XPubKey -> KeyIndex -> String 
               -> Maybe (Account, KeyIndex)
nextAccMulSig3 key pub1 pub2 idx name = do
    (accKey,accIdx) <- nextIndex (primeSubKey key) idx
    let accPub = deriveXPubKey accKey
    extKey1 <- pubSubKey pub1 0
    intKey1 <- pubSubKey pub1 1
    extKey2 <- pubSubKey pub2 0
    intKey2 <- pubSubKey pub2 1
    fromMaybe (nextAccMulSig3 key pub1 pub2 (accIdx+1) name) $ do
        extKey     <- pubSubKey accPub 0
        intKey     <- pubSubKey accPub 1
        (_,_,_,extOff) <- nextIndex3 (pubSubKey extKey) 
                                     (pubSubKey extKey1) 
                                     (pubSubKey extKey2) 0
        (_,_,_,intOff) <- nextIndex3 (pubSubKey intKey) 
                                     (pubSubKey intKey1) 
                                     (pubSubKey intKey2) 0
        let info = AccInfo name extOff intOff Map.empty
        return $ Just (AccMulSig3 info accPub pub1 pub2, accIdx)

-- Find the next valid key derivation index starting at offset i
-- First argument is a partially applied key derivation function
nextIndex :: (KeyIndex -> Maybe a) -> KeyIndex -> Maybe (a, KeyIndex)
nextIndex f i = do
    guard $ i < 0x80000000
    fromMaybe (nextIndex f (i+1)) $ do
        k <- f i
        return $ Just (k,i)

nextIndex2 :: (KeyIndex -> Maybe a)
           -> (KeyIndex -> Maybe a)
           -> KeyIndex
           -> Maybe (a, a, KeyIndex)
nextIndex2 f1 f2 i = do
    guard $ i < 0x80000000
    fromMaybe (nextIndex2 f1 f2 (i+1)) $ do
        k1 <- f1 i
        k2 <- f2 i
        return $ Just (k1,k2,i)

nextIndex3 :: (KeyIndex -> Maybe a)
           -> (KeyIndex -> Maybe a)
           -> (KeyIndex -> Maybe a)
           -> KeyIndex
           -> Maybe (a, a, a, KeyIndex)
nextIndex3 f1 f2 f3 i = do
    guard $ i < 0x80000000
    fromMaybe (nextIndex3 f1 f2 f3 (i+1)) $ do
        k1 <- f1 i
        k2 <- f2 i
        k3 <- f3 i
        return $ Just (k1,k2,k3,i)

instance Binary Wallet where

    get = do     
        storeType <- getWord8  
        case storeType of
            0x00 -> do
                key     <- get
                unless (xPrvDepth key == 0) $ fail $
                    "Get: master key depth is not 0"
                unless (xPrvIndex key == 0) $ fail $
                    "Get: Master key index is not 0"
                unless (xPrvParent key == 0) $ fail $
                    "Get: Master key parent fingerprint is not 0"
                off     <- getWord32le
                len     <- fromIntegral <$> getWord32le
                let parentFP = xPrvFP key
                accList <- replicateM len $ do
                    nameLen <- getWord8
                    nameBS  <- getByteString $ fromIntegral nameLen
                    account <- get
                    unless (accParent account == parentFP) $ fail $
                        "Get: Account is not a child of master key"
                    return (bsToString nameBS, account)
                return $ MasterWallet key off (Map.fromList accList)
            0x01 -> AccWallet <$> get <*> get <*> get
            0x02 -> PubAccWallet <$> get <*> get
            _    -> fail "Get: Invalid store type"

    put s = case s of
        (MasterWallet k off xs) -> do
            putWord8 0 >> put k
            putWord32le off
            let accList = Map.toList xs
            putWord32le $ fromIntegral $ length accList
            forM_ accList $ \(name, account) -> do 
                let nameBS = stringToBS name
                putWord8 $ fromIntegral $ BS.length nameBS
                putByteString nameBS
                put account
        (AccWallet k p a) -> putWord8 1 >> put k >> put p >> put a
        (PubAccWallet p a) -> putWord8 2 >> put p >> put a

instance Binary Account where

    get = do
        accType <- getWord8 
        info    <- get
        key <- get
        unless (xPubDepth key == 1) $ fail $
            "Get: Invalid public key depth: " ++ (show $ xPubDepth key)
        unless (xPubIsPrime key) $ fail $
            "Get: Public key is not prime"
        case accType of
            0x00 -> return $ Account info key
            0x01 -> (AccMulSig2 info key) <$> get
            0x02 -> (AccMulSig3 info key) <$> get <*> get
            _    -> fail $ "Get: Invalid account type: " ++ (show accType)

    put acc = case acc of
        (Account i k) -> putWord8 0 >> put i >> put k
        (AccMulSig2 i k1 k2) -> do
            putWord8 1 >> put i 
            put k1 >> put k2
        (AccMulSig3 i k1 k2 k3) -> do
            putWord8 2 >> put i 
            put k1 >> put k2 >> put k3

instance Binary AccInfo where
    
    get = do
        nameLen   <- fromIntegral <$> getWord32le
        name      <- bsToString <$> getByteString nameLen
        intOffset <- getWord32le
        extOffset <- getWord32le
        labelSize <- fromIntegral <$> getWord32le
        labelList <- replicateM labelSize $ do
            index    <- getWord32le
            labelLen <- fromIntegral <$> getWord32le
            label    <- bsToString <$> getByteString labelLen
            return (index, label)
        return $ AccInfo name intOffset extOffset (Map.fromList labelList)

    put (AccInfo n i e m) = do
        let nameBS = stringToBS n
        putWord32le $ fromIntegral $ BS.length nameBS
        putByteString nameBS
        putWord32le i
        putWord32le e
        let labelList = Map.toList m
            labelSize = length labelList
        putWord32le $ fromIntegral labelSize
        forM_ labelList $ \(index, label) -> do
            putWord32le index 
            let labelBS = stringToBS label
            putWord32le $ fromIntegral $ BS.length labelBS
            putByteString labelBS

