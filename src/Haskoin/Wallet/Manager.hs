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
    , intOffset :: KeyIndex
    , extOffset :: KeyIndex
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
        , walletAccs   :: Map.Map KeyIndex Account
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

type WalletManager m a = S.StateT Wallet m a

withWallet :: Monad m => Wallet -> WalletManager m a -> m a
withWallet s m = S.evalStateT m s

getWallet :: Monad m => WalletManager m Wallet
getWallet = S.get

putWallet :: Monad m => Wallet -> WalletManager m ()
putWallet s = S.put s

newAccount :: Monad m => String -> WalletManager m Account
newAccount name = do
    wallet <- getWallet
    case wallet of
        (MasterWallet k off accs) -> do
            let pub = deriveXPubKey $ findValidPrimeKey k off
                acc = Account (AccInfo name 0 0 Map.empty) pub
                idx = clearBit (accIndex acc) 31
            putWallet $ MasterWallet k (idx+1) (Map.insert idx acc accs)
            return acc
        _ -> error "Can't add new account to account wallet"

getAccount :: Monad m => KeyIndex -> WalletManager m (Maybe Account)
getAccount i = do
    wallet <- getWallet
    case wallet of
        (MasterWallet _ _ accs) -> return $ Map.lookup i accs
        _                       -> return Nothing

getCurrentAddr :: Monad m => KeyIndex -> WalletManager m (Maybe Address)
getCurrentAddr i = (getAccount i) >>= return . (accCurrAddr <$>)

accCurrAddr :: Account -> Address
accCurrAddr acc = case acc of
    (Account info key) -> xPubAddr $ findValidSubKey key (extOffset info)
    (AccMulSig2 info k1 k2) -> 
        let (r1, r2) = findValidSubKey2 k1 k2 (extOffset info)
            in scriptAddr $ buildMulSig2 (xPubKey r1) (xPubKey r2)
    (AccMulSig3 info k1 k2 k3) ->
        let (r1, r2, r3) = findValidSubKey3 k1 k2 k3 (extOffset info)
            in scriptAddr $ buildMulSig3 (xPubKey r1) (xPubKey r2) (xPubKey r3)

-- Todo: Check end of index enumeration
findValidPrimeKey :: XPrvKey -> KeyIndex -> XPrvKey
findValidPrimeKey key i = fromMaybe (findValidPrimeKey key (i+1)) 
                                  (primeSubKey key i)

-- Todo: Check end of index enumeration
findValidSubKey :: XPubKey -> KeyIndex -> XPubKey
findValidSubKey key i = fromMaybe (findValidSubKey key (i+1)) (pubSubKey key i)

-- Todo: Check end of index enumeration
findValidSubKey2 :: XPubKey -> XPubKey -> KeyIndex -> (XPubKey, XPubKey)
findValidSubKey2 k1 k2 i = 
    fromMaybe (findValidSubKey2 k1 k2 (i+1))
              (liftM2 (,) (pubSubKey k1 i) (pubSubKey k2 i))

-- Todo: Check end of index enumeration
findValidSubKey3 :: XPubKey -> XPubKey -> XPubKey -> KeyIndex 
                 -> (XPubKey, XPubKey, XPubKey)
findValidSubKey3 k1 k2 k3 i = 
    fromMaybe (findValidSubKey3 k1 k2 k3 (i+1))
              (liftM3 (,,) (pubSubKey k1 i) (pubSubKey k2 i) (pubSubKey k3 i))

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
                    index   <- getWord32le
                    account <- get
                    unless (accParent account == parentFP) $ fail $
                        "Get: Account is not a child of master key"
                    unless (accIndex account == setBit index 31) $ fail $
                        "Get: Account index does not match key index"
                    return (index, account)
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
            forM_ accList $ \(index, account) -> do 
                putWord32le index
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

