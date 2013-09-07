module Haskoin.Wallet.Store
( Store(..)
, Account(..)
, AccInfo(..)
) where

import Control.Monad
import Control.Applicative

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS

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

-- Standard merchant inter-operability accounts
data Account = 
    AccountPrv
        { accInfo   :: AccInfo
        , accPrvKey :: XPrvKey
        } 
    | AccountPub
        { accInfo   :: AccInfo
        , accPubKey :: XPubKey
        } 
    | AccountPrvMulSig
        { accInfo     :: AccInfo
        , accPrvKey   :: XPrvKey
        , mulSigKeys  :: [XPubKey]
        , sigRequired :: Word8
        } 
    | AccountPubMulSig
        { accInfo     :: AccInfo
        , accPubKey   :: XPubKey
        , mulSigKeys  :: [XPubKey]
        , sigRequired :: Word8
        } deriving (Eq, Show)

data Store = 
    -- m/
    MasterStore
        { masterKey     :: XPrvKey
        , storeAccounts :: Map.Map KeyIndex Account
        } 
    -- m/i'
    | AccountStore 
        { storeParentID :: Hash160
        , storeAccount  :: Account 
        }
      deriving (Eq, Show)

instance Binary Store where

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
                len     <- fromIntegral <$> getWord32le
                accList <- replicateM len get
                return $ MasterStore key (Map.fromList accList)
            0x01 -> AccountStore <$> get <*> get
            _    -> fail "Get: Invalid store type"

    put s = case s of
        (MasterStore k xs) -> do
            putWord8 0 >> put k
            let accList = Map.toList xs
            putWord32le $ fromIntegral $ length accList
            forM_ accList put
        (AccountStore p a) -> putWord8 1 >> put p >> put a

instance Binary Account where

    get = do
        accType <- getWord8 
        info    <- get
        case accType of
            0x00 -> do
                key <- get
                unless (xPrvDepth key == 1) $ fail $
                    "Get: Invalid private key depth: " ++ (show $ xPrvDepth key)
                unless (xPrvIsPrime key) $ fail $
                    "Get: Private key is not prime"
                return $ AccountPrv info key
            0x01 -> do
                key <- get
                unless (xPubDepth key == 1) $ fail $
                    "Get: Invalid public key depth: " ++ (show $ xPubDepth key)
                unless (xPubIsPrime key) $ fail $
                    "Get: Public key is not prime"
                return $ AccountPub info key
            0x02 -> do
                key  <- get
                unless (xPrvDepth key == 1) $ fail $
                    "Get: Invalid private key depth: " ++ (show $ xPrvDepth key)
                unless (xPrvIsPrime key) $ fail $
                    "Get: Private multisig key is not prime"
                len  <- fromIntegral <$> getWord8
                -- We don't check thirdparty key hierarchy
                keys <- replicateM len get
                req  <- getWord8
                unless (  (len == 2 && req == 2) -- MultiSig 2 of 2
                       || (len == 3 && req == 2) -- MultiSig 2 of 3
                       || (len == 3 && req == 3) -- MultiSig 3 of 3
                       ) $ fail "Get: Invalid multisig account"
                return $ AccountPrvMulSig info key keys req
            0x03 -> do
                key  <- get
                unless (xPubDepth key == 1) $ fail $
                    "Get: Invalid public key depth: " ++ (show $ xPubDepth key)
                unless (xPubIsPrime key) $ fail $
                    "Get: Public multisig key is not prime"
                len  <- fromIntegral <$> getWord8
                -- We don't check thirdparty key hierarchy
                keys <- replicateM len get
                req  <- getWord8
                unless (  (len == 2 && req == 2) -- MultiSig 2 of 2
                       || (len == 3 && req == 2) -- MultiSig 2 of 3
                       || (len == 3 && req == 3) -- MultiSig 3 of 3
                       ) $ fail "Get: Invalid multisig account"
                return $ AccountPubMulSig info key keys req
            _    -> fail $ "Get: Invalid account type: " ++ (show accType)

    put acc = case acc of
        (AccountPrv i k) -> putWord8 0 >> put i >> put k
        (AccountPub i k) -> putWord8 1 >> put i >> put k
        (AccountPrvMulSig i k ks r) -> do
            putWord8 2 >> put i >> put k
            putWord8 $ fromIntegral $ length ks
            forM_ ks put
            putWord8 r
        (AccountPubMulSig i k ks r) -> do
            putWord8 3 >> put i >> put k
            putWord8 $ fromIntegral $ length ks
            forM_ ks put
            putWord8 r

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

