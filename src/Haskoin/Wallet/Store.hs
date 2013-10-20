module Haskoin.Wallet.Store where

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Applicative

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import qualified Database.LevelDB as DB

import Haskoin.Wallet.Keys
import Haskoin.Wallet.Manager
import Haskoin.Util

data WalletHandle = WalletHandle 
    { hConfig :: DB.DB
    , hAcc    :: DB.DB
    , hAddr   :: DB.DB
    }

-- Track the database handle in a ReaderT monad
type WalletDB m a = ReaderT WalletHandle m a

runWalletDB :: MonadResource m => FilePath -> WalletDB m a -> m a
runWalletDB fp wm = do
    dbConfig <- DB.open (fp ++ "/dbconfig") dbOptions 
    dbAcc    <- DB.open (fp ++ "/dbacc")    dbOptions 
    dbAddr   <- DB.open (fp ++ "/dbaddr")   dbOptions 
    runReaderT wm $ WalletHandle dbConfig dbAcc dbAddr
    where dbOptions = DB.defaultOptions { DB.createIfMissing = True
                                        , DB.cacheSize = 2048
                                        }
    
-- config = {
--    version => 1
--    masterxprv => enc(xprv...)
--    addressbook => { name => address },
--    lastaccindex => 3,
--    focus => 'default'
-- }

--accouns = {
--    accname => {
--        index => 0
--        pubkey => xpub...
--        type => '2of3 protected wallet'
--        service => 'Haskoin.org'
--        multisig => [xpub1,xpub2...]
--        required => 2
--        lastaddrindex => 15
--    }
--}
--
--addrdb => {
--   addr1 => {
--      label => 'name'
--      index => 3
--   }
--}

{- Config database functions -}

dbGet :: MonadResource m => (WalletHandle -> DB.DB) -> String 
      -> WalletDB m (Maybe BS.ByteString)
dbGet f key = ask >>= \wh -> 
    lift $ DB.get (f wh) DB.defaultReadOptions $ stringToBS key

dbPut :: MonadResource m => (WalletHandle -> DB.DB) -> String -> BS.ByteString 
      -> WalletDB m ()
dbPut f key value = ask >>= \wh -> do
    lift $ DB.put (f wh) DB.defaultWriteOptions (stringToBS key) value

dbInit :: MonadResource m => String -> WalletDB m ()
dbInit seed = dbGetLastAcc >>= \lastM -> case lastM of
    Just _  -> error "Wallet already initialized"
    Nothing -> do
        dbPut hConfig "version" $ runPut' $ putWord32le 1
        dbPutSeed seed
        dbNewAcc "default"
        dbPutFocus "default"
        return ()

isDBInit :: MonadResource m => WalletDB m Bool
isDBInit = isJust <$> dbGetLastAcc 

dbGetVersion :: MonadResource m => WalletDB m (Maybe Int)
dbGetVersion = dbGet hConfig "version" >>= return . f
    where f = (fromIntegral . (runGet' getWord32le) <$>)

dbGetLastAcc :: MonadResource m => WalletDB m (Maybe Word32)
dbGetLastAcc = dbGet hConfig "lastacc" >>= return . (runGet' getWord32le <$>)

dbPutLastAcc :: MonadResource m => Word32 -> WalletDB m ()
dbPutLastAcc w = dbPut hConfig "lastacc" $ runPut' $ putWord32le w

dbGetFocus :: MonadResource m => WalletDB m (Maybe String)
dbGetFocus = dbGet hConfig "focus" >>= return . (bsToString <$>)

dbPutFocus :: MonadResource m => String -> WalletDB m ()
dbPutFocus name = dbGetAcc name >>= \accM -> case accM of
    Nothing -> error $ "Invalid account " ++ name
    _ -> dbPut hConfig "focus" $ stringToBS name

dbGetMaster :: MonadResource m => WalletDB m (Maybe MasterKey)
dbGetMaster = dbGet hConfig "seed" >>= return . (makeMasterKey =<<)

-- Todo: encrypt this
dbPutSeed :: MonadResource m => String -> WalletDB m ()
dbPutSeed seed = dbGetMaster >>= \masterM -> case masterM of
    Just _  -> error "Seed already exists"
    Nothing -> dbPut hConfig "seed" val 
    where val = runPut' $ putByteString $ stringToBS seed

{- Account database functions -}

dbNewAcc :: MonadResource m => String -> WalletDB m WAccount
dbNewAcc name = dbGetAcc name >>= \prev -> case prev of
    Just _ -> error $ "Account already exists: " ++ name
    Nothing -> do
        master <- (fromMaybe (error "No master seed")) <$> dbGetMaster
        last   <- (fromMaybe 0) . ((+1) <$>) <$> dbGetLastAcc
        let (key,idx) = head $ accPubKeys master last
            acc       = WAccount name idx key 0 0
        dbPutLastAcc idx >> dbPutAcc acc
        return acc

dbPutAcc :: MonadResource m => WAccount -> WalletDB m ()
dbPutAcc acc = dbPut hAcc (accName acc) $ encode' acc

dbGetAcc :: MonadResource m => String -> WalletDB m (Maybe WAccount)
dbGetAcc name = dbGet hAcc name >>= return . (decodeToMaybe =<<)

{- Address database functions -}

dbGenExtAddr :: MonadResource m => String -> Int -> WalletDB m [WAddr]
dbGenExtAddr name count = dbGetAcc name >>= \accM -> case accM of
    Nothing -> error $ "Invalid account " ++ name
    Just acc -> do
        let new   = take count $ extAddrs (accKey acc) (accExt acc + 1)
            wAddr = map (\(s,i) -> WAddr s "" i (accIndex acc)) new
        forM_ wAddr dbPutAddr
        dbPutAcc acc{ accExt = wIndex $ last wAddr }
        return wAddr

dbExtAddr :: MonadResource m => String -> Int -> WalletDB m [WAddr]
dbExtAddr name count = dbGetAcc name >>= \accM -> case accM of
    Nothing -> error $ "Invalid account " ++ name
    Just acc -> do
        let addr   = take count $ extAddrs' (accKey acc) (accExt acc)
        res <- forM addr (dbGetAddr . fst)
        return $ reverse $ catMaybes res

dbGetAddr :: MonadResource m => String -> WalletDB m (Maybe WAddr)
dbGetAddr addr = dbGet hAddr addr >>= return . (decodeToMaybe =<<)

dbPutAddr :: MonadResource m => WAddr -> WalletDB m ()
dbPutAddr addr = dbPut hAddr (wAddr addr) $ encode' addr

data WAddr = WAddr { wAddr  :: String
                   , wLabel :: String
                   , wIndex :: Word32
                   , wAcc   :: Word32
                   } deriving (Eq, Show)

instance Binary WAddr where

    get = do
        addrLen <- fromIntegral <$> getWord8
        addr <- bsToString <$> getByteString addrLen
        labelLen <- fromIntegral <$> getWord16le
        label <- bsToString <$> getByteString labelLen
        idx <- getWord32le
        acc <- getWord32le
        return $ WAddr addr label idx acc

    put (WAddr a l i ai) = do
        let bsAddr  = stringToBS a
            bsLabel = stringToBS l
        putWord8 $ fromIntegral $ BS.length bsAddr
        putByteString bsAddr
        putWord16le $ fromIntegral $ BS.length bsLabel
        putByteString bsLabel
        putWord32le i
        putWord32le ai

data WAccount = WAccount   { accName  :: String
                           , accIndex :: Word32
                           , accKey   :: AccPubKey
                           , accExt   :: Word32
                           , accInt   :: Word32
                           }
              | WAccountMS { accName  :: String
                           , accIndex :: Word32
                           , accKey   :: AccPubKey
                           , accExt   :: Word32
                           , accInt   :: Word32
                           , accMSKey :: [XPubKey]
                           , accMSReq :: Int
                           , accMSUrl :: String
                           } deriving (Show, Eq)

instance Binary WAccount where

    get = getWord8 >>= \t -> do
        nameLen <- fromIntegral <$> getWord16le
        name    <- bsToString <$> getByteString nameLen
        idx     <- getWord32le
        key     <- AccPubKey <$> get
        ext     <- getWord32le
        int     <- getWord32le
        case t of
            0x00 -> return $ WAccount name idx key ext int
            0x01 -> do
                keyLen <- fromIntegral <$> getWord8
                -- 15 + this account key = 16
                when (keyLen > 15) $ fail $
                    "Invalid number of multisig keys: " ++ (show keyLen)
                msKeys <- replicateM keyLen get 
                msReq  <- fromIntegral <$> getWord8
                -- + 1 because we count this account key as well
                when (msReq > keyLen + 1) $ fail $
                    "Invalid number of required keys: " ++ (show msReq)
                urlLen <- fromIntegral <$> getWord16le
                msURL  <- bsToString <$> getByteString urlLen
                return $ WAccountMS name idx key ext int msKeys msReq msURL
            _ -> fail $ "Invalid account type: " ++ (show t)
        
    put a = case a of
        (WAccount n i (AccPubKey k) ext int) -> do
            putWord8 0x00 -- type byte
            let bs = stringToBS n
            putWord16le $ fromIntegral $ BS.length bs 
            putByteString bs >> putWord32le i >> put k 
            putWord32le ext >> putWord32le int
        (WAccountMS n i (AccPubKey k) ext int ks r u) -> do
            putWord8 0x01 -- type byte
            let bs = stringToBS n
            putWord16le $ fromIntegral $ BS.length bs 
            putByteString bs >> putWord32le i >> put k 
            putWord32le ext >> putWord32le int
            putWord8 $ fromIntegral $ length ks
            forM_ ks put
            putWord8 $ fromIntegral r
            let bsurl = stringToBS u
            putWord16le $ fromIntegral $ BS.length bsurl
            putByteString bsurl

