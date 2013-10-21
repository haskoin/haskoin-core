module Haskoin.Wallet.Store where

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Applicative

import Data.List
import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import qualified Database.LevelDB as DB

import Haskoin.Wallet.Keys
import Haskoin.Wallet.Manager
import Haskoin.Util

-- Track the database handle in a ReaderT monad
type WalletDB m a = ReaderT DB.DB m a

runWalletDB :: MonadResource m => FilePath -> WalletDB m a -> m a
runWalletDB fp wm = do
    db <- DB.open (fp ++ "/dbwallet") dbOptions 
    runReaderT wm db
    where dbOptions = DB.defaultOptions { DB.createIfMissing = True
                                        , DB.cacheSize = 2048
                                        }

-- config_{version} = 1
-- config_{seed} = "Hello World"
-- config_{lastaccindex} = 5
-- config_{acccount} = 4
-- config_{addressbook} = { address => value }
-- config_{focus} = "default"
-- 
-- account_{count} => {
--     name => "mambo"
--     index => 0
--     pubkey => xpub...
--     extAcc => 3
--     intAcc => 2 
--     mulsig => [xpub1,xpub2..]
--     required => 2
--     service => "Haskoin.org"
-- }
-- 
-- accountmap_{name} => "account_1"
-- 
-- addr_{account}_{count} => {
--     address => "13ab43..."
--     label => "banana"
--     index => 4
--     account => 2 
--     time -> unix
-- }
-- 
-- addrmap_{address} => "addr_2_1"

    
{- Default database get/put functions -}

dbGet :: MonadResource m => String -> WalletDB m (Maybe BS.ByteString)
dbGet key = ask >>= \db -> lift $
    DB.get db DB.defaultReadOptions $ stringToBS key

dbPut :: MonadResource m => String -> BS.ByteString -> WalletDB m ()
dbPut key value = ask >>= \db -> lift $
    DB.put db DB.defaultWriteOptions (stringToBS key) value

dbIterate :: MonadResource m 
          => DB.Iterator -> String -> Int -> m [BS.ByteString]
dbIterate iter prefix count
    | count > 0 = DB.iterKey iter >>= \keyM -> case keyM of
        Just key 
            | isPrefixOf prefix (bsToString key) -> do
                val <- fromJust <$> DB.iterValue iter
                DB.iterNext iter
                liftM2 (:) (return val) (dbIterate iter prefix $ count - 1)
            | otherwise -> return []
        Nothing -> return []
    | otherwise = return []

dbGetConfig :: MonadResource m => String -> WalletDB m (Maybe BS.ByteString)
dbGetConfig key = dbGet $ "config_" ++ key

dbPutConfig :: MonadResource m => String -> BS.ByteString -> WalletDB m ()
dbPutConfig key value = dbPut ("config_" ++ key) value

dbGetAcc :: MonadResource m => String -> WalletDB m (Maybe WAccount)
dbGetAcc name = dbGet ("accountmap_" ++ name) >>= \keyM -> case keyM of
    Just key -> do
        val <- dbGet $ bsToString key
        return $ decodeToMaybe =<< val
    _ -> return Nothing

dbPutAcc :: MonadResource m => WAccount -> WalletDB m ()
dbPutAcc acc = do
    dbPut ("accountmap_" ++ accName acc) $ stringToBS key
    dbPut key $ encode' acc
    where key = "account_" ++ (show $ accPos acc)

dbGetAddr :: MonadResource m => String -> WalletDB m (Maybe WAddr)
dbGetAddr name = dbGet ("addrmap_" ++ name) >>= \keyM -> case keyM of
    Just key -> do
        val <- dbGet $ bsToString key
        return $ decodeToMaybe =<< val
    _ -> return Nothing
        
dbPutAddr :: MonadResource m => WAddr -> WalletDB m ()
dbPutAddr addr = do
    dbPut ("addrmap_" ++ wAddr addr) $ stringToBS key
    dbPut key $ encode' addr
    where key = "addr_" ++ (show $ wAccPos addr) ++ "_" ++ (show $ wPos addr)

{- Database Config functions -}

isDBInit :: MonadResource m => WalletDB m Bool
isDBInit = dbGetConfig "initialized" >>= \init -> case init of
    Just bs -> return $ runGet' getWord8 bs == 1
    _       -> return False

dbInit :: MonadResource m => String -> WalletDB m ()
dbInit seed = isDBInit >>= \init -> if init
    then error "Wallet already initialized"
    else do
        dbPutConfig "version" $ runPut' $ putWord32le 1
        dbPutSeed seed
        dbNewAcc "default"
        dbPutConfig "initialized" $ runPut' $ putWord8 1

dbGetMaster :: MonadResource m => WalletDB m (Maybe MasterKey)
dbGetMaster = dbGetConfig "seed" >>= return . (makeMasterKey =<<)

-- Todo: encrypt this
dbPutSeed :: MonadResource m => String -> WalletDB m ()
dbPutSeed seed = dbGetMaster >>= \masterM -> case masterM of
    Just _  -> error "Seed already exists"
    Nothing -> dbPutConfig "seed" $ stringToBS seed

dbGetVersion :: MonadResource m => WalletDB m (Maybe Int)
dbGetVersion = dbGetConfig "version" >>= return . f
    where f = (fromIntegral . (runGet' getWord32le) <$>)

dbGetAccIndex :: MonadResource m => WalletDB m (Maybe Word32)
dbGetAccIndex = dbGetConfig "accindex" >>= return . (runGet' getWord32le <$>)

dbPutAccIndex :: MonadResource m => Word32 -> WalletDB m ()
dbPutAccIndex w = dbPutConfig "accindex" $ runPut' $ putWord32le w

dbGetAccCount :: MonadResource m => WalletDB m (Maybe Int)
dbGetAccCount = dbGetConfig "accnum" >>= \cntM -> return $
    fromIntegral . (runGet' getWord32le) <$> cntM

dbPutAccCount :: MonadResource m => Int -> WalletDB m ()
dbPutAccCount i = dbPutConfig "accnum" bs
    where bs = runPut' $ putWord32le $ fromIntegral i

dbGetFocus :: MonadResource m => WalletDB m (Maybe String)
dbGetFocus = dbGetConfig "focus" >>= return . (bsToString <$>)

dbPutFocus :: MonadResource m => String -> WalletDB m ()
dbPutFocus name = dbGetAcc name >>= \accM -> case accM of
    Nothing -> error $ "Invalid account " ++ name
    _ -> dbPutConfig "focus" $ stringToBS name

{- Account database functions -}

dbNewAcc :: MonadResource m => String -> WalletDB m WAccount
dbNewAcc name = dbGetAcc name >>= \prev -> case prev of
    Just _ -> error $ "Account already exists: " ++ name
    Nothing -> do
        master <- fromJust <$> dbGetMaster
        index  <- (fromMaybe 0) . ((+1) <$>) <$> dbGetAccIndex
        count  <- (fromMaybe 0) <$> dbGetAccCount
        let (key,accIndex) = head $ accPubKeys master index
            acc = WAccount { accName = name
                           , accIndex = accIndex 
                           , accPos = (count+1)
                           , accKey = key
                           , accExt = maxBound
                           , accExtCount = 0
                           , accInt = maxBound
                           , accIntCount = 0
                           }
        dbPutAccIndex index >> dbPutAccCount (count+1) >> dbPutAcc acc
        dbPutFocus $ accName acc
        return acc

dbListAcc :: MonadResource m => WalletDB m [WAccount]
dbListAcc = ask >>= \db -> do
    count <- (fromMaybe 0) <$> dbGetAccCount
    lift $ DB.withIterator db DB.defaultReadOptions $ \iter -> do
        DB.iterSeek iter $ stringToBS prefix
        vals <- dbIterate iter prefix count
        return $ map decode' vals
    where prefix = "account_"

{- Address database functions -}

dbGenExtAddr :: MonadResource m => String -> Int -> WalletDB m [WAddr]
dbGenExtAddr name count = dbGetAcc name >>= \accM -> case accM of
    Nothing -> error $ "Invalid account " ++ name
    Just acc -> do
        let new   = take count $ extAddrs (accKey acc) (accExt acc + 1)
            wAddr = map (buildAddr acc) $ zip new [1..]
        forM_ wAddr dbPutAddr
        dbPutAcc acc{ accExt      = wIndex $ last wAddr
                    , accExtCount = accExtCount acc + count
                    }
        return wAddr
    where buildAddr acc ((s,i),n) =  WAddr { wAddr     = s
                                           , wLabel    = ""
                                           , wIndex    = i
                                           , wPos      = accExtCount acc + n
                                           , wAccIndex = accIndex acc
                                           , wAccPos   = accPos acc
                                           }

dbListExtAddr :: MonadResource m => String -> Int -> Int -> WalletDB m [WAddr]
dbListExtAddr name from count 
    | count <= 0 = error $ "Invalid count: " ++ (show count)
    | otherwise = dbGetAcc name >>= \accM -> case accM of
        Nothing -> error $ "Invalid account " ++ name
        Just acc -> ask >>= \db -> lift $
            DB.withIterator db DB.defaultReadOptions $ \iter -> do
                let prefix = "addr_" ++ (show $ accPos acc) ++ "_" 
                    key    = prefix ++ (show from)
                DB.iterSeek iter $ stringToBS key
                vals <- dbIterate iter prefix count
                return $ catMaybes $ map decodeToMaybe vals

{- Data types -}

data WAddr = WAddr { wAddr     :: String
                   , wLabel    :: String
                   , wIndex    :: Word32
                   , wPos      :: Int
                   , wAccIndex :: Word32
                   , wAccPos   :: Int
                   } deriving (Eq, Show)

instance Binary WAddr where

    get = do
        addrLen <- fromIntegral <$> getWord8
        addr <- bsToString <$> getByteString addrLen
        labelLen <- fromIntegral <$> getWord16le
        label <- bsToString <$> getByteString labelLen
        idx <- getWord32le
        pos <- fromIntegral <$> getWord32le
        accIdx <- getWord32le
        accPos <- fromIntegral <$> getWord32le
        return $ WAddr addr label idx pos accIdx accPos

    put (WAddr a l i p ai ap) = do
        let bsAddr  = stringToBS a
            bsLabel = stringToBS l
        putWord8 $ fromIntegral $ BS.length bsAddr
        putByteString bsAddr
        putWord16le $ fromIntegral $ BS.length bsLabel
        putByteString bsLabel
        putWord32le i
        putWord32le $ fromIntegral p
        putWord32le ai
        putWord32le $ fromIntegral ap

data WAccount = WAccount   { accName     :: String
                           , accIndex    :: Word32
                           , accPos      :: Int
                           , accKey      :: AccPubKey
                           , accExt      :: Word32
                           , accExtCount :: Int
                           , accInt      :: Word32
                           , accIntCount :: Int
                           }
              | WAccountMS { accName     :: String
                           , accIndex    :: Word32
                           , accPos      :: Int
                           , accKey      :: AccPubKey
                           , accExt      :: Word32
                           , accExtCount :: Int
                           , accInt      :: Word32
                           , accIntCount :: Int
                           , accMSKey    :: [XPubKey]
                           , accMSReq    :: Int
                           , accMSUrl    :: String
                           } deriving (Show, Eq)

isMSAcc :: WAccount -> Bool
isMSAcc (WAccount _ _ _ _ _ _ _ _) = False
isMSAcc _ = True

instance Binary WAccount where

    get = getWord8 >>= \t -> do
        nameLen <- fromIntegral <$> getWord16le
        name    <- bsToString <$> getByteString nameLen
        idx     <- getWord32le
        pos     <- fromIntegral <$> getWord32le
        key     <- AccPubKey <$> get
        ext     <- getWord32le
        extCnt  <- fromIntegral <$> getWord32le
        int     <- getWord32le
        intCnt  <- fromIntegral <$> getWord32le
        case t of
            0x00 -> return WAccount{ accName = name
                                   , accIndex = idx
                                   , accPos = pos 
                                   , accKey = key 
                                   , accExt = ext 
                                   , accExtCount = extCnt 
                                   , accInt = int 
                                   , accIntCount = intCnt 
                                   }
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
                return WAccountMS{ accName = name 
                                 , accIndex = idx
                                 , accPos = pos 
                                 , accKey = key 
                                 , accExt = ext 
                                 , accExtCount = extCnt 
                                 , accInt = int 
                                 , accIntCount = intCnt 
                                 , accMSKey = msKeys 
                                 , accMSReq = msReq 
                                 , accMSUrl = msURL
                                 }
            _ -> fail $ "Invalid account type: " ++ (show t)
        
    put a = case a of
        (WAccount n i p (AccPubKey k) ext extC int intC) -> do
            putWord8 0x00 -- type byte
            let bs = stringToBS n
            putWord16le $ fromIntegral $ BS.length bs 
            putByteString bs 
            putWord32le i 
            putWord32le $ fromIntegral p
            put k 
            putWord32le ext 
            putWord32le $ fromIntegral extC
            putWord32le int
            putWord32le $ fromIntegral intC
        (WAccountMS n i p (AccPubKey k) ext extC int intC ks r u) -> do
            putWord8 0x01 -- type byte
            let bs = stringToBS n
            putWord16le $ fromIntegral $ BS.length bs 
            putByteString bs 
            putWord32le i 
            putWord32le $ fromIntegral p
            put k 
            putWord32le ext 
            putWord32le $ fromIntegral extC
            putWord32le int
            putWord32le $ fromIntegral intC
            putWord8 $ fromIntegral $ length ks
            forM_ ks put
            putWord8 $ fromIntegral r
            let bsurl = stringToBS u
            putWord16le $ fromIntegral $ BS.length bsurl
            putByteString bsurl

