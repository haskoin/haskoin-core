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

import Haskoin.Wallet
import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
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

{-
    config_{version} = 1
    config_{seed} = "Hello World"
    config_{lastaccindex} = 5
    config_{acccount} = 4
    config_{addressbook} = { address => value }
    config_{focus} = "default"

    account_{count} => {
        name => "mambo"
        index => 0
        pubkey => xpub...
        extAcc => 3
        intAcc => 2 
        mulsig => [xpub1,xpub2..]
        required => 2
        service => "Haskoin.org"
    }

    accountmap_{name} => "account_1"

    addr_{account}_{count} => {
        address => "13ab43..."
        label => "banana"
        index => 4
        account => 2 
        time -> unix
    }

    addrmap_{address} => "addr_2_1"
-}

{- 0 padded base 16 -}

toHexKey :: Int -> String
toHexKey i | i < 0x80000000 = pad ++ str
           | otherwise = error $ "Invalid index " ++ (show i)
           where str = bsToHex $ integerToBS $ fromIntegral i
                 pad = replicate (8 - length str) '0'

fromHexKey :: String -> Maybe Int
fromHexKey str = do
    i <- fromIntegral . bsToInteger <$> (hexToBS str)
    guard $ i < 0x80000000
    return i
    
data DBKey = KeyConfig     { dbKeyName       :: String }
           | KeyAcc        { dbKeyPos        :: Int }
           | KeyAccMap     { dbKeyName       :: String }
           | KeyExtAddr    { dbKeyAccPos     :: Int
                           , dbKeyAddrPos    :: Int
                           }
           | KeyExtAddrMap { dbKeyAddr       :: String }
           | KeyIntAddr    { dbKeyAccPos     :: Int
                           , dbKeyAddrPos    :: Int
                           }
           | KeyIntAddrMap { dbKeyAddr       :: String }
           | KeyCoin       { dbKeyCoinAccPos :: Int
                           , dbKeyCoinPos    :: Int 
                           }
           | KeyCoinMap    { dbKeyCoinOP     :: OutPoint }
           deriving (Eq, Show)


keyPrefix :: DBKey -> BS.ByteString
keyPrefix key = stringToBS $ case key of
    KeyConfig _     -> "config_"
    KeyAcc _        -> "account_"
    KeyAccMap _     -> "accountmap_"
    KeyExtAddr p1 _ -> "extaddr_" ++ (toHexKey p1) ++ "_"
    KeyExtAddrMap _ -> "extaddrmap_"
    KeyIntAddr p1 _ -> "intaddr_" ++ (toHexKey p1) ++ "_"
    KeyIntAddrMap _ -> "intaddrmap_"
    KeyCoin p1 _    -> "coin_" ++ (toHexKey p1) ++ "_"
    KeyCoinMap _    -> "coinmap_" 

keyToBS :: DBKey -> BS.ByteString
keyToBS key = BS.append (keyPrefix key) $ stringToBS $ case key of
    KeyConfig s     -> s
    KeyAcc p        -> toHexKey p
    KeyAccMap s     -> s
    KeyExtAddr _ p2 -> toHexKey p2
    KeyExtAddrMap s -> s
    KeyIntAddr _ p2 -> toHexKey p2
    KeyIntAddrMap s -> s
    KeyCoin _ p2    -> toHexKey p2
    KeyCoinMap op   -> bsToHex $ encode' op

bsToKey :: BS.ByteString -> Maybe DBKey
bsToKey bs 
    | isPrefixOf "config_" str = 
        KeyConfig <$> stripPrefix "config_" str
    | isPrefixOf "account_" str = 
        KeyAcc <$> (fromHexKey =<< stripPrefix "account_" str)
    | isPrefixOf "accountmap_" str = 
        KeyAccMap <$> stripPrefix "accountmap_" str
    | isPrefixOf "extaddrmap_" str = 
        KeyExtAddrMap <$> stripPrefix "extaddrmap_" str
    | isPrefixOf "intaddrmap_" str = 
        KeyIntAddrMap <$> stripPrefix "intaddrmap_" str
    | isPrefixOf "extaddr_" str = do
        case break (== '_') <$> (stripPrefix "extaddr_" str) of
            Just (p1,_:p2) -> liftM2 KeyExtAddr (fromHexKey p1) (fromHexKey p2)
            _ -> Nothing
    | isPrefixOf "intaddr_" str =
        case break (== '_') <$> (stripPrefix "intaddr_" str) of
            Just (p1,_:p2) -> liftM2 KeyIntAddr (fromHexKey p1) (fromHexKey p2)
            _ -> Nothing
    | isPrefixOf "coin_" str =
        case break (== '_') <$> (stripPrefix "coin_" str) of
            Just (p1,_:p2) -> liftM2 KeyCoin (fromHexKey p1) (fromHexKey p2)
    | isPrefixOf "coinmap_" str = stripPrefix "coinmap_" str >>= \key ->
        KeyCoinMap <$> (decodeToMaybe =<< hexToBS key)
    | otherwise = Nothing
    where str = bsToString bs

{- Default database get/put functions -}

dbGet :: MonadResource m => DBKey -> WalletDB m (Maybe BS.ByteString)
dbGet key = ask >>= \db -> lift $
    DB.get db DB.defaultReadOptions $ keyToBS key

dbPut :: MonadResource m => DBKey -> BS.ByteString -> WalletDB m ()
dbPut key value = ask >>= \db -> lift $
    DB.put db DB.defaultWriteOptions (keyToBS key) value

dbIterate :: MonadResource m 
          => DB.Iterator -> BS.ByteString -> Int -> m [BS.ByteString]
dbIterate iter prefix count
    | count > 0 = DB.iterKey iter >>= \keyM -> case keyM of
        Just key 
            | BS.isPrefixOf prefix key -> do
                val <- fromJust <$> DB.iterValue iter
                DB.iterNext iter
                liftM2 (:) (return val) (dbIterate iter prefix $ count - 1)
            | otherwise -> return []
        Nothing -> return []
    | otherwise = return []

dbGetConfig :: MonadResource m => String -> WalletDB m (Maybe BS.ByteString)
dbGetConfig key = dbGet $ KeyConfig key

dbPutConfig :: MonadResource m => String -> BS.ByteString -> WalletDB m ()
dbPutConfig key value = dbPut (KeyConfig key) value

dbGetAcc :: MonadResource m => String -> WalletDB m (Maybe WAccount)
dbGetAcc name = dbGet (KeyAccMap name) >>= \keyM -> case bsToKey =<< keyM of
    Just key -> (decodeToMaybe =<<) <$> dbGet key 
    _ -> return Nothing

dbGetAccPos :: MonadResource m => Int -> WalletDB m (Maybe WAccount)
dbGetAccPos pos = (decodeToMaybe =<<) <$> (dbGet $ KeyAcc pos)

dbPutAcc :: MonadResource m => WAccount -> WalletDB m ()
dbPutAcc acc = do
    dbPut (KeyAccMap $ accName acc) $ keyToBS key
    dbPut key $ encode' acc
    where key = KeyAcc $ accPos acc

dbGetExtAddr :: MonadResource m => String -> WalletDB m (Maybe WAddr)
dbGetExtAddr name = dbGetAddr name False

dbGetIntAddr :: MonadResource m => String -> WalletDB m (Maybe WAddr)
dbGetIntAddr name = dbGetAddr name True

dbGetAnyAddr :: MonadResource m => String -> WalletDB m (Maybe WAddr)
dbGetAnyAddr name = dbGetAddr name False >>= \addr -> case addr of
    Nothing -> dbGetAddr name False
    _       -> return addr

-- Generic address fetching function (for internal or external addr)
dbGetAddr :: MonadResource m => String -> Bool -> WalletDB m (Maybe WAddr)
dbGetAddr name int = dbGet keyMap >>= \keyM -> case bsToKey =<< keyM of
    Just key -> (decodeToMaybe =<<) <$> dbGet key
    _ -> return Nothing
    where keyMap = if int then KeyIntAddrMap name else KeyExtAddrMap name

dbGetExtAddrPos :: MonadResource m => String -> Int -> WalletDB m (Maybe WAddr)
dbGetExtAddrPos name pos = dbGetAddrPos name pos False

dbGetIntAddrPos :: MonadResource m => String -> Int -> WalletDB m (Maybe WAddr)
dbGetIntAddrPos name pos = dbGetAddrPos name pos True

dbGetAnyAddrPos :: MonadResource m => String -> Int -> WalletDB m (Maybe WAddr)
dbGetAnyAddrPos name pos = dbGetAddrPos name pos False >>= \addr -> case addr of
    Nothing -> dbGetAddrPos name pos True
    _       -> return addr

-- Generic function for fetching internal or external addresses by position
dbGetAddrPos :: MonadResource m => String -> Int -> Bool
               -> WalletDB m (Maybe WAddr)
dbGetAddrPos name pos int = dbGetAcc name >>= \accM -> case accM of
    Just acc -> (decodeToMaybe =<<) <$> (dbGet $ f (accPos acc) pos)
    _        -> return Nothing
    where f = if int then KeyIntAddr else KeyExtAddr
        
dbPutExtAddr :: MonadResource m => WAddr -> WalletDB m ()
dbPutExtAddr name = dbPutAddr name False

dbPutIntAddr :: MonadResource m => WAddr -> WalletDB m ()
dbPutIntAddr name = dbPutAddr name True

-- Generic function for saving internal or external addresses
dbPutAddr :: MonadResource m => WAddr -> Bool -> WalletDB m ()
dbPutAddr addr int = do
    dbPut (f $ wAddr addr) $ keyToBS key
    dbPut key $ encode' addr
    where key = if int then KeyIntAddr (wAccPos addr) (wPos addr)
                       else KeyExtAddr (wAccPos addr) (wPos addr)
          f   = if int then KeyIntAddrMap else KeyExtAddrMap

dbGetCoin :: MonadResource m => OutPoint -> WalletDB m (Maybe WCoin)
dbGetCoin op = dbGet (KeyCoinMap op) >>= \keyM -> case bsToKey =<< keyM of
    Just key -> (decodeToMaybe =<<) <$> dbGet key
    _ -> return Nothing

dbGetCoinPos :: MonadResource m => String -> Int -> WalletDB m (Maybe WCoin)
dbGetCoinPos name pos = dbGetAcc name >>= \accM -> case accM of
    Just acc -> (decodeToMaybe =<<) <$> (dbGet $ KeyCoin (accPos acc) pos)
    _        -> return Nothing

dbPutCoin :: MonadResource m => Int -> Int -> WCoin -> WalletDB m ()
dbPutCoin accPos pos coin = do
    dbPut key $ encode' coin
    dbPut (KeyCoinMap $ coinOutPoint coin) $ keyToBS key
    where key = KeyCoin accPos pos

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
            acc = WAccount { accName      = name
                           , accIndex     = accIndex 
                           , accPos       = (count+1)
                           , accKey       = key
                           , accExt       = maxBound
                           , accExtCount  = 0
                           , accInt       = maxBound
                           , accIntCount  = 0
                           , accCoinCount = 0
                           }
        dbPutAccIndex index >> dbPutAccCount (count+1) >> dbPutAcc acc
        dbPutFocus $ accName acc
        return acc

dbNewMSAcc :: MonadResource m => String -> Int -> [XPubKey] 
           -> WalletDB m WAccount
dbNewMSAcc name r mskeys = dbGetAcc name >>= \prev -> case prev of
    Just _  -> error $ "Account already exists: " ++ name
    Nothing -> do
        master <- fromJust <$> dbGetMaster
        index  <- (fromMaybe 0) . ((+1) <$>) <$> dbGetAccIndex
        count  <- (fromMaybe 0) <$> dbGetAccCount
        let (key,accIndex) = head $ accPubKeys master index
            acc = WAccountMS { accName      = name
                             , accIndex     = accIndex 
                             , accPos       = (count+1)
                             , accKey       = key
                             , accExt       = maxBound
                             , accExtCount  = 0
                             , accInt       = maxBound
                             , accIntCount  = 0
                             , accCoinCount = 0
                             , accMSKey     = mskeys
                             , accMSReq     = r
                             , accMSUrl     = ""
                             }
        dbPutAccIndex index >> dbPutAccCount (count+1) >> dbPutAcc acc
        dbPutFocus $ accName acc
        return acc

dbListAcc :: MonadResource m => WalletDB m [WAccount]
dbListAcc = ask >>= \db -> do
    count <- (fromMaybe 0) <$> dbGetAccCount
    lift $ DB.withIterator db DB.defaultReadOptions $ \iter -> do
        DB.iterSeek iter $ keyToBS key
        vals <- dbIterate iter (keyPrefix key) count
        return $ map decode' vals
    where key = KeyAcc 1

{- Address database functions -}

dbGenExtAddr :: MonadResource m => String -> Int -> WalletDB m [WAddr]
dbGenExtAddr name count = dbGenAddr name count False

dbGenIntAddr :: MonadResource m => String -> Int -> WalletDB m [WAddr]
dbGenIntAddr name count = dbGenAddr name count True

dbGenAddr :: MonadResource m => String -> Int -> Bool -> WalletDB m [WAddr]
dbGenAddr name count int = dbGetAcc name >>= \accM -> case accM of
    Nothing  -> error $ "Invalid account " ++ name
    Just acc -> do
        let wAddr  = map (buildAddr acc) $ zip (take count $ f acc) [1..]
        dbPutAcc $ if int then acc{ accInt      = wIndex $ last wAddr
                                  , accIntCount = accIntCount acc + count
                                  }
                          else acc{ accExt      = wIndex $ last wAddr
                                  , accExtCount = accExtCount acc + count
                                  }
        forM_ wAddr $ if int then dbPutIntAddr else dbPutExtAddr
        return wAddr
    where f (WAccountMS _ _ _ k i _ i' _ _ mk r _) = if int
              then intMulSigAddrs k mk r (i' + 1)
              else extMulSigAddrs k mk r (i  + 1)
          f (WAccount _ _ _ k i _ i' _ _) = if int
              then intAddrs k (i' + 1)
              else extAddrs k (i  + 1)
          buildAddr acc ((s,i),n) = 
              WAddr { wAddr     = s
                    , wLabel    = ""
                    , wIndex    = i
                    , wPos      = accExtCount acc + n
                    , wAccIndex = accIndex acc
                    , wAccPos   = accPos acc
                    , wInt      = int
                    }

dbListExtAddr :: MonadResource m => String -> Int -> Int -> WalletDB m [WAddr]
dbListExtAddr name from count 
    | from  <= 0 = error $ "Invalid from: " ++ (show from)
    | count <= 0 = error $ "Invalid count: " ++ (show count)
    | otherwise = dbGetAcc name >>= \accM -> case accM of
        Nothing -> error $ "Invalid account " ++ name
        Just acc -> ask >>= \db -> lift $
            DB.withIterator db DB.defaultReadOptions $ \iter -> do
                let key = KeyExtAddr (accPos acc) from
                DB.iterSeek iter $ keyToBS key
                vals <- dbIterate iter (keyPrefix key) count
                return $ catMaybes $ map decodeToMaybe vals

{- Transaction related functions -}

dbImportCoin :: MonadResource m => Hash256 -> (TxOut,Word32) 
             -> WalletDB m (Maybe WCoin)
dbImportCoin id (txout,i) = dbGetCoin op >>= \prevM -> case prevM of
    Just _  -> return Nothing
    Nothing -> do
        waddrM <- case out of
            Right (PayPKHash a)     -> dbGetAnyAddr $ addrToBase58 a
            Right (PayScriptHash a) -> dbGetAnyAddr $ addrToBase58 a
            _                       -> return Nothing
        case waddrM of
            Just waddr -> dbGetAccPos (wAccPos waddr) >>= \accM -> case accM of
                Just acc -> do
                    let newPos = accCoinCount acc + 1
                        coin = WCoin op txout
                    dbPutCoin (wAccPos waddr) newPos coin
                    dbPutAcc acc{ accCoinCount = newPos }
                    return $ Just coin
                _ -> return Nothing
            _ -> return Nothing
    where out = decodeOutput $ scriptOutput txout
          op  = OutPoint id i

dbImportTx :: MonadResource m => Tx -> WalletDB m [WCoin]
dbImportTx tx = do
    coins <- forM (zip (txOut tx) [0..]) $ dbImportCoin id
    return $ catMaybes coins
    where id = txid tx

dbListCoins :: MonadResource m => String -> WalletDB m [WCoin]
dbListCoins name = dbGetAcc name >>= \accM -> case accM of
    Nothing -> error $ "Invalid account: " ++ name
    Just acc -> ask >>= \db -> lift $
        DB.withIterator db DB.defaultReadOptions $ \iter -> do
            let key = KeyCoin (accCoinCount acc) 1
            DB.iterSeek iter $ keyToBS key
            vals <- dbIterate iter (keyPrefix key) $ accCoinCount acc
            return $ catMaybes $ map decodeToMaybe vals

{- Signing functions -}

dbSigData :: MonadResource m => Script -> OutPoint -> SigHash 
         -> WalletDB m (Maybe (SigInput,PrvKey))
dbSigData out op sh = do
    masterM <- dbGetMaster
    addrM   <- case decodeOutput out of
        Right (PayPKHash a)     -> dbGetAnyAddr $ addrToBase58 a
        Right (PayScriptHash a) -> dbGetAnyAddr $ addrToBase58 a
        _                       -> return Nothing
    case addrM of
        Just addr -> dbGetAccPos (wAccPos addr) >>= \accM -> return $ do
            acc <- accM
            mst <- masterM
            buildSigData out op sh mst addr acc
        _ -> return Nothing

buildSigData :: Script -> OutPoint -> SigHash -> MasterKey -> WAddr -> WAccount 
             -> Maybe (SigInput,PrvKey)
buildSigData out op sh master addr acc = do
    sgi <- if isMSAcc acc 
        then do
            aks <- fms (accKey acc) (accMSKey acc) (wIndex addr)
            let pks = map (xPubKey . runAddrPubKey) aks
                rdm = sortMulSig $ PayMulSig pks (accMSReq acc)
            return $ SigInputSH out op (encodeOutput rdm) sh
        else return $ SigInput out op sh
    accPrvKey <- accPrvKey master $ wAccIndex addr
    sigKey <- g accPrvKey $ wIndex addr
    return (sgi, xPrvKey $ runAddrPrvKey $ sigKey)
    where fms = if wInt addr then intMulSigKey else extMulSigKey
          g   = if wInt addr then intPrvKey else extPrvKey
    
{- Data types -}

data WCoin = WCoin { coinOutPoint :: OutPoint
                   , coinTxOut    :: TxOut
                   } deriving (Eq, Show)

instance Binary WCoin where
    get = WCoin <$> get <*> get
    put (WCoin o t) = put o >> put t

data WAddr = WAddr { wAddr     :: String
                   , wLabel    :: String
                   , wIndex    :: Word32
                   , wPos      :: Int
                   , wAccIndex :: Word32
                   , wAccPos   :: Int
                   , wInt      :: Bool
                   } deriving (Eq, Show)

instance Binary WAddr where

    get = do
        addrLen  <- fromIntegral <$> getWord8
        addr     <- bsToString <$> getByteString addrLen
        labelLen <- fromIntegral <$> getWord16le
        label    <- bsToString <$> getByteString labelLen
        idx      <- getWord32le
        pos      <- fromIntegral <$> getWord32le
        accIdx   <- getWord32le
        accPos   <- fromIntegral <$> getWord32le
        int      <- (/= 0) <$> getWord8
        return $ WAddr addr label idx pos accIdx accPos int

    put (WAddr a l i p ai ap int) = do
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
        putWord8 $ if int then 1 else 0

data WAccount = WAccount   { accName      :: String
                           , accIndex     :: Word32
                           , accPos       :: Int
                           , accKey       :: AccPubKey
                           , accExt       :: Word32
                           , accExtCount  :: Int
                           , accInt       :: Word32
                           , accIntCount  :: Int
                           , accCoinCount :: Int
                           }
              | WAccountMS { accName      :: String
                           , accIndex     :: Word32
                           , accPos       :: Int
                           , accKey       :: AccPubKey
                           , accExt       :: Word32
                           , accExtCount  :: Int
                           , accInt       :: Word32
                           , accIntCount  :: Int
                           , accCoinCount :: Int
                           , accMSKey     :: [XPubKey]
                           , accMSReq     :: Int
                           , accMSUrl     :: String
                           } deriving (Show, Eq)

isMSAcc :: WAccount -> Bool
isMSAcc (WAccount _ _ _ _ _ _ _ _ _) = False
isMSAcc _ = True

instance Binary WAccount where

    get = getWord8 >>= \t -> do
        nameLen  <- fromIntegral <$> getWord16le
        name     <- bsToString <$> getByteString nameLen
        idx      <- getWord32le
        pos      <- fromIntegral <$> getWord32le
        key      <- AccPubKey <$> get
        ext      <- getWord32le
        extCnt   <- fromIntegral <$> getWord32le
        int      <- getWord32le
        intCnt   <- fromIntegral <$> getWord32le
        txOutCnt <- fromIntegral <$> getWord32le
        case t of
            0x00 -> return WAccount{ accName      = name
                                   , accIndex     = idx
                                   , accPos       = pos 
                                   , accKey       = key 
                                   , accExt       = ext 
                                   , accExtCount  = extCnt 
                                   , accInt       = int 
                                   , accIntCount  = intCnt 
                                   , accCoinCount = txOutCnt
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
                return WAccountMS{ accName      = name 
                                 , accIndex     = idx
                                 , accPos       = pos 
                                 , accKey       = key 
                                 , accExt       = ext 
                                 , accExtCount  = extCnt 
                                 , accInt       = int 
                                 , accIntCount  = intCnt 
                                 , accCoinCount = txOutCnt
                                 , accMSKey     = msKeys 
                                 , accMSReq     = msReq 
                                 , accMSUrl     = msURL
                                 }
            _ -> fail $ "Invalid account type: " ++ (show t)
        
    put a = case a of
        (WAccount n i p (AccPubKey k) ext extC int intC oc) -> do
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
            putWord32le $ fromIntegral oc
        (WAccountMS n i p (AccPubKey k) ext extC int intC oc ks r u) -> do
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
            putWord32le $ fromIntegral oc
            putWord8 $ fromIntegral $ length ks
            forM_ ks put
            putWord8 $ fromIntegral r
            let bsurl = stringToBS u
            putWord16le $ fromIntegral $ BS.length bsurl
            putByteString bsurl

