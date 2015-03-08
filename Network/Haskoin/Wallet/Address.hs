module Network.Haskoin.Wallet.Address 
( getAddress
, addressList
, addressPage
, unusedAddrs
, unlabeledAddrs
, unusedAddrsGeneric
, newAddrs
, newAddrsGeneric
, setAddrLabel
, addressPrvKey
, addLookAhead
, adjustLookAhead
, toPaymentAddr
, getBloomFilter
, getRedeem
, getRedeemIndex
, addrPubKey
) where

import Control.Monad (liftM, when, forM)
import Control.Monad.Reader (ReaderT)
import Control.Exception (throwIO)
import Control.Monad.Trans (MonadIO, liftIO)

import Data.Maybe (fromJust, isNothing, catMaybes)
import Data.Time (getCurrentTime)
import qualified Data.Text as T (Text, null)

import Database.Persist
    ( PersistUnique
    , PersistQuery
    , Entity(..)
    , getBy
    , get
    , replace
    , count
    , selectList
    , selectFirst
    , insertMany
    , updateWhere
    , entityVal
    , (=.), (==.), (>.)
    , SelectOpt( Asc, Desc, LimitTo, OffsetBy )
    , Key
    )

import Network.Haskoin.Node
import Network.Haskoin.Script
import Network.Haskoin.Crypto
import Network.Haskoin.Util

import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Account
import Network.Haskoin.Wallet.Root

toPaymentAddr :: DbAddressGeneric b -> LabeledAddress
toPaymentAddr x = LabeledAddress (dbAddressValue x) 
                                 (dbAddressLabel x) 
                                 (dbAddressIndex x)

-- Get an address by account name and key
getAddress :: (MonadIO m, PersistUnique b, PersistQuery b)
           => WalletName  -- ^ Wallet name
           -> AccountName -- ^ Account name
           -> KeyIndex    -- ^ Derivation index (key)
           -> Bool        -- ^ Internal address
           -> ReaderT b m LabeledAddress  -- ^ Payment address
getAddress wallet name key internal = do
    Entity wk _ <- getWalletEntity wallet
    aEnt <- getAccountEntity wk name
    liftM (toPaymentAddr . entityVal) $ getAddressEntity aEnt key internal

getAddressEntity :: (MonadIO m, PersistUnique b, PersistQuery b)
                 => Entity (DbAccountGeneric b)
                 -> KeyIndex 
                 -> Bool 
                 -> ReaderT b m (Entity (DbAddressGeneric b))
getAddressEntity (Entity ai av) key internal = do
    addrCnt <- count [ DbAddressAccount ==. ai 
                     , DbAddressInternal ==. internal
                     ]
    entM <- getBy $ UniqueAddressKey ai key internal
    when (isNothing entM) $ liftIO $ 
        throwIO $ WalletException "The address has not been generated yet"
    when ((fromIntegral key) + 1 > addrCnt - dbAccountGap av) $ liftIO $
        throwIO $ WalletException "The address is in the hidden gap"
    return $ fromJust entM

-- | Returns all addresses for an account.
addressList :: (MonadIO m, PersistUnique b, PersistQuery b)
            => WalletName  -- ^ Wallet name
            -> AccountName -- ^ Account name
            -> Bool        -- ^ Internal address
            -> ReaderT b m [LabeledAddress] -- ^ Payment addresses
addressList wallet name internal = do
    Entity wk _ <- getWalletEntity wallet
    Entity ai av <- getAccountEntity wk name
    addrCnt <- count [ DbAddressAccount ==. ai 
                     , DbAddressInternal ==. internal
                     ]
    addrs <- selectList [ DbAddressAccount ==. ai 
                        , DbAddressInternal ==. internal
                        ]
                        [ Asc DbAddressId 
                        , LimitTo $ addrCnt - dbAccountGap av
                        ]
    return $ map (toPaymentAddr . entityVal) addrs

-- | Returns a page of addresses for an account. Pages are numbered starting
-- from page 1. Requesting page 0 will return the last page. 
addressPage :: (MonadIO m, PersistUnique b, PersistQuery b)
            => WalletName  -- ^ Wallet name
            -> AccountName -- ^ Account name
            -> Int         -- ^ Requested page number
            -> Int         -- ^ Number of addresses per page
            -> Bool        -- ^ Internal address
            -> ReaderT b m ([LabeledAddress], Int) 
                -- ^ (Requested page, Highest page number)
addressPage wallet name pageNum resPerPage internal
    | pageNum < 0 = liftIO $ throwIO $ WalletException $ 
        unwords ["Invalid page number:", show pageNum]
    | resPerPage < 1 = liftIO $ throwIO $ WalletException $
        unwords ["Invalid results per page:",show resPerPage]
    | otherwise = do
        Entity wk _  <- getWalletEntity wallet
        Entity ai av <- getAccountEntity wk name
        totCount     <- addressCount ai
        let addrCount = totCount - dbAccountGap av
            maxPage = max 1 $ (addrCount + resPerPage - 1) `div` resPerPage
            page | pageNum == 0 = maxPage
                 | otherwise    = pageNum
            offset = (page - 1) * resPerPage
        when (page > maxPage) $ liftIO $ throwIO $ WalletException $ 
            unwords [ "The page number", show pageNum, "is too high" ]
        addrs <- selectList 
                [ DbAddressAccount ==. ai
                , DbAddressInternal ==. internal
                ] 
                [ Asc DbAddressId
                , LimitTo $ min resPerPage (addrCount - offset)
                , OffsetBy offset
                ]
        return $ ((map (toPaymentAddr . entityVal) addrs), maxPage)
  where
    addressCount ai = count [ DbAddressAccount ==. ai 
                            , DbAddressInternal ==. internal
                            ]

-- | Get list of unused addresses: those in the account gap.
unusedAddrs :: (MonadIO m, PersistUnique b, PersistQuery b)
            => WalletName  -- ^ Wallet name
            -> AccountName -- ^ Account name
            -> Bool        -- ^ Internal
            -> ReaderT b m [LabeledAddress] -- ^ Unused addresses
unusedAddrs wallet name internal = do
    Entity wk _ <- getWalletEntity wallet
    accE        <- getAccountEntity wk name
    liftM (map toPaymentAddr) $ unusedAddrsGeneric accE internal

-- | Get unused and unlabeled addresses.
unlabeledAddrs :: (MonadIO m, PersistUnique b, PersistQuery b)
              => WalletName  -- ^ Account name
              -> AccountName -- ^ Account name
              -> ReaderT b m [LabeledAddress] 
                -- ^ Unlabeled and unused addresses
unlabeledAddrs wallet name = 
    liftM f $ unusedAddrs wallet name False
  where
    f = dropWhile (not . T.null . laLabel)

-- | Get a list of unused addresses (generic version)
unusedAddrsGeneric :: (MonadIO m, PersistUnique b, PersistQuery b)
                    => Entity (DbAccountGeneric b)      -- ^ Account
                    -> Bool                             -- ^ Internal
                    -> ReaderT b m [DbAddressGeneric b] -- ^ Unused addresses
unusedAddrsGeneric (Entity ai acc) internal = do
    when (dbAccountGap acc <= 0) $ liftIO . throwIO $ WalletException 
        "No addresses available: account gap is zero (or less)"
    addrs <- selectList [ DbAddressAccount ==. ai
                        , DbAddressInternal ==. internal
                        ]
                        [ Desc DbAddressIndex
                        , OffsetBy $ dbAccountGap acc
                        , LimitTo $ dbAccountGap acc
                        ]
    return $ reverse $ map entityVal addrs

-- | Generate new payment addresses for an account
newAddrs :: (MonadIO m, PersistUnique b, PersistQuery b)
         => WalletName  -- ^ Wallet name
         -> AccountName -- ^ Account name
         -> Int         -- ^ Count
         -> ReaderT b m [LabeledAddress]  -- ^ Newly generated addresses
newAddrs wallet name cnt = do
    Entity wk _ <- getWalletEntity wallet
    acc <- getAccountEntity wk name
    liftM (map toPaymentAddr) $ newAddrsGeneric acc False cnt

newAddrsGeneric :: (MonadIO m, PersistUnique b, PersistQuery b)
                => Entity (DbAccountGeneric b)
                -> Bool      -- ^ Internal
                -> Int       -- ^ Count
                -> ReaderT b m [DbAddressGeneric b]
newAddrsGeneric (Entity ai acc) internal cnt = do
    time <- liftIO getCurrentTime
    lstEntM <- selectFirst
        [ DbAddressAccount ==. ai
        , DbAddressInternal ==. internal
        ]
        [ Desc DbAddressIndex ]
    let nxtIdx = case lstEntM of Nothing -> 0
                                 Just (Entity _ lst) -> dbAddressIndex lst + 1
    let build (a,i) = DbAddress a "" i ai internal time
        as = map build . take cnt $ f nxtIdx
    _ <- insertMany as
    -- Add the new addresses to the bloom filter
    incrementFilter as
    return as
  where 
    f idx | isMSAccount $ dbAccountValue acc = 
              (if internal then intMulSigAddrs else extMulSigAddrs)
                  (AccPubKey $ head $ accountKeys $ dbAccountValue acc)
                  (tail $ accountKeys $ dbAccountValue acc) 
                  (accountRequired $ dbAccountValue acc)
                  idx
          | otherwise =
              (if internal then intAddrs else extAddrs)
                  (accountKey $ dbAccountValue acc)
                  idx

-- | Add a label to an address.
setAddrLabel :: (MonadIO m, PersistQuery b, PersistUnique b)
             => WalletName  -- ^ Wallet name
             -> AccountName -- ^ Account name
             -> KeyIndex    -- ^ Derivation index of the address
             -> T.Text      -- ^ New label
             -> ReaderT b m LabeledAddress -- ^ New address information
setAddrLabel wallet name key label = do
    Entity wk _ <- getWalletEntity wallet
    aEnt <- getAccountEntity wk name
    (Entity i add) <- getAddressEntity aEnt key False
    let new = add { dbAddressLabel = label }
    replace i new
    return $ toPaymentAddr new

-- | Returns the private key of an address.
addressPrvKey :: (MonadIO m, PersistQuery b, PersistUnique b)
              => WalletName  -- ^ Account name
              -> AccountName -- ^ Account name
              -> KeyIndex    -- ^ Derivation index of the address
              -> ReaderT b m PrvKeyC -- ^ Private key
addressPrvKey wallet name key = do
    accPrv <- accountPrvKey wallet name
    add    <- getAddress wallet name key False
    let addrPrvKey = fromJust $ extPrvKey accPrv $ laIndex add
    return $ xPrvKey $ getAddrPrvKey addrPrvKey

-- Returns the number of new addresses created
adjustLookAhead :: (MonadIO m, PersistUnique b, PersistQuery b)
                => DbAddressGeneric b -> ReaderT b m Int
adjustLookAhead a = do
    acc <- liftM fromJust $ get ai
    cnt <- count [ DbAddressIndex >. dbAddressIndex a
                 , DbAddressAccount ==. ai
                 , DbAddressInternal ==. dbAddressInternal a
                 ]
    let diff = (2 * dbAccountGap acc) - cnt
    when (diff > 0) $ do
        _ <- newAddrsGeneric (Entity ai acc) (dbAddressInternal a) diff
        return ()
    return $ max 0 diff
  where
    ai = dbAddressAccount a

-- | Add addresses to an account and increase gap accordingly.  This will add
-- both internal and external addresses.
addLookAhead :: (MonadIO m, PersistUnique b, PersistQuery b)
             => WalletName  -- ^ Wallet name
             -> AccountName -- ^ Account name
             -> Int         -- ^ Count
             -> ReaderT b m ()
addLookAhead wallet name cnt = do
    Entity wk _ <- getWalletEntity wallet
    accE@(Entity ai acc) <- getAccountEntity wk name
    _ <- newAddrsGeneric accE True  $ cnt * 2
    _ <- newAddrsGeneric accE False $ cnt * 2
    replace ai acc { dbAccountGap = dbAccountGap acc + cnt }
    return ()

{- Bloom filters -}

-- | Add the given addresses to the bloom filter 
incrementFilter :: (MonadIO m, PersistUnique b, PersistQuery b)
                => [DbAddressGeneric b] -> ReaderT b m ()
incrementFilter addrs = do
    rdms <- liftM catMaybes $ forM addrs getRedeem
    pks  <- liftM catMaybes $ forM addrs addrPubKey
    (bloom, elems, _) <- getBloomFilter
    let newElems = elems + length addrs + length rdms + length pks
    if filterLen newElems > filterLen elems 
        then computeNewFilter
        else setBloomFilter (addToFilter bloom addrs rdms pks) newElems

-- | Generate a new bloom filter from the data in the database
computeNewFilter :: (MonadIO m, PersistUnique b, PersistQuery b) 
                 => ReaderT b m ()
computeNewFilter = do
    (_, _, fpRate) <- getBloomFilter
    addrs <- liftM (map entityVal) $ selectList [] []
    rdms  <- liftM catMaybes $ forM addrs getRedeem
    pks   <- liftM catMaybes $ forM addrs addrPubKey
    let len    = length addrs + length rdms + length pks
        -- Generate bloom filters of length multiple of 1000
        -- TODO: Choose a random nonce for the bloom filter
        bloom1 = bloomCreate (filterLen len) fpRate 0 BloomUpdateNone
        bloom  = addToFilter bloom1 addrs rdms pks
    setBloomFilter bloom len

-- | Add elements to a bloom filter
addToFilter :: BloomFilter 
            -> [DbAddressGeneric b] 
            -> [RedeemScript] 
            -> [PubKeyC]
            ->  BloomFilter
addToFilter bloom addrs rdms pks = 
    bloom3
  where
    -- Add the Hash160 of the addresses
    f b a  = bloomInsert b $ encode' $ getAddrHash a
    bloom1 = foldl f bloom $ map dbAddressValue addrs
    -- Add the redeem scripts
    g b r  = bloomInsert b $ encodeOutputBS r
    bloom2 = foldl g bloom1 rdms
    -- Add the public keys
    h b p  = bloomInsert b $ encode' p
    bloom3 = foldl h bloom2 pks

-- | Produces a bloom filter containing all the addresses in this wallet. This
-- includes internal and external addresses. The bloom filter can be set on a
-- peer connection to filter the transactions received by that peer.
getBloomFilter :: (MonadIO m, PersistQuery b) 
               => ReaderT b m (BloomFilter, Int, Double)
getBloomFilter = do
    cnfM <- selectFirst [] []
    case cnfM of
        Nothing -> liftIO $ throwIO $
            WalletException "getBloomFilter: Database not initialized"
        Just (Entity _ cnf) -> 
            return ( dbConfigBloomFilter cnf
                   , dbConfigBloomElems cnf
                   , dbConfigBloomFp cnf
                   )

-- | Save a bloom filter and the number of elements it contains
setBloomFilter :: (MonadIO m, PersistQuery b)
               => BloomFilter -> Int -> ReaderT b m ()
setBloomFilter bloom elems = 
    updateWhere [] [ DbConfigBloomFilter =. bloom
                   , DbConfigBloomElems  =. elems
                   ]

-- Builds a redeem script given an address. Only relevant for addresses
-- linked to multisig accounts. Otherwise it returns Nothing
getRedeem :: (MonadIO m, PersistUnique b, PersistQuery b)
          => DbAddressGeneric b -> ReaderT b m (Maybe RedeemScript)
getRedeem add = do
    acc <- liftM fromJust (get $ dbAddressAccount add)
    let deriv    = dbAddressIndex add
        internal = dbAddressInternal add
    return $ getRedeemIndex (dbAccountValue acc) deriv internal

getRedeemIndex :: Account -> KeyIndex -> Bool -> Maybe RedeemScript
getRedeemIndex acc deriv internal 
    | isMSAccount acc = Just $ sortMulSig $ PayMulSig pks req
    | otherwise       = Nothing
  where
    key      = head $ accountKeys acc 
    msKeys   = tail $ accountKeys acc
    addrKeys = fromJust $ f (AccPubKey key) msKeys deriv
    pks      = map (toPubKeyG . xPubKey . getAddrPubKey) addrKeys
    req      = accountRequired acc
    f        = if internal then intMulSigKey else extMulSigKey

addrPubKey :: (MonadIO m, PersistUnique b, PersistQuery b)
           => DbAddressGeneric b
           -> ReaderT b m (Maybe PubKeyC)
addrPubKey add = do
    acc <- liftM fromJust (get $ dbAddressAccount add)
    if isMSAccount (dbAccountValue acc) then return Nothing else do
        let deriv    = dbAddressIndex add
            internal = dbAddressInternal add
            accKey   = accountKey $ dbAccountValue acc
            f        = if internal then intPubKey else extPubKey
            pk       = fromJust $ f accKey deriv
        return $ Just $ xPubKey $ getAddrPubKey pk

