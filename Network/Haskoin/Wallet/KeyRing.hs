module Network.Haskoin.Wallet.KeyRing where

import Control.Applicative ((<$>))
import Control.Monad (unless, when, forM_)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Exception (throwIO, throw)

import Data.Text (Text, pack, unpack)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import Data.Aeson.TH (deriveJSON)
import Data.Conduit (Source, mapOutput)
import qualified Data.ByteString as BS (ByteString, null)

import Database.Persist
    ( PersistUnique, PersistQuery, PersistStore, Filter
    , Entity(..), SelectOpt( Asc, Desc, OffsetBy, LimitTo ), entityVal
    , getBy, insertUnique, update, updateGet, replace, count, get
    , insertMany_, selectFirst, selectList
    , selectSource, updateWhere
    , (=.), (==.), (<.), (>.)
    )
import Database.Persist.Sql (SqlPersistT)

import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Node
import Network.Haskoin.Util

import Network.Haskoin.Wallet.Types.DeriveJSON
import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Model

{- KeyRing -}

-- | Create a new KeyRing from a seed
newKeyRing :: MonadIO m => KeyRingName -> BS.ByteString -> SqlPersistT m KeyRing
newKeyRing keyRingName seed
    | BS.null seed = liftIO . throwIO $ WalletException "The seed is empty"
    | otherwise = do
        keyRingCreated <- liftIO getCurrentTime
        let keyRingMaster   = makeXPrvKey seed
            keyRingAccIndex = Nothing
            keyRing         = KeyRing{..}
        insertUnique keyRing >>= \resM -> case resM of
            Just _ -> return keyRing
            _ -> liftIO . throwIO $ WalletException $ unwords
                [ "KeyRing", unpack keyRingName, "already exists." ]

-- | Stream all KeyRings 
keyRingSource :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => Source (SqlPersistT m) KeyRing
keyRingSource = mapOutput entityVal $ selectSource [] []

-- Helper functions to get a KeyRing if it exists, or throw an exception
-- otherwise.
getKeyRing :: MonadIO m => KeyRingName -> SqlPersistT m (Entity KeyRing)
getKeyRing keyRingName = do
    resM <- getBy $ UniqueKeyRing keyRingName
    case resM of
        Just keyRingEnt -> return keyRingEnt
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "KeyRing", unpack keyRingName, "does not exist." ]

{- Account -}

-- | Stream all accounts in a keyring
accountSource :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => KeyRingName -> Source (SqlPersistT m) Account
accountSource keyRingName = do
    Entity ki _  <- lift $ getKeyRing keyRingName
    mapOutput entityVal $ selectSource [ AccountKeyRing ==. ki ] []

-- | Create a new account in the given KeyRing. Accounts within the same
-- KeyRing must have unique names. To start using your account, you can
-- call the function 'setLookAhead' to create a new address gap.
newAccount :: MonadIO m
           => KeyRingName           -- ^ KeyRing name
           -> AccountName           -- ^ New account name
           -> SqlPersistT m Account -- ^ New regular account
newAccount keyRingName accountName = do
    Entity accountKeyRing (KeyRing _ master _) <- getKeyRing keyRingName
    deriv <- nextAccountDeriv accountKeyRing
    let accountKeys         = [deriveXPubKey $ derivePath deriv master]
        accountType         = AccountRegular
        accountDerivation   = Just deriv
        accountRequiredSigs = Nothing
        accountTotalKeys    = Nothing
        accountGap          = 0
    accountCreated <- liftIO getCurrentTime
    saveAccount Account{..}

-- | Create a new multisignature account. You can add the keys with the
-- function `addAccountKeys`.
newAccountMultisig 
    :: MonadIO m
    => KeyRingName           -- ^ KeyRing name
    -> AccountName           -- ^ New account name
    -> Int                   -- ^ Required signatures (m in m of n)
    -> Int                   -- ^ Total keys (n in m of n)
    -> SqlPersistT m Account -- ^ New multisig account
newAccountMultisig keyRingName accountName m n = do
    unless (validMultisigParams m n) $ liftIO . throwIO $ WalletException 
        "Invalid multisig parameters m of n"
    Entity accountKeyRing (KeyRing _ master _) <- getKeyRing keyRingName
    deriv <- nextAccountDeriv accountKeyRing
    let accountKeys         = [deriveXPubKey $ derivePath deriv master]
        accountType         = AccountMultisig
        accountDerivation   = Just deriv
        accountRequiredSigs = Just m
        accountTotalKeys    = Just n
        accountGap          = 0
    accountCreated <- liftIO getCurrentTime
    saveAccount Account{..}

-- | Create a new read-only account.
newAccountRead :: MonadIO m
               => KeyRingName           -- ^ KeyRing name
               -> AccountName           -- ^ New account name
               -> XPubKey               -- ^ Read-only key
               -> SqlPersistT m Account -- ^ New regular account
newAccountRead keyRingName accountName key = do
    Entity accountKeyRing _ <- getKeyRing keyRingName
    let accountKeys         = [key]
        accountType         = AccountRead
        accountDerivation   = Nothing
        accountRequiredSigs = Nothing
        accountTotalKeys    = Nothing
        accountGap          = 0
    accountCreated <- liftIO getCurrentTime
    saveAccount Account{..}

-- | Create a new read-only multisignature account. You can add the keys with
-- the function `addAccountKeys`
newAccountReadMultisig 
    :: MonadIO m
    => KeyRingName           -- ^ KeyRing name
    -> AccountName           -- ^ New account name
    -> Int                   -- ^ Required signatures (m in m of n)
    -> Int                   -- ^ Total keys (n in m of n)
    -> SqlPersistT m Account -- ^ New multisig account
newAccountReadMultisig keyRingName accountName m n = do
    unless (validMultisigParams m n) $ liftIO . throwIO $ WalletException 
        "Invalid multisig parameters m of n"
    Entity accountKeyRing _ <- getKeyRing keyRingName
    let accountKeys         = []
        accountType         = AccountReadMultisig
        accountDerivation   = Nothing
        accountRequiredSigs = Just m
        accountTotalKeys    = Just n
        accountGap          = 0
    accountCreated <- liftIO getCurrentTime
    saveAccount Account{..}

-- Helper function to save an account in the database if it doesn't exist, or
-- throw an exception otherwise. 
saveAccount :: MonadIO m => Account -> SqlPersistT m Account
saveAccount acc = insertUnique acc >>= \resM -> case resM of
    -- The account got created. 
    Just _ -> return acc 
    -- The account already exists
    _ -> liftIO . throwIO $ WalletException $ unwords
        [ "Account", unpack $ accountName acc, "already exists." ]

-- | Add new thirdparty keys to a multisignature account. This function can
-- fail if the multisignature account already has all required keys. 
addAccountKeys :: MonadIO m
               => KeyRingName           -- ^ KeyRing name
               -> AccountName           -- ^ Account name
               -> [XPubKey]             -- ^ Thirdparty public keys to add
               -> SqlPersistT m Account -- ^ Returns the account information
addAccountKeys keyRingName accName keys 
    | null keys = liftIO . throwIO $ 
        WalletException "No keys have been provided."
    | otherwise = do
        Entity ki _ <- getKeyRing keyRingName
        f =<< getBy (UniqueAccount ki accName)
  where
    f (Just (Entity ai acc))
        | not $ isMultisigAccount acc 
            = liftIO . throwIO $ WalletException $ unwords
                [ "Account", unpack accName, "is not a multisig account" ]
        | any (`elem` accountKeys acc) keys 
            = liftIO . throwIO $ WalletException $ unwords 
                [ "Adding duplicate keys to account", unpack accName ]
        | length (accountKeys acc ++ keys) > fromMaybe 0 (accountTotalKeys acc) 
            = liftIO . throwIO $ WalletException $ unwords 
                [ "Adding too many keys to account", unpack accName ]
        -- Add the new keys at the end of the list.
        | otherwise = updateGet ai [ AccountKeys =. (accountKeys acc ++ keys) ]
    f _ = liftIO . throwIO $ WalletException $ unwords
        [ "Account", unpack accName, "does not exist." ]

-- | Compute the next derivation path for a new account
nextAccountDeriv :: MonadIO m => KeyRingId -> SqlPersistT m HardPath
nextAccountDeriv ki = do
    lastM <- selectFirst [ AccountKeyRing ==. ki ] [ Desc AccountId ]
    let next = maybe (Deriv :| 0) f $ (accountDerivation . entityVal) =<< lastM
        f (prev :| i) = prev :| (i + 1)
    return next

-- Helper functions to get an Account if it exists, or throw an exception
-- otherwise.
getAccount :: MonadIO m => KeyRingName -> AccountName 
           -> SqlPersistT m (Entity Account)
getAccount keyRingName accName = do
    Entity ki _ <- getKeyRing keyRingName
    accM <- getBy $ UniqueAccount ki accName
    case accM of
        Just accEnt -> return accEnt
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Account", unpack accName, "does not exist." ]

{- Addresses -}

-- | Get an address if it exists, or throw an exception otherwise. Fetching
-- addresses in the hidden gap will also throw an exception.
getAddress :: MonadIO m
           => KeyRingName                        -- ^ KeyRing name
           -> AccountName                        -- ^ Account name
           -> KeyIndex                           -- ^ Derivation index (key)
           -> AddressType                        -- ^ Address type
           -> SqlPersistT m (Entity KeyRingAddr) -- ^ Address
getAddress keyRingName accName i addrType = do
    Entity ai acc <- getAccount keyRingName accName
    resM <- getBy $ UniqueAddrIndex ai i addrType
    case resM of
        Just addrEnt -> do
            addrCnt <- count [ KeyRingAddrAccount ==. ai 
                             , KeyRingAddrType    ==. addrType
                             ]
            when ((fromIntegral i) + 1 > addrCnt - accountGap acc) $ 
                liftIO . throwIO $ WalletException $ unwords
                    [ "Address index", show i, "is in the hidden gap." ]
            return addrEnt
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Address index", show i, "does not exist." ]

-- | Stream all addresses in the wallet, including hidden gap addresses. This
-- is useful for building a bloom filter.
addressSourceAll :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
                 => Source (SqlPersistT m) KeyRingAddr
addressSourceAll = mapOutput entityVal $ selectSource [] []

-- | Stream all addresses in one account. Hidden gap addresses are not included.
addressSource :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => KeyRingName -> AccountName -> AddressType 
              -> Source (SqlPersistT m) KeyRingAddr
addressSource keyRingName accName addrType = do
    Entity ai acc <- lift $ getAccount keyRingName accName
    addrCnt <- lift $ count [ KeyRingAddrAccount ==. ai 
                            , KeyRingAddrType    ==. addrType
                            ]
    mapOutput entityVal $ selectSource 
        [ KeyRingAddrAccount ==. ai 
        , KeyRingAddrType ==. addrType
        , KeyRingAddrIndex <. fromIntegral (addrCnt - (accountGap acc))
        ] []

-- | Get addresses by pages. 
addressPage :: MonadIO m 
            => KeyRingName                 -- ^ KeyRing name
            -> AccountName                 -- ^ Account name
            -> AddressType                 -- ^ Address type 
            -> PageRequest                 -- ^ Page request
            -> SqlPersistT m [KeyRingAddr] -- ^ Page result
addressPage keyRingName accountName addrType page@PageRequest{..}
    | validPageRequest page = do
        Entity ai acc <- getAccount keyRingName accountName
        addrCnt <- count [ KeyRingAddrAccount ==. ai
                         , KeyRingAddrType    ==. addrType
                         ]
        let maxIndex = fromIntegral (addrCnt - (accountGap acc))
        res <- selectList [ KeyRingAddrAccount ==. ai
                          , KeyRingAddrType ==. addrType
                          , KeyRingAddrIndex <. maxIndex
                          ]
                          [ if pageReverse 
                                then Desc KeyRingAddrId 
                                else Asc KeyRingAddrId
                          , LimitTo pageLen
                          , OffsetBy $ (pageNum - 1) * pageLen
                          ]
        return $ map entityVal res
    | otherwise = liftIO . throwIO $ WalletException $
        unwords [ "Invalid page request:", show page ]

-- | Get a list of all unused addresses.
addressUnused :: MonadIO m 
              => KeyRingName -> AccountName -> AddressType
              -> SqlPersistT m [KeyRingAddr]
addressUnused keyRingName accountName addrType = do
    Entity ai acc <- getAccount keyRingName accountName
    when (accountGap acc <= 0) $ liftIO . throwIO $ WalletException $
        unwords [ "Account", unpack accountName, "has no unused addresses" ]
    res <- selectList [ KeyRingAddrAccount ==. ai
                      , KeyRingAddrType ==. addrType
                      ]
                      [ Desc KeyRingAddrId
                      , LimitTo $ accountGap acc
                      , OffsetBy $ accountGap acc
                      ]
    return $ reverse $ map entityVal res

-- | Add a label to an address.
setAddrLabel :: MonadIO m
             => KeyRingName               -- ^ KeyRing name
             -> AccountName               -- ^ Account name
             -> KeyIndex                  -- ^ Derivation index
             -> AddressType               -- ^ Address type
             -> Text                      -- ^ New label
             -> SqlPersistT m KeyRingAddr -- ^ New Address
setAddrLabel keyRingName accountName i addrType label = do
    Entity k addr <- getAddress keyRingName accountName i addrType
    let newAddr = addr{ keyRingAddrLabel = label }
    replace k newAddr
    return newAddr

-- | Returns the private key of an address.
addressPrvKey :: MonadIO m
              => KeyRingName           -- ^ KeyRing name
              -> AccountName           -- ^ Account name
              -> KeyIndex              -- ^ Derivation index of the address
              -> AddressType           -- ^ Address type
              -> SqlPersistT m PrvKeyC -- ^ Private key
addressPrvKey keyRingName accountName i addrType = do
    Entity _ (KeyRing _ master _) <- getKeyRing keyRingName
    Entity _ addr <- getAddress keyRingName accountName i addrType
    case keyRingAddrRootDerivation addr of
        Just deriv -> return $ xPrvKey $ derivePath deriv master
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Can not get private keys from read-only account"
            , unpack accountName
            ]

-- | Create new addresses in an account and increment the internal bloom filter.
-- This is a low-level function that simply creates the desired amount of new
-- addresses in an account, disregarding visible and hidden address gaps. You
-- should use the function `setAddrGap` if you want to control the gap of an
-- account instead.
createAddrs :: MonadIO m
            => Entity Account
            -> AddressType 
            -> Int       
            -> SqlPersistT m [KeyRingAddr]
createAddrs (Entity keyRingAddrAccount acc) keyRingAddrType n 
    | n < 0 = liftIO . throwIO $ WalletException $ 
        unwords [ "Invalid negative value", show n ]
    | n == 0 = return []
    | otherwise = do
        keyRingAddrCreated <- liftIO getCurrentTime
        lastM <- selectFirst [ KeyRingAddrAccount ==. keyRingAddrAccount
                             , KeyRingAddrType    ==. keyRingAddrType
                             ] 
                             [ Desc KeyRingAddrId ]

            -- Find the next derivation index from the last address
        let nextI = maybe 0 (+1) $ keyRingAddrIndex . entityVal <$> lastM
            keyRingAddrLabel = ""
            -- Build the new addresses
            build (keyRingAddrAddress, keyRingAddrIndex) = 
                    -- Full derivation from the root
                let f x = (toMixed x) :/ branchType :/ keyRingAddrIndex
                    keyRingAddrRootDerivation = f <$> accountDerivation acc
                    -- Partial derivation under the account derivation
                    keyRingAddrDerivation = 
                        Deriv :/ branchType :/ keyRingAddrIndex
                in  KeyRingAddr{..}
            res = map build $ take n $ deriveFrom nextI

        -- Save the addresses and increment the bloom filter
        insertMany_ res
        incrementFilter acc res
        return res
  where 
    -- Branch type (external = 0, internal = 1)
    branchType = addrTypeIndex keyRingAddrType
    m = flip fromMaybe (accountRequiredSigs acc) $ 
            throw $ WalletException $ unwords
                [ "createAddrs: No required sigs in multisig account"
                , unpack $ accountName acc
                ]
    deriveFrom 
        | isMultisigAccount acc = 
            derivePathMSAddrs (accountKeys acc) (Deriv :/ branchType) m
        | otherwise = case accountKeys acc of
            [] -> throw $ WalletException $ unwords
                [ "createAddrs: No key available in regular account"
                , unpack $ accountName acc 
                ]
            (key:_) -> derivePathAddrs key (Deriv :/ branchType)

-- | Use an address and make sure we have enough gap addresses after it.
-- Returns the number of new gap addresses created.
useAddress :: MonadIO m => KeyRingAddr -> SqlPersistT m Int
useAddress KeyRingAddr{..} = do
    accM <- get keyRingAddrAccount
    case accM of
        Just acc -> do
            cnt <- count [ KeyRingAddrIndex >. keyRingAddrIndex
                         , KeyRingAddrAccount ==. keyRingAddrAccount
                         , KeyRingAddrType ==. keyRingAddrType
                         ]
            let diff = (2 * accountGap acc) - cnt
            when (diff > 0) $ do
                let accE = Entity keyRingAddrAccount acc
                _ <- createAddrs accE keyRingAddrType diff
                return ()
            return $ max 0 diff
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Invalid foreign account key in address"
            , addrToBase58 keyRingAddrAddress
            ]

-- | Set the address gap of an account to a new value. This will create new
-- internal and external addresses as required. The gap can only be increased,
-- not decreased in size.
setAddrGap :: MonadIO m
           => KeyRingName -- ^ KeyRing name
           -> AccountName -- ^ Account name
           -> Int         -- ^ New gap value
           -> SqlPersistT m ()
setAddrGap keyRingName accountName gap = do
    accE@(Entity ai acc) <- getAccount keyRingName accountName
    let diff = gap - accountGap acc
    if diff < 0
        then liftIO . throwIO $ WalletException $ unwords
            [ "Can not decrease the value of the address gap for account"
            , unpack accountName, "from", show $ accountGap acc, "to", show gap
            ]
        else when (diff > 0) $ do
            createAddrs accE AddressExternal diff
            createAddrs accE AddressInternal diff
            replace ai acc{ accountGap = gap }

{- Bloom filters -}

-- | Add the given addresses to the bloom filter. If the number of elements
-- becomes too large, a new bloom filter is computed from scratch.
incrementFilter :: MonadIO m => Account -> [KeyRingAddr] -> SqlPersistT m ()
incrementFilter acc addrs
    | isMultisigAccount acc = go [] rdmScrp
    | otherwise             = go pubKeys []
  where
    rdmScrp = map (getPathRedeem acc . keyRingAddrDerivation) addrs
    pubKeys = map (getPathPubKey acc . keyRingAddrDerivation) addrs
    go xs ys = do
        (bloom, elems, _) <- getBloomFilter
        let newElems = elems + length addrs + length xs + length ys
        if filterLen newElems > filterLen elems 
            then computeNewFilter
            else setBloomFilter (addToFilter bloom addrs xs ys) newElems

-- | Generate a new bloom filter from the data in the database
computeNewFilter :: MonadIO m => SqlPersistT m ()
computeNewFilter = do
    (_, _, fpRate) <- getBloomFilter
    -- Create a new empty bloom filter
    -- TODO: Choose a random nonce for the bloom filter
    -- TODO: Check global bloom filter length limits
    cnt <- count ([] :: [Filter KeyRingAddr])
    setBloomFilter (bloomCreate (filterLen cnt) fpRate 0 BloomUpdateNone) cnt
    -- Loop over all accounts in the database
    accs <- selectList [] []
    forM_ accs $ \(Entity ai acc@Account{..}) -> do
        -- Only update bloom filters on complete multisig accounts
        let complete = not (isMultisigAccount acc)
                     || Just (length accountKeys) == accountTotalKeys
        when complete $ do
            addrs <- selectList [KeyRingAddrAccount ==. ai] []
            -- This will not recurse back here because the bloom length is 
            -- sufficiently big to include all addresses.
            incrementFilter acc $ map entityVal addrs

-- Compute the size of a filter given a number of elements. Scale
-- the filter length by powers of 2.
filterLen :: Int -> Int
filterLen = round . pow2 . ceiling . log2
  where
    pow2 x = (2 :: Double) ** (fromInteger x)
    log2 x = log (fromIntegral x) / log (2 :: Double)

-- | Add elements to a bloom filter
addToFilter :: BloomFilter -> [KeyRingAddr] -> [PubKeyC] -> [RedeemScript] 
            -> BloomFilter
addToFilter bloom addrs pks rdms = 
    bloom3
  where
    -- Add the Hash160 of the addresses
    f1 b a  = bloomInsert b $ encode' $ getAddrHash a
    bloom1 = foldl f1 bloom $ map keyRingAddrAddress addrs
    -- Add the redeem scripts
    f2 b r  = bloomInsert b $ encodeOutputBS r
    bloom2 = foldl f2 bloom1 rdms
    -- Add the public keys
    f3 b p  = bloomInsert b $ encode' p
    bloom3 = foldl f3 bloom2 pks

-- | Returns a bloom filter containing all the addresses in this wallet. This
-- includes internal and external addresses. The bloom filter can be set on a
-- peer connection to filter the transactions received by that peer.
getBloomFilter :: MonadIO m => SqlPersistT m (BloomFilter, Int, Double)
getBloomFilter = do
    cnfM <- selectFirst [] []
    case cnfM of
        Just (Entity _ KeyRingConfig{..}) -> 
            return ( keyRingConfigBloomFilter
                   , keyRingConfigBloomElems
                   , keyRingConfigBloomFp
                   )
        Nothing -> liftIO . throwIO $
            WalletException "getBloomFilter: Database not initialized"

-- | Save a bloom filter and the number of elements it contains
setBloomFilter :: MonadIO m => BloomFilter -> Int -> SqlPersistT m ()
setBloomFilter bloom elems = 
    updateWhere [] [ KeyRingConfigBloomFilter =. bloom
                   , KeyRingConfigBloomElems  =. elems
                   ]

-- Helper function to compute the redeem script of a given derivation path
-- for a given multisig account.
getPathRedeem :: Account -> SoftPath -> RedeemScript
getPathRedeem acc@Account{..} deriv 
    | not $ isMultisigAccount acc = throw $ WalletException $ 
        unwords [ "getPathRedeem: Account", unpack accountName
                , "is not a multisig account" 
                ]
    | length accountKeys < n = throw $ WalletException $ 
        unwords [ "getPathRedeem: Incomplete multisig account"
                , unpack accountName 
                ]
    | otherwise = sortMulSig $ PayMulSig pubKeys m
  where
    pubKeys = map (toPubKeyG . xPubKey . derivePubPath deriv) accountKeys
    m = flip fromMaybe accountRequiredSigs $ throw $ WalletException $ unwords
        [ "getPathRedeem: No required sigs in multisig account"
        , unpack accountName
        ]
    n = flip fromMaybe accountTotalKeys $ throw $ WalletException $ unwords
        [ "getPathRedeem: No total keys in multisig account"
        , unpack accountName
        ]

-- Helper function to compute the public key of a given derivation path for
-- a given non-multisig account.
getPathPubKey :: Account -> SoftPath -> PubKeyC
getPathPubKey acc@Account{..} deriv
    | isMultisigAccount acc = throw $ WalletException $ 
        unwords [ "getPathPubKey: Account", unpack accountName
                , "is not a regular non-multisig account" 
                ]
    | otherwise = case accountKeys of
        [] -> throw $ WalletException $ unwords
            [ "getPathPubKey: No keys are available in account"
            , unpack accountName 
            ]
        (key:_) -> xPubKey $ derivePubPath deriv key

{- Helpers -}

validMultisigParams :: Int -> Int -> Bool
validMultisigParams m n = n >= 1 && n <= 15 && m >= 1 && m <= n

isMultisigAccount :: Account -> Bool
isMultisigAccount acc = case accountType acc of
    AccountMultisig     -> True
    AccountReadMultisig -> True
    _                   -> False

isReadAccount :: Account -> Bool
isReadAccount acc = case accountType acc of
    AccountRead         -> True
    AccountReadMultisig -> True
    _                   -> False

