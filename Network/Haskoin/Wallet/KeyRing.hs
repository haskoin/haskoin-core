module Network.Haskoin.Wallet.KeyRing where

import Control.Applicative ((<$>))
import Control.Monad (unless, when, liftM)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Exception (throwIO, throw)

import Data.Text (Text, unpack)
import Data.Maybe (fromMaybe, mapMaybe, isNothing, isJust)
import Data.Time.Clock (getCurrentTime)
import Data.Conduit (Source, mapOutput, await, ($$))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.ByteString as BS (ByteString, null)

import Database.Persist
    ( Filter
    , Entity(..), SelectOpt( Asc, Desc, OffsetBy, LimitTo ), entityVal
    , getBy, insertUnique, updateGet, replace, count, get
    , insertMany_, selectFirst, selectList, insert_
    , selectSource, updateWhere
    , (=.), (==.), (<.), (>.)
    )
import Database.Persist.Sql (SqlPersistT)

import Network.Haskoin.Crypto
import Network.Haskoin.Block
import Network.Haskoin.Script
import Network.Haskoin.Node
import Network.Haskoin.Util
import Network.Haskoin.Constants

import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Model

{- Initialization -}

initWallet :: MonadIO m => Double -> SqlPersistT m ()
initWallet fpRate = do
    prevConfig <- selectFirst [] [Asc KeyRingConfigCreated]
    when (isNothing prevConfig) $ do
        time <- liftIO getCurrentTime
        -- Create an initial bloom filter
        -- TODO: Compute a random nonce 
        let bloom = bloomCreate (filterLen 0) fpRate 0 BloomUpdateNone
        insert_ $ 
            KeyRingConfig 0 (headerHash genesisHeader) bloom 0 fpRate 1 time

{- KeyRing -}

-- | Create a new KeyRing from a seed
newKeyRing :: MonadIO m => KeyRingName -> BS.ByteString -> SqlPersistT m KeyRing
newKeyRing keyRingName seed
    | BS.null seed = liftIO . throwIO $ WalletException "The seed is empty"
    | otherwise = do
        keyRingCreated <- liftIO getCurrentTime
        let keyRingMaster   = makeXPrvKey seed
            keyRing         = KeyRing{..}
        insertUnique keyRing >>= \resM -> case resM of
            Just _ -> return keyRing
            _ -> liftIO . throwIO $ WalletException $ unwords
                [ "KeyRing", unpack keyRingName, "already exists" ]

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
              => KeyRingName -> Source (SqlPersistT m) KeyRingAccount
accountSource keyRingName = do
    Entity ki _  <- lift $ getKeyRing keyRingName
    mapOutput entityVal $ selectSource [ KeyRingAccountKeyRing ==. ki ] []

-- | Generic account creation helper.
newAccountG :: MonadIO m
            => Text -- ^ Account name
            -> Entity KeyRing
            -> AccountType
            -> Maybe HardPath
            -> [XPubKey]
            -> Maybe Int -- ^ Required signatures (multisig)
            -> Maybe Int -- ^ Total keys (multisig)
            -> SqlPersistT m KeyRingAccount
newAccountG accountName keyRingE accountType derivation keys m n = do
    t <- liftIO getCurrentTime
    let Entity keyRingI keyRing = keyRingE
    let acc = KeyRingAccount
            { keyRingAccountName            = accountName
            , keyRingAccountKeyRingName     = keyRingName keyRing
            , keyRingAccountType            = accountType
            , keyRingAccountDerivation      = derivation
            , keyRingAccountKeys            = keys
            , keyRingAccountRequiredSigs    = m
            , keyRingAccountTotalKeys       = n
            , keyRingAccountGap             = 0
            , keyRingAccountCreated         = t
            , keyRingAccountKeyRing         = keyRingI
            }
    insertUnique acc >>= \resM -> case resM of
        -- The account got created. 
        Just _ -> return acc
        -- The account already exists
        Nothing -> liftIO . throwIO $ WalletException $ unwords
            [ "Account", unpack $ keyRingAccountName acc, "already exists" ]



-- | Create a new account in the given KeyRing. Accounts within the same
-- KeyRing must have unique names. To start using your account, you can
-- call the function 'setLookAhead' to create a new address gap.
newAccount :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
           => KeyRingName                  -- ^ KeyRing name
           -> AccountName                  -- ^ New account name
           -> SqlPersistT m KeyRingAccount -- ^ New regular account
newAccount keyRingName accountName = do
    keyRingE@(Entity keyRingI keyRing) <- getKeyRing keyRingName
    deriv <- nextAccountDeriv keyRingI
    let keys = [deriveXPubKey $ derivePath deriv (keyRingMaster keyRing)]
    _ <- newAccountG
        accountName keyRingE AccountRegular (Just deriv) keys Nothing Nothing
    setAccountGap keyRingName accountName 10

-- | Create a new multisignature account. You can add the keys with the
-- function `addAccountKeys`.
newAccountMultisig 
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
    => KeyRingName                  -- ^ KeyRing name
    -> AccountName                  -- ^ New account name
    -> [XPubKey]                    -- ^ Keys
    -> Int                          -- ^ Required signatures (m in m of n)
    -> Int                          -- ^ Total keys (n in m of n)
    -> SqlPersistT m KeyRingAccount -- ^ New multisig account
newAccountMultisig keyRingName accountName keys m n = do
    unless (validMultisigParams m n) $ liftIO . throwIO $ 
        WalletException "Invalid multisig parameters"
    keyRingE@(Entity keyRingI keyRing) <- getKeyRing keyRingName
    deriv <- nextAccountDeriv keyRingI
    let ks = [deriveXPubKey $ derivePath deriv (keyRingMaster keyRing)]
    acc <- newAccountG
        accountName keyRingE AccountMultisig (Just deriv) ks (Just m) (Just n)
    -- Add keys if some are provided
    if null keys 
        then return acc
        else do
            acc' <- addAccountKeys keyRingName accountName keys
            if incompleteMultisig acc'
                then return acc'
                else setAccountGap keyRingName accountName 10

-- | Create a new read-only account.
newAccountRead :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
               => KeyRingName                  -- ^ KeyRing name
               -> AccountName                  -- ^ New account name
               -> XPubKey                      -- ^ Read-only key
               -> SqlPersistT m KeyRingAccount -- ^ New regular account
newAccountRead keyRingName accountName key = do
    keyRingE <- getKeyRing keyRingName
    _ <- newAccountG
        accountName keyRingE AccountRead Nothing [key] Nothing Nothing
    setAccountGap keyRingName accountName 10

-- | Create a new read-only multisignature account. You can add the keys with
-- the function `addAccountKeys`
newAccountReadMultisig 
    :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
    => KeyRingName                  -- ^ KeyRing name
    -> AccountName                  -- ^ New account name
    -> [XPubKey]                    -- ^ Keys
    -> Int                          -- ^ Required signatures (m in m of n)
    -> Int                          -- ^ Total keys (n in m of n)
    -> SqlPersistT m KeyRingAccount -- ^ New multisig account
newAccountReadMultisig keyRingName accountName keys m n = do
    unless (validMultisigParams m n) $ liftIO . throwIO $ WalletException 
        "Invalid multisig parameters m of n"
    keyRingE <- getKeyRing keyRingName
    acc <- newAccountG
        accountName keyRingE AccountReadMultisig Nothing [] (Just m) (Just n)
    if null keys 
        then return acc 
        else do
            acc' <- addAccountKeys keyRingName accountName keys
            if incompleteMultisig acc'
                then return acc'
                else setAccountGap keyRingName accountName 10

-- | Add new thirdparty keys to a multisignature account. This function can
-- fail if the multisignature account already has all required keys. 
addAccountKeys :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
               => KeyRingName                  -- ^ KeyRing name
               -> AccountName                  -- ^ Account name
               -> [XPubKey]                    -- ^ Thirdparty public keys to add
               -> SqlPersistT m KeyRingAccount -- ^ Account information
addAccountKeys keyRingName accName keys 
    | null keys = liftIO . throwIO $ 
        WalletException "No keys have been provided"
    | otherwise = do
        Entity ki _ <- getKeyRing keyRingName
        f =<< getBy (UniqueAccount ki accName)
  where
    f (Just (Entity ai acc))
        | not $ isMultisigAccount acc 
            = liftIO . throwIO $ WalletException $ unwords
                [ "Account", unpack accName, "is not a multisig account" ]
        | any (`elem` keyRingAccountKeys acc) keys 
            = liftIO . throwIO $ WalletException $ unwords 
                [ "Adding duplicate keys to account", unpack accName ]
        | length (keyRingAccountKeys acc ++ keys) > 
          fromMaybe 0 (keyRingAccountTotalKeys acc) 
            = liftIO . throwIO $ WalletException $ unwords 
                [ "Adding too many keys to account", unpack accName ]
        -- Add the new keys at the end of the list.
        | otherwise = do
            acc' <- updateGet ai
                [ KeyRingAccountKeys =. (keyRingAccountKeys acc ++ keys) ]
            if incompleteMultisig acc'
                then return acc'
                else setAccountGap keyRingName accName 10
    f _ = liftIO . throwIO $ WalletException $ unwords
        [ "Account", unpack accName, "does not exist." ]

-- | Compute the next derivation path for a new account
nextAccountDeriv :: MonadIO m => KeyRingId -> SqlPersistT m HardPath
nextAccountDeriv ki = do
    lastM <- selectFirst [ KeyRingAccountKeyRing ==. ki ] 
                         [ Desc KeyRingAccountId ]
    let next = maybe (Deriv :| 0) f $ 
            (keyRingAccountDerivation . entityVal) =<< lastM
        f (prev :| i) = prev :| (i + 1)
        f _ = undefined
    return next

-- Helper functions to get an Account if it exists, or throw an exception
-- otherwise.
getAccount :: MonadIO m => KeyRingName -> AccountName 
           -> SqlPersistT m (Entity KeyRingAccount)
getAccount keyRingName accName = do
    Entity ki _ <- getKeyRing keyRingName
    accM <- getBy $ UniqueAccount ki accName
    case accM of
        Just accEnt -> return accEnt
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Account", unpack accName, "does not exist" ]

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
            when (fromIntegral i + 1 > addrCnt - keyRingAccountGap acc) $ 
                liftIO . throwIO $ WalletException $ unwords
                    [ "Address index", show i, "is in the hidden gap" ]
            return addrEnt
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Address index", show i, "does not exist" ]

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
        , KeyRingAddrIndex <. fromIntegral (addrCnt - keyRingAccountGap acc)
        ] []

-- | Get addresses by pages. 
addressPage :: MonadIO m 
            => KeyRingName -- ^ KeyRing name
            -> AccountName -- ^ Account name
            -> AddressType -- ^ Address type 
            -> PageRequest -- ^ Page request
            -> SqlPersistT m ([KeyRingAddr], Int) -- ^ Page result
addressPage keyRingName accountName addrType page@PageRequest{..}
    | validPageRequest page = do
        Entity ai acc <- getAccount keyRingName accountName
        addrCnt <- count [ KeyRingAddrAccount ==. ai
                         , KeyRingAddrType    ==. addrType
                         ]
        if addrCnt == 0 then return ([], 1) else do
            let maxIndex = addrCnt - keyRingAccountGap acc
                (d, m)   = maxIndex `divMod` pageLen
                maxPage  = d + min 1 m
            when (pageNum > maxPage) $ liftIO . throwIO $ WalletException $
                unwords [ "Invalid page number", show pageNum ]
            res <- selectList [ KeyRingAddrAccount ==. ai
                              , KeyRingAddrType ==. addrType
                              , KeyRingAddrIndex <. fromIntegral maxIndex
                              ]
                              [ if pageReverse 
                                      then Asc KeyRingAddrId 
                                      else Desc KeyRingAddrId
                              , LimitTo pageLen
                              , OffsetBy $ (pageNum - 1) * pageLen
                              ]
            let f | pageReverse = id
                  | otherwise   = reverse
            return (f $ map entityVal res, maxPage)
    | otherwise = liftIO . throwIO $ WalletException $
        concat [ "Invalid page request"
               , " (Page: ", show pageNum, ", Page size: ", show pageLen, ")"
               ]

-- | Get a list of all unused addresses.
addressUnused :: MonadIO m 
              => KeyRingName -> AccountName -> AddressType
              -> SqlPersistT m [KeyRingAddr]
addressUnused keyRingName accountName addrType = do
    Entity ai acc <- getAccount keyRingName accountName
    when (keyRingAccountGap acc <= 0) $ liftIO . throwIO $ WalletException $
        unwords [ "Account", unpack accountName, "has no unused addresses" ]
    res <- selectList [ KeyRingAddrAccount ==. ai
                      , KeyRingAddrType ==. addrType
                      ]
                      [ Desc KeyRingAddrId
                      , LimitTo $ keyRingAccountGap acc
                      , OffsetBy $ keyRingAccountGap acc
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
-- should use the function `setAccountGap` if you want to control the gap of an
-- account instead.
createAddrs :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
            => Entity KeyRingAccount
            -> AddressType 
            -> Int       
            -> SqlPersistT m [KeyRingAddr]
createAddrs (Entity keyRingAddrAccount acc) keyRingAddrType n 
    | n < 0 = liftIO . throwIO $ WalletException $ 
        unwords [ "Invalid negative value", show n ]
    | isMultisigAccount acc && incompleteMultisig acc =
        liftIO . throwIO $ WalletException $ unwords
            [ "Keys are still missing from the incomplete multisig account"
            , unpack $ keyRingAccountName acc
            ]
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
            build (keyRingAddrAddress, keyM, rdmM, keyRingAddrIndex) = 
                    -- Full derivation from the root
                let f x = toMixed x :/ branchType :/ keyRingAddrIndex
                    keyRingAddrRootDerivation = 
                        f <$> keyRingAccountDerivation acc
                    -- Partial derivation under the account derivation
                    keyRingAddrDerivation = 
                        Deriv :/ branchType :/ keyRingAddrIndex
                    keyRingAddrKey = keyM
                    keyRingAddrRedeem = rdmM
                    keyRingAddrKeyRingName = keyRingAccountKeyRingName acc
                    keyRingAddrAccountName = keyRingAccountName acc
                    keyRingAddrInBalance = 0
                    keyRingAddrOutBalance = 0
                    keyRingAddrInOfflineBalance = 0
                    keyRingAddrOutOfflineBalance = 0
                in  KeyRingAddr{..}
            res = map build $ take n $ deriveFrom nextI

        -- Save the addresses and increment the bloom filter
        insertMany_ res
        incrementFilter res
        return res
  where 
    -- Branch type (external = 0, internal = 1)
    branchType = addrTypeIndex keyRingAddrType
    m = flip fromMaybe (keyRingAccountRequiredSigs acc) $ 
            throw $ WalletException $ unwords
                [ "createAddrs: No required sigs in multisig account"
                , unpack $ keyRingAccountName acc
                ]
    deriveFrom 
        | isMultisigAccount acc = 
            let f (a, r, i) = (a, Nothing, Just r, i)
                deriv       = Deriv :/ branchType
            in  map f . derivePathMSAddrs (keyRingAccountKeys acc) deriv m
        | otherwise = case keyRingAccountKeys acc of
            [] -> throw $ WalletException $ unwords
                [ "createAddrs: No key available in regular account"
                , unpack $ keyRingAccountName acc 
                ]
            (key:_) -> 
                let f (a, k, i) = (a, Just k, Nothing, i)
                in  map f . derivePathAddrs key (Deriv :/ branchType)

-- | Use an address and make sure we have enough gap addresses after it.
-- Returns the number of new gap addresses created.
useAddress :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
           => KeyRingAddr -> SqlPersistT m (KeyRingAccountId, Int)
useAddress KeyRingAddr{..} = do
    accM <- get keyRingAddrAccount
    case accM of
        Just acc -> do
            cnt <- count [ KeyRingAddrIndex >. keyRingAddrIndex
                         , KeyRingAddrAccount ==. keyRingAddrAccount
                         , KeyRingAddrType ==. keyRingAddrType
                         ]
            let diff = (2 * keyRingAccountGap acc) - cnt
            when (diff > 0) $ do
                let accE = Entity keyRingAddrAccount acc
                _ <- createAddrs accE keyRingAddrType diff
                return ()
            return (keyRingAddrAccount, max 0 diff)
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Invalid foreign account key in address"
            , addrToBase58 keyRingAddrAddress
            ]

-- | Set the address gap of an account to a new value. This will create new
-- internal and external addresses as required. The gap can only be increased,
-- not decreased in size.
setAccountGap :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => KeyRingName -- ^ KeyRing name
              -> AccountName -- ^ Account name
              -> Int         -- ^ New gap value
              -> SqlPersistT m KeyRingAccount 
setAccountGap keyRingName accountName gap = do
    accE@(Entity ai acc) <- getAccount keyRingName accountName
    let diff = gap - keyRingAccountGap acc
    if diff < 0
        then liftIO . throwIO $ WalletException $ unwords
            [ "Can not decrease the gap from"
            , show $ keyRingAccountGap acc, "to", show gap
            ]
        else do
            let newAcc = acc{ keyRingAccountGap = gap }
            when (diff > 0) $ do
                _ <- createAddrs accE AddressExternal $ diff*2
                _ <- createAddrs accE AddressInternal $ diff*2
                replace ai newAcc
            return newAcc

-- Return the creation time of the first address in the wallet.
firstAddrTime :: MonadIO m => SqlPersistT m (Maybe Timestamp)
firstAddrTime = do
    fstKeyTimeM <- selectFirst [] [Asc KeyRingAddrId]
    return $ (toPOSIX . keyRingAddrCreated . entityVal) <$> fstKeyTimeM
  where
    toPOSIX = fromInteger . round . utcTimeToPOSIXSeconds

{- Bloom filters -}

-- TODO: Revwrite adding addresses to bloom filters using the conduits
-- instead of selectList.

-- | Add the given addresses to the bloom filter. If the number of elements
-- becomes too large, a new bloom filter is computed from scratch.
incrementFilter :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
                => [KeyRingAddr] 
                -> SqlPersistT m ()
incrementFilter addrs = do
    (bloom, elems, _) <- getBloomFilter
    let newElems = elems + (length addrs * 2)
    if filterLen newElems > filterLen elems 
        then computeNewFilter
        else setBloomFilter (addToFilter bloom addrs) newElems

-- | Generate a new bloom filter from the data in the database
computeNewFilter :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
                 => SqlPersistT m ()
computeNewFilter = do
    (_, _, fpRate) <- getBloomFilter
    -- Create a new empty bloom filter
    -- TODO: Choose a random nonce for the bloom filter
    -- TODO: Check global bloom filter length limits
    elems <- liftM (*2) $ count ([] :: [Filter KeyRingAddr])
    let newBloom = bloomCreate (filterLen elems) fpRate 0 BloomUpdateNone
    bloom <- addressSourceAll $$ bloomSink newBloom
    setBloomFilter bloom elems
  where
    bloomSink bloom = await >>= \addrM -> case addrM of
        Just addr -> bloomSink $ addToFilter bloom [addr]
        _         -> return bloom

-- Compute the size of a filter given a number of elements. Scale
-- the filter length by powers of 2.
filterLen :: Int -> Int
filterLen = round . pow2 . ceiling . log2
  where
    pow2 x = (2 :: Double) ** fromInteger x
    log2 x = logBase (2 :: Double) (fromIntegral x)

-- | Add elements to a bloom filter
addToFilter :: BloomFilter -> [KeyRingAddr] -> BloomFilter
addToFilter bloom addrs = 
    bloom3
  where
    pks  = mapMaybe keyRingAddrKey addrs
    rdms = mapMaybe keyRingAddrRedeem addrs
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
getPathRedeem :: KeyRingAccount -> SoftPath -> RedeemScript
getPathRedeem acc@KeyRingAccount{..} deriv 
    | not $ isMultisigAccount acc = throw $ WalletException $ 
        unwords [ "getPathRedeem: Account", unpack keyRingAccountName
                , "is not a multisig account" 
                ]
    | length keyRingAccountKeys < n = throw $ WalletException $ 
        unwords [ "getPathRedeem: Incomplete multisig account"
                , unpack keyRingAccountName 
                ]
    | otherwise = sortMulSig $ PayMulSig pubKeys m
  where
    pubKeys = map (toPubKeyG . xPubKey . derivePubPath deriv) keyRingAccountKeys
    m = flip fromMaybe keyRingAccountRequiredSigs $ 
        throw $ WalletException $ unwords
            [ "getPathRedeem: No required sigs in multisig account"
            , unpack keyRingAccountName
            ]
    n = flip fromMaybe keyRingAccountTotalKeys $ 
        throw $ WalletException $ unwords
            [ "getPathRedeem: No total keys in multisig account"
            , unpack keyRingAccountName
            ]

-- Helper function to compute the public key of a given derivation path for
-- a given non-multisig account.
getPathPubKey :: KeyRingAccount -> SoftPath -> PubKeyC
getPathPubKey acc@KeyRingAccount{..} deriv
    | isMultisigAccount acc = throw $ WalletException $ 
        unwords [ "getPathPubKey: Account", unpack keyRingAccountName
                , "is not a regular non-multisig account" 
                ]
    | otherwise = case keyRingAccountKeys of
        [] -> throw $ WalletException $ unwords
            [ "getPathPubKey: No keys are available in account"
            , unpack keyRingAccountName 
            ]
        (key:_) -> xPubKey $ derivePubPath deriv key

{- Helpers -}

validMultisigParams :: Int -> Int -> Bool
validMultisigParams m n = n >= 1 && n <= 15 && m >= 1 && m <= n

isMultisigAccount :: KeyRingAccount -> Bool
isMultisigAccount acc = case keyRingAccountType acc of
    AccountMultisig     -> True
    AccountReadMultisig -> True
    _                   -> False

isReadAccount :: KeyRingAccount -> Bool
isReadAccount acc = case keyRingAccountType acc of
    AccountRead         -> True
    AccountReadMultisig -> True
    _                   -> False

incompleteMultisig :: KeyRingAccount -> Bool
incompleteMultisig acc = isJust (keyRingAccountTotalKeys acc) &&
    Just (length $ keyRingAccountKeys acc) < keyRingAccountTotalKeys acc

