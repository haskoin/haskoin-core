module Network.Haskoin.Wallet.KeyRing 
( 
-- *Database KeyRings
  initWallet
, newKeyRing
, keyRings
, keyRingSource
, getKeyRing

-- *Database Accounts
, accounts
, accountSource
, newAccount
, addAccountKeys
, getAccount
, isMultisigAccount
, isReadAccount
, isCompleteAccount

-- *Database Addresses
, getAddress
, addressSourceAll
, addressSource
, addressPage
, unusedAddresses
, addressCount
, setAddrLabel
, addressPrvKey
, useAddress
, generateAddrs
, setAccountGap
, firstAddrTime
, getPathRedeem
, getPathPubKey

-- *Database Bloom Filter
, getBloomFilter

-- * Helpers
, subSelectAddrCount
) where

import Control.Applicative ((<$>))
import Control.Monad (unless, when, liftM)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Exception (throwIO, throw)

import Data.Text (Text, unpack)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Conduit (Source, mapOutput, await, ($$))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.List (nub)
import Data.Word (Word32)
import qualified Data.ByteString as BS (ByteString, null)

import qualified Database.Persist as P
    ( Filter, SelectOpt( Asc )
    , updateWhere, update
    , (=.), (==.)
    )
import Database.Esqueleto 
    ( Value(..), SqlExpr, SqlQuery
    , InnerJoin(..), on
    , select, from, where_, val, sub_select, countRows, count, unValue
    , orderBy, limit, asc, desc, offset, selectSource, get
    , max_, not_, isNothing, case_, when_, then_, else_
    , (^.), (==.), (&&.), (<=.), (>=.), (>.), (-.), (<.)
    -- Reexports from Database.Persist
    , SqlPersistT, Entity(..)
    , getBy, insertUnique, insertMany_, insert_
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Block
import Network.Haskoin.Script
import Network.Haskoin.Node
import Network.Haskoin.Util
import Network.Haskoin.Constants
import Network.Haskoin.Node.HeaderTree

import Network.Haskoin.Wallet.Types
import Network.Haskoin.Wallet.Model

{- Initialization -}

initWallet :: MonadIO m => Double -> SqlPersistT m ()
initWallet fpRate = do
    prevConfigRes <- select $ from $ \c -> return $ count $ c ^. KeyRingConfigId
    let cnt = maybe 0 unValue $ listToMaybe prevConfigRes
    if cnt == (0 :: Int)
        then do
            time <- liftIO getCurrentTime
            -- Create an initial bloom filter
            -- TODO: Compute a random nonce 
            let bloom = bloomCreate (filterLen 0) fpRate 0 BloomUpdateNone
            insert_ $ KeyRingConfig 
                { keyRingConfigHeight      = 0 
                , keyRingConfigBlock       = headerHash genesisHeader
                , keyRingConfigBloomFilter = bloom 
                , keyRingConfigBloomElems  = 0 
                , keyRingConfigBloomFp     = fpRate 
                , keyRingConfigVersion     = 1 
                , keyRingConfigCreated     = time
                }
        else return () -- Nothing to do

{- KeyRing -}

-- | Create a new KeyRing from a seed
newKeyRing :: MonadIO m => KeyRingName -> BS.ByteString -> SqlPersistT m KeyRing
newKeyRing name seed
    | BS.null seed = liftIO . throwIO $ WalletException "The seed is empty"
    | otherwise = do
        now <- liftIO getCurrentTime
        let keyRing = KeyRing
                    { keyRingName    = name
                    , keyRingMaster  = makeXPrvKey seed
                    , keyRingCreated = now
                    }
        insertUnique keyRing >>= \resM -> case resM of
            Just _ -> return keyRing
            _ -> liftIO . throwIO $ WalletException $ unwords
                [ "KeyRing", unpack name, "already exists" ]

keyRings :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
         => SqlPersistT m [KeyRing]
keyRings = liftM (map entityVal) $ select $ from return

-- | Stream all KeyRings 
keyRingSource :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => Source (SqlPersistT m) KeyRing
keyRingSource = mapOutput entityVal $ selectSource $ from return

-- Helper functions to get a KeyRing if it exists, or throw an exception
-- otherwise.
getKeyRing :: MonadIO m => KeyRingName -> SqlPersistT m (Entity KeyRing)
getKeyRing name = getBy (UniqueKeyRing name) >>= \resM -> case resM of
    Just keyRingEnt -> return keyRingEnt
    _ -> liftIO . throwIO $ WalletException $ unwords
        [ "KeyRing", unpack name, "does not exist." ]

{- Account -}

-- | Fetch all the accounts in a keyring
accounts :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
         => KeyRingId -> SqlPersistT m [KeyRingAccount]
accounts ki = liftM (map entityVal) $ select $ accountsFrom ki

-- | Stream all accounts in a keyring
accountSource :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => KeyRingId -> Source (SqlPersistT m) KeyRingAccount
accountSource ki = mapOutput entityVal $ selectSource $ accountsFrom ki

accountsFrom :: KeyRingId -> SqlQuery (SqlExpr (Entity KeyRingAccount))
accountsFrom ki = 
    from $ \a -> do
        where_ $ a ^. KeyRingAccountKeyRing ==. val ki
        return a

-- | Create a new account
newAccount :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
           => Entity KeyRing
           -> AccountName 
           -> AccountType
           -> [XPubKey]
           -> SqlPersistT m (Entity KeyRingAccount)
newAccount (Entity ki keyRing) accountName accountType extraKeys = do
    unless (validAccountType accountType) $ 
        liftIO . throwIO $ WalletException "Invalid account type"

    -- Get the next account derivation
    derivM <- if accountTypeRead accountType then return Nothing else
        liftM Just $ nextAccountDeriv ki

    -- Derive the next account key
    let f d  = [ deriveXPubKey (derivePath d $ keyRingMaster keyRing) ] 
        keys = (maybe [] f derivM) ++ extraKeys
        
    -- Build the account
    now <- liftIO getCurrentTime
    let acc = KeyRingAccount
            { keyRingAccountKeyRing      = ki
            , keyRingAccountName         = accountName
            , keyRingAccountType         = accountType
            , keyRingAccountDerivation   = derivM
            , keyRingAccountKeys         = keys
            , keyRingAccountGap          = 0
            , keyRingAccountCreated      = now
            }

    -- Check if all the keys are valid
    unless (isValidAccKeys acc) $ 
        liftIO . throwIO $ WalletException "Invalid account keys"

    -- Insert our account in the database
    let canSetGap = isCompleteAccount acc
        newAcc    = acc{ keyRingAccountGap = if canSetGap then 10 else 0 }

    insertUnique newAcc >>= \resM -> case resM of
        -- The account got created. 
        Just ai -> do
            let accE = Entity ai newAcc
            -- If we can set the gap, create the gap addresses
            when canSetGap $ do
                _ <- createAddrs accE AddressExternal 20
                _ <- createAddrs accE AddressInternal 20
                return ()
            return accE
        -- The account already exists
        Nothing -> liftIO . throwIO $ WalletException $ unwords
            [ "Account", unpack accountName, "already exists" ]

-- | Add new thirdparty keys to a multisignature account. This function can
-- fail if the multisignature account already has all required keys. 
addAccountKeys :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
               => Entity KeyRingAccount -- ^ Account Entity
               -> [XPubKey]             -- ^ Thirdparty public keys to add
               -> SqlPersistT m KeyRingAccount -- ^ Account information
addAccountKeys (Entity ai acc) keys 
    -- We can only add keys on incomplete accounts
    | isCompleteAccount acc = liftIO . throwIO $
        WalletException "The account is already complete"
    | null keys || (not $ isValidAccKeys accKeys) = liftIO . throwIO $
        WalletException "Invalid account keys"
    | otherwise = do
        let canSetGap = isCompleteAccount accKeys
            updGap = if canSetGap then [ KeyRingAccountGap P.=. 10 ] else []
            newAcc = accKeys{ keyRingAccountGap = if canSetGap then 10 else 0 }
        -- Update the account with the keys and the new gap if it is complete
        P.update ai $ (KeyRingAccountKeys P.=. newKeys) : updGap
        -- If we can set the gap, create the gap addresses
        when canSetGap $ do
            let accE = Entity ai newAcc
            _ <- createAddrs accE AddressExternal 20
            _ <- createAddrs accE AddressInternal 20
            return ()
        return newAcc
  where
    newKeys = keyRingAccountKeys acc ++ keys
    accKeys = acc{ keyRingAccountKeys = newKeys }

isValidAccKeys :: KeyRingAccount -> Bool
isValidAccKeys KeyRingAccount{..} = case keyRingAccountType of
    AccountRegular _        -> length keyRingAccountKeys == 1
    -- read-only accounts can have 0 keys. Otherwise 1 key is required.
    AccountMultisig r _ n   -> goMultisig n (if r then 0 else 1)
  where
    goMultisig n minLen = 
        length keyRingAccountKeys == length (nub keyRingAccountKeys) &&
        length keyRingAccountKeys <= n &&
        length keyRingAccountKeys >= minLen

-- | Compute the next derivation path for a new account
nextAccountDeriv :: MonadIO m => KeyRingId -> SqlPersistT m HardPath
nextAccountDeriv ki = do
    lastRes <- select $ from $ \a -> do
        where_ (   a ^. KeyRingAccountKeyRing ==. val ki 
               &&. not_ (isNothing (a ^. KeyRingAccountDerivation))
               )
        orderBy [ desc (a ^. KeyRingAccountId) ]
        limit 1
        return $ a ^. KeyRingAccountDerivation
    return $ case lastRes of
        (Value (Just (prev :| i)):_) -> prev :| (i + 1)
        _ -> Deriv :| 0

-- Helper functions to get an Account if it exists, or throw an exception
-- otherwise.
getAccount :: MonadIO m => KeyRingName -> AccountName 
           -> SqlPersistT m (KeyRing, Entity KeyRingAccount)
getAccount keyRingName accountName = do
    as <- select $ from $ \(k `InnerJoin` a) -> do
        on $ a ^. KeyRingAccountKeyRing ==. k ^. KeyRingId
        where_ (   k ^. KeyRingName        ==. val keyRingName
               &&. a ^. KeyRingAccountName ==. val accountName
               )
        return (k, a)
    case as of
        ((Entity _ k, accEnt):_) -> return (k, accEnt)
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Account", unpack accountName, "does not exist" ]

{- Addresses -}

-- | Get an address if it exists, or throw an exception otherwise. Fetching
-- addresses in the hidden gap will also throw an exception.
getAddress :: MonadIO m
           => Entity KeyRingAccount              -- ^ Account Entity
           -> AddressType                        -- ^ Address type
           -> KeyIndex                           -- ^ Derivation index (key)
           -> SqlPersistT m (Entity KeyRingAddr) -- ^ Address
getAddress accE@(Entity ai _) addrType index = do
    res <- select $ from $ \x -> do
        where_ (   x ^. KeyRingAddrAccount ==. val ai
               &&. x ^. KeyRingAddrType    ==. val addrType
               &&. x ^. KeyRingAddrIndex   ==. val index
               &&. x ^. KeyRingAddrIndex   <.  subSelectAddrCount accE addrType
               )
        limit 1
        return x
    case res of
        (addrE:_) -> return addrE
        _ -> liftIO . throwIO $ WalletException $ unwords
            [ "Invalid address index", show index ]

-- | Stream all addresses in the wallet, including hidden gap addresses. This
-- is useful for building a bloom filter.
addressSourceAll :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
                 => Source (SqlPersistT m) KeyRingAddr
addressSourceAll = mapOutput entityVal $ selectSource $ from return

-- | Stream all addresses in one account. Hidden gap addresses are not included.
addressSource :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => Entity KeyRingAccount -- ^ Account Entity
              -> AddressType           -- ^ Address Type
              -> Source (SqlPersistT m) KeyRingAddr -- ^ Source of addresses
addressSource accE@(Entity ai _) addrType = mapOutput entityVal $ 
    selectSource $ from $ \x -> do
        where_ (   x ^. KeyRingAddrAccount ==. val ai
               &&. x ^. KeyRingAddrType    ==. val addrType
               &&. x ^. KeyRingAddrIndex   <.  subSelectAddrCount accE addrType
               )
        return x

-- | Get addresses by pages. 
addressPage :: MonadIO m 
            => Entity KeyRingAccount -- ^ Account Entity
            -> AddressType           -- ^ Address type 
            -> PageRequest           -- ^ Page request
            -> SqlPersistT m ([KeyRingAddr], Word32) 
                -- ^ Page result
addressPage accE@(Entity ai _) addrType page@PageRequest{..}
    | validPageRequest page = do
        cnt <- addressCount accE addrType

        let (d, m)  = cnt `divMod` pageLen
            maxPage = max 1 $ d + min 1 m

        when (pageNum > maxPage) $ liftIO . throwIO $ WalletException $
            unwords [ "Invalid page number", show pageNum ]

        if cnt == 0 then return ([], maxPage) else do
            res <- liftM (map entityVal) $ select $ from $ \x -> do
                where_ (   x ^. KeyRingAddrAccount ==. val ai
                       &&. x ^. KeyRingAddrType    ==. val addrType
                       &&. x ^. KeyRingAddrIndex   <.  val cnt
                       )
                let order = if pageReverse then asc else desc
                orderBy [ order (x ^. KeyRingAddrIndex) ]
                limit $ fromIntegral pageLen
                offset $ fromIntegral $ (pageNum - 1) * pageLen
                return x

            -- Flip the order back to ASC if we had it DEC
            let f = if pageReverse then id else reverse
            return (f res, maxPage)

    | otherwise = liftIO . throwIO $ WalletException $
        concat [ "Invalid page request"
               , " (Page: ", show pageNum, ", Page size: ", show pageLen, ")"
               ]

-- | Get a count of all the addresses in an account
addressCount :: MonadIO m 
             => Entity KeyRingAccount -- ^ Account Entity
             -> AddressType           -- ^ Address type 
             -> SqlPersistT m Word32  -- ^ Address Count
addressCount (Entity ai acc) addrType = do
    res <- select $ from $ \x -> do
        where_ (   x ^. KeyRingAddrAccount ==. val ai
               &&. x ^. KeyRingAddrType    ==. val addrType
               )
        return countRows
    let cnt = maybe 0 unValue $ listToMaybe res
    return $ if cnt > keyRingAccountGap acc
        then cnt - keyRingAccountGap acc
        else 0

-- | Get a list of all unused addresses.
unusedAddresses :: MonadIO m 
                => Entity KeyRingAccount -- ^ Account ID
                -> AddressType           -- ^ Address type
                -> SqlPersistT m [KeyRingAddr] -- ^ Unused addresses
unusedAddresses (Entity ai acc) addrType = do
    liftM (reverse . map entityVal) $ select $ from $ \x -> do
        where_ (   x ^. KeyRingAddrAccount ==. val ai
               &&. x ^. KeyRingAddrType    ==. val addrType
               )
        orderBy [ desc $ x ^. KeyRingAddrIndex ]
        limit $ fromIntegral $ keyRingAccountGap acc
        offset $ fromIntegral $ keyRingAccountGap acc
        return x

-- | Add a label to an address.
setAddrLabel :: MonadIO m
             => Entity KeyRingAccount -- ^ Account ID
             -> KeyIndex              -- ^ Derivation index
             -> AddressType           -- ^ Address type
             -> Text                  -- ^ New label
             -> SqlPersistT m KeyRingAddr
setAddrLabel accE i addrType label = do
    Entity addrI addr <- getAddress accE addrType i
    P.update addrI [ KeyRingAddrLabel P.=. label ]
    return $ addr{ keyRingAddrLabel = label }

-- | Returns the private key of an address.
addressPrvKey :: MonadIO m
              => KeyRing               -- ^ KeyRing
              -> Entity KeyRingAccount -- ^ Account Entity
              -> KeyIndex              -- ^ Derivation index of the address
              -> AddressType           -- ^ Address type
              -> SqlPersistT m PrvKeyC -- ^ Private key
addressPrvKey keyRing accE@(Entity ai acc) index addrType = do
    res <- select $ from $ \x -> do
        where_ (   x ^. KeyRingAddrAccount ==. val ai
               &&. x ^. KeyRingAddrType    ==. val addrType
               &&. x ^. KeyRingAddrIndex   ==. val index
               &&. x ^. KeyRingAddrIndex   <.  subSelectAddrCount accE addrType
               )
        return (x ^. KeyRingAddrFullDerivation)
    case res of
        (Value (Just deriv):_) -> 
            return $ xPrvKey $ derivePath deriv $ keyRingMaster keyRing
        _ -> liftIO . throwIO $ WalletException "Invalid address"

-- | Create new addresses in an account and increment the internal bloom filter.
-- This is a low-level function that simply creates the desired amount of new
-- addresses in an account, disregarding visible and hidden address gaps. You
-- should use the function `setAccountGap` if you want to control the gap of an
-- account instead.
createAddrs :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
            => Entity KeyRingAccount
            -> AddressType 
            -> Word32       
            -> SqlPersistT m [KeyRingAddr]
createAddrs (Entity ai acc) addrType n 
    | n == 0 = liftIO . throwIO $ WalletException $ 
        unwords [ "Invalid value", show n ]
    | not (isCompleteAccount acc) =
        liftIO . throwIO $ WalletException $ unwords
            [ "Keys are still missing from the incomplete account"
            , unpack $ keyRingAccountName acc
            ]
    | otherwise = do
        now <- liftIO getCurrentTime
        -- Find the next derivation index from the last address
        lastRes <- select $ from $ \x -> do
            where_ (   x ^. KeyRingAddrAccount ==. val ai
                   &&. x ^. KeyRingAddrType    ==. val addrType
                   )
            return $ max_ (x ^. KeyRingAddrIndex)
        let nextI = case lastRes of
                (Value (Just lastI):_) -> lastI + 1
                _ -> 0
            build (addr, keyM, rdmM, i) = KeyRingAddr
                { keyRingAddrAccount = ai
                , keyRingAddrAddress = addr
                , keyRingAddrIndex   = i
                , keyRingAddrType    = addrType
                , keyRingAddrLabel   = ""
                -- Full derivation from the master key
                , keyRingAddrFullDerivation = 
                    let f d = toMixed d :/ branchType :/ i
                    in  f <$> keyRingAccountDerivation acc
                -- Partial derivation under the account derivation
                , keyRingAddrDerivation = Deriv :/ branchType :/ i
                , keyRingAddrRedeem     = rdmM
                , keyRingAddrKey        = keyM
                , keyRingAddrCreated    = now
                }
            res = map build $ take (fromIntegral n) $ deriveFrom nextI

        -- Save the addresses and increment the bloom filter
        insertMany_ res
        incrementFilter res
        return res
  where 
    -- Branch type (external = 0, internal = 1)
    branchType = addrTypeIndex addrType
    deriveFrom = case keyRingAccountType acc of
        AccountMultisig _ m _ ->
            let f (a, r, i) = (a, Nothing, Just r, i)
                deriv  = Deriv :/ branchType
            in  map f . derivePathMSAddrs (keyRingAccountKeys acc) deriv m
        AccountRegular _ -> case keyRingAccountKeys acc of
            (key:_) -> let f (a, k, i) = (a, Just k, Nothing, i)
                       in  map f . derivePathAddrs key (Deriv :/ branchType)
            [] -> throw $ WalletException $ unwords
                [ "createAddrs: No key available in regular account"
                , unpack $ keyRingAccountName acc 
                ]

-- | Generate all the addresses up to a certain index
generateAddrs :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => Entity KeyRingAccount
              -> AddressType 
              -> KeyIndex
              -> SqlPersistT m Int
generateAddrs accE@(Entity ai acc) addrType genIndex = do
    cnt <- addressCount accE addrType
    let toGen = (fromIntegral genIndex) - (fromIntegral cnt) + 1
    if toGen > 0
        then do
            createAddrs accE addrType $ fromIntegral toGen
            return toGen
        else return 0

-- | Use an address and make sure we have enough gap addresses after it.
-- Returns the new addresses that have been created.
useAddress :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
           => KeyRingAddr -> SqlPersistT m [KeyRingAddr]
useAddress KeyRingAddr{..} = do
    res <- select $ from $ \x -> do
        where_ (   x ^. KeyRingAddrAccount ==. val keyRingAddrAccount
               &&. x ^. KeyRingAddrType  ==. val keyRingAddrType
               &&. x ^. KeyRingAddrIndex >.  val keyRingAddrIndex
               )
        return countRows
    case res of
        ((Value cnt):_) -> get keyRingAddrAccount >>= \accM -> case accM of
            Just acc -> do
                let accE    = Entity keyRingAddrAccount acc
                    gap     = fromIntegral (keyRingAccountGap acc) :: Int
                    missing = 2*gap - cnt 
                if missing > 0
                    then createAddrs accE keyRingAddrType $ fromIntegral missing
                    else return []
            _ -> return [] -- Should not happen
        _ -> return [] -- Should not happen

-- | Set the address gap of an account to a new value. This will create new
-- internal and external addresses as required. The gap can only be increased,
-- not decreased in size.
setAccountGap :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m) 
              => Entity KeyRingAccount -- ^ Account Entity
              -> Word32                -- ^ New gap value
              -> SqlPersistT m KeyRingAccount
setAccountGap accE@(Entity ai acc) gap
    | not (isCompleteAccount acc) =
        liftIO . throwIO $ WalletException $ unwords
            [ "Keys are still missing from the incomplete account"
            , unpack $ keyRingAccountName acc
            ]
    | missing <= 0 = liftIO . throwIO $ WalletException
        "The gap of an account can only be increased"
    | otherwise = do
        createAddrs accE AddressExternal $ fromInteger $ missing*2
        createAddrs accE AddressInternal $ fromInteger $ missing*2
        P.update ai [ KeyRingAccountGap P.=. gap ]
        return $ acc{ keyRingAccountGap = gap }
  where
    missing = toInteger gap - toInteger (keyRingAccountGap acc)

-- Return the creation time of the first address in the wallet.
firstAddrTime :: MonadIO m => SqlPersistT m (Maybe Timestamp)
firstAddrTime = do
    res <- select $ from $ \x -> do
        orderBy [ asc (x ^. KeyRingAddrId) ]
        limit 1
        return $ x ^. KeyRingAddrCreated
    return $ case res of
        (Value d:_) -> Just $ toPOSIX d
        _ -> Nothing
  where
    toPOSIX = fromInteger . round . utcTimeToPOSIXSeconds

{- Bloom filters -}

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
    cntRes <- select $ from $ \x -> return $ count $ x ^. KeyRingAddrId
    let elems = maybe 0 unValue $ listToMaybe cntRes
        newBloom = bloomCreate (filterLen elems) fpRate 0 BloomUpdateNone
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
    res <- select $ from $ \c -> do
        limit 1
        return ( c ^. KeyRingConfigBloomFilter
               , c ^. KeyRingConfigBloomElems
               , c ^. KeyRingConfigBloomFp
               )
    case res of
        ((Value b, Value n, Value fp):_) -> return (b, n, fp)
        _ -> liftIO . throwIO $
            WalletException "getBloomFilter: Database not initialized"

-- | Save a bloom filter and the number of elements it contains
setBloomFilter :: MonadIO m => BloomFilter -> Int -> SqlPersistT m ()
setBloomFilter bloom elems = 
    P.updateWhere [] [ KeyRingConfigBloomFilter P.=. bloom
                     , KeyRingConfigBloomElems  P.=. elems
                     ]

-- Helper function to compute the redeem script of a given derivation path
-- for a given multisig account.
getPathRedeem :: KeyRingAccount -> SoftPath -> RedeemScript
getPathRedeem acc@KeyRingAccount{..} deriv = case keyRingAccountType of
    AccountMultisig _ m _ -> if isCompleteAccount acc
        then sortMulSig $ PayMulSig pubKeys m
        else throw $ WalletException $ unwords 
            [ "getPathRedeem: Incomplete multisig account"
            , unpack keyRingAccountName 
            ]
    _ -> throw $ WalletException $ unwords 
        [ "getPathRedeem: Account", unpack keyRingAccountName
        , "is not a multisig account" 
        ]
  where
    f       = toPubKeyG . xPubKey . derivePubPath deriv
    pubKeys = map f keyRingAccountKeys

-- Helper function to compute the public key of a given derivation path for
-- a given non-multisig account.
getPathPubKey :: KeyRingAccount -> SoftPath -> PubKeyC
getPathPubKey acc@KeyRingAccount{..} deriv
    | isMultisigAccount acc = throw $ WalletException $ 
        unwords [ "getPathPubKey: Account", unpack keyRingAccountName
                , "is not a regular non-multisig account" 
                ]
    | otherwise = case keyRingAccountKeys of
        (key:_) -> xPubKey $ derivePubPath deriv key
        _ -> throw $ WalletException $ unwords
            [ "getPathPubKey: No keys are available in account"
            , unpack keyRingAccountName 
            ]

{- Helpers -}

subSelectAddrCount :: Entity KeyRingAccount
                   -> AddressType
                   -> SqlExpr (Value KeyIndex)
subSelectAddrCount (Entity ai acc) addrType =
    sub_select $ from $ \x -> do
        where_ (   x ^. KeyRingAddrAccount ==. val ai
               &&. x ^. KeyRingAddrType    ==. val addrType
               )
        let gap = val $ keyRingAccountGap acc
        return $ case_
            [ when_ (countRows >. gap)
              then_ (countRows -. gap)
            ] (else_ $ val 0)

validMultisigParams :: Int -> Int -> Bool
validMultisigParams m n = n >= 1 && n <= 15 && m >= 1 && m <= n

validAccountType :: AccountType -> Bool
validAccountType t = case t of
    AccountRegular _      -> True
    AccountMultisig _ m n -> validMultisigParams m n

isMultisigAccount :: KeyRingAccount -> Bool
isMultisigAccount acc = case keyRingAccountType acc of
    AccountRegular _      -> False
    AccountMultisig _ _ _ -> True

isReadAccount :: KeyRingAccount -> Bool
isReadAccount acc = case keyRingAccountType acc of
    AccountRegular r      -> r
    AccountMultisig r _ _ -> r

isCompleteAccount :: KeyRingAccount -> Bool
isCompleteAccount acc = case keyRingAccountType acc of
    AccountRegular _      -> length (keyRingAccountKeys acc) == 1
    AccountMultisig _ _ n -> length (keyRingAccountKeys acc) == n

