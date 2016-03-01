module Network.Haskoin.Wallet.Accounts
(
-- *Database KeyRings
  initWallet

-- *Database Accounts
, accounts
, newAccount
, renameAccount
, addAccountKeys
, getAccount
, isMultisigAccount
, isReadAccount
, isCompleteAccount

-- *Database Addresses
, getAddress
, addressesAll
, addresses
, addressList
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

import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Exception (throw)

import Data.Text (Text, unpack)
import Data.Maybe (mapMaybe, listToMaybe, isJust, isNothing)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.List (nub)
import Data.Word (Word32)
import Data.String.Conversions (cs)

import qualified Database.Persist as P (updateWhere, update , (=.))
import Database.Esqueleto
    ( Value(..), SqlExpr
    , select, from, where_, val, sub_select, countRows, count, unValue
    , orderBy, limit, asc, desc, offset, get
    , max_, case_, when_, then_, else_
    , (^.), (==.), (&&.), (>.), (-.), (<.)
    -- Reexports from Database.Persist
    , SqlPersistT, Entity(..)
    , insertUnique, insert_
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
    prevConfigRes <- select $ from $ \c -> return $ count $ c ^. WalletStateId
    let cnt = maybe 0 unValue $ listToMaybe prevConfigRes
    when (cnt == (0 :: Int)) $ do
        time <- liftIO getCurrentTime
        -- Create an initial bloom filter
        -- TODO: Compute a random nonce
        let bloom = bloomCreate (filterLen 0) fpRate 0 BloomUpdateNone
        insert_ WalletState
            { walletStateHeight      = 0
            , walletStateBlock       = headerHash genesisHeader
            , walletStateBloomFilter = bloom
            , walletStateBloomElems  = 0
            , walletStateBloomFp     = fpRate
            , walletStateVersion     = 1
            , walletStateCreated     = time
            }

{- Account -}

-- | Fetch all accounts
accounts :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
         => SqlPersistT m [Account]
accounts = fmap (map entityVal) $ select $ from return

initGap :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
        => Entity Account -> SqlPersistT m ()
initGap accE = do
    void $ createAddrs accE AddressExternal 20
    void $ createAddrs accE AddressInternal 20

-- | Create a new account
newAccount :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
           => NewAccount
           -> SqlPersistT m (Entity Account, Maybe Mnemonic)
newAccount NewAccount{..} = do
    unless (validAccountType newAccountType) $
        throwM $ WalletException "Invalid account type"
    let gen = isNothing newAccountMnemonic &&
              isNothing newAccountMaster &&
              null newAccountKeys
    (mnemonicM, masterM, keys) <- if gen
        then do
            when (isJust newAccountMaster || isJust newAccountMnemonic) $
                throwM $ WalletException
                "Master key or mnemonic not allowed for generate"
            ent <- liftIO $ getEntropy 16
            let ms = fromRight $ toMnemonic ent
                root = makeXPrvKey $ fromRight $ mnemonicToSeed "" ms
                master = case newAccountDeriv of
                    Nothing -> root
                    Just d  -> derivePath d root
                keys = deriveXPubKey master : newAccountKeys
            return (Just ms, Just master, keys)
        else case newAccountMnemonic of
             Just ms -> do
                 when (isJust newAccountMaster) $ throwM $ WalletException
                     "Cannot provide both master key and mnemonic"
                 root <- case mnemonicToSeed "" (cs ms) of
                     Right s -> return $ makeXPrvKey s
                     Left _ -> throwM $ WalletException
                         "Mnemonic sentence invalid"
                 let master = case newAccountDeriv of
                         Nothing -> root
                         Just d -> derivePath d root
                     keys = deriveXPubKey master : newAccountKeys
                 return (Nothing, Just master, keys)
             Nothing -> case newAccountMaster of
                 Just master -> do
                     let keys = deriveXPubKey master : newAccountKeys
                     return (Nothing, newAccountMaster, keys)
                 Nothing -> return (Nothing, newAccountMaster, newAccountKeys)

    -- Build the account
    now <- liftIO getCurrentTime
    let acc = Account
            { accountName       = newAccountName
            , accountType       = newAccountType
            , accountMaster     = if newAccountReadOnly
                                  then Nothing
                                  else masterM
            , accountDerivation = newAccountDeriv
            , accountKeys       = nub keys
            , accountGap        = 0
            , accountCreated    = now
            }

    -- Check if all the keys are valid
    unless (isValidAccKeys acc) $
        throwM $ WalletException "Invalid account keys"

    -- Insert our account in the database
    let canSetGap = isCompleteAccount acc
        newAcc    = acc{ accountGap = if canSetGap then 10 else 0 }

    insertUnique newAcc >>= \resM -> case resM of
        -- The account got created.
        Just ai -> do
            let accE = Entity ai newAcc
            -- If we can set the gap, create the gap addresses
            when canSetGap $ initGap accE
            return (accE, mnemonicM)
        -- The account already exists
        Nothing -> throwM $ WalletException "Account already exists"

renameAccount :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
              => Entity Account
              -> AccountName
              -> SqlPersistT m Account
renameAccount (Entity ai acc) name = do
    P.update ai [ AccountName P.=. name ]
    return $ acc{ accountName = name }

-- | Add new thirdparty keys to a multisignature account. This function can
-- fail if the multisignature account already has all required keys.
addAccountKeys :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
               => Entity Account        -- ^ Account Entity
               -> [XPubKey]             -- ^ Thirdparty public keys to add
               -> SqlPersistT m Account -- ^ Account information
addAccountKeys (Entity ai acc) keys
    -- We can only add keys on incomplete accounts
    | isCompleteAccount acc = throwM $
        WalletException "The account is already complete"
    | null keys || not (isValidAccKeys accKeys) = throwM $
        WalletException "Invalid account keys"
    | otherwise = do
        let canSetGap = isCompleteAccount accKeys
            updGap = [ AccountGap P.=. 10 | canSetGap]
            newAcc = accKeys{ accountGap = if canSetGap then 10 else 0 }
        -- Update the account with the keys and the new gap if it is complete
        P.update ai $ (AccountKeys P.=. newKeys) : updGap
        -- If we can set the gap, create the gap addresses
        when canSetGap $ initGap $ Entity ai newAcc
        return newAcc
  where
    newKeys = accountKeys acc ++ keys
    accKeys = acc{ accountKeys = newKeys }

isValidAccKeys :: Account -> Bool
isValidAccKeys Account{..} = testMaster && case accountType of
    AccountRegular -> length accountKeys == 1
    AccountMultisig _ n -> goMultisig n
  where
    goMultisig n =
        length accountKeys == length (nub accountKeys) &&
        length accountKeys <= n && not (null accountKeys)
    testMaster = case accountMaster of
        Just m -> deriveXPubKey m `elem` accountKeys
        Nothing -> True

-- Helper functions to get an Account if it exists, or throw an exception
-- otherwise.
getAccount :: (MonadIO m, MonadThrow m) => AccountName
           -> SqlPersistT m (Entity Account)
getAccount accountName = do
    as <- select $ from $ \a -> do
        where_ $ a ^. AccountName ==. val accountName
        return a
    case as of
        (accEnt:_) -> return accEnt
        _ -> throwM $ WalletException $ unwords
            [ "Account", unpack accountName, "does not exist" ]

{- Addresses -}

-- | Get an address if it exists, or throw an exception otherwise. Fetching
-- addresses in the hidden gap will also throw an exception.
getAddress :: (MonadIO m, MonadThrow m)
           => Entity Account                    -- ^ Account Entity
           -> AddressType                       -- ^ Address type
           -> KeyIndex                          -- ^ Derivation index (key)
           -> SqlPersistT m (Entity WalletAddr) -- ^ Address
getAddress accE@(Entity ai _) addrType index = do
    res <- select $ from $ \x -> do
        where_ (   x ^. WalletAddrAccount ==. val ai
               &&. x ^. WalletAddrType    ==. val addrType
               &&. x ^. WalletAddrIndex   ==. val index
               &&. x ^. WalletAddrIndex   <.  subSelectAddrCount accE addrType
               )
        limit 1
        return x
    case res of
        (addrE:_) -> return addrE
        _ -> throwM $ WalletException $ unwords
            [ "Invalid address index", show index ]

-- | All addresses in the wallet, including hidden gap addresses. This is useful
-- for building a bloom filter.
addressesAll :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
             => SqlPersistT m [WalletAddr]
addressesAll = fmap (map entityVal) $ select $ from return

-- | All addresses in one account excluding hidden gap.
addresses :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
          => Entity Account             -- ^ Account Entity
          -> AddressType                -- ^ Address Type
          -> SqlPersistT m [WalletAddr] -- ^ Addresses
addresses accE@(Entity ai _) addrType = fmap (map entityVal) $
    select $ from $ \x -> do
        where_ (   x ^. WalletAddrAccount ==. val ai
               &&. x ^. WalletAddrType    ==. val addrType
               &&. x ^. WalletAddrIndex   <.  subSelectAddrCount accE addrType
               )
        return x

-- | Get address list.
addressList :: MonadIO m
            => Entity Account -- ^ Account Entity
            -> AddressType    -- ^ Address type
            -> ListRequest    -- ^ List request
            -> SqlPersistT m ([WalletAddr], Word32)
            -- ^ List result
addressList accE@(Entity ai _) addrType ListRequest{..} = do
    cnt <- addressCount accE addrType

    when (listOffset > 0 && listOffset >= cnt) $ throw $ WalletException
        "Offset beyond end of data set"

    res <- fmap (map entityVal) $ select $ from $ \x -> do
        where_ (    x ^. WalletAddrAccount ==. val ai
                &&. x ^. WalletAddrType    ==. val addrType
                &&. x ^. WalletAddrIndex   <.  val cnt
                )
        let order = if listReverse then asc else desc
        orderBy [ order (x ^. WalletAddrIndex) ]
        limit $ fromIntegral listLimit
        offset $ fromIntegral listOffset
        return x

    return (res, cnt)

-- | Get a count of all the addresses in an account
addressCount :: MonadIO m
             => Entity Account        -- ^ Account Entity
             -> AddressType           -- ^ Address type
             -> SqlPersistT m Word32  -- ^ Address Count
addressCount (Entity ai acc) addrType = do
    res <- select $ from $ \x -> do
        where_ (   x ^. WalletAddrAccount ==. val ai
               &&. x ^. WalletAddrType    ==. val addrType
               )
        return countRows
    let cnt = maybe 0 unValue $ listToMaybe res
    return $ if cnt > accountGap acc then cnt - accountGap acc else 0

-- | Get a list of all unused addresses.
unusedAddresses :: MonadIO m
                => Entity Account             -- ^ Account ID
                -> AddressType                -- ^ Address type
                -> SqlPersistT m [WalletAddr] -- ^ Unused addresses
unusedAddresses (Entity ai acc) addrType =
    fmap (reverse . map entityVal) $ select $ from $ \x -> do
        where_ (   x ^. WalletAddrAccount ==. val ai
               &&. x ^. WalletAddrType    ==. val addrType
               )
        orderBy [ desc $ x ^. WalletAddrIndex ]
        limit $ fromIntegral $ accountGap acc
        offset $ fromIntegral $ accountGap acc
        return x

-- | Add a label to an address.
setAddrLabel :: (MonadIO m, MonadThrow m)
             => Entity Account        -- ^ Account ID
             -> KeyIndex              -- ^ Derivation index
             -> AddressType           -- ^ Address type
             -> Text                  -- ^ New label
             -> SqlPersistT m WalletAddr
setAddrLabel accE i addrType label = do
    Entity addrI addr <- getAddress accE addrType i
    P.update addrI [ WalletAddrLabel P.=. label ]
    return $ addr{ walletAddrLabel = label }

-- | Returns the private key of an address.
addressPrvKey :: (MonadIO m, MonadThrow m)
              => Entity Account        -- ^ Account Entity
              -> Maybe XPrvKey         -- ^ If not in account
              -> KeyIndex              -- ^ Derivation index of the address
              -> AddressType           -- ^ Address type
              -> SqlPersistT m PrvKeyC -- ^ Private key
addressPrvKey accE@(Entity ai acc) masterM index addrType = do
    ret <- select $ from $ \x -> do
        where_ (   x ^. WalletAddrAccount ==. val ai
               &&. x ^. WalletAddrType    ==. val addrType
               &&. x ^. WalletAddrIndex   ==. val index
               &&. x ^. WalletAddrIndex   <.  subSelectAddrCount accE addrType
               )
        return $ x ^. WalletAddrIndex
    case ret of
        (Value idx:_) -> do
            accKey <- case accountMaster acc <|> masterM of
                Just key -> return key
                Nothing -> throwM $ WalletException "Could not get private key"
            let addrKey =
                  prvSubKey (prvSubKey accKey (addrTypeIndex addrType)) idx
            return $ xPrvKey addrKey
        _ -> throwM $ WalletException "Invalid address"

-- | Create new addresses in an account and increment the internal bloom filter.
-- This is a low-level function that simply creates the desired amount of new
-- addresses in an account, disregarding visible and hidden address gaps. You
-- should use the function `setAccountGap` if you want to control the gap of an
-- account instead.
createAddrs :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
            => Entity Account
            -> AddressType
            -> Word32
            -> SqlPersistT m [WalletAddr]
createAddrs (Entity ai acc) addrType n
    | n == 0 = throwM $ WalletException $
        unwords [ "Invalid value", show n ]
    | not (isCompleteAccount acc) =
        throwM $ WalletException $ unwords
            [ "Keys are still missing from the incomplete account"
            , unpack $ accountName acc
            ]
    | otherwise = do
        now <- liftIO getCurrentTime
        -- Find the next derivation index from the last address
        lastRes <- select $ from $ \x -> do
            where_ (   x ^. WalletAddrAccount ==. val ai
                   &&. x ^. WalletAddrType    ==. val addrType
                   )
            return $ max_ (x ^. WalletAddrIndex)
        let nextI = case lastRes of
                (Value (Just lastI):_) -> lastI + 1
                _ -> 0
            build (addr, keyM, rdmM, i) = WalletAddr
                { walletAddrAccount    = ai
                , walletAddrAddress    = addr
                , walletAddrIndex      = i
                , walletAddrType       = addrType
                , walletAddrLabel      = ""
                , walletAddrRedeem     = rdmM
                , walletAddrKey        = keyM
                , walletAddrCreated    = now
                }
            res = map build $ take (fromIntegral n) $ deriveFrom nextI

        -- Save the addresses and increment the bloom filter
        splitInsertMany_ res
        incrementFilter res
        return res
  where
    -- Branch type (external = 0, internal = 1)
    branchType = addrTypeIndex addrType
    deriveFrom = case accountType acc of
        AccountMultisig m _ ->
            let f (a, r, i) = (a, Nothing, Just r, i)
                deriv  = Deriv :/ branchType
            in  map f . derivePathMSAddrs (accountKeys acc) deriv m
        AccountRegular -> case accountKeys acc of
            (key:_) -> let f (a, k, i) = (a, Just k, Nothing, i)
                       in  map f . derivePathAddrs key (Deriv :/ branchType)
            [] -> throw $ WalletException $ unwords
                [ "createAddrs: No key in regular account (corrupt database)"
                , unpack $ accountName acc
                ]

-- | Generate all the addresses up to certain index.
generateAddrs :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
              => Entity Account
              -> AddressType
              -> KeyIndex
              -> SqlPersistT m Int
generateAddrs accE addrType genIndex = do
    cnt <- addressCount accE addrType
    let toGen = fromIntegral genIndex - fromIntegral cnt + 1
    if toGen > 0
        then do
            void $ createAddrs accE addrType $ fromIntegral toGen
            return toGen
        else return 0

-- | Use an address and make sure we have enough gap addresses after it.
-- Returns the new addresses that have been created.
useAddress :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
           => WalletAddr -> SqlPersistT m [WalletAddr]
useAddress WalletAddr{..} = do
    res <- select $ from $ \x -> do
        where_ (   x ^. WalletAddrAccount ==. val walletAddrAccount
               &&. x ^. WalletAddrType    ==. val walletAddrType
               &&. x ^. WalletAddrIndex   >.  val walletAddrIndex
               )
        return countRows
    case res of
        (Value cnt:_) -> get walletAddrAccount >>= \accM -> case accM of
            Just acc -> do
                let accE    = Entity walletAddrAccount acc
                    gap     = fromIntegral (accountGap acc) :: Int
                    missing = 2*gap - cnt
                if missing > 0
                    then createAddrs accE walletAddrType $ fromIntegral missing
                    else return []
            _ -> return [] -- Should not happen
        _ -> return [] -- Should not happen

-- | Set the address gap of an account to a new value. This will create new
-- internal and external addresses as required. The gap can only be increased,
-- not decreased in size.
setAccountGap :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
              => Entity Account -- ^ Account Entity
              -> Word32         -- ^ New gap value
              -> SqlPersistT m (Entity Account)
setAccountGap accE@(Entity ai acc) gap
    | not (isCompleteAccount acc) =
        throwM $ WalletException $ unwords
            [ "Keys are still missing from the incomplete account"
            , unpack $ accountName acc
            ]
    | missing <= 0 = throwM $ WalletException
        "The gap of an account can only be increased"
    | otherwise = do
        _ <- createAddrs accE AddressExternal $ fromInteger $ missing*2
        _ <- createAddrs accE AddressInternal $ fromInteger $ missing*2
        P.update ai [ AccountGap P.=. gap ]
        return $ Entity ai acc{ accountGap = gap }
  where
    missing = toInteger gap - toInteger (accountGap acc)

-- Return the creation time of the first address in the wallet.
firstAddrTime :: MonadIO m => SqlPersistT m (Maybe Timestamp)
firstAddrTime = do
    res <- select $ from $ \x -> do
        orderBy [ asc (x ^. WalletAddrId) ]
        limit 1
        return $ x ^. WalletAddrCreated
    return $ case res of
        (Value d:_) -> Just $ toPOSIX d
        _ -> Nothing
  where
    toPOSIX = fromInteger . round . utcTimeToPOSIXSeconds

{- Bloom filters -}

-- | Add the given addresses to the bloom filter. If the number of elements
-- becomes too large, a new bloom filter is computed from scratch.
incrementFilter :: (MonadIO m, MonadThrow m, MonadBase IO m, MonadResource m)
                => [WalletAddr]
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
    cntRes <- select $ from $ \x -> return $ count $ x ^. WalletAddrId
    let elems = maybe 0 unValue $ listToMaybe cntRes
        newBloom = bloomCreate (filterLen elems) fpRate 0 BloomUpdateNone
    addrs <- addressesAll
    let bloom = addToFilter newBloom addrs
    setBloomFilter bloom elems

-- Compute the size of a filter given a number of elements. Scale
-- the filter length by powers of 2.
filterLen :: Int -> Int
filterLen = round . pow2 . ceiling . log2
  where
    pow2 x = (2 :: Double) ** fromInteger x
    log2 x = logBase (2 :: Double) (fromIntegral x)

-- | Add elements to a bloom filter
addToFilter :: BloomFilter -> [WalletAddr] -> BloomFilter
addToFilter bloom addrs =
    bloom3
  where
    pks  = mapMaybe walletAddrKey addrs
    rdms = mapMaybe walletAddrRedeem addrs
    -- Add the Hash160 of the addresses
    f1 b a  = bloomInsert b $ encode' $ getAddrHash a
    bloom1 = foldl f1 bloom $ map walletAddrAddress addrs
    -- Add the redeem scripts
    f2 b r  = bloomInsert b $ encodeOutputBS r
    bloom2 = foldl f2 bloom1 rdms
    -- Add the public keys
    f3 b p  = bloomInsert b $ encode' p
    bloom3 = foldl f3 bloom2 pks

-- | Returns a bloom filter containing all the addresses in this wallet. This
-- includes internal and external addresses. The bloom filter can be set on a
-- peer connection to filter the transactions received by that peer.
getBloomFilter :: (MonadIO m, MonadThrow m)
               => SqlPersistT m (BloomFilter, Int, Double)
getBloomFilter = do
    res <- select $ from $ \c -> do
        limit 1
        return ( c ^. WalletStateBloomFilter
               , c ^. WalletStateBloomElems
               , c ^. WalletStateBloomFp
               )
    case res of
        ((Value b, Value n, Value fp):_) -> return (b, n, fp)
        _ -> throwM $
            WalletException "getBloomFilter: Database not initialized"

-- | Save a bloom filter and the number of elements it contains
setBloomFilter :: MonadIO m => BloomFilter -> Int -> SqlPersistT m ()
setBloomFilter bloom elems =
    P.updateWhere [] [ WalletStateBloomFilter P.=. bloom
                     , WalletStateBloomElems  P.=. elems
                     ]

-- Helper function to compute the redeem script of a given derivation path
-- for a given multisig account.
getPathRedeem :: Account -> SoftPath -> RedeemScript
getPathRedeem acc@Account{..} deriv = case accountType of
    AccountMultisig m _ -> if isCompleteAccount acc
        then sortMulSig $ PayMulSig pubKeys m
        else throw $ WalletException $ unwords
            [ "getPathRedeem: Incomplete multisig account"
            , unpack accountName
            ]
    _ -> throw $ WalletException $ unwords
        [ "getPathRedeem: Account", unpack accountName
        , "is not a multisig account"
        ]
  where
    f       = toPubKeyG . xPubKey . derivePubPath deriv
    pubKeys = map f accountKeys

-- Helper function to compute the public key of a given derivation path for
-- a given non-multisig account.
getPathPubKey :: Account -> SoftPath -> PubKeyC
getPathPubKey acc@Account{..} deriv
    | isMultisigAccount acc = throw $ WalletException $
        unwords [ "getPathPubKey: Account", unpack accountName
                , "is not a regular non-multisig account"
                ]
    | otherwise = case accountKeys of
        (key:_) -> xPubKey $ derivePubPath deriv key
        _ -> throw $ WalletException $ unwords
            [ "getPathPubKey: No keys are available in account"
            , unpack accountName
            ]

{- Helpers -}

subSelectAddrCount :: Entity Account
                   -> AddressType
                   -> SqlExpr (Value KeyIndex)
subSelectAddrCount (Entity ai acc) addrType =
    sub_select $ from $ \x -> do
        where_ (   x ^. WalletAddrAccount ==. val ai
               &&. x ^. WalletAddrType    ==. val addrType
               )
        let gap = val $ accountGap acc
        return $ case_
            [ when_ (countRows >. gap)
              then_ (countRows -. gap)
            ] (else_ $ val 0)

validMultisigParams :: Int -> Int -> Bool
validMultisigParams m n = n >= 1 && n <= 15 && m >= 1 && m <= n

validAccountType :: AccountType -> Bool
validAccountType t = case t of
    AccountRegular      -> True
    AccountMultisig m n -> validMultisigParams m n

isMultisigAccount :: Account -> Bool
isMultisigAccount acc = case accountType acc of
    AccountRegular    -> False
    AccountMultisig{} -> True

isReadAccount :: Account -> Bool
isReadAccount = isNothing . accountMaster

isCompleteAccount :: Account -> Bool
isCompleteAccount acc = case accountType acc of
    AccountRegular      -> length (accountKeys acc) == 1
    AccountMultisig _ n -> length (accountKeys acc) == n

