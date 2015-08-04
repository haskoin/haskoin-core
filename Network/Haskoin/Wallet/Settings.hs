module Network.Haskoin.Wallet.Settings 
( SPVMode(..)
, OutputFormat(..)
, Config(..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM, mzero)
import Control.Exception (throw)
import Control.Monad.Logger (LogLevel(..))

import Data.Default (Default, def)
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Data.Word (Word32, Word64)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Traversable as V (mapM)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Text as T (Text)
import Data.Aeson 
    ( Value(..)
    , FromJSON
    , Result(..)
    , parseJSON
    , fromJSON
    , withArray
    , withObject
    , (.:), (.:?), (.!=)
    )

import Network.Haskoin.Constants
import Network.Haskoin.Wallet.Database
import Network.Haskoin.Wallet.Types

data SPVMode = SPVOnline | SPVOffline
    deriving (Eq, Show, Read)

data OutputFormat
    = OutputNormal
    | OutputJSON
    | OutputYAML

data Config = Config
    { configKeyRing       :: !T.Text
    -- ^ Keyring to use in commands
    , configCount         :: !Word32
    -- ^ Output size of commands
    , configMinConf       :: !Word32
    -- ^ Minimum number of confirmations 
    , configSignTx        :: !Bool
    -- ^ Sign transactions
    , configFee           :: !Word64
    -- ^ Fee to pay per 1000 bytes when creating new transactions
    , configRcptFee       :: !Bool
    -- ^ Recipient pays fee (dangerous, no config file setting)
    , configAddrType      :: !AddressType
    -- ^ Return internal instead of external addresses
    , configOffline       :: !Bool
    -- ^ Display the balance including offline transactions
    , configReversePaging :: !Bool
    -- ^ Use reverse paging for displaying addresses and transactions
    , configPass          :: !(Maybe T.Text)
    -- ^ Passphrase to use when creating new keyrings (bip39 mnemonic)
    , configFormat        :: !OutputFormat
    -- ^ How to format the command-line results
    , configConnect       :: !String
    -- ^ ZeroMQ socket to connect to (location of the server)
    , configDetach        :: !Bool
    -- ^ Detach server when launched from command-line
    , configFile          :: !FilePath
    -- ^ Configuration file
    , configTestnet       :: !Bool
    -- ^ Use Testnet3 network
    , configDir           :: !FilePath
    -- ^ Working directory
    , configBind          :: !String
    -- ^ Bind address for the zeromq socket
    , configBTCNodes      :: !(HashMap T.Text [(String, Int)])
    -- ^ Trusted Bitcoin full nodes to connect to
    , configMode          :: !SPVMode
    -- ^ Operation mode of the SPV node.
    , configBloomFP       :: !Double
    -- ^ False positive rate for the bloom filter.
    , configDatabase      :: !(HashMap T.Text DatabaseConfType)
    -- ^ Database configuration
    , configLogFile       :: !FilePath
    -- ^ Log file
    , configPidFile       :: !FilePath
    -- ^ PID File
    , configLogLevel      :: !LogLevel
    -- ^ Log level
    , configVerbose       :: !Bool
    -- ^ Verbose
    } 

configBS :: BS.ByteString
configBS = $(embedFile "config/config.yml")

instance Default Config where
    def = either throw id $ decodeEither' configBS

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        let configRcptFee    = False
            configFile       = "config.yml"
            configPass       = Nothing
        configKeyRing        <- o .:? "keyring-name"
                                  .!= configKeyRing def
        configCount          <- o .:? "output-size"
                                  .!= configCount def
        configMinConf        <- o .:? "minimum-confirmations"
                                  .!= configMinConf def
        configSignTx         <- o .:? "sign-transactions"
                                  .!= configSignTx def
        configFee            <- o .:? "transaction-fee"
                                  .!= configFee def
        configAddrType <- k =<< o .:? "address-type"
        configOffline        <- o .:? "offline"
                                  .!= configOffline def
        configReversePaging  <- o .:? "reverse-paging"
                                  .!= configReversePaging def
        configFormat   <- f =<< o .:? "display-format"
        configConnect        <- o .:? "connect-uri"          
                                  .!= configConnect def
        configDetach         <- o .:? "detach-server"
                                  .!= configDetach def
        configTestnet        <- o .:? "use-testnet"
                                  .!= configTestnet def
        configDir            <- o .:? "work-dir"
                                  .!= configDir def
        configBind           <- o .:? "bind-socket"          
                                  .!= configBind def
        configBTCNodes <- g =<< o .:? "bitcoin-full-nodes"
        configMode     <- h =<< o .:? "server-mode"
        configBloomFP        <- o .:? "bloom-false-positive" 
                                  .!= configBloomFP def
        configDatabase <- i =<< o .:? "database" 
        configLogFile        <- o .:? "log-file"
                                  .!= configLogFile def
        configPidFile        <- o .:? "pid-file"
                                  .!= configPidFile def
        configLogLevel <- j =<< o .:? "log-level"
        configVerbose        <- o .:? "verbose"
                                  .!= configVerbose def
        return Config {..}
      where
        f format = case format of
            Just (String "normal") -> return OutputNormal
            Just (String "json")   -> return OutputJSON
            Just (String "yaml")   -> return OutputYAML
            Just _                 -> mzero
            Nothing                -> return $ configFormat def
        g (Just x) = flip (withObject "btcnodesobj") x $ V.mapM $ \a -> do
            ls <- parseJSON a
            forM ls $ withObject "bitcoinnode" $ \o ->
                (,) <$> (o .: "host") <*> (o .: "port")
        g Nothing = return $ configBTCNodes def
        h mode = case mode of
            Just (String "online")  -> return SPVOnline
            Just (String "offline") -> return SPVOffline
            Just _                  -> mzero
            Nothing                 -> return $ configMode def
        i (Just x) = flip (withObject "databases") x $ V.mapM .
            withObject "database" $ \v -> v .: databaseEngine
        i Nothing = return $ configDatabase def
        j level = case level of
            Just (String "debug") -> return LevelDebug
            Just (String "info")  -> return LevelInfo
            Just (String "warn")  -> return LevelWarn
            Just (String "error") -> return LevelError
            Just _                -> mzero
            Nothing               -> return $ configLogLevel def
        k addrtype = case addrtype of
            Just (String "internal") -> return AddressInternal
            Just (String "external") -> return AddressExternal
            Just _                   -> mzero
            Nothing                  -> return $ configAddrType def

