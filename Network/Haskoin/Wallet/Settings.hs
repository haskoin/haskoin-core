module Network.Haskoin.Wallet.Settings 
( SPVMode(..)
, OutputFormat(..)
, Config(..)
, configBS
, configValue
, hardConfig 
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM, mzero)
import Control.Exception (throw)
import Control.Monad.Logger (LogLevel(..))

import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Data.Monoid (mempty)
import Data.Word (Word32, Word64)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Text as T (Text)
import Data.Aeson 
    ( Value(..)
    , FromJSON
    , Result(..)
    , parseJSON
    , fromJSON
    , withObject
    , (.:), (.:?), (.!=)
    )

import Yesod.Default.Config2 (applyEnvValue)

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
    , configBTCNodes      :: ![(String, Int)]
    -- ^ Trusted Bitcoin full nodes to connect to
    , configMode          :: !SPVMode
    -- ^ Operation mode of the SPV node.
    , configBloomFP       :: !Double
    -- ^ False positive rate for the bloom filter.
    , configDatabase      :: !DatabaseConfType
    -- ^ Database configuration
    , configLogFile       :: !FilePath
    -- ^ Log file
    , configPidFile       :: !FilePath
    -- ^ PID File
    , configLogLevel      :: !LogLevel
    } 

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        let configRcptFee    = False
        configKeyRing        <- o .: "keyring-name"
        configCount          <- o .: "output-size"
        configMinConf        <- o .: "minimum-confirmations"
        configSignTx         <- o .: "sign-transactions"
        configFee            <- o .: "transaction-fee"
        configAddrType       <- k =<< o .: "address-type"
        configReversePaging  <- o .: "reverse-paging"
        configPass           <- o .:? "mnemonic-passphrase"
        configFormat         <- f =<< o .: "display-format"
        configConnect        <- o .: "connect-uri"          
        configDetach         <- o .: "detach-server"
        configFile           <- o .: "config-file"
        configTestnet        <- o .: "use-testnet"
        configDir            <- o .: "work-dir"
        configBind           <- o .: "bind-socket"          
        configBTCNodes       <- g =<< o .: "bitcoin-full-nodes"
        configMode           <- h =<< o .: "server-mode"
        configBloomFP        <- o .: "bloom-false-positive" 
        configDatabase       <- i =<< o .: "database" 
        configLogFile        <- o .: "log-file"
        configPidFile        <- o .: "pid-file"
        configLogLevel       <- j =<< o .: "log-level"
        return Config {..}
      where
        f format = case format of
            String "normal" -> return OutputNormal
            String "json"   -> return OutputJSON
            String "yaml"   -> return OutputYAML
            _ -> mzero
        g vs = forM vs $ withObject "bitcoinnode" $ \o ->
            (,) <$> (o .: "host") <*> (o .:? "port" .!= defaultPort)
        h mode = case mode of
            String "online"  -> return SPVOnline
            String "offline" -> return SPVOffline
            _ -> mzero
        i = withObject "database" $ \v -> v .: databaseEngine
        j level = case level of
            String "debug" -> return LevelDebug
            String "info"  -> return LevelInfo
            String "warn"  -> return LevelWarn
            String "error" -> return LevelError
            _              -> mzero
        k addrtype = case addrtype of
            String "internal" -> return AddressInternal
            String "external" -> return AddressExternal
            _                 -> mzero

-- | Raw bytes at compile time of @config/config.yml@
configBS :: BS.ByteString
configBS = $(embedFile "config/config.yml")

-- | @config/config.yml@, parsed to a @Value@.
configValue :: Value
configValue = either throw id $ decodeEither' configBS

-- | A version of @Config@ parsed at compile time from
-- @config/config.yml@.
hardConfig :: Config
hardConfig = case fromJSON $ applyEnvValue False mempty configValue of
    Error e -> error e
    Success settings -> settings

