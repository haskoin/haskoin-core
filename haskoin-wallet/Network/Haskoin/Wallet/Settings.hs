module Network.Haskoin.Wallet.Settings
( SPVMode(..)
, OutputFormat(..)
, Config(..)
) where

import Control.Monad (mzero)
import Control.Exception (throw)
import Control.Monad.Logger (LogLevel(..))

import Data.Default (Default, def)
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Data.Word (Word32, Word64)
import Data.HashMap.Strict (HashMap, unionWith)
import Data.String.Conversions (cs)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
    ( Value(..), FromJSON, ToJSON
    , parseJSON, toJSON, withObject
    , (.:)
    )

import Network.Haskoin.Crypto
import Network.Haskoin.Wallet.Database
import Network.Haskoin.Wallet.Types

import Data.Restricted (Restricted, Div5)
import System.ZMQ4 (toRestricted)

data SPVMode = SPVOnline | SPVOffline
    deriving (Eq, Show, Read)

newtype LogLevelJSON = LogLevelJSON LogLevel
    deriving (Eq, Show, Read)

newtype DBConfigJSON = DBConfigJSON { dbConfigJSON :: DatabaseConfType }
    deriving Show

data OutputFormat
    = OutputNormal
    | OutputJSON
    | OutputYAML

data Config = Config
    { configCount         :: !Word32
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
    , configPath          :: !(Maybe HardPath)
    -- ^ Derivation path when creating account
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
    , configBTCNodes      :: !(HashMap Text [BTCNode])
    -- ^ Trusted Bitcoin full nodes to connect to
    , configMode          :: !SPVMode
    -- ^ Operation mode of the SPV node.
    , configBloomFP       :: !Double
    -- ^ False positive rate for the bloom filter.
    , configDatabase      :: !(HashMap Text DatabaseConfType)
    -- ^ Database configuration
    , configLogFile       :: !FilePath
    -- ^ Log file
    , configPidFile       :: !FilePath
    -- ^ PID File
    , configLogLevel      :: !LogLevel
    -- ^ Log level
    , configVerbose       :: !Bool
    -- ^ Verbose
    , configServerKey     :: !(Maybe (Restricted Div5 ByteString))
    -- ^ Server key for authentication and encryption (server config)
    , configServerKeyPub  :: !(Maybe (Restricted Div5 ByteString))
    -- ^ Server public key for authentication and encryption (client config)
    , configClientKey     :: !(Maybe (Restricted Div5 ByteString))
    -- ^ Client key for authentication and encryption (client config)
    , configClientKeyPub  :: !(Maybe (Restricted Div5 ByteString))
    -- ^ Client public key for authentication and encryption
    -- (client + server config)
    }

configBS :: ByteString
configBS = $(embedFile "config/config.yml")

instance ToJSON OutputFormat where
    toJSON OutputNormal = String "normal"
    toJSON OutputJSON   = String "json"
    toJSON OutputYAML   = String "yaml"

instance FromJSON OutputFormat where
    parseJSON (String "normal") = return OutputNormal
    parseJSON (String "json")   = return OutputJSON
    parseJSON (String "yaml")   = return OutputYAML
    parseJSON _ = mzero

instance ToJSON SPVMode where
    toJSON SPVOnline  = String "online"
    toJSON SPVOffline = String "offline"

instance FromJSON SPVMode where
    parseJSON (String "online")  = return SPVOnline
    parseJSON (String "offline") = return SPVOffline
    parseJSON _ = mzero

instance ToJSON LogLevelJSON where
    toJSON (LogLevelJSON LevelDebug)     = String "debug"
    toJSON (LogLevelJSON LevelInfo)      = String "info"
    toJSON (LogLevelJSON LevelWarn)      = String "warn"
    toJSON (LogLevelJSON LevelError)     = String "error"
    toJSON (LogLevelJSON (LevelOther t)) = String t

instance FromJSON LogLevelJSON where
    parseJSON (String "debug") = return $ LogLevelJSON LevelDebug
    parseJSON (String "info")  = return $ LogLevelJSON LevelInfo
    parseJSON (String "warn")  = return $ LogLevelJSON LevelWarn
    parseJSON (String "error") = return $ LogLevelJSON LevelError
    parseJSON (String x)       = return $ LogLevelJSON (LevelOther x)
    parseJSON _ = mzero

instance FromJSON DBConfigJSON where
    parseJSON = withObject "database" $ \o -> DBConfigJSON <$> o .: databaseEngine

instance Default Config where
    def = either throw id $ decodeEither' "{}"

instance FromJSON Config where
    parseJSON = withObject "config" $ \o' -> do
        let defValue         = either throw id $ decodeEither' configBS
            (Object o)       = mergeValues defValue (Object o')
            configPath       = Nothing
        configFile                  <- o .: "config-file"
        configRcptFee               <- o .: "recipient-fee"
        configCount                 <- o .: "output-size"
        configMinConf               <- o .: "minimum-confirmations"
        configSignTx                <- o .: "sign-transactions"
        configFee                   <- o .: "transaction-fee"
        configAddrType              <- o .: "address-type"
        configOffline               <- o .: "offline"
        configReversePaging         <- o .: "reverse-paging"
        configFormat                <- o .: "display-format"
        configConnect               <- o .: "connect-uri"
        configDetach                <- o .: "detach-server"
        configTestnet               <- o .: "use-testnet"
        configDir                   <- o .: "work-dir"
        configBind                  <- o .: "bind-socket"
        configBTCNodes              <- o .: "bitcoin-full-nodes"
        configMode                  <- o .: "server-mode"
        configBloomFP               <- o .: "bloom-false-positive"
        configLogFile               <- o .: "log-file"
        configPidFile               <- o .: "pid-file"
        LogLevelJSON configLogLevel <- o .: "log-level"
        configVerbose               <- o .: "verbose"
        configDatabase              <- fmap dbConfigJSON <$> o .: "database"
        configServerKey             <- getKey o "server-key"
        configServerKeyPub          <- getKey o "server-key-public"
        configClientKey             <- getKey o "client-key"
        configClientKeyPub          <- getKey o "client-key-public"
        return Config {..}
      where
        getKey o i = o .: i >>= \kM ->
            case kM of
              Nothing -> return Nothing
              Just k ->
                  case toRestricted $ encodeUtf8 k of
                    Just k' -> return $ Just k'
                    Nothing -> fail $ "Invalid " ++ cs k


mergeValues :: Value -> Value -> Value
mergeValues (Object d) (Object c) = Object (unionWith mergeValues d c)
mergeValues _ c = c
