module Network.Haskoin.Wallet.Settings 
( SPVConfig(..)
, SPVMode(..)
, configSettingsYmlBS
, configSettingsYmlValue
, compileTimeSPVConfig
, ClientConfig(..)
, OutputFormat(..)
, configClientYmlBS
, configClientYmlValue
, compileTimeClientConfig 
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM, mzero)
import Control.Exception (throw)

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
    , (.:)
    )

import Yesod.Default.Config2 (applyEnvValue)

import Network.Haskoin.Wallet.Database

data SPVMode = SPVOnline | SPVOffline
    deriving (Eq, Show, Read)

data SPVConfig = SPVConfig
    { spvBind         :: !String
    -- ^ Bind address for the zeromq socket
    , spvBitcoinNodes :: ![(String, Int)]
    -- ^ Trusted Bitcoin full nodes to connect to
    , spvMode         :: !SPVMode
    -- ^ Operation mode of the SPV node.
    , spvBloomFP      :: !Double
    -- ^ False positive rate for the bloom filter.
    , spvGap          :: !Int
    -- ^ Number of gap addresses per account.
    , spvFinalize     :: !Bool
    -- ^ Only sign a tx if the result is complete (we are the last signer)
    , spvSignNewTxs   :: !Bool
    -- ^ Sign newly created transactions
    , spvFee          :: !Word64
    -- ^ Fee (Satoshi/1000 bytes) to pay
    , spvMinConf      :: !Word32
    -- ^ Minimum number of confirmations when spending coins
    , spvDatabase     :: !DatabaseConfType
    -- ^ Database configuration
    , spvWorkDir      :: !FilePath
    -- ^ App working directory
    , spvLogFile      :: !FilePath
    -- ^ Log file
    , spvPidFile      :: !FilePath
    -- ^ PID File
    , spvConfigFile   :: !FilePath
    -- ^ Location of the runtime configuration file
    } 

instance FromJSON SPVConfig where
    parseJSON = withObject "SPVConfig" $ \o -> do
        spvBind         <- o .: "zeromq-bind"          
        spvMode         <- f =<< o .: "server-mode"
        spvBloomFP      <- o .: "bloom-false-positive" 
        spvGap          <- o .: "address-gap"          
        spvFinalize     <- o .: "sign-finalize-only"
        spvSignNewTxs   <- o .: "sign-new-transactions"
        spvFee          <- o .: "fee-per-1000-bytes"
        spvMinConf      <- o .: "spend-minimum-confirmations"
        spvBitcoinNodes <- g =<< o .: "bitcoin-full-nodes"
        spvDatabase     <- o .: "database" 
        spvWorkDir      <- o .: "work-dir"
        spvLogFile      <- o .: "log-file"
        spvPidFile      <- o .: "pid-file"
        spvConfigFile   <- o .: "config-file"
        return SPVConfig {..}
      where
        f mode = case mode of
            String "online"  -> return SPVOnline
            String "offline" -> return SPVOffline
            _ -> mzero
        g vs = forM vs $ withObject "bitcoinnode" $ \o ->
            (,) <$> (o .: "host") <*> (o .: "port")

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: BS.ByteString
configSettingsYmlBS = $(embedFile settingsFile)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @SPVConfig@ parsed at compile time from
-- @config/settings.yml@.
compileTimeSPVConfig :: SPVConfig
compileTimeSPVConfig =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

data OutputFormat
    = OutputNormal
    | OutputJSON
    | OutputYAML

data ClientConfig = ClientConfig
    { clientWallet   :: !T.Text
    -- ^ Wallet to use in commands
    , clientCount    :: !Int
    -- ^ Number of elements to return
    , clientMinConf  :: !Word32
    -- ^ Minimum number of confirmations for displaying balances
    , clientInternal :: !Bool
    -- ^ Return internal instead of external addresses
    , clientPass     :: !T.Text
    -- ^ Passphrase to use when creating new wallets (bip39 mnemonic)
    , clientFormat   :: !OutputFormat
    -- ^ How to format the command-line results
    , clientSocket   :: !String
    -- ^ ZeroMQ socket to connect to (location of the server)
    , clientDetach   :: !Bool
    -- ^ Should
    , clientConfig   :: !FilePath
    -- ^ Location of the runtime configuration file
    } 

instance FromJSON ClientConfig where
    parseJSON = withObject "ClientConfig" $ \o -> do
        clientWallet   <- o .: "wallet-name"
        clientCount    <- o .: "output-size"
        clientMinConf  <- o .: "balance-minimum-confirmations"
        clientInternal <- o .: "display-internal-addresses"
        clientFormat   <- f =<< o .: "display-format"
        clientPass     <- o .: "mnemonic-passphrase"
        clientSocket   <- o .: "zeromq-socket"          
        clientDetach   <- o .: "detach-server"
        clientConfig   <- o .: "config-file"
        return ClientConfig {..}
      where
        f format = case format of
            String "normal" -> return OutputNormal
            String "json"   -> return OutputJSON
            String "yaml"   -> return OutputYAML
            _ -> mzero

-- | Raw bytes at compile time of @config/client.yml@
configClientYmlBS :: BS.ByteString
configClientYmlBS = $(embedFile "config/client.yml")

-- | @config/client.yml@, parsed to a @Value@.
configClientYmlValue :: Value
configClientYmlValue = either throw id $ decodeEither' configClientYmlBS

-- | A version of @ClientConfig@ parsed at compile time from
-- @config/client.yml@.
compileTimeClientConfig :: ClientConfig
compileTimeClientConfig =
    case fromJSON $ applyEnvValue False mempty configClientYmlValue of
        Error e -> error e
        Success settings -> settings

