module Main where

import           Network.Haskoin.Wallet           (Config(..),
                                                   WalletRequest(..), WalletResponse(..),
                                                   AddressType(..),   OutputFormat(..),
                                                   SPVMode(..),       NodeAction(..))

import           Network.Haskoin.Wallet.Server    (runSPVServerWithContext)
import           Network.Haskoin.Wallet.Internals (BTCNode(..), Notif(..))
import qualified Network.Haskoin.Node.STM       as Node

import           Data.String.Conversions          (cs)
import qualified System.ZMQ4                    as ZMQ
import qualified Control.Monad.Logger           as Log
import qualified Data.HashMap.Strict            as HM
import qualified Database.Persist.Sqlite        as DB
import qualified Control.Monad.Trans.Resource   as Resource
import qualified Data.Aeson                     as JSON
import qualified Control.Concurrent             as Con
import qualified Data.Aeson.Encode.Pretty       as PrettyJSON
import qualified Control.Monad                  as M
import qualified Control.Exception              as Except


databaseConf :: DB.SqliteConf
databaseConf = DB.SqliteConf "/tmp/tmpdb" 1

cmdSocket :: String
cmdSocket = "inproc://cmd"

notifSocket :: String
notifSocket = "inproc://notif"


-- |Simple example app that embeds a haskoin-wallet server.
--  Start wallet server + notification thread, and execute Status command when pressing ENTER
main :: IO ()
main = ZMQ.withContext $ \ctx -> do
    -- Server
    putStrLn "Starting server..."
    _ <- Con.forkIO $ runWallet walletServerConf ctx
    -- Notify thread
    putStrLn "Starting notification thread..."
    _ <- Con.forkIO $ notifyThread ctx notifyHandler
    -- Status loop
    M.forever $ do
        putStrLn "Press ENTER to get server status..."
        _ <- getLine
        cmdGetStatus ctx >>= printStatusJSON
  where
    printStatusJSON     = putStrLn . cs . PrettyJSON.encodePretty
    notifyHandler notif =
        putStrLn $ "NOTIFY: New block: " ++ cs (PrettyJSON.encodePretty notif)

-- |Run haskoin-wallet using the specified ZeroMQ Context,
--  and log to stderr.
runWallet :: Config -> ZMQ.Context -> IO ()
runWallet cfg ctx = run $ runSPVServerWithContext cfg ctx
    where run           = Resource.runResourceT . runLogging
          runLogging    = Log.runStderrLoggingT . Log.filterLogger logFilter
          logFilter _ l = l >= configLogLevel cfg

cmdGetStatus :: ZMQ.Context -> IO Node.NodeStatus
cmdGetStatus ctx =
    sendCmdOrFail (NodeActionReq NodeActionStatus) ctx >>=
    \res -> case res of
        Nothing     -> error "ERROR: Status command: no response."
        Just status -> return status

sendCmdOrFail :: (JSON.FromJSON a, JSON.ToJSON a)
              => WalletRequest
              -> ZMQ.Context
              -> IO (Maybe a)
sendCmdOrFail cmd ctx =
    sendCmd cmd ctx >>=
    either error return >>=
    \res -> case res of
        ResponseError e -> error $ "ERROR: Send cmd, ResponseError: " ++ cs e
        ResponseValid r -> return r

sendCmd :: (JSON.FromJSON a, JSON.ToJSON a)
        => WalletRequest
        -> ZMQ.Context
        -> IO (Either String (WalletResponse a))
sendCmd req ctx =
    ZMQ.withSocket ctx ZMQ.Req $ \sock -> do
        ZMQ.setLinger (ZMQ.restrict (0 :: Int)) sock
        ZMQ.connect sock cmdSocket
        ZMQ.send sock [] (cs $ JSON.encode req)
        JSON.eitherDecode . cs <$> ZMQ.receive sock

-- |Connect to notify socket, subscribe to new blocks,
--  and execute the supplied handler for each new block as it arrives.
notifyThread :: ZMQ.Context -> (Notif -> IO ()) -> IO ()
notifyThread ctx handler = waitAndCatch $
    ZMQ.withSocket ctx ZMQ.Sub $ \sock -> do
        ZMQ.setLinger (ZMQ.restrict (0 :: Int)) sock
        ZMQ.connect sock notifSocket
        ZMQ.subscribe sock "[block]"
        putStrLn "NOTIFY: Connected. Subscribed to new blocks."
        M.forever $ do
            [_,m] <- ZMQ.receiveMulti sock
            notif <- either failOnErr return $ JSON.eitherDecode (cs m)
            handler notif
  where
    failOnErr = fail . ("NOTIFY: ERROR: recv failed: " ++)
    waitAndCatch ioa = Con.threadDelay 10000 >> ioa `Except.finally` waitAndCatch ioa

btcNodes :: [BTCNode]
btcNodes =
    [ BTCNode "dnsseed.bluematt.me"             8333
    , BTCNode "dnsseed.bitcoin.dashjr.org"      8333
    , BTCNode "dnsseed.bluematt.me"             8333
    , BTCNode "seed.bitcoinstats.com"           8333
    , BTCNode "seed.bitcoin.jonasschnelli.ch"   8333
    , BTCNode "seed.bitcoin.sipa.be"            8333
    , BTCNode "seed.bitnodes.io"                8333
    , BTCNode "seed.btcc.com"                   8333
    ]

walletServerConf :: Config
walletServerConf = Config
    { configCount          = 100
    -- ^ Output size of commands
    , configMinConf        = 6
    -- ^ Minimum number of confirmations
    , configSignTx         = True
    -- ^ Sign transactions
    , configFee            = 50000
    -- ^ Fee to pay per 1000 bytes when creating new transactions
    , configRcptFee        = False
    -- ^ Recipient pays fee (dangerous, no config file setting)
    , configAddrType       = AddressExternal
    -- ^ Return internal instead of external addresses
    , configDisplayPubKeys = False
    -- ^ Display public keys instead of addresses
    , configOffline        = False
    -- ^ Display the balance including offline transactions
    , configEntropy        = 16
    -- ^ Entropy in bytes to use when generating a mnemonic (between 16 and 32)
    , configReversePaging  = False
    -- ^ Use reverse paging for displaying addresses and transactions
    , configDerivIndex     = 0
    -- ^ Derivation path when creating account
    , configFormat         = OutputNormal
    -- ^ How to format the command-line results
    , configConnect        = cmdSocket
    -- ^ ZeroMQ socket to connect to (location of the server)
    , configConnectNotif   = notifSocket
    -- ^ ZeroMQ socket to connect for notifications
    , configDetach         = False
    -- ^ Detach server when launched from command-line
    , configFile           = ""
    -- ^ Configuration file
    , configTestnet        = False
    -- ^ Use Testnet3 network
    , configDir            = ""
    -- ^ Working directory
    , configBind           = cmdSocket
    -- ^ Bind address for the ZeroMQ socket
    , configBindNotif      = notifSocket
    -- ^ Bind address for ZeroMQ notifications
    , configBTCNodes       = HM.fromList [ ( "prodnet", btcNodes ) ]
    -- ^ Trusted Bitcoin full nodes to connect to
    , configMode           = SPVOnline
    -- ^ Operation mode of the SPV node.
    , configBloomFP        = 0.00001
    -- ^ False positive rate for the bloom filter.
    , configDatabase       = HM.fromList [ ( "prodnet", databaseConf ) ]
    -- ^ Database configuration
    , configLogFile        = ""
    -- ^ Log file
    , configPidFile        = ""
    -- ^ PID File
    , configLogLevel       = Log.LevelInfo
    -- ^ Log level
    , configVerbose        = True
    -- ^ Verbose
    , configServerKey      = Nothing
    -- ^ Server key for authentication and encryption (server config)
    , configServerKeyPub   = Nothing
    -- ^ Server public key for authentication and encryption (client config)
    , configClientKey      = Nothing
    -- ^ Client key for authentication and encryption (client config)
    , configClientKeyPub   = Nothing
    -- ^ Client public key for authentication and encryption
    }
