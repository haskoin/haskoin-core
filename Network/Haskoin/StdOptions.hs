{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.StdOptions where

import System.Directory
    ( removeFile
    , createDirectoryIfMissing
    , doesFileExist
    , getAppUserDataDirectory
    , setCurrentDirectory
    )
import System.Console.GetOpt 
    ( OptDescr (Option)
    , ArgDescr (NoArg, ReqArg)
    )
import System.Posix.Daemon (runDetached, Redirection (ToFile), killAndWait)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless, when)
import Control.Exception (throwIO)

import Data.Default (Default, def)
import Data.Maybe (fromJust, isJust, fromMaybe)
import qualified Data.Yaml as YAML (encodeFile, decodeFile)
import Data.Aeson 
    ( FromJSON
    , ToJSON
    , parseJSON
    , object
    , toJSON
    , withObject
    , (.=), (.:), (.:?)
    )

import Network.Haskoin.Wallet
import Network.Haskoin.Constants

class (FromJSON a, ToJSON a) => StdServer a where
    getStdOptions :: a -> StdOptions
    setStdOptions :: a -> StdOptions -> a
    serviceName   :: a -> String
    newStdConfig  :: a -> IO a

data StdOptions = StdOptions
    { optDetach   :: Bool
    , optBind     :: String
    , optPort     :: Int
    , optLog      :: FilePath
    , optPid      :: FilePath
    , optDir      :: Maybe FilePath
    , optCfg      :: Maybe FilePath
    } deriving (Eq, Show, Read)

instance Default StdOptions where
    def = StdOptions
        { optDetach = False
        , optBind   = "127.0.0.1"
        , optPort   = 4000
        , optLog    = "server.log"
        , optPid    = "process.pid"
        , optDir    = Nothing
        , optCfg    = Nothing
        } 

instance ToJSON StdOptions where
    toJSON opt = object $
        [ "detach"  .= optDetach opt
        , "bind"    .= optBind opt
        , "port"    .= optPort opt
        , "logfile" .= optLog opt
        , "pidfile" .= optPid opt
        ]
        ++ maybe [] (\x -> [("workdir" .= x)]) (optDir opt)

instance FromJSON StdOptions where
    parseJSON = withObject "optserver" $ \o -> StdOptions
        <$> o .:  "detach"
        <*> o .:  "bind"
        <*> o .:  "port"
        <*> o .:  "logfile"
        <*> o .:  "pidfile"
        <*> o .:? "workdir"
        <*> return Nothing -- configuration file does not contain its own path

srvOptions :: StdServer a => [OptDescr (a -> a)]
srvOptions =
    [ Option ['h'] ["bind"]
        (ReqArg (\s a -> f a (\o -> o{ optBind = s })) "HOST")
        "Bind to this address."
    , Option ['p'] ["port"] 
        (ReqArg (\s a -> f a (\o -> o{ optPort = read s })) "PORT")
        "Listen on this port."
    , Option ['d'] ["detach"]
        (NoArg $ \a -> f a (\o -> o{ optDetach = True }))
        "Detach the process from the terminal"
    , Option ['C'] ["config"]
        (ReqArg (\s a -> f a (\o -> o{ optCfg = Just s })) "FILE")
        "Configuration file"
    , Option ['D'] ["workdir"]
        (ReqArg (\s a -> f a (\o -> o{ optDir = Just s })) "DIR")
        "Working directory"
    , Option ['L'] ["logfile"]
        (ReqArg (\s a -> f a (\o -> o{ optLog = s })) "FILE")
        "Log file"
    , Option ['P'] ["pidfile"]
        (ReqArg (\s a -> f a (\o -> o{ optPid = s })) "FILE")
        "PID file"
    ]
  where
    f :: StdServer a => a -> (StdOptions -> StdOptions) -> a
    f a g = setStdOptions a $ g $ getStdOptions a

processOptions :: (StdServer a, Default a) => [(a -> a)] -> IO a
processOptions fs = do
    let opts0 = foldl (flip ($)) def fs
    conf <- getConfig opts0
    return $ foldl (flip ($)) conf fs

getConfig :: StdServer a => a -> IO a
getConfig stdOpts = do
    workDir <- maybe (getWorkDir stdOpts) return $ optDir opts
    let defFile    = concat [workDir, "/", serviceName stdOpts, ".yaml"]
        configFile = fromMaybe defFile $ optCfg opts
    prevConfig <- doesFileExist configFile
    unless prevConfig $ do
        toWrite <- newStdConfig stdOpts
        YAML.encodeFile configFile toWrite
    configM <- YAML.decodeFile configFile
    unless (isJust configM) $ throwIO $ WalletException $ unwords
        [ "Could not parse config file:"
        , configFile
        ]
    setCurrentDirectory workDir
    return $ fromJust configM
  where
    opts = getStdOptions stdOpts

maybeDetach :: StdServer a => a -> IO () -> IO ()
maybeDetach stdOpts action = do
    prevLog <- doesFileExist $ optLog opts
    -- TODO: Should we move the log file to an archive directory?
    when prevLog $ removeFile $ optLog opts
    if optDetach opts
        then runDetached (Just $ optPid opts) (ToFile $ optLog opts) action
        else action
    putStrLn "Process started"
  where
    opts = getStdOptions stdOpts

stopProcess :: StdServer a => a -> IO ()
stopProcess stdOpts = do
    -- TODO: Should we send a message instead of killing the process ?
    killAndWait $ optPid opts
    putStrLn "Process stopped"
  where
    opts = getStdOptions stdOpts

-- Create and return haskoin working directory
getWorkDir :: StdServer a => a -> IO FilePath
getWorkDir a = do
    stdDir <- getAppUserDataDirectory $ serviceName a
    let dir  = concat [ stdDir, "/", networkName ]
        html = concat [ stdDir, "/", networkName, "/html" ]
    createDirectoryIfMissing True dir
    createDirectoryIfMissing True html
    return dir

