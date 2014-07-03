{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.Server where

import System.Directory 
    ( getAppUserDataDirectory
    , createDirectoryIfMissing
    )

import Control.Monad 
import Control.Monad.Trans 
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.DeepSeq (NFData, rnf)

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Conduit
import Data.Conduit.Network
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite

import Network.Haskoin.Util
import Network.Haskoin.Stratum
import Network.Haskoin.Wallet.Root

data WalletRequest 
    = NewWallet String String (Maybe String)
    deriving (Eq, Show, Read)

instance NFData WalletRequest where
    rnf (NewWallet n p m) = rnf n `seq` rnf p `seq` rnf m

decodeWalletRequest :: Request Value -> Maybe (WalletRequest, Maybe Id)
decodeWalletRequest (Request m p i) = parseMaybe (go m) p
  where
    go "network.haskoin.wallet.newwalletmnemo" (Just (Object o)) = do
        n <- o .:  "name"
        p <- o .:  "passphrase"
        m <- o .:? "mnemonic"
        return (NewWallet n p m, i)
    go "network.haskoin.wallet.newwalletmnemo" _ = mzero

data WalletResponse
    = Mnemonic String
    deriving (Eq, Show, Read)

instance NFData WalletResponse where
    rnf (Mnemonic s) = rnf s

instance ToJSON WalletResponse where
    toJSON (Mnemonic s) = object [ "mnemonic" .=  T.pack s ]

encodeWalletResponse :: (WalletResponse, (Maybe Id))
                     -> Response WalletResponse String String
encodeWalletResponse (wr, i) = 
    Response { resResult = Right wr
             , resId     = i
             }

processWalletRequest :: (PersistUnique m, PersistQuery m)
                     => (WalletRequest, Maybe Id) 
                     -> m (WalletResponse, Maybe Id)
processWalletRequest (wr, i) = 
    liftM (flip (,) i) $ go wr
  where
    go (NewWallet n p m) = liftM Mnemonic $ newWalletMnemo n p m

-- TODO: Handle parse errors and exceptions
runServer :: IO ()
runServer = do
    dir <- getWorkDir
    let walletFile = T.pack $ concat [dir, "/wallet"]
    pool <- createSqlitePool walletFile 10
    runTCPServer (serverSettings 4000 "127.0.0.1") $ \client -> runDB pool $ 
           appSource client
        $= CL.mapMaybe ((decodeWalletRequest =<<) . decodeStrict)
        $$ CL.mapM processWalletRequest
        =$ CL.map (toStrictBS . encode . encodeWalletResponse)
        =$ appSink client

runDB :: ConnectionPool -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB pool m = runNoLoggingT $ runResourceT $ runSqlPool m pool

-- Create and return haskoin working directory
getWorkDir :: IO FilePath
getWorkDir = do
    dir <- getAppUserDataDirectory "haskoin"
    createDirectoryIfMissing True dir
    return dir

    
