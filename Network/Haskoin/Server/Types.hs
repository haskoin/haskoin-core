{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Server.Types 
( WalletRequest(..)
, WalletResponse(..)
, encodeWalletRequest
, decodeWalletRequest
, encodeWalletResponse
, decodeWalletResponse
)
where

import Control.Applicative
import Control.Monad 
import Control.Monad.Trans 
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.DeepSeq (NFData, rnf)

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Conduit 
    ( Sink
    , awaitForever
    , yield
    , addCleanup
    , ($$), ($=), (=$)
    )
import Data.Conduit.Network
import Data.Conduit.TMChan
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite

import Network.Haskoin.Crypto
import Network.Haskoin.Stratum
import Network.Haskoin.Util

import Network.Haskoin.Node.PeerManager
import Network.Haskoin.Wallet.Root
import Network.Haskoin.Wallet.Tx
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

{- Request -}

-- TODO: Create NFData intstances for WalletRequest and WalletResponse
data WalletRequest 
    = CreateFullWallet String String (Maybe String)
    | CreateReadWallet String XPubKey
    | WalletList 
    deriving (Eq, Show, Read)

encodeWalletRequest :: WalletRequest -> (Maybe Id) -> BS.ByteString
encodeWalletRequest wr i = 
    toStrictBS $ encode $ Request (walletMethod wr) (go wr) i
  where
    go (CreateFullWallet n p m) = Just $ object $ concat
        [ [ "name"       .= n
          , "passphrase" .= p
          ]
        , if isJust m then ["mnemonic" .= (fromJust m)] else []
        ]
    go (CreateReadWallet n k) = Just $ object
        [ "name" .= n
        , "key"  .= xPubExport k
        ]
    go WalletList = Nothing

walletMethod :: WalletRequest -> T.Text
walletMethod wr = case wr of
    CreateFullWallet _ _ _ -> "network.haskoin.wallet.newwalletmnemo"
    CreateReadWallet _ _   -> error "not implemented"
    WalletList             -> "network.haskoin.wallet.walletlist"

decodeWalletRequest :: BS.ByteString -> Either String (WalletRequest, Maybe Id)
decodeWalletRequest bs = 
    eitherDecode (toLazyBS bs) >>= \(Request m p i) -> do
        r <- parseEither (go m) p
        return (r,i)
  where
    go m p = case (m,p) of
        ("network.haskoin.wallet.newwalletmnemo", Just (Object o)) -> do
            n <- o .:  "name"
            p <- o .:  "passphrase"
            m <- o .:? "mnemonic"
            return $ CreateFullWallet n p m
        ("network.haskoin.wallet.walletlist", Nothing) -> return WalletList
        _ -> mzero

{- Response -}

data WalletResponse
    = ResCreateWallet String
    | ResWalletList [Wallet]
    deriving (Eq, Show, Read)

encodeWalletResponse :: (Either String WalletResponse, (Maybe Id)) 
                     -> BS.ByteString
encodeWalletResponse (wr, i) = toStrictBS $ encode $ 
    ( Response { resResult = go <$> (f wr)
               , resId     = i
               } :: Response Value String String
    )
  where
    f (Left err) = Left $ ErrVal err
    f (Right x)  = return x
    go (ResCreateWallet s) = object ["mnemonic" .= s]
    go (ResWalletList ws)  = object ["walletlist" .= ws]

decodeWalletResponse :: BS.ByteString -> WalletRequest
                     -> Either String WalletResponse
decodeWalletResponse bs req = do
    response <- eitherDecode $ toLazyBS bs 
        :: Either String (Response Value Value String)
    let res = resResult response
    when (isLeft res) $ Left $ case fromLeft res of
        ErrObj _ err _ -> err
        ErrVal err     -> err
    parseEither (go req) $ fromRight res
  where
    go (CreateFullWallet _ _ _) (Object o) = do
        m <- o .: "mnemonic" 
        return $ ResCreateWallet m
    go (CreateReadWallet _ _) _ = error "Not implemented"
    go WalletList (Object o) = do
        ws <- o .: "walletlist"
        return $ ResWalletList ws
    go _ _ = mzero
        

