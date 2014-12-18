{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module Network.Haskoin.Yesod.TokenAuth
( Token(..)
, TokenPair(..)
, YesodTokenAuth(..)
, genToken
, refreshToken
, extractVerifyToken
, buildTokenSig
) where

import System.Entropy (getEntropy)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, unless)
import Control.Monad.Trans.Either (runEitherT, left)

import Data.Aeson (withObject)
import Data.Text (Text, pack, unpack, append)
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Typeable (Typeable)
import Text.Read (readMaybe)
import qualified Data.ByteString as BS (ByteString, append)
import qualified Data.ByteString.Base64 as B64 (encode, decode)

import Yesod

import Network.Haskoin.Util
import Network.Haskoin.Crypto

class Yesod master => YesodTokenAuth master where
    authUrl      :: HandlerT master IO Text
    lookupToken  :: BS.ByteString -> HandlerT master IO (Maybe Token)
    updateToken  :: Token -> HandlerT master IO () 
    expireToken  :: BS.ByteString -> HandlerT master IO ()

data TokenPair = TokenPair !BS.ByteString !BS.ByteString

data Token = Token
    { tokenIdent   :: !BS.ByteString
    , tokenSecret  :: !BS.ByteString
    , tokenNonce   :: !Int
    , tokenExpires :: !(Maybe UTCTime)
    , tokenCreated :: !UTCTime
    } deriving (Show, Read, Eq, Typeable)

newtype CachedToken = CachedToken 
    { unCachedToken :: Either String Token }
    deriving Typeable

instance FromJSON TokenPair where
    parseJSON = withObject "tokenpair" $ \o -> TokenPair
        <$> (stringToBS <$> o .: "token")
        <*> (stringToBS <$> o .: "secret")

instance ToJSON TokenPair where
    toJSON (TokenPair token secret) = object 
        [ "token"  .= bsToString token
        , "secret" .= bsToString secret
        ]

genToken :: MonadIO m => Maybe Integer -> m Token
genToken expiresSec = do
    now    <- liftIO getCurrentTime
    ident  <- liftIO $ getEntropy 8
    secret <- liftIO $ getEntropy 16
    let expires = (\s -> addUTCTime (fromIntegral s) now) <$> expiresSec 
    return $ Token (B64.encode ident) (B64.encode secret) 0 expires now

refreshToken :: YesodTokenAuth master 
             => Token
             -> HandlerT master IO Token
refreshToken token = do
    now <- liftIO getCurrentTime
    let prevExpires = tokenExpires token
        newExpires  = maxTime prevExpires (Just $ addUTCTime 1800 now)
        -- Only change the expiry date if it is after the existing one
        updToken    = token{ tokenExpires = newExpires }
    updateToken updToken
    return updToken
  where
    maxTime _ Nothing = Nothing
    maxTime Nothing _ = Nothing
    maxTime (Just t1) (Just t2) = Just $ if t1 > t2 then t1 else t2

-- | Extract the token from the request and return it if it is valid.
extractVerifyToken 
    :: YesodTokenAuth master
    => BS.ByteString
    -> HandlerT master IO (Either String Token)
extractVerifyToken body = 
    unCachedToken <$> (cached $ CachedToken <$> processToken body)

processToken 
    :: YesodTokenAuth master 
    => BS.ByteString
    -> HandlerT master IO (Either String Token)
processToken body = runEitherT $ do
    -- Get the URL
    routeM <- lift getCurrentRoute
    when (isNothing routeM) $ left "Route not found"
    render <- lift getUrlRenderParams
    qs <- reqGetParams <$> (lift getRequest)
    hostM <- lift $ lookupHeader "host"
    when (isNothing hostM) $ left "Missing HOST header"
    let httpHost = pack $ bsToString $ fromJust hostM
    siteRoot <- lift authUrl
    -- TODO: Is this check really necessary ?
    when (httpHost /= siteRoot) $ left "Invalid HOST header"
    let url = siteRoot `append` render (fromJust routeM) qs

    -- Get the key, signature and nonce from the headers
    accessKeyM <- lift $ lookupHeader "access_key"
    accessSigM <- lift $ lookupHeader "access_signature"
    accessNceM <- lift $ lookupHeader "access_nonce"

    when (isNothing accessKeyM) $ left "Missing ACCESS_KEY header"
    when (isNothing accessSigM) $ left "Missing ACCESS_SIGNATURE header"
    when (isNothing accessNceM) $ left "Missing ACCESS_NONCE header"

    let nonceM = readMaybe $ bsToString $ fromJust accessNceM
    when (isNothing nonceM) $ left "Invalid ACCESS_NONCE header"

    let accessKey = fromJust accessKeyM
        accessSig = fromJust accessSigM
        accessNce = fromJust nonceM

    -- API key must exist in database
    tokenM <- lift $ lookupToken accessKey
    when (isNothing tokenM) $ left "Invalid or expired API Key"

    -- Key must not be expired
    let token   = fromJust tokenM
        expireM = tokenExpires token
    now <- liftIO getCurrentTime
    when (isJust expireM && diffUTCTime (fromJust expireM) now < 0) $ do
        lift $ expireToken accessKey
        left "Invalid or expired API Key"

    -- Nonce must be striclty increasing
    unless (accessNce > tokenNonce token) $ left "Invalid nonce value"

    -- Verify signature
    sig <- liftEither $ buildTokenSig accessNce url body (tokenSecret token)
    unless (accessSig == sig) $ left "Invalid signature"

    -- Update the token nonce
    lift $ refreshToken token{ tokenNonce = accessNce }

buildTokenSig :: Int -> Text -> BS.ByteString -- Nonce, URL and Body
              -> BS.ByteString                -- Base64 token secret
              -> Either String BS.ByteString  -- Base64 encoded Signature
buildTokenSig nonce url body b64Secret = do
    secret <- B64.decode b64Secret
    return $ B64.encode $ hmac256BS secret payload
  where
    payload = nonceBS `BS.append` urlBS `BS.append` body
    nonceBS = stringToBS $ show nonce
    urlBS   = stringToBS $ unpack url

