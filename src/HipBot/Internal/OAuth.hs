{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HipBot.Internal.OAuth
  ( obtainAccessToken
  , OAuthError(..)
  , showOAuthError
  ) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Network.HTTP.Client (HttpException)
import Network.HTTP.Types
import qualified Network.Wreq as Wreq

import HipBot.API
import HipBot.Internal.HipBot
import HipBot.Internal.Types

data OAuthError
  = MissingAccessTokenUrl Registration
  | FetchCapabilitiesError Registration HttpException
  | ParseCapabilitiesError Registration Wreq.JSONError
  | FetchAccessTokenError Registration AbsoluteURI HttpException
  | ParseAccessTokenError Registration AbsoluteURI Wreq.JSONError
  | InvalidOAuthCreds Registration

instance Show OAuthError where
  show = showOAuthError

showOAuthError :: OAuthError -> String
showOAuthError = \case
  MissingAccessTokenUrl r -> mconcat
    [ "Cannot get access token. Server capabilities at "
    , r ^. capabilitiesUrl . to show
    , " is missing /capabilities/oauth2Provider/tokenUrl."
    ]
  FetchCapabilitiesError r err -> mconcat
    [ "Cannot get access token. Failure fetching HipChat server capabilities from "
    , r ^. capabilitiesUrl . to show
    , ": "
    , show err
    ]
  ParseCapabilitiesError r err -> mconcat
    [ "Cannot get access token. Failure parsing HipChat server capabilities from "
    , r ^. capabilitiesUrl . to show
    , ": "
    , show err
    ]
  FetchAccessTokenError _ turl err -> mconcat
    [ "Cannot get access token. Failure requesting access token from "
    , show turl
    , ": "
    , show err
    ]
  ParseAccessTokenError _ turl err -> mconcat
    [ "Cannot get access token. Failure parsing access token response from "
    , show turl
    , ": "
    , show err
    ]
  InvalidOAuthCreds _ -> mconcat
    [ "Cannot get access token. Authorization failed, indicating client is not longer valid. It has been removed."
    ]

obtainAccessToken
  :: (Applicative m, MonadCatch m, MonadIO m)
  => HipBot m
  -> Registration
  -> m (Either OAuthError AccessToken)
obtainAccessToken bot reg =
  runEitherT (getTokenUrl bot reg >>= fetchAccessToken bot reg)

getTokenUrl
  :: MonadIO m
  => HipBot m
  -> Registration
  -> EitherT OAuthError m AbsoluteURI
getTokenUrl bot r = getUrl =<< fetchCapabilities bot r where
  getUrl = maybe missing right .
    (^? capabilities . _Just . oauth2Provider . _Just . tokenUrl)
  missing = left . MissingAccessTokenUrl $ r

fetchCapabilities
  :: MonadIO m
  => HipBot m
  -> Registration
  -> EitherT OAuthError m AddOn
fetchCapabilities bot reg = EitherT . liftIO . handleErr . asAddOn . fetch $ reg where
  asAddOn = fmap (^. Wreq.responseBody . to Right) . (Wreq.asJSON =<<)
  fetch = Wreq.getWith (wreqDefaults bot) . view (capabilitiesUrl . to show)
  handleErr =
    handle (return . Left . FetchCapabilitiesError reg) .
    handle (return . Left . ParseCapabilitiesError reg)

fetchAccessToken
  :: (Applicative m, MonadCatch m, MonadIO m)
  => HipBot m
  -> Registration
  -> AbsoluteURI
  -> EitherT OAuthError m AccessToken
fetchAccessToken bot reg turl = EitherT . handleErr $ fetch where
  fetch = do
    res <- liftIO $ do
      res <- Wreq.postWith opts (show turl) body
      Wreq.asJSON res
    if res ^. Wreq.responseStatus == unauthorized401
      then Left (InvalidOAuthCreds reg) <$ apiDeleteRegistration (bot ^. hipBotAPI) (reg ^. oauthId)
      else fmap Right . liftIO . resolveExpiresIn . view Wreq.responseBody $ res
  opts = wreqDefaults bot
    & Wreq.auth ?~ Wreq.basicAuth oid osecret
  oid = reg ^. oauthId . to T.encodeUtf8
  osecret = reg ^. oauthSecret . to T.encodeUtf8
  body = A.toJSON $ A.object
    [ "grant_type" .= A.String "client_credentials"
    , "scope" .= unwords (show <$> capScopes)
    ]
  capScopes = botAddOn bot ^. capabilities . folded . hipchatApiConsumer . folded . scopes
  handleErr =
    handle (return . Left . FetchAccessTokenError reg turl) .
    handle (return . Left . ParseAccessTokenError reg turl)

data HCAccessToken = HCAccessToken
  { _hcAccessToken:: Text
  , _hcExpiresIn :: Int
  }

instance A.FromJSON HCAccessToken where
  parseJSON = A.withObject "access token" $ \o -> HCAccessToken
    <$> o .: "access_token"
    <*> o .: "expires_in"

resolveExpiresIn :: HCAccessToken -> IO AccessToken
resolveExpiresIn t = do
  now <- getCurrentTime
  let
    diff = realToFrac . _hcExpiresIn $ t
    expiring = addUTCTime diff now
  -- TODO get rid of this once confident the timing is right
  print $ mconcat
    [ "OAuthToken: expires_in="
    , show (_hcExpiresIn t)
    , ", now="
    , show now
    , ", expiring="
    , show expiring
    ]
  return $ AccessToken (_hcAccessToken t) expiring


