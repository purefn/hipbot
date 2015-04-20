{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module HipBot
  ( HipBot
  , HipBotAPI(..)
  , newHipBot
  , hipBotResources
  , configResource
  , verifySignature
  , sendNotification
  , module HipBot.Internal.Types
  ) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.Bifunctor
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Network.HTTP.Types
import qualified Network.Wreq as Wreq

import HipBot.API
import HipBot.Internal.HipBot
import HipBot.Internal.OAuth
import HipBot.Internal.Resources
import HipBot.Internal.Types

data NotificationError
  = NoSuchRegistration OAuthId
  | TokenError OAuthError

sendNotification
  :: (Applicative m, MonadCatch m, MonadIO m)
  => HipBot m
  -> OAuthId
  -> Either RoomName RoomId
  -> Notification
  -> m (Maybe NotificationError)
sendNotification bot oid room n = getToken bot oid >>= either (return . Just) send where
  -- TODO handle failures posting, right now an exception is thrown for non-2xx responses
  -- we should turn those into more specific errors, e.g. RateLimitExceeded
  send (reg, tok) = Nothing <$ liftIO (Wreq.postWith (opts tok) (notificationUrl room reg) msg)
  opts tok = wreqDefaults bot
    & Wreq.header hAuthorization .~ [("Bearer " <>) .  T.encodeUtf8 . view accessToken $ tok]
    & Wreq.header hContentType .~ ["application/json"]
  msg = case n of
    TextNotification t -> A.object
      [ "message_format" .= ("text" :: Text)
      , "message" .= t
      ]
    HtmlNotification t -> A.object
      [ "message_format" .= ("html" :: Text)
      , "message" .= t
      ]

getToken
  :: (Applicative m, MonadCatch m, MonadIO m)
  => HipBot m
  -> OAuthId
  -> m (Either NotificationError (Registration, AccessToken))
getToken bot oid = runEitherT (lookupReg >>= fetch) where
  lookupReg = EitherT .
    fmap (maybe (Left . NoSuchRegistration $ oid) Right) .
    apiLookupRegistration (view hipBotAPI bot) $
    oid
  fetch (reg, tok) = do
    now <- liftIO getCurrentTime
    tok' <- if now > addUTCTime 300 (tok ^. expires)
      then right tok
      else do
        tok' <- EitherT .
          fmap (first TokenError) .
          obtainAccessToken bot $
          reg
        lift . apiUpdateAccessToken (bot ^. hipBotAPI) oid $ tok'
        return tok'
    return (reg, tok')

notificationUrl :: Either RoomName RoomId -> Registration -> String
notificationUrl room =
  show .
    relativeTo ["room", either id (T.pack . show) room, "notification"] .
    view capabilitiesUrl

