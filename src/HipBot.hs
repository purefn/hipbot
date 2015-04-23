{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
import qualified Data.ByteString.UTF8 as B
import qualified Data.List as List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Network.Wreq as Wreq
import Safe

import HipBot.API
import HipBot.Internal.HipBot
import HipBot.Internal.OAuth
import HipBot.Internal.Resources
import HipBot.Internal.Types

data NotificationError
  = NoSuchRegistration OAuthId
  | TokenError OAuthError
  | RateLimitExceeded (Maybe UTCTime)
  | HttpError HttpException
  deriving Show

sendNotification
  :: (Applicative m, MonadCatch m, MonadIO m)
  => HipBot m
  -> OAuthId
  -> Either RoomName RoomId
  -> Notification
  -> m (Maybe NotificationError)
sendNotification bot oid room n =
  let
    msg = case n of
      TextNotification t -> A.object
        [ "message_format" .= ("text" :: Text)
        , "message" .= t
        ]
      HtmlNotification t -> A.object
        [ "message_format" .= ("html" :: Text)
        , "message" .= t
        ]
    opts tok = wreqDefaults bot
      & Wreq.header hAuthorization .~
          [("Bearer " <>) .  T.encodeUtf8 . view accessToken $ tok]
      & Wreq.header hContentType .~ ["application/json"]
    nurl = show .
      relativeTo ["room", either id (T.pack . show) room, "notification"] .
      view capabilitiesUrl
    send reg tok = Nothing <$ Wreq.postWith (opts tok) (nurl reg) msg
    trySend reg tok onAuthErr = liftIO (send reg tok) `catch` handler onAuthErr
    handler onAuthErr e = case e of
      StatusCodeException s hdrs _
        | s == unauthorized401 -> onAuthErr
        | s ^. Wreq.statusCode == 429 ->
            return . Just . RateLimitExceeded . rateLimitReset $ hdrs
      httpExc -> return . Just . HttpError $ httpExc
    reauth reg = updatedToken bot reg >>= \tok -> trySend reg tok (authFailed reg)
    authFailed = return . Just . TokenError . InvalidOAuthCreds
    firstTry = getToken bot oid >>= \(reg, tok) -> trySend reg tok (reauth reg)
  in
    either Just id <$> runEitherT firstTry

rateLimitReset :: ResponseHeaders -> Maybe UTCTime
rateLimitReset =
  let
    hdr = List.find ((==) "X-RateLimit-Reset" . fst)
    readMayInt :: (HeaderName, B.ByteString) -> Maybe Int
    readMayInt = readMay . B.toString . snd
  in
    fmap (posixSecondsToUTCTime . realToFrac) . (readMayInt =<<) . hdr

getToken
  :: (Applicative m, MonadCatch m, MonadIO m)
  => HipBot m
  -> OAuthId
  -> EitherT NotificationError m (Registration, AccessToken)
getToken bot oid = lookupReg >>= fetch where
  lookupReg = EitherT .
    fmap (maybe (Left . NoSuchRegistration $ oid) Right) .
    apiLookupRegistration (view hipBotAPI bot) $
    oid
  fetch (reg, tok) = do
    now <- liftIO getCurrentTime
    -- TODO get rid of this once confident the check is correct
    liftIO $ print $ mconcat
      [ "OAuthToken: expires="
      , show $ tok ^. expires
      , ", now="
      , show now
      ]
    if addUTCTime 300 now < tok ^. expires
      then right (reg, tok)
      else updatedToken bot reg <&> (reg,)

updatedToken
  :: (MonadCatch m, MonadIO m, Applicative m)
  => HipBot m
  -> Registration
  -> EitherT NotificationError m AccessToken
updatedToken bot reg = do
  tok <- EitherT .
    fmap (first TokenError) .
    obtainAccessToken bot $
    reg
  lift . apiUpdateAccessToken (bot ^. hipBotAPI) (reg ^. oauthId) $ tok
  return tok

