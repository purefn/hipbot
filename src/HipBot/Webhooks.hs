{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Webhooks
  ( RoomLinks(..)
  , Room(..)
  , MessageItem(..)
  , WebhookRoomItem(..)
  , WebhookRoomEvent(..)
  , HasMembers(..)
  , HasParticipants(..)
  , HasSelf(..)
  , HasWebhooks(..)
  , HasRoomId(..)
  , HasName(..)
  , HasLinks(..)
  , HasMessage(..)
  , HasWebhookId(..)
  , HasOauthId(..)
  , HasItem(..)
  , decodeWebhookRoomEvent
  , webhookResource
  , roomMessageWebhookResource
  , simpleWebhookResource
  ) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import Data.Char (toLower)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Wai as Wai
import Prelude hiding (foldl1)

import Webcrank
import Webcrank.Wai

import HipBot.AbsoluteURI
import HipBot.Internal.Types
import HipBot.Notification

data RoomLinks = RoomLinks
  { _roomLinksMembers :: Maybe AbsoluteURI
  , _roomLinksParticipants :: AbsoluteURI
  , _roomLinksSelf :: AbsoluteURI
  , _roomLinksWebhooks :: AbsoluteURI
  } deriving (Show, Eq)

makeFields ''RoomLinks

data Room = Room
  { _roomRoomId :: RoomId
  , _roomName :: RoomName
  , _roomLinks :: RoomLinks
  } deriving (Show, Eq)

makeFields ''Room

instance A.FromJSON Room where
  parseJSON = A.withObject "object" $ \o -> Room
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "links"

data MessageItem = MessageItem
  { _messageItemMessage :: Text
  } deriving (Show, Eq)

makeFields ''MessageItem

data WebhookRoomItem
  = WebhookRoomMessage Room MessageItem
  --  WebhookRoomArchived
  --  WebhookRoomDeleted
  --  WebhookRoomEnter
  --  WebhookRoomExit
  --  WebhookRoomNotification
  --  WebhookRoomTopicChange
  --  WebhookRoomUnarchived
  deriving (Show, Eq)

data WebhookRoomEvent = WebhookRoomEvent
  { _webhookRoomEventWebhookId :: Int
  , _webhookRoomEventOauthId :: Maybe String
  , _webhookRoomEventItem :: WebhookRoomItem
  } deriving (Show, Eq)

makeFields ''WebhookRoomEvent

instance A.FromJSON WebhookRoomEvent where
  parseJSON = A.withObject "object" $ \o -> WebhookRoomEvent
    <$> o .: "webhook_id"
    <*> o .:? "oauth_client_id"
    <*> readItem o

readItem :: A.Object -> A.Parser WebhookRoomItem
readItem o = do
  oi <- o .: "item"
  o .: "event" >>= \case
    RoomMessage -> WebhookRoomMessage <$> oi .: "room" <*> oi .: "message"
    _ -> A.typeMismatch "only supports room_message events at this time" (A.Object o)

decodeWebhookRoomEvent :: (Functor m, MonadIO m, MonadReader s m, HasRequest s Wai.Request) => m (Either String WebhookRoomEvent)
decodeWebhookRoomEvent = A.eitherDecode <$> getRequestBodyLBS

$(A.deriveFromJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 10) : drop 11 l
     , A.omitNothingFields = True
     }
  ''RoomLinks)

$(A.deriveFromJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 12) : drop 13 l
     , A.omitNothingFields = True
     }
  ''MessageItem)

webhookResource
  :: (MonadIO m, MonadReader r m, HasRequest r Wai.Request, MonadState s m, HasReqData s)
  => String -- ^ webhook name
  -> (WebhookRoomEvent -> HaltT m (Maybe Notification)) -- ^ event processor
  -> Resource m
webhookResource hookName f = resource
  { allowedMethods = return [ methodPost ]
  , postAction = postProcess $
      decodeWebhookRoomEvent >>= \case
        Left e -> liftIO . putStrLn . mconcat $
          [ "[ERROR] Failed to parse event to "
          , hookName
          , " webhook: "
          , e
          ]
        Right ev -> f ev >>= traverse_ (writeLBS . A.encode)
  }

roomMessageWebhookResource
  :: (MonadIO m, MonadReader r m, MonadState s m, HasReqData s, HasRequest r Wai.Request)
  => String
  -> (Room -> MessageItem -> HaltT m (Maybe Notification))
  -> Resource m
roomMessageWebhookResource hookName f = webhookResource hookName $ \ev ->
  case ev ^. item of
    WebhookRoomMessage room msg -> f room msg

-- | Creates a simple "command" processing webhook resource.
-- Commands processes are limited to pure functions that may
-- or may not produce a reply.
simpleWebhookResource
  :: MonadIO m
  => String -- ^ webhook name
  -> [Text] -- ^ command aliases, they will be removed before calling the processing function
  -> (Text -> Maybe Text) -- ^ processing function, the result will become a room notification
  -> WaiResource m
simpleWebhookResource hookName aliases f =
  let
    expr t = T.strip <$> foldl1 (<|>) (fmap (`T.stripPrefix` t) aliases)
    command = views message (return . fmap textNotification . (f =<<) . expr)
  in
    roomMessageWebhookResource hookName (const command)

