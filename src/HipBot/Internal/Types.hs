{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HipBot.Internal.Types where

import qualified Data.Aeson as A
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

type OAuthId = Text
type RoomId = Int
type RoomName = Text

data RoomEvent
  = RoomMessage
  | RoomNotification
  | RoomExit
  | RoomEnter
  | RoomTopicChange
  deriving (Show, Eq)

instance A.ToJSON RoomEvent where
  toJSON s = A.String $ case s of
    RoomMessage -> "room_message"
    RoomNotification -> "room_notification"
    RoomExit -> "room_exit"
    RoomEnter -> "room_enter"
    RoomTopicChange -> "room_topic_change"

instance A.FromJSON RoomEvent where
  parseJSON = A.withText "string" $ \case
    "room_message" -> return RoomMessage
    "room_notification" -> return RoomNotification
    "room_exit" -> return RoomExit
    "room_enter" -> return RoomEnter
    "room_topic_change" -> return RoomTopicChange
    s -> fail $ "unexpected room event" <> T.unpack s

