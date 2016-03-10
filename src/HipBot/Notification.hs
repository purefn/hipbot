{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Notification where

import Control.Lens hiding ((.=))
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.Monoid
import Data.Text (Text)
import Data.Typeable
import Prelude
import HipBot.Card

data NotificationColor
  = Yellow
  | Green
  | Red
  | Purple
  | Gray
  | Random
  deriving (Show, Eq, Typeable)

instance A.ToJSON NotificationColor where
  toJSON = \case
    Yellow -> "yellow"
    Green -> "green"
    Red -> "red"
    Purple -> "purple"
    Gray -> "gray"
    Random -> "random"

data NotificationMessage
  = TextNotification Text
  | HtmlNotification Text
  deriving (Show, Eq, Typeable)

data Notification = Notification
  { _notificationColor :: Maybe NotificationColor
  , _notificationNotify :: Bool
  , _notificationCard :: Maybe Card
  , _notificationMessage :: NotificationMessage
  } deriving (Show, Eq, Typeable)

makeFields ''Notification

instance A.ToJSON Notification where
  toJSON n =
    let
      c = case n ^. color of
        Just x | x /= Yellow -> [ "color" .= x ]
        _ -> []
      nu =  [ "notify" .= True | n ^. notify ]
      m = case n ^. message of
        TextNotification t ->
          [ "message_format" .= ("text" :: Text)
          , "message" .= t
          , "card" .= (n ^. card)
          ]
        HtmlNotification t ->
          [ "message_format" .= ("html" :: Text)
          , "message" .= t
          , "card" .= (n ^. card)
          ]
    in
      A.object (c <> nu <> m)

defaultNotification :: NotificationMessage -> Notification
defaultNotification = Notification Nothing False Nothing

textNotification :: Text -> Notification
textNotification = defaultNotification . TextNotification

htmlNotification :: Text -> Notification
htmlNotification = defaultNotification . HtmlNotification

cardNotification :: Text -> Text -> Notification
cardNotification title' msg = textNotification msg & card .~ (Just $ cardWithDescription title' msg)
