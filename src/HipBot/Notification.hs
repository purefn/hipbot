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
          ]
        HtmlNotification t ->
          [ "message_format" .= ("html" :: Text)
          , "message" .= t
          ]
    in
      A.object (c <> nu <> m)

defaultNotification :: NotificationMessage -> Notification
defaultNotification = Notification Nothing False

textNotification :: Text -> Notification
textNotification = defaultNotification . TextNotification

htmlNotification :: Text -> Notification
htmlNotification = defaultNotification . HtmlNotification

