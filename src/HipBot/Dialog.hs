{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Dialog where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Text           (Text)
import           GHC.Generics

import HipBot.AbsoluteURI
import HipBot.Name

data Dialog = Dialog {
  _dialogKey :: Text,
  _dialogTitle :: Name,
  _dialogUrl :: AbsoluteURI,
  _dialogOptions :: Maybe DialogOptions
} deriving (Show, Generic)

defaultDialog :: Text -> Text -> AbsoluteURI -> Dialog
defaultDialog k t u = Dialog k t u Nothing

trailingFieldName :: Int -> String -> String
trailingFieldName n = (\(x:xs) -> toLower x:xs) . drop n

instance ToJSON Dialog where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = trailingFieldName 7,
    omitNothingFields = True
  }

data DialogStyle = Normal | Warning
  deriving Show

instance ToJSON DialogStyle where
  toJSON Normal = "normal"
  toJSON Warning = "warning"

data DialogOptions = DialogOptions {
  _dialogOptionsStyle :: Maybe Normal,
  _dialogOptionsPrimaryAction :: Maybe DialogAction,
  _dialogOptionsSecondaryActions :: Maybe [DialogAction],
  _dialogOptionsSize             :: Maybe DialogSize,
  _dialogOptionsHint             :: Maybe Name,
  _dialogOptionsFilter           :: Maybe DialogFilter
} deriving (Show, Eq, Generic)

instance ToJSON DialogOptions where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = trailingFieldName 14,
    omitNothingFields = True
  }

defaultDialogOptions :: DialogOptions
defaultDialogOptions = DialogOptions Nothing Nothing Nothing Nothing Nothing Nothing

data DialogAction = DialogAction {
  _dialogActionName    :: Name,
  _dialogActionEnabled :: Bool,
  _dialogActionKey     :: Maybe Text
} deriving (Show, Eq, Generic)

instance ToJSON DialogAction where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = trailingFieldName 13,
    omitNothingFields = True
  }

data DialogSize = DialogSize {
  _dialogSizeHeight :: Text, -- Either 'px' or '%'
  _dialogSizeWidth  :: Text -- Either 'px' or '%'
} deriving (Show, Eq, Generic)

instance ToJSON DialogSize where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = trailingFieldName 11,
    omitNothingFields = True
  }

data DialogFilter = DialogFilter {
  _dialogFilterPlaceholder  :: Name
} deriving (Show, Generic)

instance ToJSON DialogFilter where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = trailingFieldName 13,
    omitNothingFields = True
  }
