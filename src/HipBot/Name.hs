{-# LANGUAGE OverloadedStrings #-}

module HipBot.Name where

-- Common name object

import           Data.Aeson
import           Data.Monoid
import           Data.Text   (Text)

data Name = Name
  { nameI18n  :: Maybe Text -- ^ The optional localization key, used to look up the localized value. Valid length range: 1 - 40.
  , nameValue :: Text       -- ^ The default text. Valid length range: 1 - 100.
  } deriving Show

instance ToJSON Name where
  toJSON (Name i18n value) =
    object $ maybe [] (\x -> [ "i18n" .= x ]) i18n <> [ "value" .= value ]
