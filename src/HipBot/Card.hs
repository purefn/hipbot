{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module HipBot.Card where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (id)
import HipBot.AbsoluteURI
import HipBot.Icon
import GHC.Generics

--  file, image, application, link, media.
data CardStyle = CardFile
               | CardImage
               | CardApplication
               | CardLink
               | CardMedia
               deriving (Show, Eq)

instance ToJSON CardStyle where
  toJSON = \case
    CardFile -> "file"
    CardImage -> "image"
    CardApplication -> "application"
    CardLink -> "link"
    CardMedia -> "media"

data CardFormat = CardCompact
                | CardMedium
               deriving (Show, Eq)

instance ToJSON CardFormat where
  toJSON CardCompact = "compact"
  toJSON CardMedium = "medium"

data CardDescriptionFormat = CardDescriptionHtml
                           | CardDescriptionText
                           deriving (Show, Eq)

instance ToJSON CardDescriptionFormat where
  toJSON = \case
    CardDescriptionHtml -> "html"
    CardDescriptionText -> "text"

data CardDescription = CardDescription
  { _cardDescriptionFormat :: CardDescriptionFormat
  , _cardDescriptionValue :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON CardDescription where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> toLower x:xs) . drop 16, omitNothingFields = True}

data CardActivity = CardActivity
  { _cardActivityHtml :: Text
  , _cardActivityIcon :: Maybe Icon
  } deriving (Show, Eq, Generic)

instance ToJSON CardActivity where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> toLower x:xs) . drop 13, omitNothingFields = True}

data CardAttributeValueStyle = LozengeSuccess
                             | LozengeError
                             | LozengeCurrent
                             | LozengeComplete
                             | LozengeMoved
                             | Lozenge
                             deriving (Show, Eq, Generic)

instance ToJSON CardAttributeValueStyle where
  toJSON = \case
    LozengeSuccess  -> "lozenge-success"
    LozengeError    -> "lozenge-error"
    LozengeCurrent  -> "lozenge-current"
    LozengeComplete -> "lozenge-complete"
    LozengeMoved    -> "lozenge-moved"
    Lozenge         -> "lozenge"

data CardAttributeValue = CardAttributeValue
  { _cardAttributeValueUrl   :: Maybe AbsoluteURI
  , _cardAttributeValueStyle :: Maybe CardAttributeValueStyle
  , _cardAttributeValueLabel :: Text
  , _cardAttributeValueIcon  :: Maybe Icon
  } deriving (Show, Eq, Generic)

instance ToJSON CardAttributeValue where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> toLower x:xs) . drop 19, omitNothingFields = True}

data CardAttribute = CardAttribute
  { _cardAttributeLabel :: Text
  , _cardAttributeValue :: CardAttributeValue
  } deriving (Show, Eq, Generic)

instance ToJSON CardAttribute where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> toLower x:xs) . drop 14, omitNothingFields = True}

data Card = Card
  { _cardStyle       :: CardStyle
  , _cardDescription :: CardDescription
  , _cardFormat      :: CardFormat
  , _cardUrl         :: Maybe AbsoluteURI
  , _cardTitle       :: Text
  , _cardActivity    :: Maybe CardActivity
  , _cardId          :: Text
  , _cardAttributes  :: [CardAttribute]
  } deriving (Show, Eq, Generic)

makeFields ''Card

instance ToJSON Card where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> toLower x:xs) . drop 5, omitNothingFields = True}

cardWithDescription :: Text -> Text -> Card
cardWithDescription title' desc = Card
  { _cardStyle = CardApplication
  , _cardDescription = description'
  , _cardFormat = CardMedium
  , _cardUrl = Nothing
  , _cardTitle = title'
  , _cardActivity = Just $ CardActivity firstLine Nothing
  , _cardId = "12"
  , _cardAttributes = []
  }
  where description' = CardDescription CardDescriptionHtml desc
        firstLine = fromMaybe "" (listToMaybe (T.lines desc))
