{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HipBot.WebPanel where

-- https://www.hipchat.com/docs/apiv2/webpanels

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Monoid
import qualified Data.Text          as T
import           GHC.Generics

import           HipBot.AbsoluteURI
import           HipBot.Icon
import           HipBot.Key
import           HipBot.Name

data WebPanelLocation = HipchatSidebarRight
  deriving (Eq, Show)

instance ToJSON WebPanelLocation where
  toJSON HipchatSidebarRight = "hipchat.sidebar.right"

instance FromJSON WebPanelLocation where
  parseJSON (String "hipchat.sidebar.right") =  return HipchatSidebarRight
  parseJSON (String x) = fail $ "Unexpected string: \"" <> T.unpack x <> "\" when parsing WebPanelLocation"
  parseJSON x = typeMismatch "WebPanelLocation" x

data WebPanel = WebPanel
  { webPanelIcon     :: Maybe Icon       -- ^ Icon to display on the left side of the webPanel title.
  , webPanelKey      :: Key              -- ^ Unique key (in the context of the integration) to identify this webPanel. Valid length range: 1 - 40.
  , webPanelLocation :: WebPanelLocation -- ^ The location of this webPanel Valid values: hipchat.sidebar.right.
  , webPanelName     :: Name             -- ^ The display name of the webPanel.
  , webPanelUrl      :: AbsoluteURI      -- ^ The URL of the resource providing the view content.
  , webPanelWeight   :: Maybe Int        -- ^ Determines the order in which webPanel appear. Web panels are displayed top to bottom or left to right in order of ascending weight. Defaults to 100.
  } deriving (Eq, Generic, Show)

webPanel :: Key -> Name -> AbsoluteURI -> WebPanel
webPanel key' name url = WebPanel Nothing key' HipchatSidebarRight name url Nothing

instance ToJSON WebPanel where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> toLower x:xs) . drop 8, omitNothingFields = True}

instance FromJSON WebPanel where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> toLower x:xs) . drop 8, omitNothingFields = True}
