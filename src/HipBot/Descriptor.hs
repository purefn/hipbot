{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Descriptor where

import Control.Applicative
import Control.Lens.TH
import Data.Aeson ((.=), (.:?), (.!=))
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Prelude

import HipBot.AbsoluteURI
import HipBot.Internal.Types

data AddOn = AddOn
  { _addOnKey :: Text
  , _addOnName :: Text
  , _addOnDescription :: Text
  , _addOnLinks :: Links
  , _addOnCapabilities :: Maybe Capabilities
  , _addOnVendor :: Maybe Vendor
  } deriving (Show, Eq)

defaultAddOn
  :: Text -- ^ key
  -> Text -- ^ name
  -> Text -- ^ description
  -> Links
  -> AddOn
defaultAddOn k n d ls = AddOn k n d ls Nothing Nothing

data Links = Links
  { _linksSelf :: AbsoluteURI
  , _linksHomepage :: Maybe AbsoluteURI
  } deriving (Show, Eq)

defaultLinks
  :: AbsoluteURI  -- ^ self
  -> Links
defaultLinks s = Links s Nothing

data Capabilities = Capabilities
  { _capabilitiesInstallable        :: Maybe Installable
  , _capabilitiesHipchatApiConsumer :: Maybe APIConsumer
  , _capabilitiesOauth2Provider     :: Maybe OAuth2Provider
  , _capabilitiesWebhooks           :: [Webhook]
  , _capabilitiesConfigurable       :: Maybe Configurable
  , _capabilitiesDialog             :: [Dialog]
  , _capabilitiesWebPanel           :: [WebPanel]
  , _capabilitiesGlance             :: [Glance]
  } deriving (Show, Eq)

defaultCapabilities :: Capabilities
defaultCapabilities = Capabilities Nothing Nothing Nothing [] Nothing [] [] []

instance A.ToJSON Capabilities where
  toJSON (Capabilities is con o hs cfg dlg wp gl) = A.object $ catMaybes
    [ ("installable" .=) <$> is
    , ("hipchatApiConsumer" .=) <$> con
    , ("oauth2Provider" .=) <$> o
    , ("webhook" .=) <$> excludeEmptyList hs
    , ("configurable" .=) <$> cfg
    , ("dialog" .=) <$> excludeEmptyList dlg
    , ("webpanel" .=) <$> excludeEmptyList wp
    , ("glance" .=) <$> excludeEmptyList gl
    ]

excludeEmptyList :: [a] -> Maybe [a]
excludeEmptyList [] = Nothing
excludeEmptyList xs = Just xs

instance A.FromJSON Capabilities where
  parseJSON = A.withObject "object" $ \o -> Capabilities
    <$> o .:? "installable"
    <*> o .:? "hipchatApiConsumer"
    <*> o .:? "oauth2Provider"
    <*> o .:? "webhooks" .!= []
    <*> o .:? "configurable"
    <*> o .:? "dialog" .!= []
    <*> o .:? "webpanel" .!= []
    <*> o .:? "glance" .!= []

data Installable = Installable
  { _installableCallbackUrl :: Maybe AbsoluteURI
  , _installableAllowRoom :: Bool
  , _installableAllowGlobal :: Bool
  } deriving (Show, Eq)

instance A.ToJSON Installable where
  toJSON (Installable cb r g) = A.object $ catMaybes
    [ ("callbackUrl" .=) <$> cb
    ] <>
    [ "allowRoom" .= r
    , "allowGlobal" .= g
    ]

instance A.FromJSON Installable where
  parseJSON = A.withObject "object" $ \o -> Installable
    <$> o .:? "callbackUrl"
    <*> o .:? "allowRoom" .!= True
    <*> o .:? "allowGlobal" .!= True

defaultInstallable :: Installable
defaultInstallable = Installable Nothing True True

data APIConsumer = APIConsumer
  { _apiScopes :: [APIScope]
  , _apiFromName :: Maybe Text
  } deriving (Show, Eq)

defaultAPIConsumer :: APIConsumer
defaultAPIConsumer = APIConsumer [SendNotification] Nothing

data OAuth2Provider = OAuth2Provider
  { _oAuth2ProviderAuthorizationUrl :: AbsoluteURI
  , _oAuth2ProviderTokenUrl :: AbsoluteURI
  } deriving (Show, Eq)

data APIScope
  = AdminGroup
  | AdminRoom
  | ManageRooms
  | SendMessage
  | SendNotification
  | ViewGroup
  | ViewMessages
  deriving Eq

instance Show APIScope where
  show = apiScopeStr

instance A.ToJSON APIScope where
  toJSON = A.String . apiScopeStr

apiScopeStr :: IsString a => APIScope -> a
apiScopeStr = \case
  AdminGroup -> "admin_group"
  AdminRoom -> "admin_room"
  ManageRooms -> "manage_rooms"
  SendMessage -> "send_message"
  SendNotification -> "send_notification"
  ViewGroup -> "view_group"
  ViewMessages -> "view_messages"

instance A.FromJSON APIScope where
  parseJSON = A.withText "string" $ \case
    "admin_group" -> return AdminGroup
    "admin_room" -> return AdminRoom
    "manage_rooms" -> return ManageRooms
    "send_message" -> return SendMessage
    "send_notification" -> return SendNotification
    "view_group" -> return ViewGroup
    "view_messages" -> return ViewMessages
    s -> fail $ "unexpected API scope " <> T.unpack s

data Webhook = Webhook
  { _webhookUrl :: AbsoluteURI
  , _webhookPattern :: Maybe Text
  , _webhookEvent :: RoomEvent
  } deriving (Show, Eq)

webhook :: AbsoluteURI -> RoomEvent -> Webhook
webhook url = Webhook url Nothing

data Configurable = Configurable
  { _configurableUrl :: AbsoluteURI
  } deriving (Show, Eq)

data Vendor = Vendor
  { _vendorUrl :: AbsoluteURI
  , _vendorName :: Text
  } deriving (Show, Eq)

data Registration = Registration
  { _registrationOauthId :: OAuthId
  , _registrationCapabilitiesUrl :: AbsoluteURI
  , _registrationRoomId :: Maybe RoomId
  , _registrationGroupId :: Int
  , _registrationOauthSecret :: Text
  }

data AccessToken = AccessToken
  { _accessTokenAccessToken :: Text
  , _accessTokenExpires :: UTCTime
  }

makeFields ''AddOn
makeFields ''Links
makeFields ''Capabilities
makeFields ''Installable
makeLensesWith abbreviatedFields ''APIConsumer
makeFields ''OAuth2Provider
makeFields ''Configurable
makeFields ''Registration
makeFields ''AccessToken
makeFields ''Webhook
makeFields ''Glance

$(A.deriveJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 13) : drop 14 l
     , A.omitNothingFields = True
     }
  ''Configurable)

$(A.deriveJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 4) : drop 5 l
     , A.omitNothingFields = True
     }
  ''APIConsumer)

$(A.deriveJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 15) : drop 16 l
     , A.omitNothingFields = True
     }
  ''OAuth2Provider)

$(A.deriveJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 6) : drop 7 l
     , A.omitNothingFields = True
     }
  ''AddOn)

$(A.deriveJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 6) : drop 7 l
     , A.omitNothingFields = True
     }
  ''Links)

$(A.deriveJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 7) : drop 8 l
     , A.omitNothingFields = True
     }
  ''Vendor)

$(A.deriveJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 8) : drop 9 l
     , A.omitNothingFields = True
     }
  ''Webhook)

$(A.deriveJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 13) : drop 14 l
     , A.omitNothingFields = True
     }
  ''Registration)

