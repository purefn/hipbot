{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Internal.Types where

import Blaze.ByteString.Builder (toLazyByteString)
import Control.Applicative
import Control.Lens.TH
import Control.Monad
import Data.Aeson ((.=), (.:?), (.!=))
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Char
import Data.List (isSuffixOf)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import Network.HTTP.Types
import Network.URI (URI)
import qualified Network.URI as URI

newtype AbsoluteURI = AbsoluteURI URI
  deriving (Eq, Typeable)

parseAbsoluteURI :: String -> Maybe AbsoluteURI
parseAbsoluteURI = fmap AbsoluteURI . URI.parseAbsoluteURI

appendPath :: AbsoluteURI -> [Text] -> AbsoluteURI
appendPath (AbsoluteURI uri) xs = AbsoluteURI uri' where
  uri' = uri { URI.uriPath = URI.uriPath uri <> dropSlash (relPath xs) }
  dropSlash s = if "/" `isSuffixOf` URI.uriPath uri
    then tail s
    else s

relPath :: [Text] -> String
relPath = LB.toString . toLazyByteString . encodePathSegments

relativeTo :: [Text] -> AbsoluteURI -> AbsoluteURI
relativeTo xs (AbsoluteURI uri) = AbsoluteURI (URI.relativeTo rel uri) where
  rel = fromJust . URI.parseURIReference . drop 1 . relPath $ xs

instance Show AbsoluteURI where
  show (AbsoluteURI u) = show u

instance IsString AbsoluteURI where
  fromString s =
    fromMaybe (error $ "Not an absolute URI: " <> s) (parseAbsoluteURI s)

instance A.ToJSON AbsoluteURI where
  toJSON = A.toJSON . show

instance A.FromJSON AbsoluteURI where
  parseJSON = A.withText "String" $ \t ->
    maybe mzero return . parseAbsoluteURI . T.unpack $ t

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
  { _capabilitiesInstallable :: Maybe Installable
  , _capabilitiesHipchatApiConsumer :: Maybe APIConsumer
  , _capabilitiesOauth2Provider :: Maybe OAuth2Provider
  , _capabilitiesWebhooks :: [Webhook]
  , _capabilitiesConfigurable :: Maybe Configurable
  } deriving (Show, Eq)

defaultCapabilities :: Capabilities
defaultCapabilities = Capabilities Nothing Nothing Nothing [] Nothing

instance A.ToJSON Capabilities where
  toJSON (Capabilities is con o hs cfg) = A.object $ catMaybes
    [ ("installable" .=) <$> is
    , ("hipchatApiConsumer" .=) <$> con
    , ("oauth2Provider" .=) <$> o
    , ("webhooks" .= hs) <$ listToMaybe hs
    , ("configurable" .=) <$> cfg
    ]

instance A.FromJSON Capabilities where
  parseJSON = A.withObject "object" $ \o -> Capabilities
    <$> o .:? "installable"
    <*> o .:? "hipchatApiConsumer"
    <*> o .:? "oauth2Provider"
    <*> o .:? "webhooks" .!= []
    <*> o .:? "configurable"

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

data Configurable = Configurable
  { _configurableUrl :: AbsoluteURI
  } deriving (Show, Eq)

data Vendor = Vendor
  { _vendorUrl :: AbsoluteURI
  , _vendorName :: Text
  } deriving (Show, Eq)

type OAuthId = Text
type RoomId = Int
type RoomName = Text

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

data Notification
  = TextNotification Text
  | HtmlNotification Text

makeFields ''AddOn
makeFields ''Links
makeFields ''Capabilities
makeFields ''Installable
makeLensesWith abbreviatedFields ''APIConsumer
makeFields ''OAuth2Provider
makeFields ''Configurable
makeFields ''Registration
makeFields ''AccessToken

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
     { A.fieldLabelModifier = \l -> toLower (l !! 7) : drop 8 l
     , A.omitNothingFields = True
     }
  ''Webhook)

$(A.deriveJSON
  A.defaultOptions
     { A.fieldLabelModifier = \l -> toLower (l !! 13) : drop 14 l
     , A.omitNothingFields = True
     }
  ''Registration)

