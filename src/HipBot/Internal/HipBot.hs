{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.Internal.HipBot where

import Control.Applicative
import Control.Lens
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS
import qualified Network.Wreq as Wreq

import HipBot.API
import HipBot.Internal.Types

type OnUninstall m = OAuthId -> m ()

data HipBot m = HipBot
  { _hipBotHipBotAPI :: HipBotAPI m
  , _hipBotAddOn :: AddOn
  , _hipBotOnUninstall :: OnUninstall m
  , _hipBotManager :: HTTP.Manager
  }

makeFields ''HipBot

newHipBot :: HipBotAPI m -> AddOn -> OnUninstall m -> IO (HipBot m)
newHipBot api addon uninstall = HipBot api addon uninstall
  <$> HTTP.newManager tlsManagerSettings

newHipBot' :: Monad m => HipBotAPI m -> AddOn -> IO (HipBot m)
newHipBot' api addon = newHipBot api addon (const $ return ())

wreqDefaults :: HipBot m -> Wreq.Options
wreqDefaults bot = Wreq.defaults
  & Wreq.manager .~ Right (bot ^. manager)

