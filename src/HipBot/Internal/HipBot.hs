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

data HipBot m = HipBot
  { botAPI :: HipBotAPI m
  , botAddOn :: AddOn
  , botManager :: HTTP.Manager
  }

makeClassy ''HipBot

instance HasHipBotAPI (HipBot m) m where
  hipBotAPI = hipBot . api where
    api f (HipBot a b c) = fmap (\ a' -> HipBot a' b c) (f a)

newHipBot :: HipBotAPI m -> AddOn -> IO (HipBot m)
newHipBot api addon = HipBot api addon <$> HTTP.newManager tlsManagerSettings

wreqDefaults :: HipBot m -> Wreq.Options
wreqDefaults bot = Wreq.defaults
  & Wreq.manager .~ Right (botManager bot)

