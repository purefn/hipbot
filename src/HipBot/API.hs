{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HipBot.API where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HashMap

import HipBot.Internal.Types

data HipBotAPI m = HipBotAPI
  { apiInsertRegistration :: Registration -> AccessToken -> m ()
  , apiDeleteRegistration :: OAuthId -> m ()
  , apiLookupRegistration :: OAuthId -> m (Maybe (Registration, AccessToken))
  , apiUpdateAccessToken :: OAuthId -> AccessToken -> m ()
  }

makeClassy ''HipBotAPI

stmAPI :: MonadIO m => IO (HipBotAPI m)
stmAPI = do
  regs <- newTVarIO HashMap.empty
  return HipBotAPI
    { apiInsertRegistration = \r t ->
        liftIO .
          atomically .
          modifyTVar' regs .
          HashMap.insert (r ^. oauthId) $
          (r, t)
    , apiDeleteRegistration =
        liftIO .
          atomically .
          modifyTVar' regs .
          HashMap.delete
    , apiLookupRegistration =
        liftIO .
          atomically .
          flip fmap (readTVar regs) .
          HashMap.lookup
    , apiUpdateAccessToken = \oid t ->
        liftIO .
          atomically .
          modifyTVar' regs .
          HashMap.adjust (set _2 t) $
          oid
    }

