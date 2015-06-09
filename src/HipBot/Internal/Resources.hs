{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module HipBot.Internal.Resources where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import Network.Wai (lazyRequestBody)
import Network.Wai.Lens
import Prelude
import qualified Web.JWT as JWT
import Webcrank.Wai

import HipBot.API
import HipBot.Internal.HipBot
import HipBot.Internal.OAuth
import HipBot.Descriptor

hipBotResources
  :: (Applicative m, MonadCatch m, MonadIO m)
  => HipBot m
  -> Dispatcher (WaiResource m)
hipBotResources bot = mconcat
  [ root ==> resourceWithJson' (bot ^. addOn)
  , "installations" ==> installationsResource bot
  , "installations" </> param ==> installationResource bot
  ]

resourceWithJson' :: (Monad m, A.ToJSON a) => a -> Resource m
resourceWithJson' = resourceWithBody "application/json" . return . A.encode

installationsResource
  :: (Applicative m, MonadCatch m, MonadIO m)
  => HipBot m
  -> WaiResource m
installationsResource bot = resource
  { allowedMethods = return [methodPost]
  , postAction = postProcess $ do
      req <- view request
      body <- liftIO $ lazyRequestBody req
      reg <-decodeRegistration body
      tok <- lift . lift . obtainAccessToken bot $ reg
      either
        (werrorWith badGateway502 . LB.fromString . showOAuthError)
        (lift . lift . apiInsertRegistration (bot ^. hipBotAPI) reg)
        tok
  }

decodeRegistration :: Monad m => LB.ByteString -> HaltT (WaiCrankT m) Registration
decodeRegistration = either err return . A.eitherDecode where
  err = werrorWith badRequest400 . LB.fromString

installationResource
  :: MonadIO m
  => HipBot m
  -> Text
  -> WaiResource m
installationResource bot oid = resource
  { allowedMethods = return [methodDelete]
  , deleteResource = lift . lift $ do
      bot ^. onUninstall . to ($ oid)
      apiDeleteRegistration (bot ^. hipBotAPI) oid
      return True
  }

configResource
  :: (Applicative m, Monad m)
  => HipBot m
  -> (Registration -> WaiCrankT m Body)
  -> WaiResource m
configResource bot body = resource
  { contentTypesProvided = return
      [("text/html", verifySignature bot >>= lift . body)]
  }

verifySignature
  :: (Applicative m, Monad m)
  => HipBot m
  -> HaltT (WaiCrankT m) Registration
verifySignature bot = lift (runMaybeT verify) >>= handleErr where
  verify = do
    jwt <- decode =<< signature
    (reg, _) <- lookupReg =<< oid jwt
    reg <$ hoistMaybe (JWT.verify (JWT.secret (reg ^. oauthSecret)) jwt)
  signature = MaybeT sig where
    sig = join <$> preview (request . queryString . value "signed_request")
  decode = hoistMaybe . JWT.decode . T.decodeUtf8
  oid = hoistMaybe . fmap JWT.stringOrURIToText . JWT.iss . JWT.claims
  lookupReg = MaybeT . lift . apiLookupRegistration (view hipBotAPI bot)
  handleErr = maybe (halt notFound404) return

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return
{-# INLINE hoistMaybe #-}

