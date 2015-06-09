{-# LANGUAGE DeriveDataTypeable #-}

module HipBot.AbsoluteURI where

import Blaze.ByteString.Builder (toLazyByteString)
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.List (isSuffixOf)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP.Types
import Network.URI (URI)
import qualified Network.URI as URI
import Prelude

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

