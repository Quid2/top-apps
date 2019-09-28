{-# LANGUAGE OverloadedStrings ,ScopedTypeVariables #-}
module Network.Wai.Middleware.StaticHost (staticHost,Host(..)) where

import Prelude hiding (FilePath)
import Network.Wai
import Data.Monoid (mempty)
import Control.Monad
-- import Data.Enumerator (enumEOF, ($$))
import Data.List
import Data.Char
import Data.Maybe
-- import System.FilePath
-- import Data.Text.Internal.Builder(fromText,fromString)
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive(CI(..),mk,original)
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Types
-- import Network.Wai.Application.Static(staticApp,defaultFileServerSettings,ssFolder,fileSystemLookup,ssMaxAge,ssGetMimeType,defaultMimeTypeByExt,MaxAge(..),toFilePath,fileName,FilePath(..))
import Network.Wai.Application.Static(staticApp)
import WaiAppStatic.Types(StaticSettings(..),MaxAge(..),ssMaxAge,fromPiece,fileName)
import WaiAppStatic.Storage.Filesystem(defaultFileServerSettings)
-- import  Filesystem.Path.CurrentOS -- (fromText)
import Data.String
import Network.Mime
-- import qualified Network.HTTP.Types as H

-- TODO: might rewrite using Network.Wai.Middleware.Vhost
data Host = Host {hostDomain::T.Text -- B.ByteString -- NOTE: intercept all domains that ends with hostDomain, first host is also considered the default host.
                 ,hostDir::T.Text -- B.ByteString
                 ,mutable::Bool
                 } deriving (Show,Read)

staticHost :: [Host] -> [String] -> Middleware
staticHost hosts nonStaticPrefixes =
  let nonStaticPaths = map (tail . T.splitOn "/" . fromString) nonStaticPrefixes
      sites = map (\host -> (encodeUtf8 . hostDomain $ host,staticApp ((defaultFileServerSettings . T.unpack . hostDir $ host) {ssUseHash=True,ssGetMimeType = getMimeType,ssMaxAge = if mutable host then NoMaxAge else MaxAgeForever}))) hosts
  in chooseHost nonStaticPaths sites

-- WARN: if a file is substituted with another file with the same creation date, the old one will be returned.
chooseHost nonStaticPaths sites app req =
  if requestMethod req /= "GET" || any (\nonStatic -> isPrefixOf nonStatic $ pathInfo req) nonStaticPaths
    then app req
    else
      let hostH = mk "Host" -- hostH :: CI Ascii = mk "Host"
          hostName = B.takeWhile (/= ':') . fromJust . lookup hostH $ requestHeaders req
          -- use first site in list as a default site
          Just (_,site) = find (\(d,s) -> d `B.isSuffixOf` hostName) sites `mplus` listToMaybe sites
      in site req

getMimeType = return . defaultMimeLookup . T.toLower . fromPiece . fileName

{-
staticHost :: [Host] -> [String] -> Middleware
staticHost hosts nonStaticPrefixes app req = do
  let nonStaticPaths = map (tail . T.splitOn "/" . fromString) nonStaticPrefixes
  -- liftIO $ print "kok"
  if (requestMethod req /= "GET" || (any (\nonStatic -> isPrefixOf nonStatic $ pathInfo req) nonStaticPaths ))
    then app req
    else
      let hostH :: CI Ascii = mk "Host"
          hostName = B.takeWhile (/= ':') . fromJust . lookup hostH $ requestHeaders req
          -- NOTE: wai-static bug.
          staticSettings = defaultFileServerSettings {ssGetMimeType = return . defaultMimeTypeByExt . FilePath . T.toLower . unFilePath . fileName}
          Just host = find (\h -> hostDomain h `B.isSuffixOf` hostName) hosts `mplus` listToMaybe hosts

          statApp = staticApp (staticSettings {
            ssFolder = fileSystemLookup (toFilePath . B.unpack . hostDir $ host)
            ,ssMaxAge = if mutable host then NoMaxAge else MaxAgeForever
            })

      in statApp req
-}
