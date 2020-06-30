-- Copyright 2020 Pfizer Inc.

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     https://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Pfizer.Datalake.Client.Servant.Cookie (
    GlobalCookieJar -- opaque
  , newGlobalCookieJar
  , runClientMWithCookie
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Data.Proxy
import Network.HTTP.Client (ManagerSettings, CookieJar, Response, newManager)
import Network.HTTP.Media (matches)
import Servant.API
import Servant.Common.Req (reqAccept, performRequest)
import Servant.Client
import qualified Network.HTTP.Client as Client

import Pfizer.Datalake.Interface.API

-- | Adapted from the 'Post ctypes a' instance
instance {-# OVERLAPPABLE #-} (MimeUnrender ct a, cts' ~ (ct ': cts))
      => HasClient (WithCookie (Post cts' a)) where
  type Client (WithCookie (Post cts' a)) = GlobalCookieJar -> ClientM a

  clientWithRoute Proxy req cookieJar = do
      (_status, respBody, respCT, _hdrs, response) <-
        performRequest method (req { reqAccept = [acceptCT] })
      unless (matches respCT (acceptCT)) $
        throwError $ UnsupportedContentType respCT respBody
      case mimeUnrender ct respBody of
        Left err  -> throwError $ DecodeFailure err respCT respBody
        Right val -> updateGlobalCookieJar cookieJar response >> return val
    where
      ct       = Proxy :: Proxy ct
      acceptCT = contentType ct
      method   = reflectMethod (Proxy :: Proxy 'POST)

instance {-# OVERLAPPING #-} HasClient (WithCookie (Post cts' NoContent)) where
  type Client (WithCookie (Post cts' NoContent)) = GlobalCookieJar -> ClientM NoContent

  clientWithRoute Proxy req cookieJar = do
      (_status, _respBody, _respCT, _hdrs, response) <-
        performRequest method req
      updateGlobalCookieJar cookieJar response
      return NoContent
    where
      method = reflectMethod (Proxy :: Proxy 'POST)

{-------------------------------------------------------------------------------
  Global cookie jar

  The global cookie jar is used for all requests. Potentially this might get
  confusing if there are multiple concurrent requests, but this is not an
  issue for us.
-------------------------------------------------------------------------------}

newtype GlobalCookieJar = GlobalCookieJar (MVar CookieJar)

updateGlobalCookieJar :: MonadIO m => GlobalCookieJar -> Response body -> m ()
updateGlobalCookieJar (GlobalCookieJar ref) response = liftIO $
    modifyMVar_ ref $ \_oldCookieJar ->
      return $ Client.responseCookieJar response

runClientMWithCookie :: BaseUrl
                     -> ManagerSettings
                     -> (GlobalCookieJar -> ClientM a)
                     -> IO (Either ServantError a)
runClientMWithCookie baseUrl initMgrSettings act = do
    (cookieJar, mgrSettings) <- newGlobalCookieJar initMgrSettings
    manager <- newManager mgrSettings
    let clientEnv = ClientEnv manager baseUrl
    (`runClientM` clientEnv) $ act cookieJar

-- | Initialize new cookie jar
--
-- NOTE: If we use http-client >= 0.5.5 we can simplify this whole set up,
-- because we can then take advantage of 'managerModifyResponse'. This would
-- obsolete the need for a customized 'HasClient' instance for 'WithCookie'.
newGlobalCookieJar :: ManagerSettings -> IO (GlobalCookieJar,  ManagerSettings)
newGlobalCookieJar settings = do
    globalCookieJar <- newMVar mempty
    let settings' = settings {
            Client.managerModifyRequest = \req -> do
              req'      <- Client.managerModifyRequest settings req
              cookieJar <- readMVar globalCookieJar
              return $ req' { Client.cookieJar = Just cookieJar }
          }
    return (GlobalCookieJar globalCookieJar, settings')
