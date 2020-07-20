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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lagoon.Client.Servant.Conduit (
      ResponseSource
    , ResClientM
      -- ** Utility
    , genericMediaType
    ) where

import Control.Exception (SomeException(..))
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Proxy
import Network.HTTP.Client (Response, HttpException(..))
import Network.HTTP.Types
import Network.HTTP.Media (MediaType, (//), parseAccept)
import Servant.API
import Servant.Client
import Servant.Common.Req (ClientM(..), UrlReq(..), reqToRequest)
import qualified Data.ByteString          as BS.S
import qualified Data.ByteString.Lazy     as BS.L
import qualified Data.Conduit.Combinators as C
import qualified Network.HTTP.Client      as Client
import qualified Network.HTTP.Conduit     as Client

import Lagoon.Interface.API

-- | Streaming response
type ResponseSource m = Response (ResumableSource m BS.S.ByteString)

-- | 'ClientM' monad extended with resource tracking
type ResClientM = ResourceT ClientM

-- | This is adapted from the 'HasClient' instance for 'Verb'
--
-- For now the @response@ type variable is used just for documentation.
instance ReflectMethod method => HasClient (StreamResponse response method) where
  type Client (StreamResponse response method) =
    ResClientM (ResponseSource ResClientM)

  clientWithRoute Proxy req = do
      ClientEnv manager baseurl <- ask
      partialRequest <- liftIO $ reqToRequest req baseurl
      let request = partialRequest { Client.method = method }
      mResponse <- catchHttpException $ Client.http request manager
      case mResponse of
        Left  httpErr  -> throwError $ ConnectionError (SomeException httpErr)
        Right response -> do
          let status = Client.responseStatus response
          ct <- responseMediaType response
          unless (status >= status200 && status < status300) $ do
            bodyBS <- streamBody response
            throwError $ FailureResponse (UrlReq baseurl req) status ct bodyBS
          return response
    where
      method = reflectMethod (Proxy :: Proxy method)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

catchHttpException :: MonadCatch m => m a -> m (Either HttpException a)
catchHttpException action = catch (Right <$> action) (return . Left)

-- | Extract media type (content type) from response
responseMediaType :: MonadError ServantError m
                  => ResponseSource m -> m MediaType
responseMediaType response =
    case lookup "Content-Type" $ Client.responseHeaders response of
      Nothing -> return genericMediaType
      Just t -> case parseAccept t of
        Nothing -> do bodyBS <- streamBody response
                      throwError $ InvalidContentTypeHeader
                                     (BS.L.fromStrict t)
                                     bodyBS
        Just t' -> return t'

genericMediaType :: MediaType
genericMediaType = "application"//"octet-stream"

-- | Collect response body in memory
--
-- If an exception occurs, we stream the body into a bytestring so that we can
-- throw an informative error.
--
-- TODO: We might want to limit the size of the body we read here.
streamBody :: forall m. Monad m => ResponseSource m -> m BS.L.ByteString
streamBody source = do
    Client.responseBody source $$+- collect
  where
    collect :: Sink BS.S.ByteString m BS.L.ByteString
    collect = BS.L.fromChunks <$> C.sinkList
