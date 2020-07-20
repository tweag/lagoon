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
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lagoon.Server.Servant.Conduit (
    ServerStreamResponse(..)
  ) where

import Control.Exception
import Control.Monad.Trans.Resource
import Data.ByteString.Builder (Builder)
import Data.Conduit
import Network.HTTP.Types (Status, ResponseHeaders, Method)
import Network.Wai.Conduit
import Servant
import Servant.Server.Internal

import Lagoon.Interface.API hiding (Source)

-- | Server-side interpretation of 'StreamResponse'
data ServerStreamResponse (method :: StdMethod) = StreamResponse {
      streamResponseBody    :: Source (ResourceT IO) (Flush Builder)
    , streamResponseStatus  :: Status
    , streamResponseHeaders :: ResponseHeaders
    }

-- | For now the @response@ argument is used just for documentation
instance ReflectMethod method => HasServer (StreamResponse response method) ctxt where
  type ServerT (StreamResponse response method) m = m (ServerStreamResponse method)

  route Proxy _ctxt sub = leafRouter $ \env req k ->
      bracket createInternalState closeInternalState $ \st ->
        runAction (sub `addMethodCheck` methodCheck method req)
                  env
                  req
                  k
                  (mkResponse st)
    where
      method :: Method
      method = reflectMethod (Proxy :: Proxy method)

      mkResponse :: InternalState
                 -> ServerStreamResponse method
                 -> RouteResult Response
      mkResponse st StreamResponse{..} =
          Route
        . responseSource streamResponseStatus streamResponseHeaders
        . transPipe (`runInternalState` st)
        $ streamResponseBody
