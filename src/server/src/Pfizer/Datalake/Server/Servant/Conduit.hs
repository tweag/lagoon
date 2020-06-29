{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pfizer.Datalake.Server.Servant.Conduit (
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

import Pfizer.Datalake.Interface.API hiding (Source)

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
