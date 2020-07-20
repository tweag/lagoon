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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lagoon.Server.Servant.QueryParamsEither () where

import Data.Maybe (mapMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable
import GHC.TypeLits
import Network.HTTP.Types hiding (Header, ResponseHeaders)
import Network.Wai
import Servant.API
import Servant.Server
import Servant.Server.Internal.RoutingApplication
import Web.Internal.HttpApiData (parseQueryParamMaybe)
import qualified Data.Text as Text

import Lagoon.Interface.API

-- | Adapted from the instance for 'QueryParam'
--
-- TODO: The 'Maybe' in the continuation type is unfortunate. It would be
-- better if we could /require/ the parameter. But this doesn't fit quite so
-- smoothly with the existing instances.
--
-- TODO: We should really throw an error if both parameters are present, or
-- if they are present more than once.
instance ( KnownSymbol symA
         , KnownSymbol symB
         , FromHttpApiData a
         , FromHttpApiData b
         , HasServer sub ctxt
         ) => HasServer (QueryParamEither symA symB a b :> sub) ctxt where
  type ServerT (QueryParamEither symA symB a b :> sub) m =
    Maybe (Either a b) -> ServerT sub m

  route Proxy ctxt sub =
      route (Proxy :: Proxy (QueryParamsEither symA symB a b :> sub))
            ctxt
            ((. listToMaybe) <$> sub)

-- | Adapted from the instance for 'QueryParams'
instance ( KnownSymbol symA
         , KnownSymbol symB
         , FromHttpApiData a
         , FromHttpApiData b
         , HasServer sub ctxt
         ) => HasServer (QueryParamsEither symA symB a b :> sub) ctxt where

  type ServerT (QueryParamsEither symA symB a b :> sub) m =
    [Either a b] -> ServerT sub m

  route Proxy ctxt sub =
      route (Proxy :: Proxy sub)
            ctxt
            (passToServer sub (mapMaybe (uncurry isParam) . queryText))
    where
      isParam :: Text -> Maybe Text -> Maybe (Either a b)
      isParam param val
        | param `looksLike` paramA = Left  <$> (val >>= parseQueryParamMaybe)
        | param `looksLike` paramB = Right <$> (val >>= parseQueryParamMaybe)
        | otherwise                = Nothing

      queryText :: Request -> [(Text, Maybe Text)]
      queryText r = parseQueryText $ rawQueryString r

      paramA, paramB :: Text
      paramA = Text.pack $ symbolVal (Proxy :: Proxy symA)
      paramB = Text.pack $ symbolVal (Proxy :: Proxy symB)

      -- if sym is "foo", we look for query string parameters named "foo" or
      -- "foo[]" and call parseQueryParam on the corresponding values
      looksLike :: Text -> Text -> Bool
      looksLike param name = name == param || name == (param <> "[]")
