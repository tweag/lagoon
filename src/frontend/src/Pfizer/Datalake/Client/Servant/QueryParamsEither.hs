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
module Pfizer.Datalake.Client.Servant.QueryParamsEither () where

import Data.Proxy
import Data.Text (Text)
import GHC.TypeLits
import Servant.API
import Servant.Client
import Servant.Common.Req
import qualified Data.Text as Text

import Pfizer.Datalake.Interface.API

instance ( KnownSymbol symA
         , KnownSymbol symB
         , ToHttpApiData a
         , ToHttpApiData b
         , HasClient sub
         )
      => HasClient (QueryParamEither symA symB a b :> sub) where
  type Client (QueryParamEither symA symB a b :> sub) =
    Either a b -> Client sub

  clientWithRoute Proxy req param =
    clientWithRoute
      (Proxy :: Proxy (QueryParamsEither symA symB a b :> sub))
      req
      [param]

-- | Adapted from the instance for 'QueryParams'
instance ( KnownSymbol symA
         , KnownSymbol symB
         , ToHttpApiData a
         , ToHttpApiData b
         , HasClient sub
         )
      => HasClient (QueryParamsEither symA symB a b :> sub) where
  type Client (QueryParamsEither symA symB a b :> sub) =
    [Either a b] -> Client sub

  clientWithRoute Proxy req params =
      clientWithRoute (Proxy :: Proxy sub) (add params req)
    where
      add :: [Either a b] -> Req -> Req
      add []           !r = r
      add (Left  a:ps) !r = let val = Just $ toQueryParam a
                            in add ps (appendToQueryString paramA val r)
      add (Right b:ps) !r = let val = Just $ toQueryParam b
                            in add ps (appendToQueryString paramB val r)

      paramA, paramB :: Text
      paramA = Text.pack $ symbolVal (Proxy :: Proxy symA)
      paramB = Text.pack $ symbolVal (Proxy :: Proxy symB)
