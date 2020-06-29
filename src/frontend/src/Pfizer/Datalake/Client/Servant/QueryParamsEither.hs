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
