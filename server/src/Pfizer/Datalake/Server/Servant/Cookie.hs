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
module Pfizer.Datalake.Server.Servant.Cookie (ServerWithCookie(..)) where

import Network.HTTP.Types (ok200)
import Servant
import Servant.API.ContentTypes
import Servant.Server.Internal
import Web.Cookie
import qualified Data.ByteString.Builder   as BS.Bld
import qualified Data.ByteString.Lazy.UTF8 as BS.L.UTF8
import qualified Data.Text                 as Text

import Pfizer.Datalake.Interface.API

-- | Adapted from the 'HasServer' instance for 'Verb .. Headers'
--
--  TODO: This would make more sense:
--
--  > instance HasServer (WithCookie (Verb method status ctypes a)) ctxt where
--  >   type ServerT (WithCookie (Verb method status ctypes a)) m = m (ServerWithCookie a)
--
--  but it doens't work for some reason. I don't know why.
instance AllCTRender ctypes a => HasServer (WithCookie (Post ctypes a)) ctxt where
  type ServerT (WithCookie (Post ctypes a)) m = m (ServerWithCookie a)

  route Proxy _ctxt sub =
      methodRouterHeaders
        method
        (Proxy :: Proxy ctypes)
        status
        (fmap aux <$> sub)
    where
      aux :: ServerWithCookie a -> Headers '[Header "Set-Cookie" SetCookie] a
      aux (NoCookie          a) = noHeader         a
      aux (WithCookie cookie a) = addHeader cookie a

      method = reflectMethod (Proxy :: Proxy 'POST)
      status = ok200

-- | Server-side interpretation of 'WithCookie'
data ServerWithCookie a =
    WithCookie SetCookie a
  | NoCookie a

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Annoyingly @cookie@ provides 'Text' versions for 'Cookies'
-- but not for 'SetCookie'
instance ToHttpApiData SetCookie where
  toQueryParam = Text.pack
               . BS.L.UTF8.toString
               . BS.Bld.toLazyByteString
               . renderSetCookie
