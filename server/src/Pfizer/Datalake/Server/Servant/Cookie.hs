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
