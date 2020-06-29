{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pfizer.Datalake.Client.Servant.Session () where

import Data.Proxy
import Servant.API
import Servant.Client

import Pfizer.Datalake.Interface.API

-- | We don't keep explicit track of the session
--
-- After all, the client knows whether it logged in or not; cookie handling
-- is dealt with separately in "Pfizer.Datalake.Client.Servant.Cookie".
instance HasClient sub => HasClient (Session :> sub) where
  type Client (Session :> sub) = Client sub
  clientWithRoute Proxy = clientWithRoute (Proxy :: Proxy sub)
