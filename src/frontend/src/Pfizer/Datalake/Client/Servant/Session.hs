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
