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
module Lagoon.Server.FriendlyException.ReportError (
    ReportError(..)
  ) where

import Control.Monad.Catch hiding (Handler)
import Servant
import qualified Data.ByteString.Lazy.UTF8 as BS.L.UTF8
import qualified Network.HTTP.Client       as Client

import Lagoon.Interface
import Lagoon.Verified
import Lagoon.FriendlyException.Orphans ()
import Lagoon.Util.PostgreSQL.Exception

-- | Turn exceptions into servant errors with a human-friendly error message in
-- response body.
class Exception e => ReportError e where
  reportError :: e -> ServantErr
  default reportError :: FriendlyException e => e -> ServantErr
  reportError e = (reportErrorCode e) {
      errBody = BS.L.UTF8.fromString $ displayFriendly e
    }

  reportErrorCode :: e -> ServantErr
  reportErrorCode _ = err400

-- TODO: Not sure if we need to have some special cases here
instance ReportError ServantErr where
  reportError     = id
  reportErrorCode = id

instance ReportError PermissionDeniedException where reportErrorCode _ = err403
instance ReportError NotFoundException         where reportErrorCode _ = err404
instance ReportError AlreadyExistsException
instance ReportError Client.HttpException
instance ReportError SomeSqlException
