module Pfizer.Datalake.Server.FriendlyException.ReportError (
    ReportError(..)
  ) where

import Control.Monad.Catch hiding (Handler)
import Servant
import qualified Data.ByteString.Lazy.UTF8 as BS.L.UTF8
import qualified Network.HTTP.Client       as Client

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Verified
import Pfizer.Datalake.FriendlyException.Orphans ()
import Pfizer.Datalake.Util.PostgreSQL.Exception

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
