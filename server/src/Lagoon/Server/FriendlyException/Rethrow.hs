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
module Lagoon.Server.FriendlyException.Rethrow (
    rethrowFriendly
  , finalLogMessage
  ) where

import Control.Exception (SomeException(..), throwIO)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Data.Typeable
import Servant
import System.IO
import qualified Control.Monad.Catch       as C
import qualified Data.ByteString.Lazy.UTF8 as BS.L.UTF8
import qualified Network.HTTP.Client       as Client

import Lagoon.Interface
import Lagoon.Util.Exception
import Lagoon.Util.PostgreSQL.Exception
import Lagoon.Verified
import Lagoon.Server.FriendlyException.ReportError
import Lagoon.Server.Servant.Session

rethrowFriendly :: forall m a. (MonadCatch m, MonadIO m) => m a -> m a
rethrowFriendly act = catches act $ concat [
      friendly (Proxy :: Proxy PermissionDeniedException)
    , friendly (Proxy :: Proxy NotFoundException)
    , friendly (Proxy :: Proxy AlreadyExistsException)
    , friendly (Proxy :: Proxy SessionException)
    , friendly (Proxy :: Proxy Client.HttpException)
    , friendly (Proxy :: Proxy ServantErr)
    , friendly (Proxy :: Proxy SomeSqlException)
    , [ C.Handler $ \(SomeException e) -> liftIO $ do
          -- We don't want to return arbitrary exceptions to the client as
          -- this is a potential security risk.
          hPutStrLn stderr $ "Warning: uncaught "
                          ++ show (typeOf e)
                          ++ ": "
                          ++ displayException e
          throwIO e
      ]
    ]
  where
    friendly :: forall e. ReportError e => Proxy e -> [C.Handler m a]
    friendly _ =
      [ C.Handler $ \(WithCallStack cs e) -> do
          liftIO $ hPutStrLn stderr $ "Warning: Dropping callstack for:"
                                   ++ show (typeOf e)
                                   ++ ": "
                                   ++ show cs
          liftIO $ throwIO $ reportError (e :: e)
      , C.Handler $ \err -> liftIO $ throwIO $ reportError (err :: e)
      ]

finalLogMessage :: Either ServantErr a -> Either IngestProgress a
finalLogMessage = either (Left . aux) Right
  where
    aux :: ServantErr -> IngestProgress
    aux = IngestAborted . Just . BS.L.UTF8.toString . errBody
