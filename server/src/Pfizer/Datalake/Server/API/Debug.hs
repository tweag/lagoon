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
module Pfizer.Datalake.Server.API.Debug (server) where

import Control.Concurrent
import Control.Monad.IO.Class
import Servant

import Pfizer.Datalake.Server.HandlerM
import Pfizer.Datalake.Verified
import qualified Pfizer.Datalake.Interface.API as API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

server :: STrans API.Debug
server = debugDumpDbInfo
    :<|> debugRebuildCanReadCache

{-------------------------------------------------------------------------------
  Handlers proper
-------------------------------------------------------------------------------}

-- | Get all sources
--
-- TODO: This is a relatively expensive operation and might return quite a bit
-- of data. If this would be used for more than just integration testing we
-- might want to stream the result here.
debugDumpDbInfo :: STrans API.DebugDumpDbInfo
debugDumpDbInfo session = do
    admin  <- getSessionAdmin session
    result <- liftIO $ newEmptyMVar
    _pid   <- forkConnection $ \conn schema -> return $ do
                sources <- dumpDbInfo admin conn schema
                putMVar result sources
    liftIO $ readMVar result

-- | Rebuild the can-read cache
debugRebuildCanReadCache :: STrans API.DebugRebuildCanReadCache
debugRebuildCanReadCache session = do
    admin <- getSessionAdmin session
    execTransaction $ rebuildCanReadCache admin
    return NoContent
