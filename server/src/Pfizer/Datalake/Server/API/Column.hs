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
module Pfizer.Datalake.Server.API.Column (server) where

import Servant

import Pfizer.Datalake.Server.HandlerM
import Pfizer.Datalake.Verified
import qualified Pfizer.Datalake.Interface.API as API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

server :: STrans API.Column
server = columnSetType

{-------------------------------------------------------------------------------
  Handlers proper
-------------------------------------------------------------------------------}

columnSetType :: STrans API.ColumnSetType
columnSetType session columnIx columnType = do
    user <- getSessionUser session
    execTransaction $ do
      perm <- checkHasPermission user columnIx
      setColumnType perm columnType
      return NoContent
