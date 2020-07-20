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
module Lagoon.Server.API.Groups (server) where

import Servant

import Lagoon.Server.HandlerM
import Lagoon.Verified
import qualified Lagoon.Interface.API as API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

server :: STrans API.Groups
server = groupsCreate

{-------------------------------------------------------------------------------
  Handlers proper
-------------------------------------------------------------------------------}

groupsCreate :: STrans API.GroupsCreate
groupsCreate session groupName = do
    user <- getSessionUser session
    execTransaction $ do
      perm     <- checkHasPermission user ()
      _groupIx <- createGroup perm groupName
      return NoContent
