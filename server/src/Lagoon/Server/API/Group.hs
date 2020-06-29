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
module Lagoon.Server.API.Group (server) where

import Servant

import Lagoon.Interface
import Lagoon.Server.HandlerM
import Lagoon.Util.PostgreSQL
import Lagoon.Verified
import qualified Lagoon.Interface.API as API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

server :: STrans API.Group
server = groupAddUser
    :<|> groupRemoveUser
    :<|> groupAddAdmin
    :<|> groupRemoveAdmin

{-------------------------------------------------------------------------------
  Handlers proper
-------------------------------------------------------------------------------}

groupAddUser :: STrans API.GroupAddUser
groupAddUser = groupManageOp addUserToGroup

groupRemoveUser :: STrans API.GroupRemoveUser
groupRemoveUser = groupManageOp removeUserFromGroup

groupAddAdmin :: STrans API.GroupAddAdmin
groupAddAdmin = groupManageOp grantManageGroup

groupRemoveAdmin :: STrans API.GroupRemoveAdmin
groupRemoveAdmin = groupManageOp revokeManageGroup

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

groupManageOp :: (Can 'Manage GroupIx -> User -> Transaction HandlerM ())
              -> ServerSession -> GroupName -> UserName -> HandlerM NoContent
groupManageOp f session groupName userName = do
    sessionUser <- getSessionUser session
    execTransaction $ do
      groupIx <- getGroup groupName
      user    <- lookupUserName userName
      perm    <- checkHasPermission sessionUser groupIx
      f perm user
      return NoContent
