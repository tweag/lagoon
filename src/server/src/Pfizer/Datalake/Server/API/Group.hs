module Pfizer.Datalake.Server.API.Group (server) where

import Servant

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Server.HandlerM
import Pfizer.Datalake.Util.PostgreSQL
import Pfizer.Datalake.Verified
import qualified Pfizer.Datalake.Interface.API as API

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
