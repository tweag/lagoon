module Pfizer.Datalake.Server.API.Groups (server) where

import Servant

import Pfizer.Datalake.Server.HandlerM
import Pfizer.Datalake.Verified
import qualified Pfizer.Datalake.Interface.API as API

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
