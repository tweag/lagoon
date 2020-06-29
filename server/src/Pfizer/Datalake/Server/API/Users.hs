module Pfizer.Datalake.Server.API.Users (server) where

import Servant

import Pfizer.Datalake.Server.HandlerM
import Pfizer.Datalake.Verified
import qualified Pfizer.Datalake.Interface.API as API

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

server :: STrans API.Users
server = usersCreate

{-------------------------------------------------------------------------------
  Handlers proper
-------------------------------------------------------------------------------}

usersCreate :: STrans API.UsersCreate
usersCreate session userName = do
    isAdmin <- getSessionAdmin session
    execTransaction $ do
      _user <- createUser isAdmin userName
      return NoContent
