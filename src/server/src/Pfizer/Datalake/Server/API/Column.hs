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
