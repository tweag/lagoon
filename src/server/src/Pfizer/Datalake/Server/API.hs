{-# LANGUAGE NoMonoLocalBinds #-}
module Pfizer.Datalake.Server.API (server) where

import Servant

import Pfizer.Datalake.Server.HandlerM
import qualified Pfizer.Datalake.Interface.API      as API
import qualified Pfizer.Datalake.Server.API.Column  as Column
import qualified Pfizer.Datalake.Server.API.Debug   as Debug
import qualified Pfizer.Datalake.Server.API.Group   as Group
import qualified Pfizer.Datalake.Server.API.Groups  as Groups
import qualified Pfizer.Datalake.Server.API.Source  as Source
import qualified Pfizer.Datalake.Server.API.Sources as Sources
import qualified Pfizer.Datalake.Server.API.SQL     as SQL
import qualified Pfizer.Datalake.Server.API.User    as User
import qualified Pfizer.Datalake.Server.API.Users   as Users

-- | Top-level server definition
server :: STrans API.Datalake
server = Source.server
    :<|> Sources.server
    :<|> Column.server
    :<|> SQL.server
    :<|> User.server
    :<|> Users.server
    :<|> Group.server
    :<|> Groups.server
    :<|> Debug.server
