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
{-# LANGUAGE NoMonoLocalBinds #-}
module Lagoon.Server.API (server) where

import Servant

import Lagoon.Server.HandlerM
import qualified Lagoon.Interface.API      as API
import qualified Lagoon.Server.API.Column  as Column
import qualified Lagoon.Server.API.Debug   as Debug
import qualified Lagoon.Server.API.Group   as Group
import qualified Lagoon.Server.API.Groups  as Groups
import qualified Lagoon.Server.API.Source  as Source
import qualified Lagoon.Server.API.Sources as Sources
import qualified Lagoon.Server.API.SQL     as SQL
import qualified Lagoon.Server.API.User    as User
import qualified Lagoon.Server.API.Users   as Users

-- | Top-level server definition
server :: STrans API.Lagoon
server = Source.server
    :<|> Sources.server
    :<|> Column.server
    :<|> SQL.server
    :<|> User.server
    :<|> Users.server
    :<|> Group.server
    :<|> Groups.server
    :<|> Debug.server
