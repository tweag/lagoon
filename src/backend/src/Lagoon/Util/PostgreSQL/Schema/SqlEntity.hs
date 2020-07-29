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
{-# LANGUAGE OverloadedStrings #-}
-- | Abstract over SQL entities (tables, functions, triggers, etc.)
module Lagoon.Util.PostgreSQL.Schema.SqlEntity (
    SqlEntity(..)
  , Cascade(..)
  , cascadeToQuery
  ) where

import Database.PostgreSQL.Simple
import Lagoon.Util.PostgreSQL.Transaction

data Cascade = Cascade | Restrict

cascadeToQuery :: Cascade -> Query
cascadeToQuery Cascade  = "CASCADE"
cascadeToQuery Restrict = "RESTRICT"

class SqlEntity e where
  createEntity :: e -> QueryS
  dropEntity   :: Cascade -> e -> QueryS