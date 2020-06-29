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
module Lagoon.Util.PostgreSQL.Schema.SqlTable (
    SqlTable(..)
  ) where

import Lagoon.Interface
import Lagoon.Util
import Lagoon.Util.PostgreSQL.Quoted
import Lagoon.Util.PostgreSQL.Schema.SqlEntity
import Lagoon.Util.PostgreSQL.Transaction

-- | SQL table
data SqlTable = SqlTable {
    -- | Table name
    sqlTableName :: TableName

    -- | Function to create the table
    --
    -- Right now we don't really reify the table structure so this function
    -- is currently part of the 'SqlTable' itself. If we refactor so that we
    -- do reify the table structure, this would be a derived function
    -- (cf. 'createSqlFun' or 'createSqlTrigger').
  , createSqlTable :: QueryS
  }

instance SqlEntity SqlTable where
  createEntity = createSqlTable
  dropEntity   = dropSqlTable

instance Quoted (Schema, SqlTable) where
  quoted (schema, SqlTable{..}) = quoted (schema, sqlTableName)

dropSqlTable :: Cascade -> SqlTable -> QueryS
dropSqlTable cascade SqlTable{..} schema = intercalateM " " [
      "DROP TABLE IF EXISTS"
    , quoted (schema, sqlTableName)
    , cascadeToQuery cascade
    ]
