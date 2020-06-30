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
module Pfizer.Datalake.Util.PostgreSQL.Schema.SqlIndex (
    SqlIndex(..)
  ) where

import Data.Monoid ((<>))

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util
import Pfizer.Datalake.Util.PostgreSQL.Quoted
import Pfizer.Datalake.Util.PostgreSQL.Schema.SqlEntity
import Pfizer.Datalake.Util.PostgreSQL.Transaction

-- | SQL table
data SqlIndex =
    SqlBTreeIndex {
        sqlIndexName  :: IndexName
      , sqlIndexTable :: TableName
      , sqlIndexOn    :: QueryS
      }
  | SqlUniqueIndex {
        sqlIndexName  :: IndexName
      , sqlIndexTable :: TableName
      , sqlIndexOn    :: QueryS
      }
  | SqlTrigramIndex {
        sqlIndexName  :: IndexName
      , sqlIndexTable :: TableName
      , sqlIndexOn    :: QueryS
      }
  | SqlGinIndex {
        sqlIndexName  :: IndexName
      , sqlIndexTable :: TableName
      , sqlIndexOn    :: QueryS
      }

instance SqlEntity SqlIndex where
  createEntity = createSqlIndex
  dropEntity   = dropSqlIndex

createSqlIndex :: SqlIndex -> QueryS
createSqlIndex SqlBTreeIndex{..} schema = intercalateM " " [
      "CREATE INDEX " <> quoted sqlIndexName
    , "ON " <> quoted (schema, sqlIndexTable) <> sqlIndexOn schema
    ]
createSqlIndex SqlUniqueIndex{..} schema = intercalateM " " [
      "CREATE UNIQUE INDEX " <> quoted sqlIndexName
    , "ON " <> quoted (schema, sqlIndexTable) <> sqlIndexOn schema
    ]
createSqlIndex SqlTrigramIndex{..} schema = intercalateM " " [
      "CREATE INDEX " <> quoted sqlIndexName
    , "ON " <> quoted (schema, sqlIndexTable)
    , "USING GIST (" <> sqlIndexOn schema <> " GIST_TRGM_OPS)"
    ]
createSqlIndex SqlGinIndex{..} schema = intercalateM " " [
      "CREATE INDEX " <> quoted sqlIndexName
    , "ON " <> quoted (schema, sqlIndexTable)
    , "USING GIN (" <> sqlIndexOn schema <> ")"
    ]

dropSqlIndex :: Cascade -> SqlIndex -> QueryS
dropSqlIndex cascade idx schema = intercalateM " " [
      "DROP INDEX IF EXISTS"
    , quoted (schema, sqlIndexName idx)
    , cascadeToQuery cascade
    ]
