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
module Pfizer.Datalake.Util.PostgreSQL.Schema.SqlType (
    SqlType(..)
  ) where

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util
import Pfizer.Datalake.Util.PostgreSQL.Quoted
import Pfizer.Datalake.Util.PostgreSQL.Schema.SqlEntity
import Pfizer.Datalake.Util.PostgreSQL.Transaction

-- | SQL type
data SqlType = SqlType {
    sqlTypeName   :: TypeName
  , createSqlType :: QueryS
  }

instance SqlEntity SqlType where
  createEntity = createSqlType
  dropEntity   = dropSqlType

instance Quoted (Schema, SqlType) where
  quoted (schema, SqlType{..}) = quoted (schema, sqlTypeName)

dropSqlType :: Cascade -> SqlType -> QueryS
dropSqlType cascade typ schema = intercalateM " " [
      "DROP TYPE IF EXISTS "
    , quoted (schema, typ)
    , cascadeToQuery cascade
    ]
