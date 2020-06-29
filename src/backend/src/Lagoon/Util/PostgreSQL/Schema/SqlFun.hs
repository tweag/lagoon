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
-- | PL/pgSQL Functions
module Lagoon.Util.PostgreSQL.Schema.SqlFun (
    SqlFun(..)
  , sqlFun
  , sqlTriggerFun
  ) where

import Data.Monoid
import Database.PostgreSQL.Simple (Query)

import Lagoon.Interface
import Lagoon.Util
import Lagoon.Util.PostgreSQL.Transaction
import Lagoon.Util.PostgreSQL.Quoted
import Lagoon.Util.PostgreSQL.Schema.SqlEntity

-- | PL/pgSQL function
data SqlFun = SqlFun{
      sqlFunName      :: FunctionName
    , sqlFunParams    :: Query
    , sqlFunReturns   :: Query
    , sqlFunBody      :: QueryS
    , sqlFunImmutable :: Bool
    }

instance SqlEntity SqlFun where
  createEntity = createSqlFun
  dropEntity   = dropSqlFun

instance Quoted (Schema, SqlFun) where
  quoted (schema, SqlFun{..}) = quoted (schema, sqlFunName)

-- | Default smart constructor
--
-- This leaves a bunch of fields @undefined@.
sqlFun :: SqlFun
sqlFun = SqlFun {
      sqlFunName      = undefined
    , sqlFunParams    = undefined
    , sqlFunReturns   = undefined
    , sqlFunBody      = undefined
    , sqlFunImmutable = False
    }

-- | Function to be executed on a trigger
sqlTriggerFun :: SqlFun
sqlTriggerFun = sqlFun {
      sqlFunParams  = "()"
    , sqlFunReturns = "TRIGGER"
    }

createSqlFun :: SqlFun -> QueryS
createSqlFun fun@SqlFun{..} schema = intercalateM " " [
      "CREATE FUNCTION " <> quoted (schema, fun) <> sqlFunParams
    , "RETURNS " <> sqlFunReturns <> " AS $$"
    , "BEGIN"
    , sqlFunBody schema
    , "END"
    , "$$ LANGUAGE PLPGSQL" <> if sqlFunImmutable then " IMMUTABLE" else ""
    ]

dropSqlFun :: Cascade -> SqlFun -> QueryS
dropSqlFun cascade fun@SqlFun{..} schema = intercalateM " " [
      "DROP FUNCTION IF EXISTS "
    , quoted (schema, fun)
    , sqlFunParams
    , cascadeToQuery cascade
    ]
