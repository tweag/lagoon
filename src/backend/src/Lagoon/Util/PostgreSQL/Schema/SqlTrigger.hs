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
-- | PL/pgSQL: Triggers
module Lagoon.Util.PostgreSQL.Schema.SqlTrigger (
    SqlTrigger(..)
  , sqlTriggerName
  ) where

import Data.Monoid
import Database.PostgreSQL.Simple (Query)

import Lagoon.Interface
import Lagoon.Util
import Lagoon.Util.PostgreSQL.Transaction
import Lagoon.Util.PostgreSQL.Quoted
import Lagoon.Util.PostgreSQL.Schema.SqlFun
import Lagoon.Util.PostgreSQL.Schema.SqlEntity

-- | PL/pgSQL trigger
data SqlTrigger = SqlTrigger {
     sqlTriggerExec  :: SqlFun     -- ^ Function to execute
   , sqlTriggerEvent :: Query      -- ^ "BEFORE UPDATE", "AFTER INSERT", etc.
   , sqlTriggerTable :: TableName
   }

instance SqlEntity SqlTrigger where
  createEntity = createSqlTrigger
  dropEntity   = dropSqlTrigger

createSqlTrigger :: SqlTrigger -> QueryS
createSqlTrigger SqlTrigger{..} schema = intercalateM " " [
      "CREATE TRIGGER " <> quoted (sqlTriggerName sqlTriggerExec)
    , sqlTriggerEvent <> " ON " <> quoted (schema, sqlTriggerTable)
    , "  FOR EACH ROW" -- hardcoded for now
    , "  EXECUTE PROCEDURE " <> quoted (schema, sqlTriggerExec) <> "()"
    ]

dropSqlTrigger :: Cascade -> SqlTrigger -> QueryS
dropSqlTrigger cascade SqlTrigger{..} schema = intercalateM " " [
    "DROP TRIGGER IF EXISTS"
  , quoted (sqlTriggerName sqlTriggerExec)
  , "ON"
  , quoted (schema, sqlTriggerTable)
  , cascadeToQuery cascade
  ]

-- | Construct 'TriggerNames' from the 'FunctionName's they call
sqlTriggerName :: SqlFun -> TriggerName
sqlTriggerName SqlFun{sqlFunName = FunctionName nm} =
    TriggerName (nm ++ "_trigger")
