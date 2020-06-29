{-# LANGUAGE OverloadedStrings #-}
-- | PL/pgSQL: Triggers
module Pfizer.Datalake.Util.PostgreSQL.Schema.SqlTrigger (
    SqlTrigger(..)
  , sqlTriggerName
  ) where

import Data.Monoid
import Database.PostgreSQL.Simple (Query)

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util
import Pfizer.Datalake.Util.PostgreSQL.Transaction
import Pfizer.Datalake.Util.PostgreSQL.Quoted
import Pfizer.Datalake.Util.PostgreSQL.Schema.SqlFun
import Pfizer.Datalake.Util.PostgreSQL.Schema.SqlEntity

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
