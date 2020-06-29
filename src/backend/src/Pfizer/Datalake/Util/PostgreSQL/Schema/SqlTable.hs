{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Util.PostgreSQL.Schema.SqlTable (
    SqlTable(..)
  ) where

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util
import Pfizer.Datalake.Util.PostgreSQL.Quoted
import Pfizer.Datalake.Util.PostgreSQL.Schema.SqlEntity
import Pfizer.Datalake.Util.PostgreSQL.Transaction

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
