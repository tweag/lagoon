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
