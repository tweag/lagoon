{-# LANGUAGE OverloadedStrings #-}
-- | PL/pgSQL Functions
module Pfizer.Datalake.Util.PostgreSQL.Schema.SqlFun (
    SqlFun(..)
  , sqlFun
  , sqlTriggerFun
  ) where

import Data.Monoid
import Database.PostgreSQL.Simple (Query)

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util
import Pfizer.Datalake.Util.PostgreSQL.Transaction
import Pfizer.Datalake.Util.PostgreSQL.Quoted
import Pfizer.Datalake.Util.PostgreSQL.Schema.SqlEntity

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
