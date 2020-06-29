{-# LANGUAGE OverloadedStrings #-}
-- | Abstract over SQL entities (tables, functions, triggers, etc.)
module Pfizer.Datalake.Util.PostgreSQL.Schema.SqlEntity (
    SqlEntity(..)
  , Cascade(..)
  , cascadeToQuery
  ) where

import Database.PostgreSQL.Simple
import Pfizer.Datalake.Util.PostgreSQL.Transaction

data Cascade = Cascade | Restrict

cascadeToQuery :: Cascade -> Query
cascadeToQuery Cascade  = "CASCADE"
cascadeToQuery Restrict = "RESTRICT"

class SqlEntity e where
  createEntity :: e -> QueryS
  dropEntity   :: Cascade -> e -> QueryS
