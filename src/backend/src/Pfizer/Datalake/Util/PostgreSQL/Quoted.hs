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
-- | Quote names
--
-- NOTE: 'TableName' and co intentionally do /not/ have a 'Quoted' instance; the
-- instance is only given for /qualified/ table names @(Schema, TableName)@.
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
module Pfizer.Datalake.Util.PostgreSQL.Quoted (
    Quoted(..)
  , quoteInSchema
  ) where

import Database.PostgreSQL.Simple
import Data.String

import Pfizer.Datalake.Interface

class Quoted a where
  quoted :: a -> Query

-- | Helper function for defining 'Quoted' instances
quoteInSchema :: Schema -> String -> Query
quoteInSchema (Schema schema) entity = fromString $
  show schema ++ "." ++ show entity

instance Quoted (Schema, TableName) where
  quoted (schema, TableName nm) = quoteInSchema schema nm

instance Quoted (Schema, ViewName) where
  quoted (schema, ViewName nm) = quoteInSchema schema nm

instance Quoted (Schema, TypeName) where
  quoted (schema, TypeName nm) = quoteInSchema schema nm

instance Quoted (Schema, FunctionName) where
  quoted (schema, FunctionName nm) = quoteInSchema schema nm

-- | Triggers are defined in the schema of the function they call, so there
-- is a 'Quoted' instance for @TriggerName@ rather than for
-- @(Schema, TriggerName)@.
instance Quoted TriggerName where
  quoted (TriggerName nm) = fromString $ show nm

instance Quoted ColumnName where
  quoted (ColumnName nm) = fromString $ show nm

-- | Indices do not need a schema when they are created
-- (because they inherit the schema from their underlying table)
instance Quoted IndexName where
  quoted (IndexName nm) = fromString $ show nm

-- | Indices need a schema when they are dropped
instance Quoted (Schema, IndexName) where
  quoted (schema, IndexName nm) = quoteInSchema schema nm

-- | Column names never need a schema
instance Quoted SourcesColumn where
  quoted SourcesIx          = "ix"
  quoted SourcesSourceName  = "sourcename"
  quoted SourcesUrl         = "url"
  quoted SourcesVersion     = "version"
  quoted SourcesCreated     = "created"
  quoted SourcesAddedBy     = "addedby"
  quoted SourcesSchema      = "schema"
  quoted SourcesTableName   = "tablename"
  quoted SourcesViewName    = "viewname"
  quoted SourcesDescription = "description"
