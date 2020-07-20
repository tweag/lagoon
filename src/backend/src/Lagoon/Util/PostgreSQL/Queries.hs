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
-- | Some specific SQL queries
module Lagoon.Util.PostgreSQL.Queries (
    -- * Specific queries
    addPrimaryKey
  , addForeignKey
  , addIndex
  , getNumRows
  ) where

import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Query, Only(..))
import Data.Int
import Data.Monoid
import Data.String

import Lagoon.Interface
import Lagoon.Util
import Lagoon.Util.PostgreSQL.Transaction
import Lagoon.Util.PostgreSQL.Quoted

{-------------------------------------------------------------------------------
  Specific SQL queries
-------------------------------------------------------------------------------}

-- | Specify the primary key for the table
--
-- See notes for 'streamToDb' why we do this as a separate step.
addPrimaryKey :: MonadIO m => (Schema, TableName) -> Transaction m ()
addPrimaryKey qTable@(_, tableName) = do
    execute_ $ intercalateM " " [
        "ALTER TABLE"
      , quoted qTable
      , "ADD CONSTRAINT"
      , primaryKeyConstraint tableName
      , "PRIMARY KEY(ix)"
      ]

-- Assuming the referenced column has 'UNIQUE' constraint
addForeignKey :: MonadIO m
              => (Schema, TableName)
              -> ForeignSpec
              -> Transaction m ()
addForeignKey qTable@(schema, tableName) ForeignSpec{..} =
    execute_ $ intercalateM " " [
        "ALTER TABLE"
      , quoted qTable
      , "ADD CONSTRAINT"
      , foreignKeyConstraint tableName
      , foreignKey pointingColumn
      , references schema referencedTable referencedColumn
      ]

-- | Create index for the specified column
--
-- The index will be called @<table>_<column>_index".
addIndex :: MonadIO m
         => (Schema, TableName)
         -> ColumnName
         -> Maybe Query      -- optional index type (e.g. "GIN")
         -> Transaction m ()
addIndex qTable@(_, tableName) columnName mIndexType = do
    execute_ $ intercalateM " " [
        "CREATE INDEX " <> indexForColumn tableName columnName
      , "ON " <> quoted qTable
      , case mIndexType of
          Just indexType -> "USING " <> indexType
          Nothing        -> mempty
      , "(" <> quoted columnName <> ")"
      ]

getNumRows :: MonadIO m => (Schema, TableName) -> Transaction m Int64
getNumRows qTable = do
    numRows <- query_ $ intercalateM " " [
        "SELECT COUNT(*)"
      , "FROM " <> quoted qTable
      ]
    case numRows of
      [Only n]   -> return n
      _otherwise -> error "getNumRows: Unexpected response from DB"

{-------------------------------------------------------------------------------
  Auxiliary: constructing index and constraint names
-------------------------------------------------------------------------------}

-- | Construct index name
indexForColumn :: TableName -> ColumnName -> Query
indexForColumn (TableName table) (ColumnName column) = fromString $
    table ++ "_" ++ column ++ "_index"

foreignKey :: ColumnName -> Query
foreignKey (ColumnName column) = fromString $
    "FOREIGN KEY(" ++ column ++ ")"

references :: Schema -> TableName -> ColumnName -> Query
references schema tableName columnName =
    "REFERENCES " <> quoted (schema, tableName) <> " (" <> quoted columnName <> ")"

primaryKeyConstraint :: TableName -> Query
primaryKeyConstraint (TableName table) = fromString $
    table ++ "_pk"

foreignKeyConstraint :: TableName -> Query
foreignKeyConstraint (TableName table) = fromString $
    table ++ "_fk"
