{-# LANGUAGE OverloadedStrings #-}
-- | Some specific SQL queries
module Pfizer.Datalake.Util.PostgreSQL.Queries (
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

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util
import Pfizer.Datalake.Util.PostgreSQL.Transaction
import Pfizer.Datalake.Util.PostgreSQL.Quoted

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
