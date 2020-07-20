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
module Lagoon.DB.Typed (makeTyped, dropTyped) where

import Data.Monoid
import Data.String

import Lagoon.DB.ColumnSpec
import Lagoon.DB.Indices
import Lagoon.DB.Schema
import Lagoon.DB.Sources
import Lagoon.Ingest.Progress
import Lagoon.Ingest.TypeUniverse
import Lagoon.Interface hiding (createTypedTable)
import Lagoon.Util
import Lagoon.Util.PostgreSQL

-- | Create typed version of the specified source
makeTyped :: forall m. (MonadMask m, MonadIO m)
          => Logger IO IngestProgress
          -> CreateIndices
          -> SourceInfo
          -> Transaction m SourceInfo
makeTyped logger createIndices info@SourceInfo{..} = do
    bracketLog logger IngestTypedTable $ do
      dropTyped info

      execute_ $ createTypedTable     info typedTable
      execute_ $ convertForColumnSpec info typedTable
      createView sourceSchema typedTable typedView sourceColumns

      executeS (\schema -> intercalateM " " [
                   "INSERT INTO " <> quoted (schema, tableTypedSources) <> "(source, tableName, viewName)"
                 , "VALUES (?, ?, ?)"
                 ])
               (sourceIx, typedTable, typedView)

    createIndicesFor logger createIndices (sourceSchema, typedTable) sourceColumns

    return info { sourceTyped = Just (CompactName (Right typedTable), typedView) }
  where
    typedTable :: TableName
    typedTable = typedTableNameForIx sourceIx

    typedView :: ViewName
    typedView = typedViewName sourceViewName

dropTyped :: MonadIO m => SourceInfo -> Transaction m ()
dropTyped SourceInfo{..} =
    case sourceTyped of
      Nothing ->
        return ()
      Just (table, _view) -> do
        execute_ $ dropCompact table sourceSchema
        executeS (\schema -> intercalateM " " [
                     "DELETE FROM " <> quoted (schema, tableTypedSources)
                   , "WHERE source = ?"
                   ])
                 (Only sourceIx)

-- | Query to construct the typed table for a source
createTypedTable :: SourceInfo -> TableName -> Query
createTypedTable SourceInfo{sourceColumns = ColumnSpec cols, ..}
                 typedTableName
               = intercalateM " " [
      "CREATE TABLE " <> quoted (sourceSchema, typedTableName) <> "("
    , intercalateM ", " ("ix INTEGER":[
          quoted columnName <> " " <> fieldPostgresType columnType
        | Column{..} <- cols
        ])
    , ")"
    ]

-- | SQL query to transfer data from the untyped to the typed table
--
-- TODO: Custom conversion functions are not yet supported.
convertForColumnSpec :: SourceInfo -> TableName -> Query
convertForColumnSpec SourceInfo{sourceColumns = ColumnSpec spec, ..}
                     typedTableName
                   = intercalateM " " [
      "INSERT INTO " <> quoted (sourceSchema, typedTableName)
    , "(" <> (intercalateM ", " ("ix":targetCols)) <> ")"
    , "SELECT " <> (intercalateM ", " ("ix":sourceCols))
    , "FROM " <> quoted (sourceSchema, tableNameFromCompact sourceTableName)
    ]
  where
    sourceCols, targetCols :: [Query]
    sourceCols = [ quoted columnName <> " :: " <> fieldPostgresType columnType
                 | Column{..} <- spec
                 ]
    targetCols = [ quoted columnName
                 | Column{..} <- spec
                 ]

{-------------------------------------------------------------------------------
  Constructing names
-------------------------------------------------------------------------------}

-- | Generate table name
typedTableNameForIx :: Ix -> TableName
typedTableNameForIx (Ix ix) = fromString $ "typed" ++ show ix

-- | Derive name for the typed view
typedViewName :: ViewName -> ViewName
typedViewName (ViewName untypedView) = fromString $ untypedView ++ typedSuffix

{-------------------------------------------------------------------------------
  Aux
-------------------------------------------------------------------------------}

dropCompact :: CompactName -> Schema -> Query
dropCompact compactName schema = case compactName of
    CompactName (Right tableName) ->
        dropTable tableName schema
    CompactName (Left viewName) ->
        dropView viewName schema


dropTable :: TableName -> Schema -> Query
dropTable nm schema = "DROP TABLE IF EXISTS "
                   <> quoted (schema, nm)
                   <> " CASCADE"


dropView :: ViewName -> Schema -> Query
dropView nm schema = "DROP VIEW IF EXISTS "
                   <> quoted (schema, nm)
                   <> " CASCADE"
