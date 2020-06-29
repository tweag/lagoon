{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.DB.Typed (makeTyped, dropTyped) where

import Data.Monoid
import Data.String

import Pfizer.Datalake.DB.ColumnSpec
import Pfizer.Datalake.DB.Indices
import Pfizer.Datalake.DB.Schema
import Pfizer.Datalake.DB.Sources
import Pfizer.Datalake.Ingest.Progress
import Pfizer.Datalake.Ingest.TypeUniverse
import Pfizer.Datalake.Interface hiding (createTypedTable)
import Pfizer.Datalake.Util
import Pfizer.Datalake.Util.PostgreSQL

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
