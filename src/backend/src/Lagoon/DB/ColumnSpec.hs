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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lagoon.DB.ColumnSpec (
    -- * Column specificaiton
    mkColumnSpec
  , mkColumn
  , getColumnSpec
  , writeColumnSpec
    -- * Views
  , createView
  , createCompactView
    -- * Access individual columns
  , getColumn
  , getSourceWithColumn
  , setColumnType
  ) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text

import Lagoon.DB.IfNotFound
import Lagoon.DB.Orphans ()
import Lagoon.DB.Schema
import Lagoon.DB.SensibleNames
import Lagoon.Interface
import Lagoon.Util.PostgreSQL

-- | Get column specification for a specific source
--
-- TODO: We should probably do a sanity check that the list is non-empty.
getColumnSpec :: MonadIO m => SourceIx -> Transaction m ColumnSpec
getColumnSpec sourceIx = do
    ColumnSpec <$> queryS
      (\schema -> intercalateM " " [
          "SELECT columnName,  columnHeader, type, columnInView"
        , "FROM " <> quoted (schema, tableSourceColumns)
        , "WHERE source = ?"
        , "ORDER BY ix"
        ])
      (Only sourceIx)

-- | Write column specification to the database
writeColumnSpec :: MonadIO m => SourceIx -> ColumnSpec -> Transaction m ()
writeColumnSpec sourceIx (ColumnSpec spec) =
    forM_ spec $ \Column{..} ->
      executeS
        (\schema -> intercalateM " " [
            "INSERT INTO " <> quoted (schema, tableSourceColumns) <> "("
          , "  source, columnName, columnHeader, type, columnInView"
          , ")"
          , "VALUES(?, ?, ?, ?, ?)"
          ])
        (sourceIx, columnName, columnHeader, columnType, columnInView)

-- | Construct a 'ColumnSpec'
--
-- Duplicate columns are renamed.
mkColumnSpec :: MaxIdLen -> [Column_ PreferredName] -> ColumnSpec
mkColumnSpec maxIdLen =
      ColumnSpec
    . map (fmap mkColName)
    . noDupNames maxIdLen columnInView setName
  where
    mkColName :: PreferredName -> ColumnName
    mkColName = fromString . sanitize maxIdLen

    setName :: PreferredName -> Column_ PreferredName -> Column_ PreferredName
    setName nm col = col { columnInView = nm }

-- | Construct a 'Column'
mkColumn :: ColumnName -> Maybe Text -> ColumnType -> Column_ PreferredName
mkColumn columnName@(ColumnName name) columnHeader columnType = Column{..}
  where
    columnInView = PrefName {
        prefName   = case columnHeader of
                       Just n  -> Text.unpack n
                       Nothing -> name
      , prefSuffix = ""
      }

-- | Create view for the given table, using the user friendly names
--
-- NOTE: We quote the view name and its column names so that original
-- capitalization is preserved. This means that /if/ the view name of column
-- names contain capitals, then any DB clients /must/ also quote the view name
-- or column names or else PostgreSQL will not find the view or the column.
createView :: MonadIO m
           => Schema -> TableName -> ViewName -> ColumnSpec -> Transaction m ()
createView schema table view (ColumnSpec cols) =
    execute_ $ intercalateM " " [
        "CREATE VIEW " <> quoted (schema, view) <> " AS"
      , "SELECT"
      , intercalateM ", " ("ix AS ix":[
            quoted columnName <> " AS " <> quoted columnInView
          | Column{..} <- cols
          ])
      , "FROM " <> quoted (schema, table)
      ]

-- | Creates a view for a source using a compacted table, dropping the source's
-- original table (or view)
createCompactView :: MonadIO m
                  => Schema
                  -> TableName
                  -- ^ the compaction result
                  -> CompactName
                  -- ^ the name of the new view
                  -> ColumnSpec
                  -- ^ the columns of the new view
                  -> SourceIx
                  -- ^ The index in the "ixs" column
                  -> Transaction m ()
createCompactView schema table view (ColumnSpec cols) (Ix idx) = do
    execute_ $ dropCompact view schema
    execute (intercalateM " " [
        "CREATE VIEW " <> quoted (schema, newViewName) <> " AS"
      , "SELECT"
      , intercalateM ", " ("ix AS ix":[
            quoted columnName
          | Column{..} <- cols
          ])
      , "FROM " <> quoted (schema, table)
      , "WHERE ? <@ ixs"
      ])
      (Only (Arr [idx]))
  where
    newViewName =
     let TableName sn = tableNameFromCompact $ view
     in ViewName sn

{-------------------------------------------------------------------------------
  Individual columns
-------------------------------------------------------------------------------}

-- | Resolve a column name
getColumn :: MonadIO m => SourceIx -> ColumnName -> Transaction m ColumnIx
getColumn sourceIx columnName = do
    rows <- queryS (\schema -> intercalateM " " [
                       "SELECT ix"
                     , "FROM " <> quoted (schema, tableSourceColumns)
                     , "WHERE source = ? AND columnName = ?"
                     ])
                   (sourceIx, columnName)
    case rows of
      [Only ix]  -> return ix
      _otherwise -> liftIO $ throwIO $ NotFound (sourceIx, columnName)

-- | Which source does this column belong to? (Inverse to 'getColumn')
getSourceWithColumn :: MonadIO m => ColumnIx -> Transaction m SourceIx
getSourceWithColumn columnIx = do
    rows <- queryS (\schema -> intercalateM " " [
                       "SELECT source"
                     , "FROM " <> quoted (schema, tableSourceColumns)
                     , "WHERE ix = ?"
                     ])
                   (Only columnIx)
    case rows of
      [Only sourceIx] -> return sourceIx
      _otherwise      -> liftIO $ throwIO $ NotFound columnIx

-- | Override column type
--
-- TODO: We might want to do a sanity check that this updates exactly one row.
-- However, since the 'ColumnIx' will most likely first be obtained using
-- 'getColumnIx', which will throw an exception if the specified column could
-- not be found, it's not hugely important.
setColumnType :: MonadIO m => ColumnIx -> ColumnType -> Transaction m ()
setColumnType columnIx typ =
    executeS (\schema -> intercalateM " " [
                 "UPDATE " <> quoted (schema, tableSourceColumns)
               , "SET type = ?"
               , "WHERE ix = ?"
               ])
             (typ, columnIx)

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
