-- | Table indices created for ingested data sources
--
-- NOTE: Right now this is a pretty simple module. This might however want to
-- get smarter about this in the future (especially for JSON data).
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.DB.Indices (createIndicesFor) where

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe (catMaybes)
import Pfizer.Datalake.Ingest.Progress
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL

toForeignSpec :: Column -> Maybe ForeignSpec
toForeignSpec Column{..} =
  case columnType of
    ColBool                -> Nothing
    ColInt _               -> Nothing
    ColReal                -> Nothing
    ColText                -> Nothing
    ColDocument            -> Nothing
    ColArr                 -> Nothing
    ColJSON _              -> Nothing
    ColCustom _            -> Nothing
    ColForeign tName cName -> Just ForeignSpec {
        pointingColumn = columnName
      , referencedTable = tName
      , referencedColumn = cName }

-- | Create indices for the specified table
--
-- TODO: Assuming same schema. Is that right?
createIndicesFor :: (MonadMask m, MonadIO m)
                 => Logger IO IngestProgress
                 -> CreateIndices
                 -> (Schema, TableName)
                 -> ColumnSpec
                 -> Transaction m ()
createIndicesFor logger createIndices qTable@(_schema,_) columnSpec =
  bracketLog logger (IngestIndices qTable) $ do
    bracketLog logger IngestPrimaryKey $
      addPrimaryKey qTable

    let foreignSpecs = catMaybes $ toForeignSpec <$> columnSpecToList columnSpec

    forM_ foreignSpecs $ \fk@ForeignSpec{..} ->
        bracketLog logger IngestForeignKey $
          addForeignKey qTable fk

    when createIndices $
      forM_ (columnSpecToList columnSpec) $ \Column{..} ->
        case columnType of
          ColDocument ->
            -- TODO: For now we don't create any indices for columns of
            -- 'Document' type. One could imagine creating full text indices
            -- for such columns.
            logNotice logger $ IngestIndexSkipped columnName
          ColJSON _ ->
            -- For JSON, we create GIN indices
            -- See Section "8.14.4 jsonb indexing" of "8.14 JSON Types"
            -- <https://www.postgresql.org/docs/9.4/static/datatype-json.html>
            bracketLog logger (IngestIndex columnName) $
              addIndex qTable columnName (Just "GIN")
          ColArr ->
            -- For arrays, we create GIN indices
            bracketLog logger (IngestIndex columnName) $
              addIndex qTable columnName (Just "GIN")
          _otherwise ->
            bracketLog logger (IngestIndex columnName) $
              addIndex qTable columnName Nothing
