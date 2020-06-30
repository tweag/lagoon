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
-- | Download previously ingested files
module Pfizer.Datalake.Download (
    downloadSource
  , downloadSourceToHandle
  ) where

import Control.Monad
import Data.ByteString.Builder (Builder)
import Data.Conduit
import System.IO
import qualified Data.ByteString.Builder as Bld
import qualified Data.Text               as Text

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.Conduit
import Pfizer.Datalake.Util.PostgreSQL

-- | Download the specified source and stream it to a handle
--
-- See 'downloadSource' for details.
downloadSourceToHandle :: MonadIO m
                       => SourceInfo
                       -> Handle
                       -> Transaction m ()
downloadSourceToHandle src handle =
    downloadSource src $$ sinkBuilderHandle handle

-- | Download the specified source
--
-- Tabular data will be output in CSV format. JSON data will be output in
-- JSON format, possibly with multiple top-level JSON values if there is
-- more than one row in the table.
downloadSource :: MonadIO m
               => SourceInfo
               -> forall i . ConduitM i (Flush Builder) (Transaction m) ()
downloadSource src@SourceInfo{..} = do
    case isJsonSource src of
      Nothing -> do
        yieldColumnNames

        -- Here we issue two different copy commands depending on whether there
        -- are zero or at least columns and whether we're reading a view or an
        -- actual table. For the zero column case and the view case we use
        -- COPY (SELECT <columns> FROM <table>) TO ...
        -- (where <columns> is @NULL@ in the zero-column case) as opposed to
        -- COPY <table> (<columns>) TO ...
        -- when we are reading from a table that has at least one column.
        let stmt =
              case ([
                    quoted (columnName col)
                  | col <- columnSpecToList sourceColumns
                  ], sourceTableName) of
                ([],_) -> "(SELECT NULL)"
                (cs, CompactName (Right tableName)) ->
                  intercalateM " "
                    [ quoted (sourceSchema, tableName)
                    , "(" <> intercalateM "," cs <> ")"
                    ]
                (cs, CompactName (Left viewName)) ->
                  intercalateM " "
                    [ "("
                    , "SELECT", intercalateM "," cs
                    , "FROM", quoted (sourceSchema, viewName)
                    , ")"
                    ]


        -- We use 'COPY (SELECT ... FROM @table@) TO ...' as opposed to 'COPY
        -- @table@ (...) TO ...' as the latter works on views as well, which we
        -- may want to download in the case of compacted tables.
        -- TODO: ideally 'SourceInfo' would contain the info of whether we are
        -- dealing with a table or a view
        lift $ copy_ $ intercalateM " " [
            "COPY"
          , stmt
          , "TO STDOUT (FORMAT CSV)"
          ]
        void $ copyToCsv

      Just col -> do
        let stmt =
              case sourceTableName of
                (CompactName (Right tableName)) ->
                  intercalateM " "
                    [ quoted (sourceSchema, tableName)
                    , "(" <> quoted col <> ")"
                    ]
                (CompactName (Left viewName)) ->
                  intercalateM " "
                    [ "("
                    , "SELECT", quoted col
                    , "FROM", quoted (sourceSchema, viewName)
                    , ")"
                    ]
        lift $ copy_ $ intercalateM " " [
            "COPY " <> stmt
          , "TO STDOUT (FORMAT BINARY)"
          ]
        void $ copyToMultipleJson WithJsonVersionPrefix
  where
    yieldColumnNames :: Monad m => Producer m (Flush Builder)
    yieldColumnNames = do
        intercalateM (yieldB $ Bld.charUtf8 ',') [
            do yieldB $ Bld.charUtf8 '"'
               case columnHeader of
                 Just header -> yieldName (Text.unpack header)
                 Nothing     -> yieldName name
               yieldB $ Bld.charUtf8 '"'
          | Column{..}      <- columnSpecToList sourceColumns
          , ColumnName name <- [columnName]
          ]
        yieldB $ Bld.charUtf8 '\n'

    -- | Yield a column name, escaping any quote characters that might appear
    -- See CSV spec at <https://tools.ietf.org/html/rfc4180>
    yieldName :: Monad m => String -> Producer m (Flush Builder)
    yieldName []       = return ()
    yieldName ('"':cs) = do yieldB $ Bld.charUtf8 '"'
                            yieldB $ Bld.charUtf8 '"'
                            yieldName cs
    yieldName (c:cs)   = do yieldB $ Bld.charUtf8 c
                            yieldName cs

    yieldB :: Monad m => Builder -> Producer m (Flush Builder)
    yieldB = yield . Chunk
