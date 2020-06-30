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
{-# LANGUAGE ScopedTypeVariables #-}

module Pfizer.Datalake.Ingest.DataFormat (
    -- * Data format and inference
    DataFormat(..)
  , ForeignField
  , formatNumCols
  , inferTabularFormat
  , inferTabularFormat'
  , inferJsonFormat
  , inferExtraData
    -- * Database support
  , createTable
  , addColumns
  , columnSpecForRow
  ) where

import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Conduit
import Data.Either
import Data.Foldable
import Data.String
import Data.Text (Text)

import Pfizer.Datalake.DB
import Pfizer.Datalake.Ingest.Tabular.UntypedRecord
import Pfizer.Datalake.Ingest.TypeUniverse
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util
import Pfizer.Datalake.Util.Conduit
import Pfizer.Datalake.Util.PostgreSQL
import Pfizer.Datalake.Util.JSON

-- | Information about the data format
--
-- The type parameters correspond to the input type and the inferred type.
data DataFormat a t where
  -- | Input is a .csv or .tsv file
  FormatTabular :: {
      -- | Total number of columns (not including the Ix column)
      numColumns  :: TupleLen

      -- | The names of the columns (if known)
    , columnHeaders :: Maybe [Text]
    } -> DataFormat (Ix :- UntypedRecord) RowType

  -- | Input has columns references foreign rowos
  FormatForeign :: {
      -- | Name of the referenced source
      foreignSourceName :: TableName

      -- | Name of the referenced columns
    , foreignColumnName :: ColumnName

      -- | The names of the non-foreign, data columns
    , dataColumns :: [Text]
    } -> DataFormat (Ix :- ForeignField :- Text :- UntypedRecord) ()

  -- | Input is a JSON file
  FormatJSON :: DataFormat (Ix, FlatJSON) JsonType
  FormatCompacted :: DataFormat b t -> DataFormat (Arr :- b) t

deriving instance Show (DataFormat a t)

type ForeignField = Ix

formatNumCols :: DataFormat a t -> TupleLen
formatNumCols FormatTabular{..} = numColumns
formatNumCols FormatJSON        = 1
formatNumCols FormatForeign{..} = 2 + fromIntegral (length dataColumns)
formatNumCols (FormatCompacted df) = 1 + formatNumCols df -- array + the rest

-- | Infer data format for tabular input
inferTabularFormat
  :: MonadIO m
  => HasHeaders
  -> (DataFormat (Ix :- UntypedRecord) RowType -> Sink UntypedRecord m r)
  -> Sink UntypedRecord m r
inferTabularFormat HasHeaders k = do
    mr <- await
    case mr of
      Nothing -> inferTabularFormat (NoHeaders 0) k -- no headers after all
      Just (UntypedRecord fs) -> do
        k FormatTabular{
              numColumns    = fromIntegral $ length fs
            , columnHeaders = Just fs
            }
inferTabularFormat (NoHeaders numRows) k = do
    rs <- peekAt numRows
    k FormatTabular {
        numColumns    = maximum (0 : map recordLength rs)
      , columnHeaders = Nothing
      }

-- | Infer data format for tabular input
inferTabularFormat'
  :: MonadIO m
  => HasHeaders
  -> (i -> UntypedRecord)
  -> (DataFormat (Ix :- UntypedRecord) RowType -> Sink i m r)
  -> Sink i m r
inferTabularFormat' HasHeaders f k = do
    mr <- await
    case mr of
      Nothing -> inferTabularFormat' (NoHeaders 0) f k -- no headers after all
      Just (f -> UntypedRecord fs) -> do
        k FormatTabular{
              numColumns    = fromIntegral $ length fs
            , columnHeaders = Just fs
            }
inferTabularFormat' (NoHeaders numRows) f k = do
    rs <- fmap f <$> peekAt numRows
    k FormatTabular {
        numColumns    = maximum (0 : map recordLength rs)
      , columnHeaders = Nothing
      }

-- -- | Infer data format for tabular input
inferExtraData
  :: forall m r. MonadIO m
  => (Text -> Maybe Ix)
  -> TableName
  -> (DataFormat (Ix :- ForeignField :- Text :- UntypedRecord) ()
      -> Sink (ForeignField :- Text :- UntypedRecord)  m r)
  -> Sink UntypedRecord m r
inferExtraData lookupHeaderIx mtname k = do
    mr <- await
    case mr of
      Nothing -> do
        let format = FormatForeign {
                foreignSourceName = mtname
              , foreignColumnName = ColumnName "ix"
              , dataColumns = []
              }
        k' [] .| k format
      Just (UntypedRecord headers) -> do
        let dataOrIx = headerOrIx <$> headers
            format = FormatForeign {
                foreignSourceName = mtname
              , foreignColumnName = ColumnName "ix"
              , dataColumns = lefts dataOrIx
              }
        k' dataOrIx .| k format
  where
    headerOrIx :: Text -> Either Text Ix
    headerOrIx header = case lookupHeaderIx header of
      Nothing -> Left header
      Just ix -> Right ix

    k' :: [Either Text Ix]
        -- ^ Either a data column or the index of the referenced row
       -> Conduit UntypedRecord m (ForeignField :- Text :- UntypedRecord)
    k' columns = awaitForever $ \(UntypedRecord values) -> do
       let row = zipWith (\v c -> bimap (const v) (,v) c) values columns
       let (vals, cols) = partitionEithers row
       forM_ cols $ \(ix, v) -> do
         yield (ix :- v :- (UntypedRecord vals))

-- | Infer data format for JSON files
--
-- TODO: Right now this function does nothing of interest, except fix types.
-- In the future it could do things like attempt to automatically infer
-- a JsonPath to use.
inferJsonFormat
  :: (DataFormat (Ix, FlatJSON) JsonType -> Sink (Context, Token) m r)
  -> Sink (Context, Token) m r
inferJsonFormat k = k FormatJSON

{-------------------------------------------------------------------------------
  Database support
-------------------------------------------------------------------------------}

-- | Create table for ingested data source
--
-- NOTE: We intentionally do not add any constraints here or declare primary
-- keys; we add these constraints /after/ ingests completes (for improved
-- performance).
createTable :: (MonadIO m, ?loc :: CallStack)
            => (Schema, TableName) -> DataFormat a t -> Transaction m ()
createTable qTable format = do
    execute_ $ intercalateM " " [
        "CREATE TABLE " <> quoted qTable <> "("
      , intercalateM ", " (formatColumns format)
      , ")"
      ]

formatColumns :: DataFormat a t -> [Query]
formatColumns FormatTabular{..} =
    ("ix INTEGER":columns)
  where
    -- The columns in the table are named numerically to avoid any and all
    -- problems with derived names. We will separately construct a view with
    -- more user friendly names.
    columns :: [Query]
    columns = [
        fromString $ "c" ++ show n ++ " TEXT"
      | n <- [1 .. numColumns]
      ]
formatColumns FormatForeign{dataColumns} =
    ("ix INTEGER":columns)
  where
    -- The columns in the table are named numerically to avoid any and all
    -- problems with derived names. We will separately construct a view with
    -- more user friendly names.
    columns :: [Query]
    columns =
      ["c1 INTEGER" -- Foreign key
      ,"c2 TEXT"    -- Value
      ] <> [        -- Data columns
        fromString $ "c" ++ show (n+2) ++ " TEXT"
      | n <- [1 .. length dataColumns]
      ]

formatColumns FormatJSON =
    [ "ix  INTEGER"
    , "c1  JSONB"
    ]
formatColumns (FormatCompacted sub) =
    ("ixs int[]":(formatColumns sub))

-- | Add additional columns to a table
--
-- This happens only if our initial estimate of the number of columns is
-- incorrect:
--
-- * If the file contains headers, then this happens if the number of headers
--   is smaller than the number of fields in one of the rows.
-- * If the file does not contain headers, then this happens if we didn't look
--   at enough rows, and all the rows we did look at all contain fewer fields
--   than some row further down the file.
--
-- Note that in both cases we don't actually have any name for the column, so
-- we just give it a numeric name.
--
-- Returns the updated 'DataFormat'.
addColumns :: MonadIO m
           => (Schema, TableName)
           -> DataFormat a t -- ^ Original data format
           -> TupleLen       -- ^ New number of columns
           -> Transaction m (DataFormat a t)
addColumns qTable oldFormat@FormatTabular{..} newNumCols = do
    execute_ $ intercalateM " " [
        "ALTER TABLE"
      , quoted qTable
      , intercalateM ", " [
            fromString $ "ADD COLUMN c" ++ show n ++ " TEXT"
          | n <- [numColumns + 1 .. newNumCols]
          ]
      ]
    return oldFormat { numColumns = newNumCols }
addColumns qTable (FormatCompacted fmt) newNumCols =
    FormatCompacted <$> addColumns qTable fmt newNumCols
addColumns _ FormatForeign{} _ =
    error "impossible: number of columns for data tables is fixed"
addColumns _ FormatJSON _ =
    error "impossible: number of columns for JSON tables is fixed"

-- | Construct column specification for a row
--
-- This must be provided with the /updated/ 'DataFormat'; we assume that
-- we have at least a correct column count. The number of column headers in the
-- 'DataFormat' might not match the column count if we discovered additional
-- columns during ingest. Similarly, we may or may not have done type inference,
-- and even if we have, we might not have done type inference across the full
-- table, so we don't assume that even if we did infer a type, that the number
-- of types matches the column count.
columnSpecForRow :: MaxIdLen -> DataFormat a t -> Maybe t -> ColumnSpec
columnSpecForRow maxIdLen (FormatCompacted df) mType =
    let
      ColumnSpec cols = columnSpecForRow maxIdLen df mType -- TODO check this
    in ColumnSpec (Column
        { columnName  ="ixs"
        , columnHeader = Nothing
        , columnType = ColArr
        , columnInView = ColumnName "ixs"
        } : cols)
columnSpecForRow maxIdLen FormatTabular{..} mType =
      mkColumnSpec maxIdLen
    . take (fromIntegral numColumns)
    $ zipWith3 mkColumn columnNames headers types
  where
    columnNames :: [ColumnName]
    columnNames = [fromString ("c" ++ show (n :: Int)) | n <- [1..]]

    headers :: [Maybe Text]
    headers = case columnHeaders of
                Just hs -> map Just hs ++ repeat Nothing
                Nothing -> repeat Nothing

    types :: [ColumnType]
    types = case mType of
              Just (RowType fs) -> toList fs ++ repeat ColText
              _otherwise        -> repeat ColText

columnSpecForRow maxIdLen FormatForeign{..} _mType =
      mkColumnSpec maxIdLen
    $ zipWith3 mkColumn columnNames headers types
  where
    columnNames :: [ColumnName]
    columnNames = [fromString ("c" ++ show (n :: Int)) | n <- [1..]]

    headers :: [Maybe Text]
    headers = [Nothing, Just "value"] <> fmap Just dataColumns

    types :: [ColumnType]
    types = [ColForeign foreignSourceName foreignColumnName] <> repeat ColText

columnSpecForRow _maxIdLen FormatJSON mType = ColumnSpec [
      Column {
          columnName   = "c1"
        , columnHeader = Just "data"
        , columnType   = ColJSON mType
        , columnInView = "data"
        }
    ]
