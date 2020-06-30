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
{-# OPTIONS_GHC -fno-full-laziness #-}
-- | Elementary support for COPY TO (COPY OUT)
module Pfizer.Datalake.Util.PostgreSQL.CopyTo (
    HasJsonVersionPrefix(..)
  , copyToCsv
  , copyToJsonArray
  , copyToMultipleJson
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Conduit
import Data.Int
import qualified Data.ByteString         as BS.S
import qualified Data.ByteString.Builder as Bld

import Pfizer.Datalake.Util.PostgreSQL.Transaction

-- | Does the JSON data have a version prefix?
--
-- JSON data streamed directly from a table has a version prefix
-- <http://stackoverflow.com/questions/35600070/postgres-jsonb-specification-for-copy-in-binary-format>
-- However, that prefix is not present for data converted to JSON on the fly.
--
-- Consider:
--
-- > CREATE TABLE table1(col JSONB);
-- > CREATE TABLE table2(col TEXT);
-- > INSERT INTO table1 VALUES('"Test string"');
-- > INSERT INTO table2 VALUES('Test string');
-- > COPY table1(col) TO '/tmp/table1.json' (FORMAT BINARY);
-- > COPY (SELECT TO_JSON(col) FROM table2) TO '/tmp/table2.json' (FORMAT BINARY);
--
-- Then the string in @table1.json@ will be prefixed with @01@ (version prefix)
-- but the string in @table2.json@ will not. Similarly, if we wrap JSON data
-- already present into new JSON data computed on the fly
--
-- > COPY (
-- >   WITH result AS (SELECT col FROM table1)
-- >   SELECT ROW_TO_JSON(result) FROM result
-- > ) TO '/tmp/table1b.json' (FORMAT BINARY);
--
-- then the version prefix is also not present.
data HasJsonVersionPrefix =
    NoJsonVersionPrefix
  | WithJsonVersionPrefix

-- | Yield all CSV data resulting from a @COPY TO@
--
-- The database connection should first be put into COPY OUT mode using
--
-- > COPY ... TO STDOUT (FORMAT CSV)
--
-- or
--
-- > COPY ... TO STDOUT (FORMAT CSV, HEADER)
--
-- See <https://www.postgresql.org/docs/9.4/static/sql-copy.html>
--
-- Returns the number of rows yielded.
--
-- Implementation note: for CSV data Postgres does all the work for us; we just
-- need to stream the result to the client.
copyToCsv :: MonadIO m => ConduitM i (Flush Builder) (Transaction m) Int64
copyToCsv = do
  res <- lift $ getCopyData
  case res of
    CopyOutRow bs -> do
      yieldB $ Bld.byteString bs
      copyToCsv
    CopyOutDone numRows ->
      return numRows

-- | Yield all JSON data resulting from a @COPY TO@
--
-- The database connection should first be put into COPY OUT mode using
--
-- > COPY ... TO STDOUT (FORMAT BINARY)
--
-- The use of BINARY is critical, as PostgreSQL will otherwise insert some
-- incorrect escape characters into the JSON output.
--
-- See <https://www.postgresql.org/docs/9.4/static/sql-copy.html>
--
-- Implementation note: for JSON data we have to do a bit more work than for
-- CSV. Since we are using BINARY output, Postgres will return the JSON values
-- to us, but they are prefixed with some header information that we need to
-- strip off, and we need to insert the outer array structural elements
-- (brackets and commas).
copyToJsonArray :: forall m i. MonadIO m
                => HasJsonVersionPrefix
                -> ConduitM i (Flush Builder) (Transaction m) Int64
copyToJsonArray hasJsonVersionPrefix = do
    yieldB "["
    withFirstBinaryRow hasJsonVersionPrefix yieldAll
    yieldB "]"
    CopyOutDone numRows <- lift $ getCopyData
    return numRows
  where
    yieldAll :: ByteString -> Producer (Transaction m) (Flush Builder)
    yieldAll val = do
      yieldB $ Bld.byteString val
      withNextBinaryRow hasJsonVersionPrefix $ \val' -> do
        yieldB ","
        yieldAll val'

-- | Like 'copyToJsonArray', but using multiple top-level JSON values
-- separated by newlines.
--
-- Like PostgreSQL does for CSV, we use Unix style line endings.
-- See <https://www.postgresql.org/docs/9.4/static/sql-copy.html>
copyToMultipleJson :: forall m i. MonadIO m
                   => HasJsonVersionPrefix
                   -> ConduitM i (Flush Builder) (Transaction m) Int64
copyToMultipleJson hasJsonVersionPrefix = do
    withFirstBinaryRow hasJsonVersionPrefix yieldAll
    CopyOutDone numRows <- lift $ getCopyData
    return numRows
  where
    yieldAll :: ByteString -> Producer (Transaction m) (Flush Builder)
    yieldAll val = do
      yieldB $ Bld.byteString val
      withNextBinaryRow hasJsonVersionPrefix $ \val' -> do
        yieldB $ Bld.charUtf8 '\n'
        yieldAll val'

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

yieldB :: Monad m => Builder -> Producer m (Flush Builder)
yieldB = yield . Chunk

-- | Get first row, stripping PostgreSQL binary header and row header
--
-- Intended for use with rows containing single values only.
withFirstBinaryRow :: MonadIO m
                   => HasJsonVersionPrefix
                   -> (ByteString -> Conduit i (Transaction m) o)
                   -> Conduit i (Transaction m) o
withFirstBinaryRow hasJsonVersionPrefix k = do
    CopyOutRow firstRow <- lift $ getCopyData
    let firstRowWithoutHeader = BS.S.drop pgCopyBinaryHeaderLen firstRow
    case stripRowPrefix hasJsonVersionPrefix firstRowWithoutHeader of
      Nothing  -> return ()
      Just val -> k val

-- | Get next row (any row except the first), stripping binary row header
--
-- Intended for rows containing single values only.
withNextBinaryRow :: MonadIO m
                  => HasJsonVersionPrefix
                  -> (ByteString -> Conduit i (Transaction m) o)
                  -> Conduit i (Transaction m) o
withNextBinaryRow hasJsonVersionPrefix k = do
    CopyOutRow nextRow <- lift $ getCopyData
    case stripRowPrefix hasJsonVersionPrefix nextRow of
      Nothing  -> return ()
      Just val -> k val

-- | Strip row prefix from a row in binary format
--
-- Intended for use with rows containing a single value only.
--
-- Implementation note: in the binary format, normal rows first have a 16-bit
-- tuple count (which will be equal to 1), and each field will be preceded by a
-- 32-bit length. The last output will be @0xFFFF@.
stripRowPrefix :: HasJsonVersionPrefix -> ByteString -> Maybe ByteString
stripRowPrefix hasJsonVersionPrefix bs = do
    guard (BS.S.length bs >= 6)
    return $ case hasJsonVersionPrefix of
               WithJsonVersionPrefix -> BS.S.drop 7 bs
               NoJsonVersionPrefix   -> BS.S.drop 6 bs

-- | The length of the header Postgres prepends to binary data.
--
-- Note: the actual length is variable. However the total length can be read
-- from the first 19 bytes. This is omitted for now as the total length is
-- rarely different than 19.
--
-- See <https://www.postgresql.org/docs/9.4/static/sql-copy.html>
pgCopyBinaryHeaderLen :: Int
pgCopyBinaryHeaderLen = 19
