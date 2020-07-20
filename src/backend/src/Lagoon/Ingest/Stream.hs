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
-- | The core streaming code
module Lagoon.Ingest.Stream (
    SourceIngestConfig(..)
  , streamToDb
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Database.PostgreSQL.Simple
import Data.Conduit hiding (Source)
import Data.Int
import Data.IORef
import Data.Time
import Network.URI

import Lagoon.DB
import Lagoon.Ingest.DataFormat
import Lagoon.Ingest.Progress
import Lagoon.Interface
import Lagoon.Util.PostgreSQL

data SourceIngestConfig u = SourceIngestConfig {
    sourceCreateIndices :: CreateIndices
  , sourceSource :: Source
  , sourceIngestOptions :: IngestOptions
  , sourceCreateOptions :: CreateOptions
  , sourceInput :: Input u
  }

-- | Stream data to a new table
--
-- Returns
--
-- * Version of the data source
-- * Primary key of the entry in the @sources@ table
-- * Number of ingested rows
-- * The data format (possibly updated during ingest)
--
-- <https://www.postgresql.org/docs/current/static/populate.html> offers
-- some suggestions on how to improve the performance of ingest:
--
-- * Use the COPY protocol
-- * Create the table and do the ingest in a single transaction
-- * Create any indices _after_ the data has been ingested
--   (although the manual does not specify this explicitly, declaring a
--   primary key also creates an index so we do this as a separate step).
--
-- There are some other tweaks listed there as well that we have not yet
-- implemented.
streamToDb :: forall a t. CopyTuple a
           => Connection
           -> Schema
           -> Logger IO IngestProgress
           -> IORef (Maybe t)
           -> User
           -> Maybe URI -- The origin URI, if any
           -> CreateOptions
           -> CreateIndices
           -> Source
           -> DataFormat a t
           -> Sink a (ResourceT IO) SourceInfo
streamToDb conn
           schema
           logger
           typeRef
           User{..}
           mURI
           CreateOptions{description, tags, createTypedTable, createdTime}
           createIndices
           source@Source{..}
         = \format -> do
    let description' :: Description
        description' = case description of
                         Nothing   -> sourceName
                         Just desc -> desc

    runTransactionC conn schema $ do
      --
      -- 1. Setup
      --

      (now, sourceIx, version, tableName, viewName) <- lift $ do
        -- Create the table
        now <- case createdTime of
                 CreatedTimeFixed t -> return t
                 CreatedTimeNow     -> liftIO $ Timestamp <$> getCurrentTime
        (sourceIx, version, tableName, viewName) <-
          newSource userIx source mURI schema now description'
        createTable (schema, tableName) format

        return (now, sourceIx, version, tableName, viewName)

      --
      -- 2. Stream data into the table
      --

      -- The number of rows inserted might not match the maximum ID
      -- (if some rows failed to insert)
      (numRows, format') <- bracketLogC logger IngestCopyIn $
        stream (schema, tableName) format

      --
      -- 3. Finalize
      --

      lift $ do
        -- Store column information
        maxIdLen  <- getMaxIdLen
        mInferred <- liftIO $ readIORef typeRef
        let columnSpec = columnSpecForRow maxIdLen format' mInferred
        writeColumnSpec sourceIx columnSpec

        -- Add primary key and other indices after ingest (for performance)
        createIndicesFor logger createIndices (schema, tableName) columnSpec

        -- Create view with user-readable names
        createView schema tableName viewName columnSpec

        -- Create the tags
        -- We ignore the success value
        -- (which is 'False' if the tag was already present)
        forM_ tags $ \tag -> tagSource sourceIx tag

        -- Construct full source info structure (right now, without typed table)
        let sourceInfo = SourceInfo {
                sourceIx         = sourceIx
              , sourceVersionOf  = sourceName
              , sourceDescr      = description'
              , sourceTags       = tags
              , sourceURL        = show <$> mURI
              , sourceVersion    = version
              , sourceDeprecated = False
              , sourceCreated    = now
              , sourceAddedBy    = userName
              , sourceSchema     = schema
              , sourceTableName  = CompactName (Right tableName)
              , sourceViewName   = viewName
              , sourceTyped      = Nothing
              , sourceColumns    = columnSpec
              , sourceNumRows    = Just numRows
              }

        -- Create typed table (if requested)
        sourceInfo' <-
          if createTypedTable
            then makeTyped logger createIndices sourceInfo
            else return sourceInfo

        return sourceInfo'
  where
    -- | Stream using the PostgreSQL COPY protocol
    stream :: (Schema, TableName)
           -> DataFormat a t
           -> Sink a (Transaction (ResourceT IO)) (Int64, DataFormat a t)
    stream qTable = go 0
      where
        go :: Int64
           -> DataFormat a t
           -> Sink a (Transaction (ResourceT IO)) (Int64, DataFormat a t)
        go !n format = do
          (numRows, more) <- copyFrom qTable (formatNumCols format + 1)
          case more of
            NoMoreRows -> return (n + numRows, format)
            StillMoreRows numCols' -> do
              -- Note that the number of columns reported by PostgreSQL
              -- is one larger than the number of columns in the data model
              -- (which doesn't include the Ix column)
              format' <- lift $ addColumns qTable format (numCols' - 1)
              go (n + numRows) format'
