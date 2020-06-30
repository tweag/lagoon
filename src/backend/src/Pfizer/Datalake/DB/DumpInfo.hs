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
module Pfizer.Datalake.DB.DumpInfo (dumpDbInfo) where

import Control.Monad

import Pfizer.Datalake.DB.SourceInfo
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL

-- | Dump DB info
--
-- This is a potentially expensive operation we populate the 'sourceNumRows'
-- field of every source.
--
-- This is only for unit testing; we compare the output of @dump-db-info@ with
-- some previously stored output. For this reason, we sort by name rather than
-- by created time.
dumpDbInfo :: Connection -> Schema -> IO Sources
dumpDbInfo conn schema = do
    Sources sources <- runTransaction conn schema $ getSources spec
    sources' <- forM sources $ \info@SourceInfo{..} -> do
      numRows <- runTransaction conn schema $
                   getNumRows (sourceSchema, tableNameFromCompact sourceTableName)
      return info { sourceNumRows = Just numRows }
    return $ Sources sources'
  where
    spec :: SourcesSpec
    spec = def { sourcesSortBy = [(SourcesSourceName, Ascending)] }
