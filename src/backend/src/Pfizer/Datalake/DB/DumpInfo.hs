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
