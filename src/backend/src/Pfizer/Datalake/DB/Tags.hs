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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.DB.Tags (
    TagIx
  , tagSource
  , untagSource
  ) where

import Pfizer.Datalake.DB.IfNotFound
import Pfizer.Datalake.DB.Orphans ()
import Pfizer.Datalake.DB.Schema
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | Tag a datasource
--
-- Returns 'True' if the data source was successfully tagged, or 'False'
-- if the tag was already present.
tagSource :: MonadIO m => SourceIx -> TagName -> Transaction m Bool
tagSource sourceIx tagName = do
    tagIx  <- getTagName (createIfNotFound ()) tagName
    hasTag <- checkTag sourceIx tagIx
    if hasTag
      then return False
      else do
        executeS
          (\schema -> intercalateM " " [
              "INSERT INTO " <> quoted (schema, tableTags) <> " (source, tag)"
            , "VALUES (?, ?)"
            ])
          (sourceIx, tagIx)
        return True

-- | Untag a datasource
--
-- Returns 'True' if the tag was successfully removed, or 'False' if the tag
-- was not present.
untagSource :: MonadIO m => SourceIx -> TagName -> Transaction m Bool
untagSource sourceIx tagName = do
    mTagIx <- getTagName emptyIfNotFound tagName
    case mTagIx of
      Nothing    -> return False -- Tag itself was deleted
      Just tagIx -> do
        hasTag <- checkTag sourceIx tagIx
        if not hasTag
          then return False
          else do
            executeS
              (\schema -> intercalateM " " [
                   "DELETE FROM " <> quoted (schema, tableTags)
                 , "WHERE source = ? AND tag = ?"
                 ])
              (sourceIx, tagIx)
            return True

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Check if a datasource has been tagged with a particular tag
checkTag :: MonadIO m => SourceIx -> TagIx -> Transaction m Bool
checkTag sourceIx tagIx = do
    [Only count] <- queryS (\schema -> intercalateM " " [
                               "SELECT COUNT(*)"
                             , "FROM " <> quoted (schema, tableTags)
                             , "WHERE source = ? AND tag = ?"
                             ])
                           (sourceIx, tagIx)
    return $ count == (1 :: Int)

{-------------------------------------------------------------------------------
  Tag names
-------------------------------------------------------------------------------}

newtype TagIx = TagIx Ix
  deriving (Show, FromField, ToField)

-- | Get ID of the specified tagname
getTagName :: MonadIO m
           => IfNotFound () (Transaction m) TagIx a
           -> TagName -> Transaction m a
getTagName ifNotFound name = do
    rows <- queryS (\schema -> intercalateM " " [
                      "SELECT ix"
                    , "FROM " <> quoted (schema, tableTagNames)
                    , "WHERE name = ?"
                    ])
                  (Only name)
    ifNotFound (rowsToMaybe rows) $ \() -> newTagName name

-- | Create a new tag name
--
-- NOTE: This is never called explicitly by the user, but is only called by
-- 'getTagName'. So if the source name already exists (violating the UNIQUE
-- constraint), this indicates a bug.
newTagName :: MonadIO m => TagName -> Transaction m TagIx
newTagName tagName = do
    [Only ix] <- queryS
      (\schema -> intercalateM " " [
          "INSERT INTO " <> quoted (schema, tableTagNames) <> "(name)"
        , "VALUES (?)"
        , "RETURNING ix"
        ])
      (Only tagName)
    return ix
