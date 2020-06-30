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
module Pfizer.Datalake.DB.Sources (
    setDeprecated
  , newSource
  , getVersion
  , getVersions
  , getSourceOfVersion
  , getSourceOfTable
  , typedSuffix
  ) where

import Network.URI (URI)

import Pfizer.Datalake.DB.IfNotFound
import Pfizer.Datalake.DB.Orphans ()
import Pfizer.Datalake.DB.Schema
import Pfizer.Datalake.DB.SensibleNames
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Deprecation
-------------------------------------------------------------------------------}

-- | (Un)deprecate a source
setDeprecated :: MonadIO m => SourceIx -> Bool -> Transaction m ()
setDeprecated sourceIx deprecated = do
    executeS (\schema -> intercalateM " " [
                 "UPDATE " <> quoted (schema, tableSources)
               , "SET deprecated = ?"
               , "WHERE ix = ?"
               ])
             (deprecated, sourceIx)

{-------------------------------------------------------------------------------
  New sources
-------------------------------------------------------------------------------}

-- | Add an entry to the sources table
--
-- We don't add any entries to the sourceColumns table yet, because during
-- ingest we might discover further columns that we need; the process looks
-- like this:
--
-- 1. Allocate an entry in the sources table
-- 2. Do ingest, discovering which columns we need
-- 3. Add entries into the sourceColumns table
newSource :: MonadIO m
          => UserIx
          -> Source
          -> Maybe URI
          -> Schema
          -> Timestamp
          -> Description
          -> Transaction m (SourceIx, Version, TableName, ViewName)
newSource userIx Source{..} mURI sourceSchema timestamp description = do
    -- Get version number and SourceIx
    svrows <- queryS (\schema -> intercalateM " "
      [ "SELECT ix, version"
      , "FROM " <> quoted (schema, tableSources)
      , "WHERE sourceName = ?"
      , "ORDER BY version DESC"
      , "LIMIT 1"
      ])
      (Only sourceNameIx)
    let (msourceIx, version) = case svrows of
          [(sourceIx, n)] -> (Just sourceIx, Version $ n + 1)
          _               -> (Nothing      , Version 1      )

    -- Deprecated all previous versions
    mapM_ (`setDeprecated` True) msourceIx

    -- Get ID of the new row
    -- (We need to get this manually because we need it for the tablename)
    [Only ix] <- queryS_
      (\schema -> intercalateM " " [
        "SELECT nextval('" <> quoted (schema, sqlTableName tableSources <> "_ix_seq") <> "') :: INTEGER"
        ])

    -- Set the table name, based on the ID
    let tableName = tableNameForIx ix

    -- Allocate a unique view name
    let prefViewName = prefViewForSource sourceName version
    maxIdLen <- allowForTypedSuffix <$> getMaxIdLen
    viewName <- mkUniqueView maxIdLen prefViewName

    -- Add new entry into the @sources@ metadata table
    executeS
      (\schema -> intercalateM " " [
          "INSERT INTO " <> quoted (schema, tableSources) <> "("
        , intercalateM "," [
             "ix"
           , "sourceName"
           , "url"
           , "version"
           , "created"
           , "addedBy"
           , "schema"
           , "tableName"
           , "viewName"
           , "description"
           , "deprecated"
           ]
        , ")"
        , "VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ]) (
         ix
      :- sourceNameIx
      :- (show <$> mURI)
      :- version
      :- timestamp
      :- userIx
      :- sourceSchema
      :- tableName
      :- viewName
      :- description
      :- False
      :- ())

    return (ix, version, tableName, viewName)
  where
    -- Leave room to postfix "_typed" when we create the view for the typed
    -- table. If we didn't do that creating a guaranteed-to-be-unique name for
    -- the typed view would be hard and require further search.
    allowForTypedSuffix :: Int -> Int
    allowForTypedSuffix maxIdLen = maxIdLen - length typedSuffix

-- | The suffix we use for the view on the typed table
--
-- This is defined here rather than in "DB.Typed" to avoid module cycles.
typedSuffix :: String
typedSuffix = "_typed"

-- | Make sure the view name is unique across all views
mkUniqueView :: forall m. MonadIO m
             => MaxIdLen -> PreferredName -> Transaction m ViewName
mkUniqueView maxIdLen pref = go 0
  where
    go :: Int -> Transaction m ViewName
    go v = do
      let pref' = addVersion v pref
          nm    = fromString $ sanitize maxIdLen pref'
      unique <- isUniqueView nm
      if unique then return nm
                else go (v + 1)

-- | Check if a view name is unique (not already used)
--
-- NOTE: We do a check per attempt to find a unique view name, rather than
-- simply loading all view names in one statement, since the probability of
-- a view name clash is very small, and the number of views names already in
-- use might be large.
isUniqueView :: MonadIO m => ViewName -> Transaction m Bool
isUniqueView str = do
    [Only count] <- queryS (\schema -> intercalateM " " [
                               "SELECT COUNT(ix)"
                             , "FROM " <> quoted (schema, tableSources)
                             , "WHERE viewName = ?"
                             ])
                           (Only str)
    return $ count == (0 :: Int)

{-------------------------------------------------------------------------------
  Construct names
-------------------------------------------------------------------------------}

-- | Generate table name
tableNameForIx :: Ix -> TableName
tableNameForIx (Ix ix) = fromString $ "t" ++ show ix

-- | Preferred name of the view for a specific version of a source
prefViewForSource :: SourceName -> Version -> PreferredName
prefViewForSource src (Version v) = PrefName src ("_v" ++ show v)

{-------------------------------------------------------------------------------
  Get information about an existing source
-------------------------------------------------------------------------------}

-- | Get table ID for a particular version
--
-- This does not take an IfNotFound policy because if the user requests info
-- about a specific version of a source and that version does not exist, there
-- is not much we can do about it.
getVersion :: MonadIO m
           => SourceNameIx
           -> Maybe Version  -- ^ Latest if not specified
           -> Transaction m SourceIx
getVersion sourceName mVersion = do
    rows <-
      case mVersion of
        Just version ->
          queryS (\schema -> intercalateM " " [
                     "SELECT ix"
                   , "FROM " <> quoted (schema, tableSources)
                   , "WHERE sourceName = ? AND version = ?"
                   ])
                 (sourceName, version)
        Nothing ->
          queryS (\schema -> intercalateM " " [
                     "SELECT ix"
                   , "FROM " <> quoted (schema, tableSources)
                   , "WHERE sourceName = ?"
                   , "ORDER BY version DESC"
                   , "LIMIT 1"
                   ])
                 (Only sourceName)
    case rows of
      [Only vals] -> return vals
      _otherwise  -> liftIO $ throwIO $ NotFound (sourceName, mVersion)

-- | Get table ID for all versions of a source (i.e. sourcename). Throws an
-- exception if no versions exist.
--
-- This does not take an IfNotFound policy because if the user requests info
-- about a source that does not exist there is not much we can do about it.
getVersions :: MonadIO m
            => SourceNameIx
            -> Transaction m [SourceIx]
getVersions sourceName = do
    rows <- queryS (\schema -> intercalateM " " [
                     "SELECT ix"
                   , "FROM " <> quoted (schema, tableSources)
                   , "WHERE sourceName = ?"
                   , "ORDER BY version DESC"
                   ])
                 (Only sourceName)
    case rows of
      []  -> liftIO $ throwIO $ NotFound sourceName
      vals -> return $ fromOnly <$> vals

-- | Get the source name ID (data set ID) for a particular version
--
-- Can be regarded as the inverse of 'getVersion'
getSourceOfVersion :: MonadIO m => SourceIx -> Transaction m Source
getSourceOfVersion sourceIx = do
    rows <- queryS
      (\schema -> intercalateM " " [
          "SELECT sourcename, cached_sourcename"
        , "FROM " <> quoted (schema, tableSources)
        , "WHERE ix = ?"
        ])
      (Only sourceIx)
    case rows of
      [(sourceNameIx, sourceName)] -> return Source{..}
      _otherwise -> liftIO $ throwIO $ NotFound sourceIx

-- | Get the source name that corresponds to a particular database table
getSourceOfTable :: MonadIO m => String -> Transaction m (Maybe Source)
getSourceOfTable tableName = do
    untyped <- queryS
      (\schema -> intercalateM " " [
          "SELECT ix FROM " <> quoted (schema, tableSources)
        , "WHERE tablename = ? OR viewname = ?"
        ])
      (tableName, tableName)
    typed <- queryS
      (\schema -> intercalateM " " [
          "SELECT source FROM " <> quoted (schema, tableTypedSources)
        , "WHERE tablename = ? OR viewname = ?"
        ])
      (tableName, tableName)
    case (untyped ++ typed) of
      [Only sourceIx] -> Just <$> getSourceOfVersion sourceIx
      _otherwise      -> return Nothing
