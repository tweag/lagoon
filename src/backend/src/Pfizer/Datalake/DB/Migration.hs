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
module Pfizer.Datalake.DB.Migration (
    -- * Check database version
    MigrationRequired(..)
  , checkDbVersion
    -- * Migration proper
  , MigrationFailure(..)
  , migrate
  ) where

import Control.Exception
import Control.Monad

import Pfizer.Datalake.DB.Meta
import Pfizer.Datalake.DB.Schema
import Pfizer.Datalake.DB.Security
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Check database version
-------------------------------------------------------------------------------}

-- | Make sure that the DB version is current
--
-- May throw a 'MigrationRequired' exception.
checkDbVersion :: (MonadIO m, ?loc :: CallStack) => Transaction m ()
checkDbVersion = do
    dbVersion <- getDbMeta
    unless (dbVersion == Just dbVersionCurrent) $
      liftIO $ throwIO $ MigrationRequired dbVersionCurrent dbVersion

data MigrationRequired = MigrationRequired {
      expectedVersion :: DbVersion
    , actualVersion   :: Maybe DbVersion
    }
  deriving (Show)

instance Exception MigrationRequired

instance FriendlyException MigrationRequired where
  displayFriendly MigrationRequired{..} =
    if Just expectedVersion < actualVersion then
      "This version of Datalake is too old to handle the database version."
    else unlines [
        "The database version is too old. It can be updated by executing:"
      , ""
      , "  datalake-server migrate"
      ]

{-------------------------------------------------------------------------------
  Run migration
-------------------------------------------------------------------------------}

-- | Run migration
--
-- May throw a 'MigrationFailure' exception
migrate :: MonadIO m => Transaction m ()
migrate = do
    mVersion <- getDbMeta
    case mVersion of
      Nothing      -> liftIO $ throwIO $ UnknownDbVersion
      Just version -> do
        migrateFrom version
        setDbMeta dbVersionCurrent

migrateFrom :: MonadIO m => DbVersion -> Transaction m ()
migrateFrom v
  | v == dbVersionCurrent  = return ()
  | v == dbVersion_944d04a = migrateFrom_944d04a >> migrateFrom (succ v)
  | v == dbVersion_0c931db = migrateFrom_0c931db >> migrateFrom (succ v)
  | v == dbVersion_54ed20c = migrateFrom_54ed20c >> migrateFrom (succ v)
  | v == dbVersion_4b98410 = migrateFrom_4b98410 >> migrateFrom (succ v)
  | v == dbVersion_1189a33 = migrateFrom_1189a33 >> migrateFrom (succ v)
  | v == dbVersion_2a1f612 = migrateFrom_2a1f612 >> migrateFrom (succ v)
  | v == dbVersion_28ec204 = migrateFrom_28ec204 >> migrateFrom (succ v)
  | v == dbVersion_480c65a = migrateFrom_480c65a >> migrateFrom (succ v)
  | v == dbVersion_ce2b9f1 = migrateFrom_ce2b9f1 >> migrateFrom (succ v)
  | otherwise              = liftIO $ throwIO $ UnrecognizedDbVersion v

data MigrationFailure =
    -- | Unknown database version
    --
    -- This should never happen and points to data corruption somehow.
    UnknownDbVersion

    -- | Unrecognized database version
    --
    -- This probably means this version of ingest is /older/ than the version
    -- of ingest that created the database.
  | UnrecognizedDbVersion DbVersion
  deriving (Show)

instance Exception MigrationFailure

instance FriendlyException MigrationFailure where
  displayFriendly UnknownDbVersion{} = unlines [
        "Not enough information to migrate."
      , "It seems the database has been corrupted."
      ]
  displayFriendly UnrecognizedDbVersion{} = unlines [
        "Your version of ingest is probably too old."
      ]

{-------------------------------------------------------------------------------
  Specific migrations
-------------------------------------------------------------------------------}

-- | Fix security model migration: restore the public group
--
-- We forgot to add existing users in the public group in 'migrateFrom_0c931db';
-- in this migration we fix this by recreating the public group. We can't be
-- sure if any users were intentially removed from the public group; we can only
-- hope that didn't happen.
migrateFrom_944d04a :: MonadIO m => Transaction m ()
migrateFrom_944d04a = do
    public <- getPublicGroup
    admin  <- getAdminUser

    -- Remove all members of the public group
    executeS (\schema -> intercalateM " " [
        "DELETE FROM " <> quoted (schema, tableMembership)
      , "WHERE grp = ?"
      ])
      (Only public)

    -- Re-add all non-admin users to the public group
    executeS (\schema -> intercalateM " " [
        "INSERT INTO " <> quoted (schema, tableMembership) <> "(usr, grp)"
      , "SELECT ix, ?"
      , "FROM " <> quoted (schema, tableUsers)
      , "WHERE ix != ?"
      ])
      (public, userIx admin)

-- | Add tables to support the security model
migrateFrom_0c931db :: MonadIO m => Transaction m ()
migrateFrom_0c931db = do
    -- We used to have a "visibility" column with values "Public", "Private",
    -- "Deprecated". The "Public"/Private" distinction is made obsolete by
    -- the security layer, but we still need to know whether sources are
    -- deprecated or not, so we added a new "deprecated" column. We add the
    -- column now, then at the end populate it with default values depending
    -- on the "visibility" column (as well as granting/revoking permissions),
    -- and then finally set the NON NULL constraint and delete the old
    -- "visibility" column.
    executeS_ (\schema -> intercalateM " " [
        "ALTER TABLE " <> quoted (schema, tableSources)
      , "ADD COLUMN deprecated BOOL"
      ])

    -- Add security tables
    executeS_ $ createEntity typeDatasetAccessLevel
    executeS_ $ createEntity tableGroups
    executeS_ $ createEntity tableMembership
    executeS_ $ createEntity tableDatasetUserAccess
    executeS_ $ createEntity tableDatasetGroupAccess
    executeS_ $ createEntity tableCanCreate
    executeS_ $ createEntity tableCanCreateGroup
    executeS_ $ createEntity tableCanManageGroup
    executeS_ $ createEntity tableCachedCanRead
    executeS_ $ createEntity indexCachedCanRead

    -- New DB metadata
    setDbMeta $ DefaultCanCreateSource    True
    setDbMeta $ DefaultCanCreateGroup     True
    setDbMeta $ DefaultSourcePublicAccess DatasetAccessLevelUpdate

    -- Give the existing users CREATE and CREATEGROUP privileges
    --
    -- We intentionally do this before we initialize the security model,
    -- so that we don't add any entries for users created by 'initSecurity'.
    executeS_ (\schema -> intercalateM " " [
        "INSERT INTO " <> quoted (schema, tableCanCreate) <> "(usr)"
      , "SELECT ix FROM " <> quoted (schema, tableUsers)
      ])
    executeS_ (\schema -> intercalateM " " [
        "INSERT INTO " <> quoted (schema, tableCanCreateGroup) <> "(usr)"
      , "SELECT ix FROM " <> quoted (schema, tableUsers)
      ])

    -- Standard security layer initialization
    InitSecurityData{..} <- initSecurity

    -- Set 'deprecated' based on old visibility flag
    executeS_ (\schema -> intercalateM " " [
        "UPDATE " <> quoted (schema, tableSources)
      , "SET deprecated = CASE WHEN visibility = 'Deprecated'"
      , "                      THEN true"
      , "                      ELSE false"
      , "                      END"
      ])
    executeS_ (\schema -> intercalateM " " [
        "ALTER TABLE " <> quoted (schema, tableSources)
      , "ALTER COLUMN deprecated SET NOT NULL"
      ])

    -- Grant the public group UPDATE access to all sources with public
    -- visibility.
    --
    -- NOTE: There is a slight mismatch between the old visibility column
    -- and the new security model. Previously it was possible to mark some
    -- versions of a datasource public and other versions of that same source
    -- private; we cannot express that in the new security model. We give the
    -- public group access to all data sources that have at least some public
    -- versions. This leaves one other edge case: if all versions are marked
    -- as deprecated, we also don't give the public group access (normally only
    -- all but the most recent version are marked as deprecated).
    executeS (\schema -> intercalateM " " [
        "INSERT INTO " <> quoted (schema, tableDatasetGroupAccess)
                       <> "(sourcename, grp, level)"
      , "SELECT DISTINCT(sourcename)" -- DISTINCT must be first
      , "     , ?"      -- vvvvvv Not sure why the type annotation is required
      , "     , ? :: "  <> quoted (schema, typeDatasetAccessLevel)
      , "FROM " <> quoted (schema, tableSources)
      , "WHERE visibility = 'Public'"
      ])
      (initSecurityPublic, DatasetHasAccessLevelUpdate)

    -- Give users MANAGE access to the sources they uploaded
    executeS (\schema -> intercalateM " " [
        "INSERT INTO " <> quoted (schema, tableDatasetUserAccess)
                       <> "(sourcename, usr, level)"
      , "SELECT DISTINCT(sourcename)"
      , "     , addedby"
      , "     , ? :: " <> quoted (schema, typeDatasetAccessLevel)
      , "FROM " <> quoted (schema, tableSources)
      ])
      (Only DatasetHasAccessLevelManage)

    -- Remove the now obsolete 'visibility' column and visibility types
    executeS_ (\schema -> intercalateM " " [
        "ALTER TABLE " <> quoted (schema, tableSources)
      , "DROP COLUMN visibility"
      ])
    executeS_ $ dropEntity Restrict typeVisibility

    -- And finally construct the can-read cache
    rebuildCanReadCache

-- | Introduce TBox and ABox tables
migrateFrom_54ed20c :: MonadIO m => Transaction m ()
migrateFrom_54ed20c = do
  executeS_ $ createEntity tableTBoxes
  executeS_ $ createEntity tableABoxInstances
  executeS_ $ createEntity tableABoxGeneral

-- | Introduce DB admin password
migrateFrom_4b98410 :: MonadIO m => Transaction m ()
migrateFrom_4b98410 = do
    setDbMeta =<< liftIO initDbAdminPass
    liftIO $ putStrLn $ "NOTICE: Administrator password set to default. Please change immediately."

-- | There is a bug in 'migrateFrom_2a1f612', which didn't add default values
-- for two columns. We fix this in this separate migration; this means the
-- general migration infrastructure will take care of updating the DB structure,
-- even if that DB structure hasn't _really_ changed from version 5 to 6.
migrateFrom_1189a33 :: MonadIO m => Transaction m ()
migrateFrom_1189a33 = do
    executeS_ $ \schema -> intercalateM " " [
        "ALTER TABLE " <> quoted (schema, tableSources)
      , intercalateM "," [
            "ALTER COLUMN cached_tags       SET DEFAULT '{}'"
          , "ALTER COLUMN cached_columns    SET DEFAULT '{}'"
          ]
      ]

-- | Functions and indices used for DB denormalization and full text search
migrateFrom_2a1f612 :: MonadIO m => Transaction m ()
migrateFrom_2a1f612 = do
    -- Add in cached fields, temporarily allowing for NULLs
    executeS_ $ \schema -> intercalateM " " [
        "ALTER TABLE " <> quoted (schema, tableSources)
      , intercalateM "," [
            "ADD COLUMN cached_sourcename TEXT"
          , "ADD COLUMN cached_addedby    TEXT"
          , "ADD COLUMN cached_tags       TEXT[]"
          , "ADD COLUMN cached_columns    TEXT[]"
          ]
      ]

    -- Functions
    executeS_ $ createEntity fnGetUserName
    executeS_ $ createEntity fnGetSourceName
    executeS_ $ createEntity fnGetTagName
    executeS_ $ createEntity fnImmutableArrayToString
    executeS_ $ createEntity fnSourceFullText

    -- Indices
    executeS_ $ createEntity indexCreatedBTree
    executeS_ $ createEntity indexDescriptionTrigram
    executeS_ $ createEntity indexCachedSourceNameTrigram
    executeS_ $ createEntity indexCachedAddedByTrigram
    executeS_ $ createEntity indexCachedTagsTrigram
    executeS_ $ createEntity indexCachedColumnsTrigram
    executeS_ $ createEntity indexFullTextGin

    -- Trigger functions
    executeS_ $ createEntity fnInsertCachedAddedBy
    executeS_ $ createEntity fnInsertCachedSourceName
    executeS_ $ createEntity fnInsertCachedTag
    executeS_ $ createEntity fnDeleteCachedTag
    executeS_ $ createEntity fnInsertCachedColumn
    executeS_ $ createEntity fnDeleteCachedColumn

    -- Trigger themselves
    executeS_ $ createEntity triggerInsertCachedAddedBy
    executeS_ $ createEntity triggerInsertCachedSourceName
    executeS_ $ createEntity triggerInsertCachedTag
    executeS_ $ createEntity triggerDeleteCachedTag
    executeS_ $ createEntity triggerInsertCachedColumn
    executeS_ $ createEntity triggerDeleteCachedColumn

    -- Initialize missing cached data
    executeS_ $ \schema -> intercalateM " " [
        "UPDATE " <> quoted (schema, tableSources) <> " source"
      , "SET " <> intercalateM "," [
            "cached_sourcename = (" <> intercalateM " " [
                 "SELECT sourceName.name"
               , "FROM " <> quoted (schema, tableSourceNames) <> " sourceName"
               , "WHERE sourceName.ix = source.sourcename"
               ] <> ")"
          , "cached_addedby = (" <> intercalateM " " [
                 "SELECT usr.name"
               , "FROM " <> quoted (schema, tableUsers) <> " usr"
               , "WHERE usr.ix = source.addedby"
               ] <> ")"
          , "cached_tags = (" <> intercalateM " " [
                 "SELECT coalesce(array_agg(tagName.name), '{}')"
               , "FROM " <> quoted (schema, tableTags)     <> " tag"
               , "JOIN " <> quoted (schema, tableTagNames) <> " tagName"
               , "ON    tag.tag    = tagName.ix"
               , "WHERE tag.source = source.ix"
               ] <> ")"
          , "cached_columns = (" <> intercalateM " " [
                 "SELECT coalesce(array_agg(col.columninview), '{}')"
               , "FROM " <> quoted (schema, tableSourceColumns) <> " col"
               , "WHERE col.source = source.ix"
               ] <> ")"
          ]
      ]

    -- Require cached fields to be non-null
    executeS_ $ \schema -> intercalateM " " [
        "ALTER TABLE " <> quoted (schema, tableSources)
      , intercalateM "," [
            "ALTER COLUMN cached_sourcename SET NOT NULL"
          , "ALTER COLUMN cached_addedby    SET NOT NULL"
          , "ALTER COLUMN cached_tags       SET NOT NULL"
          , "ALTER COLUMN cached_columns    SET NOT NULL"
          ]
      ]

-- | Add UNIQUE constraints to sourceColumns
migrateFrom_28ec204 :: MonadIO m => Transaction m ()
migrateFrom_28ec204 = do
    executeS_ $ createEntity indexSourceColumnsSourceColumnNameUnique
    executeS_ $ createEntity indexSourceColumnsSourceColumnInViewUnique

-- | Add Visibility type and visibility field to sources
migrateFrom_480c65a :: MonadIO m => Transaction m ()
migrateFrom_480c65a = do
    executeS_ $ createEntity typeVisibility
    executeS_ (\schema -> intercalateM " " [
        "ALTER TABLE " <> quoted (schema, tableSources)
      , "ADD COLUMN visibility " <> quoted (schema, typeVisibility)
      , "NOT NULL DEFAULT('Private')"
      ])

-- | Add description field and tags metadata table
migrateFrom_ce2b9f1 :: MonadIO m => Transaction m ()
migrateFrom_ce2b9f1 = do
    -- Add 'description' column and populate it with the source name
    executeS_ (\schema -> intercalateM " " [
        "ALTER TABLE " <> quoted (schema, tableSources)
      , "ADD COLUMN description TEXT NOT NULL DEFAULT('')"
      ])
    executeS_ (\schema -> intercalateM " " [
        "UPDATE " <> quoted (schema, tableSources) <> " AS sources"
      , "SET description = sourceNames.name"
      , "FROM " <> quoted (schema, tableSourceNames) <> " AS sourceNames"
      , "WHERE sourceNames.ix = sources.sourceName"
      ])
    executeS_ (\schema -> intercalateM " " [
        "ALTER TABLE " <> quoted (schema, tableSources)
      , "ALTER COLUMN description"
      , "DROP DEFAULT"
      ])

    -- Create tables for supporting tags
    executeS_ $ createEntity tableTagNames
    executeS_ $ createEntity tableTags

{-------------------------------------------------------------------------------
  Obsolete definitions, now only required to support migration

  NOTE: Since there is only a single installation of this software, we should
  at some point clean up some of the migration code as it is no longer required.
-------------------------------------------------------------------------------}

typeVisibility :: SqlType
typeVisibility = SqlType {
      sqlTypeName   = name
    , createSqlType = \schema -> intercalateM " " [
          "CREATE TYPE " <> quoted (schema, name)
        , "AS ENUM ('Private', 'Public', 'Deprecated')"
        ]
    }
  where
    name :: TypeName
    name = "visibility"
