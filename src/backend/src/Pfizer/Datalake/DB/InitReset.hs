{-# LANGUAGE OverloadedStrings #-}
-- | Initialize and reset the database
module Pfizer.Datalake.DB.InitReset (
    initDb
  , resetDb
  , dropSourceTable
  , dropSourceTypedTable
  , dropSource
  , dropAllSources
  ) where

import Control.Monad

import Pfizer.Datalake.DB.Meta
import Pfizer.Datalake.DB.Schema
import Pfizer.Datalake.DB.Security (initSecurity)
import Pfizer.Datalake.DB.SourceInfo
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

initDb :: MonadIO m => DbAdminPass -> Transaction m ()
initDb pw = do
    createTables
    initDbMeta pw
    _initSecurityData <- initSecurity
    return ()

createTables :: MonadIO m => Transaction m ()
createTables = do
    -- Types
    executeS_ $ createEntity typeDatasetAccessLevel

    -- Core tables
    executeS_ $ createEntity tableDbMeta
    executeS_ $ createEntity tableUsers
    executeS_ $ createEntity tableSourceNames
    executeS_ $ createEntity tableSources
    executeS_ $ createEntity tableSourceColumns
    executeS_ $ createEntity tableTypedSources
    executeS_ $ createEntity tableTagNames
    executeS_ $ createEntity tableTags

    -- Security model
    executeS_ $ createEntity tableGroups
    executeS_ $ createEntity tableMembership
    executeS_ $ createEntity tableDatasetUserAccess
    executeS_ $ createEntity tableDatasetGroupAccess
    executeS_ $ createEntity tableCanCreate
    executeS_ $ createEntity tableCanCreateGroup
    executeS_ $ createEntity tableCanManageGroup
    executeS_ $ createEntity tableCachedCanRead
    executeS_ $ createEntity indexCachedCanRead

    -- Ontology
    executeS_ $ createEntity tableTBoxes
    executeS_ $ createEntity tableABoxInstances
    executeS_ $ createEntity tableABoxGeneral

    -- Functions
    executeS_ $ createEntity fnGetUserName
    executeS_ $ createEntity fnGetSourceName
    executeS_ $ createEntity fnGetTagName
    executeS_ $ createEntity fnImmutableArrayToString
    executeS_ $ createEntity fnSourceFullText

    -- Indices
    executeS_ $ createEntity indexSourceColumnsSourceColumnNameUnique
    executeS_ $ createEntity indexSourceColumnsSourceColumnInViewUnique
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

{-------------------------------------------------------------------------------
  Reset
-------------------------------------------------------------------------------}

-- | Reset the state of the database
--
-- We make sure that the database is never in an inconsistent state; first, we
-- use dropAllSources which deletes one source per transaction; then, in a
-- single final transaction, if we check if any source remains and if not, we
-- delete all tables and recreate them. If after 'dropAllSources' a source
-- /does/ remain, we repeat the process. In theory this could lead to livelock
-- but that would seem exceedingly unlikely.
resetDb :: Connection -> Schema -> IO ()
resetDb conn schema = do
    dropAllSources conn schema
    didReset <- runTransaction conn schema go
    unless didReset $ resetDb conn schema
  where
    go :: MonadIO m => Transaction m Bool
    go = do
      -- Check if any user created any other sources meanwhile
      mSource <- getAnySource
      case mSource of
        Just _src -> return False
        Nothing -> do
          -- We leave the admin password alone
          Just dbAdminPass <- getDbMeta

          -- Ontology
          executeS_ $ dropEntity Cascade tableTBoxes
          executeS_ $ dropEntity Cascade tableABoxInstances
          executeS_ $ dropEntity Cascade tableABoxGeneral

          -- Security model tables (and associated indices)
          executeS_ $ dropEntity Cascade tableGroups
          executeS_ $ dropEntity Cascade tableMembership
          executeS_ $ dropEntity Cascade tableDatasetUserAccess
          executeS_ $ dropEntity Cascade tableDatasetGroupAccess
          executeS_ $ dropEntity Cascade tableCanCreate
          executeS_ $ dropEntity Cascade tableCanCreateGroup
          executeS_ $ dropEntity Cascade tableCanManageGroup
          executeS_ $ dropEntity Cascade tableCachedCanRead

          -- Tables
          -- This also deletes any associated triggers and indices
          executeS_ $ dropEntity Cascade tableTags
          executeS_ $ dropEntity Cascade tableTagNames
          executeS_ $ dropEntity Cascade tableTypedSources
          executeS_ $ dropEntity Cascade tableSourceColumns
          executeS_ $ dropEntity Cascade tableSources
          executeS_ $ dropEntity Cascade tableSourceNames
          executeS_ $ dropEntity Cascade tableUsers
          executeS_ $ dropEntity Cascade tableDbMeta

          -- Functions
          executeS_ $ dropEntity Cascade fnGetUserName
          executeS_ $ dropEntity Cascade fnGetSourceName
          executeS_ $ dropEntity Cascade fnGetTagName
          executeS_ $ dropEntity Cascade fnImmutableArrayToString
          executeS_ $ dropEntity Cascade fnSourceFullText

          -- Trigger functions
          executeS_ $ dropEntity Cascade fnInsertCachedAddedBy
          executeS_ $ dropEntity Cascade fnInsertCachedSourceName
          executeS_ $ dropEntity Cascade fnInsertCachedTag
          executeS_ $ dropEntity Cascade fnDeleteCachedTag
          executeS_ $ dropEntity Cascade fnInsertCachedColumn
          executeS_ $ dropEntity Cascade fnDeleteCachedColumn

          executeS_ $ dropEntity Restrict typeDatasetAccessLevel
          -- Types

          initDb dbAdminPass
          return True

{-------------------------------------------------------------------------------
  Dropping sources
-------------------------------------------------------------------------------}

-- | Drop all sources
--
-- NOTE: This runs as a series of consecutive transactions, each of which will
-- delete a single source. This avoids the problems discussed in 'dropSource',
-- whilst making sure that other users never see an inconsistent database
-- state. However, it does mean that there is no guarantee that after this
-- completes all sources have indeed been dropped, since a concurrent user
-- might have created a new source in the meantime.
dropAllSources :: Connection -> Schema -> IO ()
dropAllSources conn schema = go
  where
    go :: IO ()
    go = do
      didDelete <- runTransaction conn schema $ do
        mSource <- getAnySource
        case mSource of
          Nothing  -> return False
          Just src -> dropSource src >> return True
      when didDelete go

-- | Cleanly drop a single source
--
-- This deletes the tables and removes the entries from the metadata.
--
-- NOTE: This this runs in a single transaction, it means the whole delete
-- is atomic. However, you should avoid deleting many sources in one
-- transaction, as PostgreSQL will require one lock per table; when deleting
-- lots of sources in the same tranasction, PostgreSQL will fail with an
-- error such as
--
-- > WARNING:  out of shared memory
-- > You might need to increase max_locks_per_transaction.
dropSource :: MonadIO m => SourceInfo -> Transaction m ()
dropSource SourceInfo{..} = do
    -- Delete metadata

    executeS (\schema -> intercalateM " " [
                 "DELETE FROM " <> quoted (schema, tableTags)
               , "WHERE source = ?"
               ])
             (Only sourceIx)
    executeS (\schema -> intercalateM " " [
                 "DELETE FROM " <> quoted (schema, tableTypedSources)
               , "WHERE source = ?"
               ])
             (Only sourceIx)
    executeS (\schema -> intercalateM " " [
                 "DELETE FROM " <> quoted (schema, tableSourceColumns)
               , "WHERE source = ?"
               ])
             (Only sourceIx)
    executeS (\schema -> intercalateM " " [
                 "DELETE FROM " <> quoted (schema, tableABoxInstances)
               , "WHERE source = ?"
               ])
             (Only sourceIx)
    executeS (\schema -> intercalateM " " [
                 "DELETE FROM " <> quoted (schema, tableABoxGeneral)
               , "WHERE source = ?"
               ])
             (Only sourceIx)
    executeS (\schema -> intercalateM " " [
                 "DELETE FROM " <> quoted (schema, tableSources)
               , "WHERE ix = ?"
               ])
             (Only sourceIx)

    -- Drop the tables proper
    execute_ $ dropCompact sourceTableName sourceSchema
    case sourceTyped of
      Nothing ->
        return ()
      Just (typedCompactName, _view) ->
        execute_ $ dropCompact typedCompactName sourceSchema

dropSourceTable :: MonadIO m => SourceInfo -> Transaction m ()
dropSourceTable SourceInfo{..} = execute_ $ dropCompact sourceTableName sourceSchema

dropSourceTypedTable :: MonadIO m => SourceInfo -> Transaction m ()
dropSourceTypedTable SourceInfo{..} = case sourceTyped of
    Just (compactName, _viewName) -> execute_ $ dropCompact compactName sourceSchema
    Nothing -> pure ()

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

dropCompact :: CompactName -> Schema -> Query
dropCompact compactName schema = case compactName of
    CompactName (Right tableName) ->
        dropTable tableName schema
    CompactName (Left viewName) ->
        dropView viewName schema


dropTable :: TableName -> Schema -> Query
dropTable nm schema = "DROP TABLE IF EXISTS "
                   <> quoted (schema, nm)
                   <> " CASCADE"


dropView :: ViewName -> Schema -> Query
dropView nm schema = "DROP VIEW IF EXISTS "
                   <> quoted (schema, nm)
                   <> " CASCADE"
