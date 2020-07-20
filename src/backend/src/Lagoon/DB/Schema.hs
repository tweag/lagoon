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
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
module Lagoon.DB.Schema (
    -- * Types
    typeDatasetAccessLevel
    -- * Functions
  , fnGetUserName
  , fnGetSourceName
  , fnGetTagName
  , fnImmutableArrayToString
  , fnSourceFullText
  , fulltextWeights
    -- * Trigger functions
  , fnInsertCachedAddedBy
  , fnInsertCachedSourceName
  , fnInsertCachedTag
  , fnDeleteCachedTag
  , fnInsertCachedColumn
  , fnDeleteCachedColumn
    -- * Core tables
  , tableDbMeta
  , tableUsers
  , tableSourceNames
  , tableSources
  , tableSourceColumns
  , tableTypedSources
  , tableTagNames
  , tableTags
    -- * Tables supporting the security model
  , tableGroups
  , tableMembership
  , tableDatasetUserAccess
  , tableDatasetGroupAccess
  , tableCanCreate
  , tableCanCreateGroup
  , tableCanManageGroup
  , tableCachedCanRead
  , indexCachedCanRead
   -- * Ontology related tables
  , tableTBoxes
  , tableABoxInstances
  , tableABoxGeneral
    -- * Trigger themselves
  , triggerInsertCachedAddedBy
  , triggerInsertCachedSourceName
  , triggerInsertCachedTag
  , triggerDeleteCachedTag
  , triggerInsertCachedColumn
  , triggerDeleteCachedColumn
    -- * Indices
  , indexSourceColumnsSourceColumnNameUnique
  , indexSourceColumnsSourceColumnInViewUnique
  , indexCreatedBTree
  , indexDescriptionTrigram
  , indexCachedSourceNameTrigram
  , indexCachedAddedByTrigram
  , indexCachedTagsTrigram
  , indexCachedColumnsTrigram
  , indexFullTextGin
  ) where

import Lagoon.Interface
import Lagoon.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Type creation commands
-------------------------------------------------------------------------------}

typeDatasetAccessLevel :: SqlType
typeDatasetAccessLevel = SqlType {
      sqlTypeName   = name
    , createSqlType = \schema -> intercalateM " " [
          "CREATE TYPE " <> quoted (schema, name)
        , "AS ENUM ('Read', 'Update', 'Manage')"
        ]
    }
  where
    name :: TypeName
    name = "datasetaccesslevel"

{-------------------------------------------------------------------------------
  Tables

  NOTE: We avoid capitals in table names to avoid forcing clients to quote them.
-------------------------------------------------------------------------------}

-- | Free form table containing some DB metadata
--
-- Primary goal of this is to support migration
tableDbMeta :: SqlTable
tableDbMeta = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "variable  TEXT    PRIMARY KEY"
            , "value     TEXT    NULL"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "dbmeta"

tableUsers :: SqlTable
tableUsers = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "ix    SERIAL  PRIMARY KEY"
            , "name  TEXT    NOT NULL UNIQUE"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "users"

tableSourceNames :: SqlTable
tableSourceNames = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "ix       SERIAL   PRIMARY KEY"
            , "name     TEXT     NOT NULL UNIQUE"
            , "addedby  INTEGER  NOT NULL " <> ref (schema, tableUsers)
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "sourcenames"

tableSources :: SqlTable
tableSources = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
            -- Core fields
              "ix          SERIAL                    PRIMARY KEY"
            , "sourcename  INTEGER                   NOT NULL " <> ref (schema, tableSourceNames)
            , "url         TEXT                      NULL" -- NULL for local
            , "version     INT                       NOT NULL"
            , "deprecated  BOOL                      NOT NULL"
            , "created     TIMESTAMP WITH TIME ZONE  NOT NULL"
            , "addedby     INTEGER                   NOT NULL " <> ref (schema, tableUsers)
            , "schema      TEXT                      NOT NULL"
            , "tablename   TEXT                      NOT NULL"
            , "viewname    TEXT                      NOT NULL"
            , "description TEXT                      NOT NULL"

            -- Cached fields
            --
            -- NOTE: All cached values are kept up to date by triggers.
            --
            -- We add these primarily to faciliate full text search, but we take
            -- advantage of them elsewhere as well.
            --
            -- Rather than denormalizing the database, we use these cached fields
            -- and use some triggers to keep them up to date. This keeps the core
            -- DB schema clean and makes it easier to see what is "extra" and what
            -- isn't. The idea is to only add cached fields with an "obvious"
            -- semantics (for some definition of obvious..).
            --
            -- The cached columns correspond to the user-friendly view.
            , "cached_sourcename TEXT   NOT NULL"
            , "cached_addedby    TEXT   NOT NULL"
            , "cached_tags       TEXT[] NOT NULL DEFAULT '{}'"
            , "cached_columns    TEXT[] NOT NULL DEFAULT '{}'"

            -- Constraints
            , "CONSTRAINT sources_source_version_unique UNIQUE(sourcename, version)"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "sources"

tableSourceColumns :: SqlTable
tableSourceColumns = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "ix            SERIAL  PRIMARY KEY"
            , "source        INT     NOT NULL " <> ref (schema, tableSources)
            , "columnname    TEXT    NOT NULL"
            , "columnheader  TEXT    NULL"     -- user provided name (if any)
            , "type          TEXT    NOT NULL"
            , "columninview  TEXT    NOT NULL" -- column name in the view
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "sourcecolumns"

tableTypedSources :: SqlTable
tableTypedSources = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
      "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "ix         SERIAL  PRIMARY KEY"
            , "source     INT     NOT NULL UNIQUE " <> ref (schema, tableSources)
            , "tablename  TEXT    NOT NULL"
            , "viewname   TEXT    NOT NULL"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "typedsources"

tableTagNames :: SqlTable
tableTagNames = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "ix    SERIAL  PRIMARY KEY"
            , "name  TEXT    NOT NULL UNIQUE"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "tagnames"

tableTags :: SqlTable
tableTags = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
      "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "source  INTEGER  NOT NULL " <> ref (schema, tableSources)
            , "tag     INTEGER  NOT NULL " <> ref (schema, tableTagNames)
              -- This constraint is necessary for performance too: it allows us
              -- to check efficienctly if a source has a particular tag.
            , "CONSTRAINT tags_source_tag_unique UNIQUE(source, tag)"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "tags"

-- | TBox table
--
-- This table is deprecated but is left untouched to avoid discrepancies in
-- migrations.
tableTBoxes :: SqlTable
tableTBoxes = SqlTable {
  sqlTableName = name
  , createSqlTable = \schema -> intercalateM " " [
      "CREATE TABLE " <> quoted (schema, name) <> " ("
      , intercalateM "," [
            "ix        SERIAL PRIMARY KEY"
          , "name      TEXT     NOT NULL UNIQUE"
          , "tboxcode  TEXT     NOT NULL"
          ]
      , ")"
      ]
  }
  where
    name :: TableName
    name = "facts_tboxes"

tableABoxGeneral :: SqlTable
tableABoxGeneral = SqlTable {
  sqlTableName = name
  , createSqlTable = \schema -> intercalateM " " [
      "CREATE TABLE " <> quoted (schema, name) <> " ("
      , intercalateM "," [
            "ix        SERIAL PRIMARY KEY"
          , "source    INTEGER"
          , "subject   TEXT NOT NULL"
          , "relation  TEXT NOT NULL"
          , "object    TEXT NOT NULL"
          , "infered   BOOLEAN NOT NULL DEFAULT FALSE"
          ]
      , ")"
      ]
  }
  where
    name :: TableName
    name = "facts_abox_general"

tableABoxInstances :: SqlTable
tableABoxInstances = SqlTable {
  sqlTableName = name
  , createSqlTable = \schema -> intercalateM " " [
      "CREATE TABLE " <> quoted (schema, name) <> " ("
      , intercalateM "," [
            "ix        SERIAL PRIMARY KEY"
          , "source    INTEGER"
          , "instance  TEXT NOT NULL"
          , "class     TEXT NOT NULL"
          , "infered   BOOLEAN NOT NULL DEFAULT FALSE"
          ]
      , ")"
      ]
  }
  where
    name :: TableName
    name = "facts_abox_instances"


{-------------------------------------------------------------------------------
  Tables that record the permissions
-------------------------------------------------------------------------------}

tableGroups :: SqlTable
tableGroups = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "ix   SERIAL  PRIMARY KEY"
            , "name TEXT    NOT NULL UNIQUE"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "groups"

tableMembership :: SqlTable
tableMembership = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "usr  INTEGER  NOT NULL " <> ref (schema, tableUsers)
            , "grp  INTEGER  NOT NULL " <> ref (schema, tableGroups)
            , "CONSTRAINT membership_usr_grp_unique UNIQUE(usr, grp)"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "membership"

tableDatasetUserAccess :: SqlTable
tableDatasetUserAccess = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "usr         INTEGER  NOT NULL " <> ref (schema, tableUsers)
            , "sourcename  INTEGER  NOT NULL " <> ref (schema, tableSourceNames)
            , "level " <> quoted (schema, typeDatasetAccessLevel) <> " NOT NULL"
            , "PRIMARY KEY (usr, sourcename)"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "datasetuseraccess"

tableDatasetGroupAccess :: SqlTable
tableDatasetGroupAccess = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "grp         INTEGER  NOT NULL " <> ref (schema, tableGroups)
            , "sourcename  INTEGER  NOT NULL " <> ref (schema, tableSourceNames)
            , "level " <> quoted (schema, typeDatasetAccessLevel) <> " NOT NULL"
            , "PRIMARY KEY (grp, sourcename)"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "datasetgroupaccess"

tableCanCreate :: SqlTable
tableCanCreate = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "usr  INTEGER  NOT NULL " <> ref (schema, tableUsers)
            , "CONSTRAINT cancreate_usr_unique UNIQUE(usr)"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "cancreate"

tableCanCreateGroup :: SqlTable
tableCanCreateGroup = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "usr  INTEGER  NOT NULL " <> ref (schema, tableUsers)
            , "CONSTRAINT cancreategroup_usr_unique UNIQUE(usr)"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "cancreategroup"

tableCanManageGroup :: SqlTable
tableCanManageGroup = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "usr  INTEGER  NOT NULL " <> ref (schema, tableUsers)
            , "grp  INTEGER  NOT NULL " <> ref (schema, tableGroups)
            , "CONSTRAINT canmanagegroup_usr_grp_unique UNIQUE(usr, grp)"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "canmanagegroup"

-- | "can read" cache
--
-- We cache a mapping from users to the set of tables they can read; this is
-- important to get reasonable search performance. We don't set up SQL triggers
-- to maintain the cache but instead manage the cache within the security layer.
-- The cache is not authoritative, but only there to support efficient dataset
-- search.
--
-- We store also the reason why the user has read access: NULL if the user
-- was given access directly, or a group id if the user was given access through
-- a group. This allows us to update the table when permissions get revoked.
tableCachedCanRead :: SqlTable
tableCachedCanRead = SqlTable {
      sqlTableName   = name
    , createSqlTable = \schema -> intercalateM " " [
          "CREATE TABLE " <> quoted (schema, name) <> " ("
        , intercalateM "," [
              "usr         INTEGER  NOT NULL " <> ref (schema, tableUsers)
            , "sourcename  INTEGER  NOT NULL " <> ref (schema, tableSourceNames)
            , "grp         INTEGER           " <> ref (schema, tableGroups)
            , "CONSTRAINT cachedcanread_usr_sourcename_grp_unique UNIQUE(usr, sourcename, grp)"
            ]
        , ")"
        ]
    }
  where
    name :: TableName
    name = "cachedcanread"

-- | We need an index so that we can efficiently check if a user has
-- read access to a particular dataset.
indexCachedCanRead :: SqlIndex
indexCachedCanRead = SqlBTreeIndex {
      sqlIndexName = "cachedcanread_usr_sourcename"
    , sqlIndexTable = sqlTableName tableCachedCanRead
    , sqlIndexOn    = \_schema -> "(usr, sourcename)"
    }

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

fnGetUserName :: SqlFun
fnGetUserName = sqlFun {
      sqlFunName    = "getUserName"
    , sqlFunParams  = "(userIx INTEGER)"
    , sqlFunReturns = "TEXT"
    , sqlFunBody    = \schema -> intercalateM " " [
           "RETURN (SELECT name FROM " <> quoted (schema, tableUsers)
         , "        WHERE ix = userIx);"
        ]
    }

fnGetSourceName :: SqlFun
fnGetSourceName = sqlFun {
      sqlFunName    = "getSourceName"
    , sqlFunParams  = "(sourceIx INTEGER)"
    , sqlFunReturns = "TEXT"
    , sqlFunBody    = \schema -> intercalateM " " [
           "RETURN (SELECT name FROM " <> quoted (schema, tableSourceNames)
         , "        WHERE ix = sourceIx);"
        ]
    }

fnGetTagName :: SqlFun
fnGetTagName = sqlFun {
      sqlFunName    = "getTagName"
    , sqlFunParams  = "(tagIx INTEGER)"
    , sqlFunReturns = "TEXT"
    , sqlFunBody    = \schema -> intercalateM " " [
           "RETURN (SELECT name FROM " <> quoted (schema, tableTagNames)
         , "        WHERE ix = tagIx);"
        ]
    }

fnImmutableArrayToString :: SqlFun
fnImmutableArrayToString = sqlFun {
      sqlFunName      = "immutableArrayToString"
    , sqlFunParams    = "(arr TEXT[], sep TEXT)"
    , sqlFunReturns   = "TEXT"
    , sqlFunImmutable = True
    , sqlFunBody      = \_schema -> "RETURN array_to_string(arr, sep);"
    }

-- | Return the full-text tsvector for a source
--
-- NOTE: If we modify this, we should also modify 'lagoonWeights'.
fnSourceFullText :: SqlFun
fnSourceFullText = sqlFun {
      sqlFunName      = "sourceFullText"
    , sqlFunParams    = "(description TEXT, cached_sourcename TEXT, cached_addedby TEXT, cached_tags TEXT[], cached_columns TEXT[])"
    , sqlFunReturns   = "TSVECTOR"
    , sqlFunImmutable = True
    , sqlFunBody      = \_schema -> "RETURN " <> intercalateM " || " [
          "setweight(to_tsvector('english', description), 'C')"
        , "setweight(to_tsvector('english', cached_sourcename), 'A')"
        , "setweight(to_tsvector('english', cached_addedby), 'D')"
        , "setweight(to_tsvector('english', array_to_string(cached_tags, ' ')), 'B')"
        , "setweight(to_tsvector('english', array_to_string(cached_columns, ' ')), 'B')"
        ]
        <> ";"
    }

-- | The weights as we currently use them
--
-- NOTE: This is tightly coupled with 'fnSourceFullText'.
--
-- TODO: Right now both columns and tags are given weight B; this means we
--       cannot distinguish between the two in search queries.
fulltextWeights :: [(TsLabel, TsWeight)]
fulltextWeights = [
      ("description" , TsWeightC)
    , ("name"        , TsWeightA)
    , ("user"        , TsWeightD)
    , ("tag"         , TsWeightB)
    , ("column"      , TsWeightB)
    ]

{-------------------------------------------------------------------------------
  Indices
-------------------------------------------------------------------------------}

-- | UNIQUE constraint (and efficient look up) for @(source, columnname)@
--
-- We don't add this as a direct constraint because for some reason the @lower@
-- call is not supported there.
indexSourceColumnsSourceColumnNameUnique :: SqlIndex
indexSourceColumnsSourceColumnNameUnique = SqlUniqueIndex {
      sqlIndexName  = "sourceColumns_source_columnname_unique"
    , sqlIndexTable = sqlTableName tableSourceColumns
    , sqlIndexOn    = \_schema -> "(source, lower(columnname))"
    }

-- | UNIQUE constraint (and efficient look up) for @(source, columninview)@
--
-- We don't add this as a direct constraint because for some reason the @lower@
-- call is not supported there.
indexSourceColumnsSourceColumnInViewUnique :: SqlIndex
indexSourceColumnsSourceColumnInViewUnique = SqlUniqueIndex {
      sqlIndexName  = "sourceColumns_source_columninview_unique"
    , sqlIndexTable = sqlTableName tableSourceColumns
    , sqlIndexOn    = \_schema -> "(source, lower(columninview))"
    }

-- | BTree index to support efficient filtering on creation date
indexCreatedBTree :: SqlIndex
indexCreatedBTree = SqlBTreeIndex {
      sqlIndexName  = "sources_created_btree"
    , sqlIndexTable = sqlTableName tableSources
    , sqlIndexOn    = \_schema -> "(created)"
    }

-- | Trigram index for ILIKE '%..%' lookups on @description@
indexDescriptionTrigram :: SqlIndex
indexDescriptionTrigram = SqlTrigramIndex {
      sqlIndexName  = "sources_description_trigram"
    , sqlIndexTable = sqlTableName tableSources
    , sqlIndexOn    = \_schema -> "description"
    }

-- | Trigram index for ILIKE '%..%' lookups on @cached_sourcename@
indexCachedSourceNameTrigram :: SqlIndex
indexCachedSourceNameTrigram = SqlTrigramIndex {
      sqlIndexName  = "sources_cached_sourcename_trigram"
    , sqlIndexTable = sqlTableName tableSources
    , sqlIndexOn    = \_schema -> "cached_sourcename"
    }

-- | Trigram index for ILIKE '%..%' lookups on @cached_addedby@
indexCachedAddedByTrigram :: SqlIndex
indexCachedAddedByTrigram = SqlTrigramIndex {
      sqlIndexName  = "sources_cached_addedby_trigram"
    , sqlIndexTable = sqlTableName tableSources
    , sqlIndexOn    = \_schema -> "cached_addedby"
    }

-- | Trigram index for ILIKE '%..%' lookups on @cached_tags@
indexCachedTagsTrigram :: SqlIndex
indexCachedTagsTrigram = SqlTrigramIndex {
      sqlIndexName  = "sources_cached_tags_trigram"
    , sqlIndexTable = sqlTableName tableSources
    , sqlIndexOn    = \schema -> "("
                              <> quoted (schema, fnImmutableArrayToString)
                              <> "(cached_tags, ' ')"
                              <> ")"
    }

-- | Trigram index for ILIKE '%..%' lookups on @cached_columns@
indexCachedColumnsTrigram :: SqlIndex
indexCachedColumnsTrigram = SqlTrigramIndex {
      sqlIndexName  = "sources_cached_columns_trigram"
    , sqlIndexTable = sqlTableName tableSources
    , sqlIndexOn    = \schema -> "("
                              <> quoted (schema, fnImmutableArrayToString)
                              <> "(cached_columns, ' ')"
                              <> ")"
    }

-- | GIN index for full text search
--
-- TODO: This long argument is repeated in various places. Refactor.
indexFullTextGin :: SqlIndex
indexFullTextGin = SqlGinIndex {
      sqlIndexName  = "sources_fulltext_GIN"
    , sqlIndexTable = sqlTableName tableSources
    , sqlIndexOn    = \schema -> "("
                              <> quoted (schema, fnSourceFullText)
                              <> "(description, cached_sourcename, cached_addedby, cached_tags, cached_columns)"
                              <> ")"
    }

{-------------------------------------------------------------------------------
  Trigger functions for maintaining cached data
-------------------------------------------------------------------------------}

fnInsertCachedAddedBy :: SqlFun
fnInsertCachedAddedBy = sqlTriggerFun {
      sqlFunName = "insertCachedAddedBy"
    , sqlFunBody = \schema -> intercalateM " " [
          "NEW.cached_addedby = " <> quoted (schema, fnGetUserName) <> "(NEW.addedby);"
        , "RETURN NEW;"
        ]
    }

fnInsertCachedSourceName :: SqlFun
fnInsertCachedSourceName = sqlTriggerFun {
      sqlFunName = "insertCachedSourceName"
    , sqlFunBody = \schema -> intercalateM " " [
          "NEW.cached_sourcename = " <> quoted (schema, fnGetSourceName) <> "(NEW.sourcename);"
        , "RETURN NEW;"
        ]
    }

fnInsertCachedTag :: SqlFun
fnInsertCachedTag = sqlTriggerFun {
      sqlFunName = "insertCachedTag"
    , sqlFunBody = \schema -> intercalateM " " [
         "UPDATE " <> quoted (schema, tableSources)
       , "SET   cached_tags = array_append(cached_tags, " <> quoted (schema, fnGetTagName) <> "(NEW.tag))"
       , "WHERE ix = NEW.source;"
       , "RETURN NEW;"
       ]
    }

fnDeleteCachedTag :: SqlFun
fnDeleteCachedTag = sqlTriggerFun {
      sqlFunName = "deleteCachedTag"
    , sqlFunBody = \schema -> intercalateM " " [
         "UPDATE " <> quoted (schema, tableSources)
       , "SET   cached_tags = array_remove(cached_tags, " <> quoted (schema, fnGetTagName) <> "(OLD.tag))"
       , "WHERE ix = OLD.source;"
       , "RETURN OLD;"
       ]
    }

fnInsertCachedColumn :: SqlFun
fnInsertCachedColumn = sqlTriggerFun {
      sqlFunName = "insertCachedColumn"
    , sqlFunBody = \schema -> intercalateM " " [
         "UPDATE " <> quoted (schema, tableSources)
       , "SET   cached_columns = array_append(cached_columns, NEW.columninview)"
       , "WHERE ix = NEW.source;"
       , "RETURN NEW;"
       ]
    }

fnDeleteCachedColumn :: SqlFun
fnDeleteCachedColumn = sqlTriggerFun {
      sqlFunName = "deleteCachedColumn"
    , sqlFunBody = \schema -> intercalateM " " [
         "UPDATE " <> quoted (schema, tableSources)
       , "SET   cached_columns = array_remove(cached_columns, OLD.columninview)"
       , "WHERE ix = OLD.source;"
       , "RETURN OLD;"
       ]
    }

{-------------------------------------------------------------------------------
  The triggers proper

  NOTE: These triggers are not exhaustive and only update cached values after
  changes to the DB that we actually support. If we add more functionality,
  we might need to add further triggers, and if the DB is manually changed,
  the cached information might have to be refreshed.
-------------------------------------------------------------------------------}

triggerInsertCachedAddedBy :: SqlTrigger
triggerInsertCachedAddedBy = SqlTrigger {
      sqlTriggerExec  = fnInsertCachedAddedBy
    , sqlTriggerEvent = "BEFORE INSERT"
    , sqlTriggerTable = sqlTableName tableSources
    }

triggerInsertCachedSourceName :: SqlTrigger
triggerInsertCachedSourceName = SqlTrigger {
      sqlTriggerExec  = fnInsertCachedSourceName
    , sqlTriggerEvent = "BEFORE INSERT"
    , sqlTriggerTable = sqlTableName tableSources
    }

triggerInsertCachedTag :: SqlTrigger
triggerInsertCachedTag = SqlTrigger {
      sqlTriggerExec  = fnInsertCachedTag
    , sqlTriggerEvent = "AFTER INSERT"
    , sqlTriggerTable = sqlTableName tableTags
    }

triggerDeleteCachedTag :: SqlTrigger
triggerDeleteCachedTag = SqlTrigger {
      sqlTriggerExec  = fnDeleteCachedTag
    , sqlTriggerEvent = "AFTER DELETE"
    , sqlTriggerTable = sqlTableName tableTags
    }

triggerInsertCachedColumn :: SqlTrigger
triggerInsertCachedColumn = SqlTrigger {
      sqlTriggerExec  = fnInsertCachedColumn
    , sqlTriggerEvent = "AFTER INSERT"
    , sqlTriggerTable = sqlTableName tableSourceColumns
    }

triggerDeleteCachedColumn :: SqlTrigger
triggerDeleteCachedColumn = SqlTrigger {
      sqlTriggerExec  = fnDeleteCachedColumn
    , sqlTriggerEvent = "AFTER DELETE"
    , sqlTriggerTable = sqlTableName tableSourceColumns
    }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ref :: (Schema, SqlTable) -> Query
ref (schema, table) = "REFERENCES " <> quoted (schema, table) <> "(ix)"
