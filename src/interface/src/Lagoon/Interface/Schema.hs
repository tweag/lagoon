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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Lagoon.Interface.Schema (
    -- * Reification
    SourcesColumn(..)
  , sourcesColumnFromString
  ) where

import GHC.Generics
import Text.Show.Pretty
import Web.HttpApiData

{-------------------------------------------------------------------------------
  Reifications
-------------------------------------------------------------------------------}

-- | A column in the 'sources' table
data SourcesColumn =
      SourcesIx
    | SourcesSourceName
    | SourcesUrl
    | SourcesVersion
    | SourcesCreated
    | SourcesAddedBy
    | SourcesSchema
    | SourcesTableName
    | SourcesViewName
    | SourcesDescription
  deriving (Show, Eq, Generic, PrettyVal)

sourcesColumnFromString :: String -> Maybe SourcesColumn
sourcesColumnFromString = go
  where
    go :: String -> Maybe SourcesColumn
    go "ix"          = Just SourcesIx
    go "sourcename"  = Just SourcesSourceName
    go "url"         = Just SourcesUrl
    go "version"     = Just SourcesVersion
    go "created"     = Just SourcesCreated
    go "addedby"     = Just SourcesAddedBy
    go "schema"      = Just SourcesSchema
    go "tablename"   = Just SourcesTableName
    go "viewname"    = Just SourcesViewName
    go "description" = Just SourcesDescription
    go _otherwise    = Nothing

instance ToHttpApiData SourcesColumn where
  toQueryParam SourcesIx          = "ix"
  toQueryParam SourcesSourceName  = "sourcename"
  toQueryParam SourcesUrl         = "url"
  toQueryParam SourcesVersion     = "version"
  toQueryParam SourcesCreated     = "created"
  toQueryParam SourcesAddedBy     = "addedby"
  toQueryParam SourcesSchema      = "schema"
  toQueryParam SourcesTableName   = "tablename"
  toQueryParam SourcesViewName    = "viewname"
  toQueryParam SourcesDescription = "description"

instance FromHttpApiData SourcesColumn where
  parseQueryParam "ix"          = Right SourcesIx
  parseQueryParam "sourcename"  = Right SourcesSourceName
  parseQueryParam "url"         = Right SourcesUrl
  parseQueryParam "version"     = Right SourcesVersion
  parseQueryParam "created"     = Right SourcesCreated
  parseQueryParam "addedby"     = Right SourcesAddedBy
  parseQueryParam "schema"      = Right SourcesSchema
  parseQueryParam "tablename"   = Right SourcesTableName
  parseQueryParam "viewname"    = Right SourcesViewName
  parseQueryParam "description" = Right SourcesDescription
  parseQueryParam _             = Left $ "Invalid SourcesColumn"
