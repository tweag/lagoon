{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pfizer.Datalake.Interface.SourceInfo (
    SourceInfo(..)
  , Sources(..)
  , SourcesSpec(..)
  , SourcesCount(..)
  , CompactName(..)
  , isJsonSource
  , iriFromSourceInfo
  , tableNameFromCompact
    -- ** Convenience re-exports
  , def
  ) where

import Data.Aeson
import Data.Bifunctor
import Data.Default
import Data.List (intersperse)
import Data.Int
import GHC.Generics (Generic)
import Text.Show.Pretty
import Web.HttpApiData
import qualified Text.PrettyPrint as PP

import Pfizer.Datalake.Interface.ColumnSpec
import Pfizer.Datalake.Interface.DB
import Pfizer.Datalake.Interface.Pretty
import Pfizer.Datalake.Interface.Schema
import Pfizer.Datalake.Interface.Source
import Pfizer.Datalake.Interface.TsQuery
import Pfizer.Datalake.Interface.Users

{-------------------------------------------------------------------------------
  Source info
-------------------------------------------------------------------------------}

data SourceInfo = SourceInfo {
      sourceIx         :: SourceIx
    , sourceVersionOf  :: SourceName -- TODO: This should really be a Source
    , sourceDescr      :: Description
    , sourceTags       :: [TagName]
    , sourceURL        :: Maybe String
    , sourceVersion    :: Version
    , sourceDeprecated :: Bool
    , sourceCreated    :: Timestamp
    , sourceAddedBy    :: UserName
    , sourceSchema     :: Schema
    , sourceTableName  :: CompactName
    , sourceViewName   :: ViewName
    , sourceTyped      :: Maybe (CompactName, ViewName)
    -- TODO: compact typed?
    , sourceColumns    :: ColumnSpec

      -- | Number of rows in the source
      --
      -- 'sourceNumRows' is only populated in two cases:
      --
      -- 1. When we construct the 'SourceInfo' for a freshly ingested
      --    source, when we have this info anyway
      -- 2. In 'dumpDbInfo', used for unit testing.

      -- In general getting the number of rows for a particular table is an
      -- expensive computation (if the table is large), so we omit this info say
      -- in 'getSources'.
    , sourceNumRows :: Maybe Int64
    }

data CompactName = CompactName (Either ViewName TableName)
  deriving (Show)

isCompact :: CompactName -> Bool
isCompact = \case
  CompactName (Left _viewName)   -> True
  CompactName (Right _tableName) -> False

compactName :: CompactName -> String
compactName = \case
  CompactName (Left (ViewName vn))   -> vn
  CompactName (Right (TableName tn)) -> tn

tableNameFromCompact :: CompactName -> TableName
tableNameFromCompact = \case
  CompactName (Right tn) -> tn
  CompactName (Left (ViewName vn)) -> TableName vn

newtype Sources = Sources [SourceInfo]

newtype SourcesCount = SourcesCount Int
  deriving (Show, FromHttpApiData, ToHttpApiData)

-- | Gives the IRI that will be used to identify this SourceInfo
-- in the reasoner
iriFromSourceInfo :: SourceInfo -> String
iriFromSourceInfo SourceInfo{..} =
    show i ++ "/" ++ n ++ "/" ++ show v
  where
    n          = sourceVersionOf
    Version v  = sourceVersion
    Ix      i  = sourceIx

-- | Check if the given source is a JSON source
--
-- If it is, return the column with the JSON data
isJsonSource :: SourceInfo -> Maybe ColumnName
isJsonSource SourceInfo{sourceColumns = ColumnSpec cols} =
    case cols of
      [Column name _header (ColJSON _) _nameInView] -> Just name
      _otherwise                                    -> Nothing

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Pretty SourceInfo where
  pretty SourceInfo{..} = PP.vcat [
        pretty sourceVersionOf PP.<+> PP.parens ("version" PP.<+> pretty sourceVersion)
      , PP.nest 2 $ PP.vcat [
            "URL        " PP.<+> maybe "(local)" pretty sourceURL
          , "description" PP.<+> pretty sourceDescr
          , "tags       " PP.<+> ppTags sourceTags
          , "created    " PP.<+> pretty sourceCreated
          , "added by   " PP.<+> pretty sourceAddedBy
          , "deprecated " PP.<+> pretty sourceDeprecated
          , "schema     " PP.<+> pretty sourceSchema
          , "table      " PP.<+> ppTableView (sourceTableName, sourceViewName)
          , "typed      " PP.<+> maybe "(not available)" ppTableView sourceTyped
          , case sourceNumRows of
              Nothing -> mempty
              Just n  -> "row count  " PP.<+> PP.integer (fromIntegral n)
          , PP.hang "columns" 2 $ pretty sourceColumns
          ]
      ]
    where
      ppTableView :: (CompactName, ViewName) -> PP.Doc
      ppTableView (table, view) = PP.sep [
          ppCompactName table
        , PP.parens $ "with view" PP.<+> pretty view
        ]
      ppCompactName :: CompactName -> PP.Doc
      ppCompactName = \case
        CompactName (Left vname)  -> pretty vname PP.<+> "(compacted)"
        CompactName (Right tname) -> pretty tname

      ppTags :: [TagName] -> PP.Doc
      ppTags [] = "(no tags)"
      ppTags ts = PP.sep . PP.punctuate PP.comma . map PP.text $ ts

instance Pretty Sources where
  pretty (Sources sources) = PP.vcat
                           . intersperse " "
                           $ map pretty sources

{-------------------------------------------------------------------------------
  Specification of a subset of the sources
-------------------------------------------------------------------------------}

-- | Specification of which sources we're looking for
--
-- NOTE: Many of these fields are intentionally left as 'String' (rather than,
-- say, 'TagName', 'ColumnName'), etc. because they really /aren't/ 'TagName's
-- (or 'ColumnName's, ..). Instead they are search terms that /match/ against
-- such values.
data SourcesSpec = SourcesSpec {
      -- | Haskell-analogue of the SQL @OFFSET@ parameter
      sourcesOffset :: Maybe Int

      -- | Haskell-analogue of the SQL @LIMIT@ parameter
    , sourcesLimit :: Maybe Int

      -- | Natural language search
      --
      -- See 'TsQuery' and 'fnSourceFullText' for details
    , sourcesSearchQuery :: Maybe TsQuery

      -- | Filter by source (version) index
    , sourcesFilterIx :: Maybe SourceIx

      -- | Filter by (substring match on) tags
    , sourcesFilterTags :: [String]

      -- | Filter by (substring match on) description
    , sourcesFilterDescription :: Maybe String

      -- | Filter by (substring match on) source name
    , sourcesFilterName :: Maybe String

      -- | Filter by (substring match on) user name
      --
      -- NOTE: Unlike the other filters, specifying multiple users will
      -- return sources uploaded by /any/ of those users.
    , sourcesFilterUsers :: [String]

      -- | Filter by (substring match on) column
    , sourcesFilterColumns :: [String]

      -- | Only return sources created on or after the specified time
    , sourcesFilterCreatedAfter :: Maybe Timestamp

      -- | Only return sources created on or before the specified time
    , sourcesFilterCreatedBefore :: Maybe Timestamp

      -- | Columns to sort on
    , sourcesSortBy :: [(SourcesColumn, SortDirection)]

      -- | Should we include deprecated sources in the result?
    , sourcesIncludeDeprecated :: Bool

      -- | Only include sources readable by the specified user
    , sourcesReadableBy :: Maybe UserIx
    }
  deriving (Generic)

instance PrettyVal SourcesSpec where
  -- use Generic default

-- | The default instances for 'SourcesSpec' includes /all/ sources.
--
-- Some of the code relies on this (for instance, 'getAllSources' and
-- 'getAnySource', and by extension 'resetDb'. At the command line and in
-- the REST API we provide different defaults.
instance Default SourcesSpec where
    def = SourcesSpec {
        sourcesOffset              = Nothing
      , sourcesLimit               = Nothing
      , sourcesSearchQuery         = Nothing
      , sourcesFilterIx            = Nothing
      , sourcesFilterTags          = []
      , sourcesFilterDescription   = Nothing
      , sourcesFilterName          = Nothing
      , sourcesFilterUsers         = []
      , sourcesFilterColumns       = []
      , sourcesFilterCreatedAfter  = Nothing
      , sourcesFilterCreatedBefore = Nothing
      , sourcesSortBy              = []
      , sourcesIncludeDeprecated   = True
      , sourcesReadableBy          = Nothing
      }

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

instance ToJSON Sources where
  toJSON (Sources sources) = toJSON sources

instance ToJSON SourceInfo where
  toJSON SourceInfo{..} = object [
      "ix"             .= sourceIx
    , "name"           .= sourceVersionOf
    , "descr"          .= sourceDescr
    , "tags"           .= sourceTags
    , "URL"            .= sourceURL
    , "version"        .= sourceVersion
    , "deprecated"     .= sourceDeprecated
    , "created"        .= sourceCreated
    , "addedBy"        .= sourceAddedBy
    , "schema"         .= sourceSchema
    , "tableName"      .= compactName sourceTableName
    , "viewName"       .= sourceViewName
    , "typed"          .= ((first compactName) <$> sourceTyped)
    , "columns"        .= sourceColumns
    , "numRows"        .= sourceNumRows
    , "isCompact"      .= isCompact sourceTableName
    , "isTypedCompact" .= ((isCompact . fst) <$> sourceTyped)
    ]

instance FromJSON Sources where
  parseJSON v = Sources <$> (parseJSON v)

instance FromJSON SourceInfo where
  parseJSON (Object o) =
    SourceInfo <$> o .: "ix"
               <*> o .: "name"
               <*> o .: "descr"
               <*> o .: "tags"
               <*> o .: "URL"
               <*> o .: "version"
               <*> o .: "deprecated"
               <*> o .: "created"
               <*> o .: "addedBy"
               <*> o .: "schema"
               <*> (do
                      isComp <- o .: "isCompact"
                      if isComp then
                        (CompactName . Left . ViewName) <$> o .: "tableName"
                      else
                        (CompactName . Right . TableName) <$> o .: "tableName"
                    )
               <*> o .: "viewName"
               <*> (do
                      isComp <- o .:? "isTypedCompact"
                      case isComp of
                        Just (Just True) ->
                          (fmap $ first $ CompactName . Left . ViewName) <$> o .: "typed"
                        Just (Just False) ->
                          (fmap $ first $ CompactName . Right . TableName) <$> o .: "typed"
                        Just Nothing -> pure Nothing
                        Nothing ->
                          -- Assume this is an old version
                          (fmap $ first $ CompactName . Right . TableName) <$> o .: "typed"
                    )
               <*> o .: "columns"
               <*> o .: "numRows"
  parseJSON _ = fail "(SourceInfo.hs) SourceInfo: no parse"
