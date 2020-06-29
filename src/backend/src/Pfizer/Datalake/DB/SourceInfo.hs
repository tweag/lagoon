{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.DB.SourceInfo (
    -- * Search for sources
    getSources
  , getSourcesCount
  , getAllSources
  , getSourceInfo
  , getSourceInfo'
  , getAnySource
  ) where

import Control.Exception
import Data.Default
import Data.Maybe (listToMaybe)

import Pfizer.Datalake.DB.ColumnSpec
import Pfizer.Datalake.DB.IfNotFound
import Pfizer.Datalake.DB.Schema
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util
import Pfizer.Datalake.Util.PostgreSQL

-- | Information about a source without information about the columns
--
-- This is an internal type only
data PartialSourceInfo = PartialSourceInfo {
      psourceIx         :: SourceIx
    , psourceName       :: SourceName
    , psourceDescr      :: Description
    , psourceURL        :: Maybe String
    , psourceVersion    :: Version
    , psourceDeprecated :: Bool
    , psourceCreated    :: Timestamp
    , psourceAddedBy    :: UserName
    , psourceSchema     :: Schema
    , psourceTableName  :: String
    , psourceViewName   :: ViewName
    , psourceTags       :: PGArray TagName
    }

mkSourceInfo :: PartialSourceInfo
             -> CompactName
             -> Maybe (CompactName, ViewName)
             -> ColumnSpec
             -> SourceInfo
mkSourceInfo PartialSourceInfo{..}
             sourceTableName
             sourceTyped
             sourceColumns
           = SourceInfo {
      sourceIx         = psourceIx
    , sourceVersionOf  = psourceName
    , sourceDescr      = psourceDescr
    , sourceURL        = psourceURL
    , sourceVersion    = psourceVersion
    , sourceDeprecated = psourceDeprecated
    , sourceCreated    = psourceCreated
    , sourceAddedBy    = psourceAddedBy
    , sourceSchema     = psourceSchema
    , sourceViewName   = psourceViewName
    , sourceTags       = fromPGArray psourceTags
    , sourceNumRows    = Nothing
    , ..
    }

completePartialInfo :: MonadIO m => PartialSourceInfo -> Transaction m SourceInfo
completePartialInfo info@PartialSourceInfo{psourceIx, psourceTableName} =
    -- for view OR table:
    -- check whether the name exists in
    -- SELECT EXISTS (SELECT 1 FROM information_schema.views WHERE table_name = 'my_view');
    -- and if not, it's a table. In order to double check that the table does exist, run
    -- SELECT EXISTS (SELECT 1 FROM information_schema.views WHERE table_name = 'my_table');
    -- which should be "TRUE".
    mkSourceInfo info
      <$> getCompact    (TableName psourceTableName)
      <*> getTyped      psourceIx
      <*> getColumnSpec psourceIx

-- | Get the name of the typed table, if it exists
--
-- NOTE: If we change the DB so that we could have multiple typed tables,
-- this Maybe should change to a list.
getTyped :: MonadIO m => SourceIx -> Transaction m (Maybe (CompactName, ViewName))
getTyped sourceIx = do
    mSimpleName <- queryS
                      (\schema -> intercalateM " " [
                          "SELECT tableName, viewName"
                        , "FROM " <> quoted (schema, tableTypedSources)
                        , "WHERE source = ?"
                        ])
                      (Only sourceIx)
    case mSimpleName of
      (str, vname):_ -> do
        (Just . (,vname)) <$>  getCompact str
      [] -> pure Nothing

getCompact :: MonadIO m => TableName -> Transaction m CompactName
getCompact tableName = do
    isView <- tableIsView tableName
    case isView of
      Just viewName ->
        pure $ CompactName (Left viewName)
      Nothing ->
        pure $ CompactName (Right tableName)


tableIsView :: MonadIO m => TableName -> Transaction m (Maybe ViewName)
tableIsView (TableName str) = do
    res <- queryS (\(Schema schema) -> intercalateM " " [
                "SELECT EXISTS"
              , "(SELECT 1 FROM information_schema.views"
              , "WHERE table_name = ?"
                -- the way we quote the schema here is not ideal, but it's only
                -- provided to us within the scope of this function. This
                -- should work just fine.
              , " AND table_schema = '" <> fromString schema <> "'"
              , ")"
              ]) (Only str)
    case res of
      [Only True] -> pure $ Just $ ViewName str
      [Only False] -> pure Nothing
      e -> liftIO $ throwIO $ userError $ unwords
             [ "Unexpected result when expecting bool:"
             , show e
             ]

instance FromRow PartialSourceInfo where
  fromRow = PartialSourceInfo
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

-- | Get sources matching the given specification
getSources :: MonadIO m => SourcesSpec -> Transaction m Sources
getSources spec = do
    partialInfo <- getPartialInfo spec
    Sources <$> mapM completePartialInfo partialInfo

-- | Get the count of sources matching the given specification
getSourcesCount :: MonadIO m => SourcesSpec -> Transaction m SourcesCount
getSourcesCount spec = do
    parts <- sourcesSpecToQuery spec
    [Only count] <- case flattenCountQuery parts of
      PartialQuery partialQuery vals ->
        queryS
          (\schema -> intercalateM " " [
              "SELECT COUNT(*)"
            , "FROM " <> quoted (schema, tableSources)
            , partialQuery schema
            ])
          vals
    return count

-- | Get all sources
getAllSources :: MonadIO m => Transaction m Sources
getAllSources = getSources def

-- | Get information about a specific source
getSourceInfo :: MonadIO m => SourceIx -> Transaction m (Maybe SourceInfo)
getSourceInfo sourceIx = do
    Sources sources <- getSources spec
    return $ listToMaybe sources
  where
    spec :: SourcesSpec
    spec = def { sourcesFilterIx = Just sourceIx }

-- | Get information about a specific source and throw an exception if the
-- source doesn't exist
getSourceInfo' :: MonadIO m => SourceIx -> Transaction m SourceInfo
getSourceInfo' sourceIx =
    getSourceInfo sourceIx >>= \case
      Nothing -> liftIO $ throwIO $ NotFound sourceIx
      Just x -> pure x

-- | Get an arbitrary source, if one exists
--
-- This is used when deleting all sources; see 'dropAllSources'.
getAnySource :: MonadIO m => Transaction m (Maybe SourceInfo)
getAnySource = do
    Sources sources <- getSources spec
    return $ listToMaybe sources
  where
    spec :: SourcesSpec
    spec = def { sourcesLimit = Just 1 }

{-------------------------------------------------------------------------------
  Specification of a subset of the sources
-------------------------------------------------------------------------------}

-- | Translate specification to a partial query
sourcesSpecToQuery :: forall m. MonadIO m
                   => SourcesSpec -> Transaction m [(Clause, PartialQuery)]
sourcesSpecToQuery SourcesSpec{..} = do
    mSearchQuery <-
      case sourcesSearchQuery of
        Nothing -> return Nothing
        Just q  -> tsQueryToPostgres fulltextWeights q
    return $ concat [
        [ (ClauseOffset, partialQuery_ "?" (Only x))
        | Just x <- [sourcesOffset]
        ]
      , [ (ClauseLimit, partialQuery_ "?" (Only x))
        | Just x <- [sourcesLimit]
        ]
      , [ (ClauseOrderBy, partialQuery_ (sortClause col dir) ())
        | (col, dir) <- sourcesSortBy
        ]
      -- Filter on ID
      -- Uses primary key index
      , [ (ClauseWhere, partialQuery_ "sources.ix = ?" (Only x))
        | Just x <- [sourcesFilterIx]
        ]
      -- Filter on description
      -- Uses trigram index
      , [ (ClauseWhere, partialQuery_ "sources.description ILIKE ?" (contains x))
        | Just x <- [sourcesFilterDescription]
        ]
      -- Filter on source name
      -- Uses trigram index
      , [ (ClauseWhere, partialQuery_ "sources.cached_sourcename ILIKE ?" (contains x))
        | Just x <- [sourcesFilterName]
        ]
      -- Filter on created date
      -- Uses btree index
      , [ (ClauseWhere, partialQuery_ "sources.created >= ?" (Only x))
        | Just x <- [sourcesFilterCreatedAfter]
        ]
      , [ (ClauseWhere, partialQuery_ "sources.created <= ?" (Only x))
        | Just x <- [sourcesFilterCreatedBefore]
        ]
      -- Filter on username
      -- Uses trigram index
      , [ (ClauseWhere, intercalateM " OR " $
            [ partialQuery_ "sources.cached_addedby ILIKE ?" (contains x)
            | x <- sourcesFilterUsers
            ])
        | not (null sourcesFilterUsers)
        ]
      -- Filter on tags
      -- Uses trigram index
      -- TODO: If we use PostgreSQL 9.6, we should use @<%@ instead of @ILIKE@
      , [ (ClauseWhere, intercalateM " AND " $
            [ PartialQuery (\schema -> quoted (schema, fnImmutableArrayToString) <> "(sources.cached_tags, ' ') ILIKE ?") (contains x)
            | x <- sourcesFilterTags
            ])
        | not (null sourcesFilterTags)
        ]
      -- Filter on columns
      -- Uses trigram index
      -- TODO: If we use PostgreSQL 9.6, we should use @<%@ instead of @ILIKE@
      , [ (ClauseWhere, intercalateM " AND " $
            [ PartialQuery (\schema -> quoted (schema, fnImmutableArrayToString) <> "(sources.cached_columns, ' ') ILIKE ?") (contains x)
            | x <- sourcesFilterColumns
            ])
        | not (null sourcesFilterColumns)
        ]
      -- Filter using full text search
      -- Uses full-text GIN
      -- TODO: This list of columns should be reused from the schema definition.
      , [ (ClauseWhere,
             PartialQuery
               (\schema -> intercalateM " " [
                   quoted (schema, fnSourceFullText)
                 , "(sources.description, sources.cached_sourcename, sources.cached_addedby, sources.cached_tags, sources.cached_columns)"
                 , "@@"
                 , "to_tsquery('english', '" <> x <> "')"
                 ])
               ())
        | Just x <- [mSearchQuery]
        ]
      -- Exclude deprecated sources
      , [ (ClauseWhere, partialQuery_ "sources.deprecated = false" ())
        | not sourcesIncludeDeprecated
        ]
      -- Limit to sources readable by the specified user
      , [ (ClauseWhere, PartialQuery
            (\schema -> intercalateM " " [
                "EXISTS ("
              , "  SELECT * FROM " <> quoted (schema, tableCachedCanRead) <> " AS cache"
              , "  WHERE cache.usr = ? AND cache.sourcename = sources.sourcename"
              , ")"
              ])
            (Only usr))
        | Just usr <- [sourcesReadableBy]
        ]
      ]
  where
    sortClause :: SourcesColumn -> SortDirection -> Query
    sortClause col Ascending  = sortColumn col <> " ASC"
    sortClause col Descending = sortColumn col <> " DESC"

    -- Sorting doesn't make sense on all columns; for instance, the sourcename
    -- column is a foreign key and hence sorting on it doesn't make much sense.
    -- Instead, we should sorted on the _cached_ sourcename.
    sortColumn :: SourcesColumn -> Query
    sortColumn col@SourcesIx          = quoted col
    sortColumn     SourcesSourceName  = "cached_sourcename"
    sortColumn col@SourcesUrl         = quoted col
    sortColumn col@SourcesVersion     = quoted col
    sortColumn col@SourcesCreated     = quoted col
    sortColumn     SourcesAddedBy     = "cached_addedby"
    sortColumn col@SourcesSchema      = quoted col
    sortColumn col@SourcesTableName   = quoted col
    sortColumn col@SourcesViewName    = quoted col
    sortColumn col@SourcesDescription = quoted col

    contains :: String -> Only String
    contains x = Only ("%" ++ x ++ "%")

getPartialInfo :: MonadIO m => SourcesSpec -> Transaction m [PartialSourceInfo]
getPartialInfo spec = do
    parts <- sourcesSpecToQuery spec
    case flattenPartialQuery parts of
      PartialQuery partialQuery vals ->
        queryS
          (\schema -> intercalateM " " [
              "SELECT "
            , intercalateM ", " [
                 "sources.ix"
               , "sources.cached_sourcename"
               , "sources.description"
               , "sources.url"
               , "sources.version"
               , "sources.deprecated"
               , "sources.created"
               , "sources.cached_addedby"
               , "sources.schema"
               , "sources.tableName"
               , "sources.viewName"
               , "sources.cached_tags"
               ]
            , "FROM " <> quoted (schema, tableSources) <> " AS sources"
            , partialQuery schema
            ])
          vals
