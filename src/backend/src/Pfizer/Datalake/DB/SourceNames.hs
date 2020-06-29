{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pfizer.Datalake.DB.SourceNames (
    getSourceName
  , newSourceName
  , getSourceNameWithId
  ) where

import Pfizer.Datalake.DB.IfNotFound
import Pfizer.Datalake.DB.Orphans ()
import Pfizer.Datalake.DB.Schema
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL

-- | Create a new source name
--
-- NOTE: This is never called explicitly by the user. Therefore, if the source
-- name already exists (violating the UNIQUE constraint), this indicates a bug.
newSourceName :: MonadIO m => SourceName -> UserIx -> Transaction m Source
newSourceName name userIx = do
    [Only ix] <- queryS
      (\schema -> intercalateM " " [
          "INSERT INTO " <> quoted (schema, tableSourceNames)
                         <> "(name, addedBy)"
        , "VALUES (?, ?)"
        , "RETURNING ix"
        ])
      (name, userIx)
    return $ Source ix name

-- | Get ID of the specified source
getSourceName :: MonadIO m
              => IfNotFound UserIx (Transaction m) Source a
              -> SourceName -> Transaction m a
getSourceName ifNotFound name = do
    rows <- queryS
      (\schema -> intercalateM " " [
          "SELECT ix"
        , "FROM " <> quoted (schema, tableSourceNames)
        , "WHERE name = ?"
        ])
      (Only name)
    ifNotFound (mkSource <$> rowsToMaybe rows) $ newSourceName name
  where
    mkSource :: SourceNameIx -> Source
    mkSource ix = Source ix name

-- | Get source name with a specific identifier
getSourceNameWithId :: MonadIO m => SourceNameIx -> Transaction m Source
getSourceNameWithId ix = do
    rows <- queryS
      (\schema -> intercalateM " " [
          "SELECT name"
        , "FROM " <> quoted (schema, tableSourceNames)
        , "WHERE ix = ?"
        ])
      (Only ix)
    case rows of
      [Only name] -> return $ Source ix name
      _otherwise  -> liftIO $ throwIO $ NotFound ix
