{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pfizer.Datalake.DB.Users (newUser, getUser) where

import Pfizer.Datalake.DB.Schema
import Pfizer.Datalake.DB.IfNotFound
import Pfizer.Datalake.DB.Orphans ()
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL

-- | Create a new user
newUser :: MonadIO m => String -> Transaction m User
newUser name = do
    getUser (errorIfFound name) name
    [Only ix] <- queryS
      (\schema -> intercalateM " " [
          "INSERT INTO " <> quoted (schema, tableUsers) <> "(name)"
        , "VALUES (?)"
        , "RETURNING ix"
        ])
      (Only name)
    return $ User ix name

-- | Get id of the specified user
getUser :: MonadIO m
        => IfNotFound () (Transaction m) User a
        -> UserName -> Transaction m a
getUser ifNotFound name = do
    rows <- queryS (\schema -> intercalateM " " [
                       "SELECT ix"
                     , "FROM " <> quoted (schema, tableUsers)
                     , "WHERE name = ?"
                     ])
                   (Only name)
    ifNotFound (mkUser <$> rowsToMaybe rows) $ \() -> newUser name
  where
    mkUser :: UserIx -> User
    mkUser ix = User ix name
