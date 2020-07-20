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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lagoon.DB.Users (newUser, getUser) where

import Lagoon.DB.Schema
import Lagoon.DB.IfNotFound
import Lagoon.DB.Orphans ()
import Lagoon.Interface
import Lagoon.Util.PostgreSQL

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
