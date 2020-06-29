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
-- | Execute user queries
module Lagoon.DB.UserQuery (execUserQuery) where

import Control.Monad
import Control.Monad.Catch
import Data.ByteString.Builder (Builder)
import Data.Conduit

import Lagoon.Interface
import Lagoon.Util.PostgreSQL hiding (trySql)

-- | Execute an arbitrary SQL query
execUserQuery :: (MonadIO m, MonadIO m', MonadCatch m')
              => UserQuery
              -> CopyToFormat
              -> Transaction m' (Conduit i (Transaction m) (Flush Builder))
execUserQuery (UserQuery sql) typ = do
    case typ of
      CopyToCSV -> do
        copy_ $ intercalateM " " [
            "COPY (" <> fromString sql <> ")"
          , "TO STDOUT (FORMAT CSV, HEADER)"
          ]
        return $ void $ copyToCsv

      CopyToJSON -> do
        copy_ $ intercalateM " " [
            "COPY ("
          , "WITH result AS (" <> fromString sql <> ")"
          , "SELECT ROW_TO_JSON(result) FROM result"
          , ") TO STDOUT (FORMAT BINARY)"
          ]
        return $ void $ copyToJsonArray NoJsonVersionPrefix
