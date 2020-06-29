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
module Lagoon.Server.API.SQL (server) where

import Data.Aeson
import Data.ByteString.Builder (Builder)
import Data.Conduit
import qualified Network.HTTP.Types as HTTP

import Lagoon.Interface
import Lagoon.Server.HandlerM
import Lagoon.Server.Servant.Conduit
import Lagoon.Util.PostgreSQL hiding (trySql)
import Lagoon.Verified
import qualified Lagoon.Interface.API as API

server :: STrans API.SQL
server session sql mAccept = do
    user  <- getSessionUser session
    mBody <- trySql $ withConnectionC $ \conn schema -> do
               src <- partialTransaction conn schema $ do
                        verifiedSql <- verifyUserQuery user sql
                        execUserQuery verifiedSql copyToFormat
               return $ transPipe (partialTransaction conn schema) src
    return $ case mBody of
      NoSqlException body -> StreamResponse {
          streamResponseBody    = body
        , streamResponseStatus  = HTTP.ok200
        , streamResponseHeaders = [contentTypeHdr]
        }
      RaisedSqlException e -> StreamResponse {
          streamResponseBody    = yieldError e
        , streamResponseStatus  = HTTP.badRequest400
        , streamResponseHeaders = [(HTTP.hContentType, "application/json")]
        }
  where
    -- We default to JSON if the client does not specify
    copyToFormat :: CopyToFormat
    copyToFormat =
      case mAccept of
        Just (API.AcceptHeader (fmt:_)) -> fmt
        _otherwise                      -> CopyToJSON

    -- We yield SQL errors in JSON format
    yieldError :: (Monad m, ToJSON a) => a -> Producer m (Flush Builder)
    yieldError = yield . Chunk . fromEncoding . toEncoding

    -- Content-type header for the specified format
    contentTypeHdr :: HTTP.Header
    contentTypeHdr = ( HTTP.hContentType
                     , case copyToFormat of
                         CopyToJSON -> "application/json"
                         CopyToCSV  -> "text/csv"
                     )
