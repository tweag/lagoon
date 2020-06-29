{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Server.API.SQL (server) where

import Data.Aeson
import Data.ByteString.Builder (Builder)
import Data.Conduit
import qualified Network.HTTP.Types as HTTP

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Server.HandlerM
import Pfizer.Datalake.Server.Servant.Conduit
import Pfizer.Datalake.Util.PostgreSQL hiding (trySql)
import Pfizer.Datalake.Verified
import qualified Pfizer.Datalake.Interface.API as API

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
