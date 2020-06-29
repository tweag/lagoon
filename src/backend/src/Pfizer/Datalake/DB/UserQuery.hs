{-# LANGUAGE OverloadedStrings #-}
-- | Execute user queries
module Pfizer.Datalake.DB.UserQuery (execUserQuery) where

import Control.Monad
import Control.Monad.Catch
import Data.ByteString.Builder (Builder)
import Data.Conduit

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL hiding (trySql)

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
