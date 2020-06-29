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

module Lagoon.Server.API.Sources (server) where

import Control.Concurrent
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Status
import Servant
import System.Timeout (timeout)
import qualified Data.Conduit as C
import qualified Data.Text.Encoding as T

import Lagoon.Ingest.Progress
import Lagoon.Interface
import Lagoon.Server.FriendlyException.Rethrow
import Lagoon.Server.HandlerM
import Lagoon.Server.Serialization ()
import Lagoon.Server.Servant.Conduit
import Lagoon.Util.PostgreSQL
import Lagoon.Verified
import qualified Lagoon.Interface.API as API

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

server :: STrans API.Sources
server = sourcesGet
    :<|> sourcesPost
    :<|> sourcesCompact

{-------------------------------------------------------------------------------
  Individual handlers
-------------------------------------------------------------------------------}

-- | Get all sources
--
-- Note: this is not very efficient as it required PostgreSQL to compute the
-- exact number of entries in the whole query.
sourcesGet :: STrans API.SourcesGet
sourcesGet session spec = do
    user <- getSessionUser session
    addHeader <$> execTransaction (getSourcesCount user spec)
              <*> execTransaction (getSources user spec)

-- | Post a new source
sourcesPost :: STrans API.SourcesPost
sourcesPost session ingestOptions createOptions noIndices private mSourceName input = do
    (logger, terminateLog, logSource) <- liftIO $ newJsonLogSource Notice
    user <- getSessionUser session

    -- Placeholder for the source acquisition status
    inputStatus <- liftIO newEmptyMVar

    -- Do the actual ingestion in a separate thread. We respond to the
    -- client through 'source'.
    _pid <- forkConnection $ \conn schema -> do
      source <- getOrCreateSource conn schema user sourceName private
      creds <- getIngestS3Config

      -- Check whether a source with the given source identifier exists: if so,
      -- skip ingest; otherwise, just proceed with ingest.
      mAlreadyIngested <- case sourceIdentifier ingestOptions of
        Nothing -> pure Nothing
        Just sourceId -> runTransaction conn schema $
          getSourceByIdentifier user sourceId
      case mAlreadyIngested of
        Just sinfo -> pure $ do
          void $ tryPutMVar inputStatus InputOk
          terminateLog $ finalLogMessage $ Right sinfo
        Nothing -> pure $ do
          mSourceInfo <- try $ rethrowFriendly $
            ingest conn
                   creds
                   schema
                   logger
                   createIndices
                   source
                   ingestOptions
                   (updateCreateOptions createOptions)
                   input
                   -- In theory the MVar should be put only once, but you never
                   -- know
                   (void . tryPutMVar inputStatus)
          terminateLog $ finalLogMessage mSourceInfo

    -- XXX: What's a good value for timeout?
    mstatus <- liftIO (timeout 10000000 (readMVar inputStatus))
    case mstatus of
      Just InputOk -> return $ StreamResponse {
          streamResponseBody    = logSource
        , streamResponseStatus  = ok200
        , streamResponseHeaders = []
        }
      Just (InputError e) -> return $ StreamResponse {
          streamResponseBody    = do
            C.yield $ C.Chunk (T.encodeUtf8Builder e)
            C.yield $ C.Flush
        , streamResponseStatus  = unprocessableEntity422
        , streamResponseHeaders = []
        }
      Nothing -> return $ StreamResponse {
          streamResponseBody    = do
            let w = "WARNING: Unknown ingest status: "
            C.yield $ C.Chunk (T.encodeUtf8Builder w)
            logSource
        , streamResponseStatus  = internalServerError500
        , streamResponseHeaders = []
        }
  where
    createIndices = not noIndices
    sourceName    = fromMaybe "unnamed" mSourceName
    -- Tag the source with the source identifier, if needed
    updateCreateOptions :: CreateOptions -> CreateOptions
    updateCreateOptions = maybe id (\sourceId opts->
      opts { tags = tags opts ++ [ getSourceIdentifierTag sourceId ] }
      ) (sourceIdentifier ingestOptions)

-- | Post a new source
sourcesCompact :: STrans API.SourcesCompact
sourcesCompact session createOpts noIndices private sourceIxs mSourceName = do
-- sourcesCompact session createOpts sourceIxs0 mSourceName = do

    (logger, terminateLog, logSource) <- liftIO $ newJsonLogSource Notice
    user <- getSessionUser session

    -- Placeholder for the source acquisition status
    inputStatus <- liftIO newEmptyMVar

    -- withConnections sourceIxs0 $ \sourceIxs -> do
    -- Do the actual ingestion in a separate thread. We respond to the
    -- client through 'source'.
    _pid <- forkConnection $ \conn schema -> do

      source <- getOrCreateSource conn schema user sourceName private
      hasPerms <- runTransaction conn schema $
        checkHasPermissions user sourceIxs

      WithConnection withConnection <- getWithConnection
      return $ do
        mSourceInfo <- try $ rethrowFriendly $
          ingestFoo withConnection
                    logger
                    createOpts
                    (not noIndices)
                    source
                    hasPerms
                    -- In theory the MVar should be put only once, but you never
                    -- know
                    -- -- TODO: figure out if this is the right place
                    (void . tryPutMVar inputStatus)
        terminateLog $ finalLogMessage mSourceInfo

    -- XXX: What's a good value for timeout?
    mstatus <- liftIO (timeout 10000000 (readMVar inputStatus))
    case mstatus of
      Just InputOk -> return $ StreamResponse {
          streamResponseBody    = logSource
        , streamResponseStatus  = ok200
        , streamResponseHeaders = []
        }
      Just (InputError e) -> return $ StreamResponse {
          streamResponseBody    = do
            C.yield $ C.Chunk (T.encodeUtf8Builder e)
            C.yield $ C.Flush
        , streamResponseStatus  = unprocessableEntity422
        , streamResponseHeaders = []
        }
      Nothing -> return $ StreamResponse {
          streamResponseBody    = do
            let w = "WARNING: Unknown ingest status"
            C.yield $ C.Chunk (T.encodeUtf8Builder w)
            logSource
        , streamResponseStatus  = internalServerError500
        , streamResponseHeaders = []
        }
  where
    sourceName    = fromMaybe "unnamed" mSourceName

-- | Gets the existing source if it exists, or create a fresh one otherwise.
getOrCreateSource :: (MonadIO m, MonadMask m)
                  => Connection
                  -> Schema
                  -> VerifiedUser
                  -> SourceName
                  -> IngestPrivate
                  -> m (Can 'Update SourceNameIx)
getOrCreateSource conn schema user sourceName private =
      runTransaction conn schema $ do
        mSource <- getExistingSourceName sourceName
        case mSource of
          Just source -> do
            when private $ do
              -- Check for MANAGE permission
              hasPerm <- checkHasPermission user (sourceNameIx source)
              public  <- getPublicGroup
              setGroupDatasetAccess hasPerm public DatasetAccessLevelNone
            checkHasPermission user (sourceNameIx source) -- UPDATE permission
          Nothing     -> do
            canCreate <- checkHasPermission user ()
            source    <- newSourceName canCreate sourceName private
            checkHasPermission user (sourceNameIx source)
