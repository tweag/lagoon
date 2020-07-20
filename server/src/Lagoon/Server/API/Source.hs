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
module Lagoon.Server.API.Source (server) where

import Control.Category ((>>>))
import Control.Exception
import Control.Monad
import Data.Conduit (transPipe)
import Network.HTTP.Types (ok200)
import Servant
import qualified Data.ByteString         as BS.S
import qualified System.FilePath.Posix   as FilePath.Posix
import qualified System.FilePath.Windows as FilePath.Windows

import Lagoon.Ingest.Progress
import Lagoon.Interface
import Lagoon.Server.FriendlyException.Rethrow (finalLogMessage)
import Lagoon.Server.HandlerM
import Lagoon.Server.Serialization ()
import Lagoon.Server.Servant.Conduit
import Lagoon.Util.PostgreSQL
import Lagoon.Verified
import qualified Lagoon.Interface.API as API

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

server :: STrans API.Source
server = sourceGet
    :<|> sourceDelete
    :<|> sourceTagsPost
    :<|> sourceTagDelete
    :<|> sourceDownload
    :<|> sourceGetColumn
    :<|> sourceMakeTyped
    :<|> sourceSetDeprecated
    :<|> sourceSetPublic
    :<|> sourceSetGroupAccessLevel
    :<|> sourceSetUserAccessLevel
    :<|> sourceByName
    :<|> sourceWithVersion
    :<|> sourceAllVersions
    :<|> sourceInferJsonType

{-------------------------------------------------------------------------------
  Handlers proper

  TODO: We might want a more general API at some point for updating arbitrary
  parts of a source in one go.
-------------------------------------------------------------------------------}

-- | Get info about a source
sourceGet :: STrans API.SourceGet
sourceGet = getSourceInfo'


sourceDelete :: STrans API.SourceDelete
sourceDelete session sourceIx = do
    user <- getSessionUser session
    execTransaction $ do
      perm <- checkVersionPerm user sourceIx
      void $ deleteSource perm
      return NoContent

-- | Get Source corresponding to a name
sourceByName :: STrans API.SourceByName
sourceByName _session name = do
    mSource <- execTransaction $ getExistingSourceName name
    case mSource of
      Just source -> return (sourceNameIx source)
      Nothing     -> throwM err404

-- | Get particular version of a source
sourceWithVersion :: STrans API.SourceWithVersion
sourceWithVersion  session src mVersion = do
    user <- getSessionUser session
    execTransaction $ do
      hasPerm <- checkHasPermission user src
      getVersion hasPerm mVersion

-- | Get particular version of a source
sourceAllVersions :: STrans API.SourceAllVersions
sourceAllVersions  session src = do
    user <- getSessionUser session
    execTransaction $ do
      hasPerm <- checkHasPermission user src
      getVersions hasPerm

-- | Add tags to a source
sourceTagsPost :: STrans API.SourceTagsPost
sourceTagsPost session sourceIx tags = do
    user <- getSessionUser session
    execTransaction $ do
      perm <- checkVersionPerm user sourceIx
      forM_ tags $ \tag -> do
        -- We ignore the success value
        -- (which is 'False' if the tag was already present)
        void $ tagSource perm tag
      return NoContent

-- | Delete a tag from a source
sourceTagDelete :: STrans API.SourceTagDelete
sourceTagDelete session sourceIx tag = do
    user <- getSessionUser session
    execTransaction $ do
      perm <- checkVersionPerm user sourceIx
      -- As in 'sourceTagsPost', we ignore the success value
      -- (which is 'False' if the tag was not present)
      void $ untagSource perm tag
      return NoContent

sourceGetColumn :: STrans API.SourceGetColumn
sourceGetColumn session sourceIx columnName = do
    user <- getSessionUser session
    execTransaction $ do
      perm <- checkHasPermission user sourceIx
      getColumn perm columnName

sourceMakeTyped :: STrans API.SourceMakeTyped
sourceMakeTyped session sourceIx noIndices = do
    (logger, terminateLog, logSource) <- liftIO $ newJsonLogSource Notice
    user <- getSessionUser session

    -- Update the table in a separate thread so that we can stream progress
    _pid <- forkConnection $ \conn schema -> do
      perm <- runTransaction conn schema $ checkHasPermission user sourceIx
      return $ do
        mSourceInfo <- try $ runTransaction conn schema $
                               makeTyped logger (not noIndices) perm
        terminateLog $ finalLogMessage mSourceInfo

    return $ StreamResponse {
        streamResponseBody    = logSource
      , streamResponseStatus  = ok200
      , streamResponseHeaders = []
      }

sourceInferJsonType :: STrans API.SourceInferJsonType
sourceInferJsonType _session decompressMethod input = do
    s3Cfg <- getIngestS3Config
    liftIO $ inferJsonTypeOfFile s3Cfg decompressMethod input (const (pure ()))

{-------------------------------------------------------------------------------
  Updating permissions
-------------------------------------------------------------------------------}

sourceSetDeprecated :: STrans API.SourceSetDeprecated
sourceSetDeprecated session sourceIx val = do
    user <- getSessionUser session
    execTransaction $ do
      perm <- checkHasPermission user sourceIx
      setDeprecated perm val
      return NoContent

sourceSetPublic :: STrans API.SourceSetPublic
sourceSetPublic session sourceIx val = do
    user <- getSessionUser session
    execTransaction $ do
      hasPerm <- checkHasPermission user sourceIx
      public  <- getPublicGroup
      case val of
        True  -> setGroupDatasetAccess hasPerm public DatasetAccessLevelUpdate
        False -> setGroupDatasetAccess hasPerm public DatasetAccessLevelNone
      return NoContent

sourceSetGroupAccessLevel :: STrans API.SourceSetGroupAccessLevel
sourceSetGroupAccessLevel session sourceIx group val = do
    user <- getSessionUser session
    execTransaction $ do
      hasPerm <- checkHasPermission user sourceIx
      groupIx <- getGroup group
      setGroupDatasetAccess hasPerm groupIx val
      return NoContent

sourceSetUserAccessLevel :: STrans API.SourceSetUserAccessLevel
sourceSetUserAccessLevel session sourceIx userName val = do
    sessionUser <- getSessionUser session
    execTransaction $ do
      hasPerm <- checkHasPermission sessionUser sourceIx
      user    <- lookupUserName userName
      setUserDatasetAccess hasPerm user val
      return NoContent

{-------------------------------------------------------------------------------
  Download sources
-------------------------------------------------------------------------------}

-- | Download source
sourceDownload :: STrans API.SourceDownload
sourceDownload session ix = do
    user <- getSessionUser session
    body <- withConnectionC $ \conn schema -> do
              hasPerm <- runTransaction conn schema $
                           checkHasPermission user ix
              return $ transPipe (partialTransaction conn schema)
                                 (downloadSource hasPerm)
    sourceInfo@SourceInfo{..} <- getSourceInfo' session ix
    return StreamResponse {
        streamResponseBody    = body
      , streamResponseStatus  = ok200
      , streamResponseHeaders = [
            ( "Content-Disposition"
            ,     "attachment; filename=\""
               <> sourceFileName sourceVersionOf
               <> case isJsonSource sourceInfo of
                    Just _  -> ".json"
                    Nothing -> ".csv"
               <> "\""
            )
            -- TODO: Set Content-Type header
          ]
      }

-- | Construct valid filename for a given sourcename
--
-- Does not add an extension.
sourceFileName :: SourceName -> BS.S.ByteString
sourceFileName =
        -- A @:@ is considered valid if it is the second character
        -- i.e., @c:...@ is valid, but @xc:...@ is not (and gets changed to
        -- @xc_...@). We therefore _first_ drop any drive component ..
        FilePath.Windows.dropDrive

        -- .. then apply 'makeValid' to strip any special characters,
        -- including any @:@ that appear elsewhere in the filename
    >>> makeValid

        -- .. and finally we strip out path separators, because they are
        -- left alone by 'makeValid'
    >>> filter (not . isPathSep)
    >>> fromString
  where
    makeValid :: FilePath -> FilePath
    makeValid = FilePath.Windows.makeValid
              . FilePath.Posix.makeValid

    isPathSep :: Char -> Bool
    isPathSep c = FilePath.Windows.isPathSeparator c
               || FilePath.Posix.isPathSeparator   c

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getSourceInfo' :: ServerSession -> SourceIx -> HandlerM SourceInfo
getSourceInfo' session sourceIx = do
    user <- getSessionUser session
    execTransaction $ do
      perm <- checkVersionPerm user sourceIx
      mSourceInfo <- getSourceInfo perm
      case mSourceInfo of
        Just sourceInfo -> return sourceInfo
        Nothing         -> throwM err404
