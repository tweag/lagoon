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
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Lagoon.Ingest (
    SourceBS
  , SourceIngestConfig(..)
  , IngestS3Config(..)
  , InputStatus(..)
  , NotifyInput
  , ingest
  , ingestFoo
  , inferJsonTypeOfFile
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Conduit hiding (Source)
import Data.Conduit.Combinators (sourceFile, sinkHandle)
import qualified Data.List.NonEmpty as NE
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, strip)
import Data.Tuple (swap)
import Database.PostgreSQL.Simple hiding (query_)
import Network.HTTP.Client (Manager, Request, parseRequest, responseStatus)
import Network.HTTP.Conduit (responseBody)
import Network.HTTP.Simple (httpSink)
import Network.HTTP.Types (statusIsSuccessful)
import System.Directory
import System.FilePath
import System.IO (hClose)
import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3
import qualified Codec.Archive.Zip   as Zip
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.ByteString     as BS.S
import qualified Data.ByteString.Lazy     as BS.L
import qualified Data.ByteString.Builder     as BB
import qualified Data.Map            as Map
import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import qualified Network.URI.Encode as URI
import qualified Path
import qualified System.IO.Temp    as Temp

import Lagoon.DB
import Lagoon.Download
import Lagoon.Ingest.DataFormat
import Lagoon.Ingest.JSON.TypeInference
import Lagoon.Ingest.Progress
import Lagoon.Ingest.Stream
import Lagoon.Ingest.Tabular.TypeInference
import Lagoon.Ingest.Tabular.UntypedRecord
import Lagoon.Ingest.TypeUniverse
import Lagoon.Interface
import Lagoon.Util.Conduit
import Lagoon.Util.JSON
import Lagoon.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

-- | Ingest a (new version of) a data source
ingest :: Connection
       -> Maybe IngestS3Config
       -> Schema
       -> Logger IO IngestProgress
       -> User
       -> SourceIngestConfig SourceBS
       -> NotifyInput
       -> IO SourceInfo
ingest conn
       creds
       schema
       logger
       user
       sourceIngestConfig
       notifyInput
     =
    case (foreignIdentifier, fromMaybe (guessFileType input) fileType) of
      (Just (s,s'), FileTypeTabular delim) ->
        fallbackToLatin1 logger encoding $ \enc ->
          ingestExtraData conn
                          creds
                          schema
                          logger
                          user
                          sourceIngestConfig
                          notifyInput
                          delim
                          enc
                          s
                          s'
      (_, FileTypeTabular delim) ->
        fallbackToLatin1 logger encoding $
          ingestTabular conn
                        creds
                        schema
                        logger
                        user
                        sourceIngestConfig
                        notifyInput
                        delim
      (_, FileTypeJSON) ->
        ingestJSON conn
                   creds
                   schema
                   logger
                   user
                   sourceIngestConfig
                   notifyInput
  where
    input = sourceInput sourceIngestConfig
    IngestOptions{..} = sourceIngestOptions sourceIngestConfig

{-------------------------------------------------------------------------------
  Deal with encodings
-------------------------------------------------------------------------------}

fallbackToLatin1 :: Logger IO IngestProgress
                 -> Encoding
                 -> (Encoding -> IO a)
                 -> IO a
fallbackToLatin1 _      Latin1 k = k Latin1
fallbackToLatin1 logger UTF8   k = do
    ma <- try $ k UTF8
    case ma of
      Right a -> return a
      Left ex -> do logNotice logger $ IngestUnicodeException ex
                    fallbackToLatin1 logger Latin1 k

{-------------------------------------------------------------------------------
  Ingesting tabular data
-------------------------------------------------------------------------------}

ingestTabular :: Connection
              -> Maybe IngestS3Config
              -> Schema
              -> Logger IO IngestProgress
              -> User
              -> SourceIngestConfig SourceBS
              -> NotifyInput
              -> Delimiter
              -> Encoding
              -> IO SourceInfo
ingestTabular conn
              creds
              schema
              logger
              user
              sourceIngestConfig
              notifyInput
              delim
              enc
            = do
    typeRef <- newIORef Nothing

    let writeInferredType :: MonadIO m => RowType -> m ()
        writeInferredType = liftIO . writeIORef typeRef . Just

    withInput creds
              (fromMaybe (guessDecompressMethod input) decompressMethod)
              input
       $  (\inputStatus -> notifyInput inputStatus *> pure (
          readCsv delim enableQuoteChar enc
      =$= (inferTabularFormat hasHeaders $ \format ->
          (if typeInference
            then (passthroughSink inferType writeInferredType =$=)
            else id)
        $ zipListWith (:-) [firstIx ..]
      =$= logProgress logger logEvery
      =$= streamToDb conn
                     schema
                     logger
                     typeRef
                     user
                     (inputURI input)
                     (sourceCreateOptions sourceIngestConfig)
                     (sourceCreateIndices sourceIngestConfig)
                     (sourceSource sourceIngestConfig)
                     format
      )))
  where
    input = sourceInput sourceIngestConfig
    IngestOptions{..} = sourceIngestOptions sourceIngestConfig
    CreateOptions{..} = sourceCreateOptions sourceIngestConfig

{-------------------------------------------------------------------------------
  Ingesting JSON
-------------------------------------------------------------------------------}

ingestJSON :: Connection
           -> Maybe IngestS3Config
           -> Schema
           -> Logger IO IngestProgress
           -> User
           -> SourceIngestConfig SourceBS
           -> NotifyInput
           -> IO SourceInfo
ingestJSON conn
           creds
           schema
           logger
           user
           sourceIngestConfig_
           notifyInput
         = do
    typeRef <- newIORef Nothing

    let writeInferredType :: MonadIO m => JsonType -> m ()
        writeInferredType = liftIO . writeIORef typeRef . Just

    withInput creds
              (fromMaybe (guessDecompressMethod input) decompressMethod)
              input
       $  (\inputStatus -> notifyInput inputStatus *> pure (
          tokenize
      =$= throwOnError
      =$= addContext jsonPath
      =$= (inferJsonFormat $ \format ->
          (if typeInference
            then (passthroughSink inferJsonType writeInferredType =$=)
            else id)
        $ topLevel
      =$= zipListWith (,) [firstIx ..]
      =$= logProgress logger logEvery
      =$= streamToDb conn
                     schema
                     logger
                     typeRef
                     user
                     (inputURI input)
                     createOptions
                     (sourceCreateIndices sourceIngestConfig)
                     (sourceSource sourceIngestConfig)
                     format
      )))
  where
    -- We never want to create a typed table for JSON data
    sourceIngestConfig =
      sourceIngestConfig_ { sourceCreateOptions = createOptions}

    createOptions :: CreateOptions
    createOptions = createOptions_ { createTypedTable = False }
    IngestOptions{..} = sourceIngestOptions sourceIngestConfig_
    createOptions_@CreateOptions{..} = sourceCreateOptions sourceIngestConfig_
    input = sourceInput sourceIngestConfig

ingestExtraData :: Connection
              -> Maybe IngestS3Config
              -> Schema
              -> Logger IO IngestProgress
              -> User
              -> SourceIngestConfig SourceBS
              -> NotifyInput
              -> Delimiter
              -> Encoding
              -> SourceName
              -> String -- Col header
              -> IO SourceInfo
ingestExtraData conn
              creds
              schema
              logger
              user
              sourceIngestConfig
              notifyInput
              delim
              enc
              sourceName
              columnHeader
            = do

    (metadataTableTableName, indexedRows) <- runTransaction conn schema $ do
      metadataSourceNameIndex <- getSourceNameIx schema sourceName `orFailWith`
                                 ("No index found for sourcename " <> sourceName)
      metadataSourceIndex     <- getTableIx schema metadataSourceNameIndex `orFailWith`
                                 ("No index found for table with sourcename " <> show metadataSourceNameIndex)
      metadataSourceTableName <- getTableName schema metadataSourceIndex `orFailWith`
                                 ("No name found for source with index " <> show metadataSourceIndex)
      columnName              <- getColumnName schema columnHeader metadataSourceIndex `orFailWith`
                                 ("No name found for column " <> columnHeader <> " on source with index " <> show metadataSourceIndex)
      indexedRows <- getIndexedRows schema columnName metadataSourceTableName
      return (metadataSourceTableName, indexedRows)

    -- Ideally we'd pass inferExtraData a function that looks up the indices in
    -- the database. However SQL was throwing errors at me, and this works as a
    -- workaround.
    let lookupRowIx :: Text -> Maybe Ix
        lookupRowIx h = lookup (strip h) (swap <$> indexedRows)

    typeRef <- newIORef Nothing

    withInput creds
              (fromMaybe (guessDecompressMethod input) decompressMethod)
              input
       $  (\inputStatus -> notifyInput inputStatus *> pure (
          readCsv delim enableQuoteChar enc
      =$= inferExtraData lookupRowIx metadataTableTableName (\format ->
        ( zipListWith (:-) [firstIx ..]
      =$= logProgress logger logEvery
      =$= streamToDb conn
                     schema
                     logger
                     typeRef
                     user
                     (inputURI input)
                     (sourceCreateOptions sourceIngestConfig)
                     (sourceCreateIndices sourceIngestConfig)
                     (sourceSource sourceIngestConfig)
                     format
      ))))
  where
    orFailWith :: (MonadIO m) => m (Maybe a) -> String -> m a
    orFailWith act reason = do
      m <- act
      case m of
        Nothing -> error $ "ingestExtraData: Aborting: " <> reason
        Just x -> return x
    input = sourceInput sourceIngestConfig
    IngestOptions{..} = sourceIngestOptions sourceIngestConfig
    CreateOptions{..} = sourceCreateOptions sourceIngestConfig

ingestFoo ::
              (forall a. (Connection -> Schema -> IO a) -> IO a)
              -> Logger IO IngestProgress
              -> User
              -> CreateOptions
              -> CreateIndices
              -> Source
              -> [SourceInfo]
              -> NotifyInput
              -> IO SourceInfo
ingestFoo
              withConn
              logger
              user
              createOptions
              createIndices
              source
              sources
              notifyInput
            = do

    typeRef <- liftIO $ newIORef Nothing

    withConn $ \conn schema -> do
      sinfo <- withSources sources $ \sources' -> do
        liftIO $ notifyInput InputOk
        runResourceT $ runConduit
          $ compactConduitsWith
              snd
              (fmap (\(Ix idx, c) -> c .| CL.map (idx,) ) $ sources')
          .| (inferTabularFormat' hasHeaders (snd.NE.head)$ \format ->
                zipListWith ((,)) [firstIx ..]
          .|    CL.map (\(idx, x) ->
                    Arr (NE.toList $ NE.map fst x) :- idx :- (snd $ NE.head x)
                  )
          .| logProgress logger (logEvery createOptions)
          .| streamToDb conn
                        schema
                        logger
                        typeRef
                        user
                        Nothing
                        createOptions
                        createIndices
                        source
                        (FormatCompacted format)
            )

      runTransaction conn schema $ do
        forM_ sources $ \sinfo' -> do
          createCompactView schema
                            (tableNameFromCompact $ sourceTableName sinfo)
                            (sourceTableName sinfo')
                            (sourceColumns  sinfo')
                            (sourceIx sinfo')

          case (sourceTyped sinfo, sourceTyped sinfo') of
            (Just (compactName, _viewName), Just (compactName', _viewName')) -> do
              createCompactView schema
                                (tableNameFromCompact compactName)
                                compactName'
                                (sourceColumns sinfo')
                                (sourceIx sinfo')
            (_, _) -> pure ()
      pure sinfo
  where
    hasHeaders = HasHeaders -- when downloaded, headers are written
    enableQuoteChar = True -- when downloaded, the quote char is enabled
    enc = Latin1 -- just in case
    delim = DelimComma -- when downloaded, the values are comma separated
    withSources :: [SourceInfo]
                -> ([(SourceIx, ConduitM i UntypedRecord (ResourceT IO) ())] -> IO a)
                -> IO a
    withSources [] act = act []
    withSources (x:xs) act = withConn $ \conn schema ->
      withSources xs $ \srcs -> act $ (sourceIx x, prepareSource conn schema x):srcs
    prepareSource
      :: Connection
      -> Schema
      -> SourceInfo
      -> forall i . ConduitM i UntypedRecord (ResourceT IO) ()
    prepareSource conn schema src = runTransactionC conn schema $
      downloadSource src
      .| CC.concatMap (\case { Flush -> Nothing; Chunk a -> Just a})
      .| CC.map BB.toLazyByteString
      .| CC.map BS.L.toStrict
      .| readCsv delim enableQuoteChar enc

getSourceNameIx :: (MonadIO m)
                => Schema
                -> SourceName
                -> Transaction m (Maybe SourceNameIx)
getSourceNameIx schema sourceName = do
    mIx <- query_ $ intercalateM " " [
          "SELECT ix"
        , "FROM " <> quoted (schema, tableSourceNames)
        , "WHERE name = '" <> fromString sourceName <> "'"
        ]
    case mIx of
      []        -> return Nothing
      [Only ix] -> return $ Just ix
      _         -> error $ "getSourceNameIx: this should not happen" <>
                           "'name' column should be UNIQUE"

getTableIx :: (MonadIO m)
           => Schema
           -> SourceNameIx
           -> Transaction m (Maybe SourceIx)
getTableIx schema (SourceNameIx (Ix sourceNameIx)) = do
  mIx <- query_ $ intercalateM " " [
      "SELECT ix"
    , "FROM " <> quoted (schema, tableSources)
    , "WHERE sourcename = " <> fromString (show sourceNameIx)
    , "ORDER BY version"
    , "LIMIT 1"
    ]
  case mIx of
    [] -> return Nothing
    [Only ix] -> return $ Just ix
    _         -> error $ "getTableIx: this should not happen" <>
                         "got more than 1 results with LIMIT 1"

getTableName :: (MonadIO m)
             => Schema
             -> SourceIx
             -> Transaction m (Maybe TableName)
getTableName schema (Ix tableIx) = do
  mName <- query_ $ intercalateM " " [
          "SELECT tablename"
        , "FROM " <> quoted (schema, tableSources)
        , "WHERE ix = " <> fromString (show tableIx)
        ]
  case mName of
    [] -> return Nothing
    [Only ix] -> return (Just ix)
    _         -> error $ "getTableName: this should not happen" <>
                         "'sourcename' column should be UNIQUE"

getColumnName :: (MonadIO m)
              => Schema
              -> String -- ^ Column header
              -> SourceIx
              -> Transaction m (Maybe ColumnName)
getColumnName schema columnHeader (Ix tableIx) = do
  mColumnName <- query_ $ intercalateM " " [
          "SELECT columnname"
        , "FROM " <> quoted (schema, tableSourceColumns)
        , "WHERE columnheader = '" <> fromString columnHeader <> "'"
        , "AND source = " <> fromString (show tableIx)
        ]
  case mColumnName of
    [] -> return Nothing
    Only ix :_ -> return (Just ix)

getIndexedRows :: (MonadIO m)
               => Schema
               -> ColumnName
               -> TableName
               -> Transaction m [(Ix, Text)]
getIndexedRows schema columnName tableName =
    query_ $ intercalateM " " [
        "SELECT ix, " <> quoted columnName
      , "FROM " <> quoted (schema, tableName)
      ]

{-------------------------------------------------------------------------------
  Standalone JSON type inference
-------------------------------------------------------------------------------}

inferJsonTypeOfFile :: Maybe IngestS3Config
                    -> Maybe DecompressMethod
                    -> Input SourceBS
                    -> NotifyInput
                    -> IO JsonType
inferJsonTypeOfFile creds decompressMethod input notifyInput =
    withInput creds
              (fromMaybe (guessDecompressMethod input) decompressMethod)
              input
       $  (\inputStatus -> notifyInput inputStatus *> pure (
          tokenize
      =$= throwOnError
      =$= addContext P_
      =$= inferJsonType))

{------------------------------------------------------------------------------
  Decompression
-------------------------------------------------------------------------------}

-- | Source producing bytestrings
type SourceBS = Conduit () IO BS.S.ByteString

-- | Sink consuming bytestrings (ResourceT)
type SinkBS = Sink BS.S.ByteString (ResourceT IO)

-- | Type of remote ingest
data RemoteType = RemoteHttp Request
                | RemoteS3 S3.Bucket Text
                  -- ^ Remote s3 file with bucket and object key

-- | Ingest configuration for using S3
data IngestS3Config = IngestS3Config
        { ingestS3Configuration   :: Aws.Configuration
        , ingestS3Manager         :: Manager
        , ingestS3S3Configuration :: S3.S3Configuration Aws.NormalQuery
        }

-- | Status of the input
data InputStatus = InputOk
                  -- ^ Notify the user that the input was accessed successfully
                 | InputError Text
                  -- ^ Notify the user that there was an error accessing the
                  -- input

-- | Input status callback. Mostly used with StreamResponse.
type NotifyInput = (InputStatus -> IO ())

-- | Try to parse a 'URI' as a 'RemoteType':
--
--  * If it can be parsed as an http endpoint, used as such
--  * If in the form of @s3://bucket_name/object_key@, interpreted as an S3
--      file
--
--  NOTE: bucket_name and object_key should be URL encoded as (AWS) S3 allows
--  spaces and other non-URL characters.
uriToRemoteType :: URI.URI -> Maybe RemoteType
uriToRemoteType (parseRequest . show -> Just req) = Just $ RemoteHttp req
uriToRemoteType uri
    | "s3:" <- URI.uriScheme uri
    , Just bucket <- URI.uriRegName <$> URI.uriAuthority uri
    , '/':okey <- URI.uriPath uri
      = Just $ RemoteS3 (pack $ URI.decode bucket) (pack $ URI.decode okey)
uriToRemoteType _ = Nothing

-- | Open the input and stream it into the specified sink
--
-- This isn't just a 'Source' primarily because the zip library puts some
-- limitations on the context in which we read from zip files ('ZipArchive'
-- monad). But it also makes life a bit easier for remote sources.
withInput :: forall a.
             Maybe IngestS3Config
          -> DecompressMethod
          -> Input SourceBS
          -> (InputStatus -> IO (SinkBS a))
            -- ^ Should be called only once, when input was accessed
          -> IO a
withInput mS3Config decompressMethod input k =
    go input decompressMethod
  where
    go :: Input SourceBS -> DecompressMethod -> IO a
    go (Remote uri) NotCompressed = runResourceT $
      case uriToRemoteType uri of
        Just (RemoteHttp req) -> do
          httpSink req (sinkResponseOk uri)
        Just (RemoteS3 b okey) -> do
          IngestS3Config cfg mgr s3Config <-
            maybe
              (inputError $ unwords [
                  "When ingesting s3 file:"
                , (show b <>  "/" <> show okey)
                , "S3 support is disabled."
                ]
              )
              pure mS3Config
          S3.GetObjectResponse { S3.gorResponse = rsp } <-
                Aws.pureAws cfg s3Config mgr $
                  S3.getObject b okey
          responseBody rsp $$+- sinkResponseOk uri rsp
        Nothing -> inputError "Could not determine remote protocol"
    go (Upload _ source) NotCompressed =
        runResourceT $ transPipe liftIO source $$ inputOk

    -- Zipped ingest. Note that any branch may break and reply with
    -- 'inputError'; however leave it to 'goLocal' to call 'inputOk'.
    go (Remote uri) UnzipSingle =
      case uriToRemoteType uri of
        Just (RemoteHttp req) ->
          unzipIngest (\k' -> httpSink req (\resp ->
                        ensureResponseOk uri resp (transPipe liftIO k')))
                      (takeBaseName (show uri))
        Just (RemoteS3 b okey) -> do
          IngestS3Config cfg mgr s3Config <-
            maybe
              (throwM $ Aws.NoCredentialsException $
                "ingesting s3 file " <> show b <> "/" <> show okey)
              pure mS3Config
          unzipIngest (\k' -> runResourceT $ do
            S3.GetObjectResponse { S3.gorResponse = rsp } <-
                  Aws.pureAws cfg s3Config mgr $
                    S3.getObject b okey
            responseBody rsp $$+- ensureResponseOk uri rsp (transPipe liftIO k')
            )
            (takeBaseName (show uri))
        Nothing -> inputError "Could not determine remote protocol"
    go (Upload fp source) UnzipSingle = do
        unzipIngest (source $$) (takeBaseName fp)

    ensureResponseOk :: forall n b c. (MonadIO n, MonadThrow n)
                  => URI.URI -> HTTP.Response b
                  -> Sink BS.S.ByteString n c -> Sink BS.S.ByteString n c
    ensureResponseOk uri resp snk =
        if statusIsSuccessful (responseStatus resp)
        then snk
        else
          let e = unwords [
                  "Could not fetch", show uri, "with HTTP status"
                , show (responseStatus resp)
                ]
          in inputError e

    sinkResponseOk :: forall b. URI.URI -> HTTP.Response b -> SinkBS a
    sinkResponseOk uri resp = ensureResponseOk uri resp inputOk

    -- Operate on a file we downloaded locally
    goLocal :: FilePath -> DecompressMethod -> IO a
    goLocal fp NotCompressed =
        runResourceT $ sourceFile fp $$ inputOk
    goLocal fp UnzipSingle = do
        path <- Path.parseAbsFile =<< canonicalizePath fp
        Zip.withArchive path $ do
          entries <- Map.keys <$> Zip.getEntries
          case entries of
            [entry] -> do
              src <- Zip.getEntrySource entry
              liftIO $ runResourceT $ src $$ inputOk
            _otherwise -> do
              let e = "ZIP files containing multiple files not supported"
              _unused <- liftIO (k (InputError (pack e)))
              throwM (userError e)

    -- Since we cannot decode ZIP files in a streaming manner, we have no choice
    -- here but to download the file first and then read the local file.
    unzipIngest :: (Sink BS.S.ByteString IO () -> IO ())
                -> String
                -> IO a
    unzipIngest runSink template = do
        Temp.withSystemTempFile template $ \fp h -> do
          runSink $ sinkHandle h
          hClose h
          goLocal fp UnzipSingle

    -- Notify input status. Note: we do not perform checks on local files
    -- before ingest, although we could. This would only be used on the
    -- (deprecated) ingest tool, which would return with a non-zero exit code
    -- either way.
    inputOk :: SinkBS a
    inputOk = liftIO (k InputOk) >>= id

    inputError :: forall n b. (MonadThrow n, MonadIO n) => String -> n b
    inputError e = liftIO (k (InputError (pack e))) *> throwM (userError e)
