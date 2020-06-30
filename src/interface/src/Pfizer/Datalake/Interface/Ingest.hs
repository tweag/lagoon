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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Interface.Ingest (
    -- * Ingest options
    IngestOptions(..)
  , CreateOptions(..)
  , FileType(..)
  , HasHeaders(..)
  , EnableQuoteChar
  , DecompressMethod(..)
  , EnableTypeInference
  , CreateTypedTable
  , Encoding(..)
  , CreatedTime(..)
  , Delimiter(..)
  , CreateIndices
  , IngestPrivate
  , Input(..)
  , SourceIdentifier(..)
  , mkSourceIdentifier
  , getSourceIdentifierTag
    -- ** Guess values based on the input
  , guessFileType
  , guessDecompressMethod
    -- ** Utility
  , inputURI
  , delimiterToChar
    -- * Progress messages
  , IngestProgress(..)
  , IngestNotice(..)
  , IngestStart(..)
  , SimpleProgress(..)
  , simpleProgress
  , ProgressOr(..)
    -- * Consuming progress messages
  , newIngestLogger
  , newSimpleLogger
  ) where

import Control.Exception (displayException)
import Control.Monad
import Data.Aeson hiding (Encoding)
import Data.Default
import Data.Foldable (asum)
import Data.Functor.Contravariant
import Data.IORef
import Data.List (isSuffixOf)
import Data.Text.Encoding.Error (UnicodeException)
import GHC.Generics (Generic)
import Network.URI
import System.FilePath
import System.IO
import Text.PrettyPrint ((<+>))
import Text.Show.Pretty (PrettyVal(..), Value(Con), dumpStr)
import Web.HttpApiData
import qualified Text.PrettyPrint as PP

import Pfizer.Datalake.Interface.DB
import Pfizer.Datalake.Interface.JsonPath
import Pfizer.Datalake.Interface.Logging
import Pfizer.Datalake.Interface.Pretty
import Pfizer.Datalake.Interface.Source

{-------------------------------------------------------------------------------
  Ingest options
-------------------------------------------------------------------------------}

data IngestOptions = IngestOptions {
      fileType          :: Maybe FileType
    -- ^ the filetype of the input
    , hasHeaders        :: HasHeaders
    -- ^ whether has a row describing the headers
    , enableQuoteChar   :: EnableQuoteChar
    -- ^ whether @"@ should be used as the quote char when parsing the input
    , decompressMethod  :: Maybe DecompressMethod
    -- ^ is the input compressed?
    , typeInference     :: EnableTypeInference
    -- ^ should we do type inference when ingesting the input?
    , jsonPath          :: JsonPath
    , encoding          :: Encoding
    -- ^ what is the input's encoding?
    , foreignIdentifier :: Maybe (SourceName, String)
    -- ^ foreign identifier for multipar ingest
    , sourceIdentifier  :: Maybe SourceIdentifier
    -- ^ unique ID for the source
    }
  deriving (Generic, PrettyVal)


-- | Options used in creating a new table
-- TODO; should "private" be part of this?
data CreateOptions = CreateOptions {
      createTypedTable  :: CreateTypedTable
    , description       :: Maybe Description
    , tags              :: [TagName]
    , createdTime       :: CreatedTime
    , logEvery          :: Int
    -- ^ when creating the new table, how often should we log?
    }
  deriving (Generic, PrettyVal)

-- | Input file type
data FileType =
    FileTypeTabular Delimiter
  | FileTypeJSON
  deriving (Generic, PrettyVal)

-- | Does the CSV file start with a header row?
data HasHeaders =
    -- | The CSV file has a row identifying all columns
    HasHeaders

    -- | The CSV file has no header information
    --
    -- We specify how many rows we should look at to determine the
    -- number of columns. Note that this is not particularly important right
    -- now; if we guess wrong, and later discover that the table has more
    -- columns than we thought, we just adjust the table and continue. In my
    -- tests so far this has no discernible effect on performance.
  | NoHeaders Int
  deriving (Generic, PrettyVal)

-- | Treat '"' as delimiter character
--
-- If this is disabled, '"' will be treated like an ordinary character.
type EnableQuoteChar = Bool

-- | How should we decompress the input?
--
-- NOTE: Currently we only support a single form of compression (.zip files)
-- and moreover only .zip files containing a single file. Relaxing these
-- limitations is future work.
data DecompressMethod =
    -- | Don't decompress (input is not compressed)
    NotCompressed

    -- | The input is a .zip file containining a single file
  | UnzipSingle
  deriving (Generic, PrettyVal)

-- | Should we do type inference?
type EnableTypeInference = Bool

-- | Should we create a typed table immediately?
type CreateTypedTable = Bool

-- | A tag that uniquely identifies a source. Used during the ingest to avoid
-- re-ingesting a source if the user already has access to a source with the
-- same tag.
--
-- Note that the ingestion process does not guarantee that the tag uniquely
-- identifies the source (this is up to the user) nor does it rely on this
-- fact.
newtype SourceIdentifier= SourceIdentifier { unSourceIdentifier :: String }
   deriving (Generic, PrettyVal)

mkSourceIdentifier :: String -> SourceIdentifier
mkSourceIdentifier = SourceIdentifier

getSourceIdentifierTag :: SourceIdentifier -> TagName
getSourceIdentifierTag s = "SUID:" ++ (unSourceIdentifier s)

-- | Character set encoding
data Encoding = UTF8 | Latin1
   deriving (Generic, PrettyVal)

-- | Time to use for the 'created' field of a table
data CreatedTime =
    -- | Use the time 'ingest' is invoked
    --
    -- This is the default
    CreatedTimeNow

    -- | Use a fixed time
    --
    -- This is used to get reproducible tests.
    | CreatedTimeFixed Timestamp
  deriving (Show, Generic, PrettyVal)

-- | Field delimiter
data Delimiter = DelimComma | DelimTab
    deriving (Show, Generic, PrettyVal)

-- | Should we create all indices by default?
type CreateIndices = Bool

-- | Should the new source be private by default?
type IngestPrivate = Bool

{-------------------------------------------------------------------------------
  Input spec
-------------------------------------------------------------------------------}

-- | Input to the ingest process
data Input a =
    -- | Ingest a remote file
    Remote URI

    -- | Upload a file
    --
    -- The 'FilePath' is the path that the client sends to the server as the
    -- name of the file. The @a@ argument is supposed to represent the file
    -- itself; at various stages of the pipeline this will be instantiated with
    --
    -- * The full path to the file (when parsing command line options)
    -- * A lazy bytestring containing the file contents (in the REST client)
    -- * A conduit representing the request body (in the server)
  | Upload FilePath a
  deriving (Generic, Functor)

instance Foldable Input where
  foldMap _ (Remote _)   = mempty
  foldMap f (Upload _ a) = f a

instance Traversable Input where
  traverse _ (Remote fp)   = pure $ Remote fp
  traverse f (Upload fp a) = Upload fp <$> f a

-- | We don't output the @a@ argument (file contents) as it will often
-- be instantiated with something we cannot (easily) show
instance PrettyVal (Input a) where
  prettyVal (Remote url)  = Con "Request" [prettyVal (show url)]
  prettyVal (Upload fp _) = Con "Upload"  [prettyVal fp]

instance Show (Input a) where
  show = dumpStr

instance Pretty (Input a) where
  pretty = PP.text . inputToFilePath

-- | Translate 'Input' back to a string
inputToFilePath :: Input a -> FilePath
inputToFilePath (Remote uri)  = show uri
inputToFilePath (Upload fp _) = fp

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

-- | The URI for this input (if remote)
inputURI :: Input a -> Maybe URI
inputURI (Remote uri) = Just uri
inputURI (Upload _ _) = Nothing

-- | The character denoted by this delimiter
delimiterToChar :: Delimiter -> Char
delimiterToChar DelimComma = ','
delimiterToChar DelimTab   = '\t'

{-------------------------------------------------------------------------------
  Smart constructor for 'IngestOptions'
-------------------------------------------------------------------------------}

-- | Guess the file type from the filepath
guessFileType :: Input a -> FileType
guessFileType input =
    case takeExtension (inputToFilePath input) of
      ".csv"     -> FileTypeTabular DelimComma
      ".tsv"     -> FileTypeTabular DelimTab
      ".txt"     -> FileTypeTabular DelimTab
      ".bed"     -> FileTypeTabular DelimTab
      ".gtf"     -> FileTypeTabular DelimTab
      ".json"    -> FileTypeJSON
      _otherwise -> def

-- | Guess the decompression method
guessDecompressMethod :: Input a -> DecompressMethod
guessDecompressMethod input =
    if inputIsZipped (inputToFilePath input)
      then UnzipSingle
      else NotCompressed

-- | Is this input @.zip@ed?
--
-- TODO: I don't actually know yet how to deal with .zip files nicely.
-- It appears there is no conduit interface for them. There is
--
-- * <http://hackage.haskell.org/package/zip-conduit>
-- * <http://hackage.haskell.org/package/zip>
--
-- but both are file based, rather than providing a Conduit from bytestring
-- to entries (or something). There is also
--
-- * <http://hackage.haskell.org/package/zip-archive>
--
-- which is a little closer to what we want, but is based on lazy IO and not
-- easily translated to a conduit. Not sure what the right solution here is.
inputIsZipped :: FilePath -> Bool
inputIsZipped = (".zip" `isSuffixOf`)

{-------------------------------------------------------------------------------
  Default values
-------------------------------------------------------------------------------}

-- | Default values in the absence of any other information
instance Default IngestOptions where
  def = IngestOptions {
      fileType          = def
    , hasHeaders        = HasHeaders
    , enableQuoteChar   = True
    , decompressMethod  = Nothing
    , typeInference     = True
    , jsonPath          = P_
    , encoding          = def
    , foreignIdentifier = Nothing
    , sourceIdentifier  = Nothing
    }

-- | Default values in the absence of any other information
instance Default CreateOptions where
  def = CreateOptions {
      createTypedTable  = True
    , description       = Nothing
    , tags              = []
    , createdTime       = CreatedTimeNow
    , logEvery          = 100000
    }

-- | The file type we default to if we don't recognize the extension
instance Default FileType where
  def = FileTypeTabular DelimComma

instance Default DecompressMethod where
  def = NotCompressed

instance Default Encoding where
  def = UTF8

{-------------------------------------------------------------------------------
  Progress
-------------------------------------------------------------------------------}

-- | Progress messages during ingest
--
-- Progress messages are split into notices and start/stop (or start/abort)
-- events; the latter can be nested.
data IngestProgress =
    IngestNotice IngestNotice
  | IngestStart IngestStart
  | IngestOk
  | IngestAborted (Maybe String)

data IngestNotice =
    IngestIndexSkipped ColumnName
  | IngestProcessed Int
  | IngestUnicodeException UnicodeException

data IngestStart =
    IngestCopyIn
  | IngestIndices (Schema, TableName)
  | IngestPrimaryKey
  | IngestForeignKey
  | IngestIndex ColumnName
  | IngestTypedTable

instance Pretty IngestProgress where
  pretty (IngestNotice notice)    = pretty notice
  pretty (IngestStart start)      = pretty start
  pretty IngestOk                 = "ok"
  pretty (IngestAborted Nothing)  = "aborted"
  pretty (IngestAborted (Just e)) = "aborted:" <+> PP.text e

instance Pretty IngestNotice where
  pretty (IngestIndexSkipped columnName) =
    "Index for" <+> pretty columnName <+> "skipped"
  pretty (IngestProcessed n) =
    "Processed" <+> PP.int n <+> "records"
  pretty (IngestUnicodeException ex) = PP.vcat [
      "Unicode exception" <+> PP.parens (PP.text (displayException ex))
    , "Falling back to Latin1 encoding."
    ]

instance Pretty IngestStart where
  pretty IngestCopyIn =
    "Starting ingest proper"
  pretty (IngestIndices qTable) =
    "Creating indices for" <+> pretty qTable
  pretty IngestPrimaryKey =
    "Creating primary key"
  pretty IngestForeignKey =
    "Creating foreign key"
  pretty (IngestIndex columnName) =
    "Creating index on column" <+> pretty columnName
  pretty IngestTypedTable =
    "Creating typed table"

{-------------------------------------------------------------------------------
  JSON serialization
-------------------------------------------------------------------------------}

-- | We currently don't serialize the full structure of 'IngestProgress'
-- message; instead, we flatten the progress message first.
data SimpleProgress =
    SimpleNotice String
  | SimpleStart String
  | SimpleOk
  | SimpleAborted (Maybe String)

simpleProgress :: IngestProgress -> SimpleProgress
simpleProgress (IngestNotice msg)       = SimpleNotice (prettyStr msg)
simpleProgress (IngestStart  msg)       = SimpleStart  (prettyStr msg)
simpleProgress IngestOk                 = SimpleOk
simpleProgress (IngestAborted Nothing)  = SimpleAborted Nothing
simpleProgress (IngestAborted (Just e)) = SimpleAborted (Just e)

instance ToJSON IngestProgress where
  toJSON = toJSON . simpleProgress

instance ToJSON SimpleProgress where
  toJSON (SimpleNotice msg)       = object ["notice" .= msg]
  toJSON (SimpleStart  msg)       = object ["start"  .= msg]
  toJSON SimpleOk                 = "ok"
  toJSON (SimpleAborted Nothing)  = "aborted"
  toJSON (SimpleAborted (Just e)) = object ["aborted" .= e]

instance FromJSON SimpleProgress where
  parseJSON (String "ok")      = return $ SimpleOk
  parseJSON (String "aborted") = return $ SimpleAborted Nothing
  parseJSON val = flip (withObject "SimpleProgress") val $ \obj -> do
    mNotice  <- obj .:? "notice"
    mStart   <- obj .:? "start"
    mAborted <- obj .:? "aborted"
    case (mNotice, mStart, mAborted) of
      (Just msg, _, _) -> return $ SimpleNotice msg
      (_, Just msg, _) -> return $ SimpleStart msg
      (_, _, Just msg) -> return $ SimpleAborted (Just msg)
      _otherwise       -> fail "Invalid SimpleProgress"

-- | Progress or completed operation
--
-- We occassionally get a bunch of progress messages followed by a final
-- message. 'ProgressOr' models this situation. The main use for this type
-- is the corresponding 'FromJSON' instance, which tries to parse a
-- 'SimpleProgress' message if possible, and defaults to @a@ otherwise.
data ProgressOr a = ProgressOngoing SimpleProgress | ProgressComplete a

instance FromJSON a => FromJSON (ProgressOr a) where
  parseJSON val = asum [
      ProgressOngoing  <$> parseJSON val
    , ProgressComplete <$> parseJSON val
    ]

{-------------------------------------------------------------------------------
  Serialization as query parameters
-------------------------------------------------------------------------------}

instance ToHttpApiData FileType where
  toQueryParam (FileTypeTabular DelimComma) = "comma"
  toQueryParam (FileTypeTabular DelimTab)   = "tab"
  toQueryParam FileTypeJSON                 = "json"

instance FromHttpApiData FileType where
  parseQueryParam "comma" = Right $ FileTypeTabular DelimComma
  parseQueryParam "tab"   = Right $ FileTypeTabular DelimTab
  parseQueryParam "json"  = Right $ FileTypeJSON
  parseQueryParam _       = Left $ "Invalid FileType"

instance ToHttpApiData DecompressMethod where
  toQueryParam NotCompressed = "notcompressed"
  toQueryParam UnzipSingle   = "unzip"

instance FromHttpApiData DecompressMethod where
  parseQueryParam "unzip" = Right $ UnzipSingle
  parseQueryParam _       = Left $ "Invalid DecompressMethod"

instance ToHttpApiData Encoding where
  toQueryParam UTF8   = "UTF8"
  toQueryParam Latin1 = "latin1"

instance FromHttpApiData Encoding where
  parseQueryParam "UTF8"   = Right $ UTF8
  parseQueryParam "latin1" = Right $ Latin1
  parseQueryParam _        = Left $ "Invalid Encoding"

{-------------------------------------------------------------------------------
  Show ingest log messages
-------------------------------------------------------------------------------}

-- | Internal logger state used by 'ingestLogger' for log formatting
data LoggerState = LoggerState {
      loggerIndent    :: Int
    , loggerAtNewline :: Bool
    , loggerCanOk     :: Bool
    }

initLoggerState :: LoggerState
initLoggerState = LoggerState {
      loggerIndent    = 0
    , loggerAtNewline = True
    , loggerCanOk     = False
    }

-- | Construct a logger for ingest log messages
--
-- NOTE: This logger is intended to be used in single-threaded mode only.
newIngestLogger :: Handle -> LogLevel -> IO (Logger IO IngestProgress)
newIngestLogger h minLogLevel =
    contramap simpleProgress <$> newSimpleLogger h minLogLevel

-- | Construct a logger for ingest log messages
--
-- NOTE: This logger is intended to be used in single-threaded mode only.
newSimpleLogger :: Handle -> LogLevel -> IO (Logger IO SimpleProgress)
newSimpleLogger h minLogLevel = do
    stRef <- newIORef initLoggerState
    return $ filterLogMessages minLogLevel Logger {
        logMessage = \_logLevel -> go stRef
      }
  where
    go :: IORef LoggerState -> SimpleProgress -> IO ()
    go stRef msg = do
      case msg of
        SimpleNotice a         -> outputMsg   stRef a
        SimpleStart  a         -> outputOpen  stRef a
        SimpleOk               -> outputClose stRef " .. ok"
        SimpleAborted Nothing  -> outputClose stRef " .. aborted"
        SimpleAborted (Just e) -> outputClose stRef (" .. aborted: " ++ e)

    outputMsg :: IORef LoggerState -> String -> IO ()
    outputMsg stRef msg = do
      LoggerState{..} <- readIORef stRef
      unless loggerAtNewline $ hPutStrLn h ""
      hPutStr h $ replicate (loggerIndent * 2) ' ' ++ msg
      hFlush h
      writeIORef stRef $ LoggerState {
          loggerAtNewline = False
        , loggerCanOk     = False
        , ..
        }

    outputOpen :: IORef LoggerState -> String -> IO ()
    outputOpen stRef msg = do
      outputMsg stRef msg
      modifyIORef' stRef $ \st -> st {
          loggerIndent = loggerIndent st + 1
        , loggerCanOk  = True
        }

    outputClose :: IORef LoggerState -> String -> IO ()
    outputClose stRef msg = do
      LoggerState{..} <- readIORef stRef
      if loggerCanOk
        then hPutStrLn h msg
        else unless loggerAtNewline $ hPutStrLn h ""
      writeIORef stRef $ LoggerState {
          loggerCanOk     = False
        , loggerIndent    = loggerIndent - 1
        , loggerAtNewline = True
        }
