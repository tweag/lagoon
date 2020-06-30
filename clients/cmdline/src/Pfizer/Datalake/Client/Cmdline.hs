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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Pfizer.Datalake.Client.Cmdline (
    Cmdline(..)
  , Command(..)
  , CommandBody(..)
  , InternalCommand(..)
  , UnrecognizedInputException(..)
  , getCmdline
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad
import Data.Foldable (asum)
import Data.Function (fix)
import Data.List (isInfixOf, isPrefixOf, stripPrefix, break)
import Data.String
import Data.Time
import GHC.Generics (Generic)
import Network.URI (parseURI, URI)
import Options.Applicative
import Options.Applicative.Help.Chunk
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty
import qualified Network.URI.Encode as URI
import qualified Text.PrettyPrint as PP

import Pfizer.Datalake.Client.AppSkeleton
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Interface.Prog hiding (Login)
import qualified Pfizer.Datalake.Interface.Prog as P

-- | Command line options
data Cmdline = Cmdline {
      -- | Core app skeleton arguments
      skeletonCmdline :: SkeletonCmdline

      -- | Dump Cmdline on startup (for debugging)
    , dumpCmdline :: Bool

      -- | Command to execute
    , cmd :: Command

      -- | Command-line overrides of the config
    , cmdlineConfig :: ClientConfig
    }
  deriving (Generic, PrettyVal)

-- | The command to execute
--
-- We record both what to do as well as some additional information about
-- the command, primarily so that we can construct better error messages
data Command = Command {
      -- | What to do
      cmdBody :: CommandBody

      -- | Show version and exit.
    , cmdShowVersion :: Bool

      -- | We are ingesting a data source?
    , cmdInput :: Maybe (Input FilePath)

      -- | We are searching for sources?
    , cmdSourcesSpec :: Maybe SourcesSpec
    }
  deriving (Generic, PrettyVal)

-- | How to execute a command?
data CommandBody =
    -- | Run the specified program. This is for regular commands.
    CmdProg ClosedProg

    -- | Internal command (primarily for debugging)
  | CmdInternal InternalCommand
  deriving (Generic, PrettyVal)

-- | Internal commands
--
-- These don't appear in the help output.
data InternalCommand =
    DumpDbInfo PlaintextPassword
  | RebuildCanReadCache PlaintextPassword
  | GetServerURL
  deriving (Generic, PrettyVal)

-- | Default command
execProg :: ClosedProg -> Command
execProg prog = Command {
      cmdShowVersion = False
    , cmdInput       = Nothing
    , cmdSourcesSpec = Nothing
    , cmdBody        = CmdProg prog
    }

-- | Internal commands
internalCommand :: InternalCommand -> Command
internalCommand cmd = Command {
      cmdShowVersion = False
    , cmdInput       = Nothing
    , cmdSourcesSpec = Nothing
    , cmdBody        = CmdInternal cmd
    }

showVersionCommand :: Bool -> Command
showVersionCommand showVersion =
    (execProg skip) { cmdShowVersion = showVersion }
  where
    skip :: ClosedProg
    skip = ClosedProg $ Pretty NoOutput done

getCmdline :: IO Cmdline
getCmdline = do
    args <- getArgs
    if null args
      then showWelcome >> exitFailure
      else do
        let parserResult = execParserPure defaultPrefs opts args
        cmdline <- handleParseResult $ overFailure addFooter parserResult
        when (dumpCmdline cmdline) $ putStrLn $ dumpStr cmdline
        return cmdline
  where
    opts :: ParserInfo Cmdline
    opts = info (helper <*> parseCmdline) $ mconcat [
        fullDesc
      , header "Ingest new data sources into the database"
      ]

    addFooter :: ParserHelp -> ParserHelp
    addFooter h =
      if not (isEmpty (helpBody h)) -- proxy for finding out if --help was used
        then h
        else h { helpFooter = stringChunk "Use datalake --help for more information." }

showWelcome :: IO ()
showWelcome = putStr $ unlines [
      "Ingest new data sources into the datalake"
    , ""
    , "EXAMPLE USAGE PATTERNS"
    , ""
    , "  datalake [-d description] -n proteinatlas/cancer sources/proteinatlas/cancer.csv"
    , ""
    , "    Imports a local file, using the name 'proteinatlas/cancer'"
    , "    Optionally you can specify a description of the data source using -d."
    , ""
    , "  datalake [-d description] -n embl/genes http://ftp.ebi.ac.uk/pub/genes.rpkm.tsv"
    , ""
    , "    Imports a remote file, using the name 'embl-ebi/genes.rpkm'"
    , ""
    , "  datalake list-sources"
    , ""
    , "    List all available sources"
    , ""
    , "  datalake show-source proteinatlas/cancer"
    , ""
    , "    Show information about a source with a specific name"
    , ""
    , "CONFIGURATION"
    , ""
    , "Ingest needs to know which database to use, and looks for this information"
    , "in a file called 'datalake.yaml' in the current directory. Your database"
    , "administrator can provide you with this file."
    , ""
    , "MORE INFORMATION"
    , ""
    , "For detailed information about the supported command line arguments, run"
    , ""
    , "  datalake --help"
    ]

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline = Cmdline
    <$> parseSkeletonCmdline
    <*> (switch $ mconcat [
            long "dump-cmdline"
          , help "Dump command line options on startup (debugging)"
          , internal
          ])
    <*> asum [
            parseShowVersion
          , parseInternalCommand
          , parseCommand
          ]
    <*> parseClientConfig

parseCreateIndices :: Parser CreateIndices
parseCreateIndices =
    (flag True False $ mconcat [
         long "no-indices"
       , help "Don't create indices for the columns of the table"
       ])

parseIngestPrivate :: Parser IngestPrivate
parseIngestPrivate =
    (flag False True $ mconcat [
         long "private"
       , help "Ingest as a private data source"
       ])

parseSourceVersion :: Parser (SourceName, Maybe Version)
parseSourceVersion = (flip (,))
    <$> (optional . option readVersion $ mconcat [
             short 'v'
           , help "Version (if not specified, use latest)"
           , metavar "VERSION"
           ])
    <*> argument readString (metavar "SOURCE")

-- TODO: This does not yet allow for custom conversion functions
parseColumnType :: Parser ColumnType
parseColumnType = mkFieldType
    <$> argument str (metavar "TYPE")
  where
    mkFieldType :: String -> ColumnType
    mkFieldType "BOOLEAN"          = ColBool
    mkFieldType "INTEGER"          = ColInt I4
    mkFieldType "BIGINT"           = ColInt I8
    mkFieldType "DOUBLE PRECISION" = ColReal
    mkFieldType "TEXT"             = ColText
    mkFieldType typ                = ColCustom $ CustomType typ Nothing

parseSourceName :: Parser SourceName
parseSourceName = strOption $ mconcat [
      long "name"
    , short 'n'
    , help "Source name. Sources can be ingested with the same name more than once and will automatically be versioned."
    ]

parseIngestOptions :: Parser IngestOptions
parseIngestOptions = IngestOptions
    <$> (optional parseFileType)
    <*> parseHasHeaders
    <*> (flag True False $ mconcat [
             long "disable-quote-char"
           , help "Treat '\"' as ordinary character (not RFC compliant)."
           ])
    <*> optional parseDecompressMethod
    <*> (flag True False $ mconcat [
             long "no-type-inference"
           , help "Disable type inference"
           ])
    <*> (option readJsonPath $ mconcat [
             long "json-path"
           , help "Ingest part of a JSON file. A path is either '_', '[path]' or '{\"field\":path}'. For example, '{\"groups\":[_]}' extracts all elements of the array of the \"groups\" field of the top-level object as separate values into the database."
           , value P_
           ])
    <*> parseEncoding
    <*> optional parseForeignIdentifier
    <*> (optional . fmap mkSourceIdentifier . strOption $ mconcat [
             long "source-identifier"
           , help "A unique identifier for the source. If an existing source has the same identifier, that source will be returned directly instead of proceeding with the ingestion. If no such source exists, the ingest will proceed normally and the new source will be tagged with the identifier."
           ])

parseCreateOptions :: Parser CreateOptions
parseCreateOptions = CreateOptions
    <$> (flag True False $ mconcat [
             long "no-typed"
           , help "Skip the creation of the typed table. The typed table can be created later with make-typed."
           ])
    <*> (optional . strOption $ mconcat [
             long "description"
           , short 'd'
           , help "Free-form description for this data source. If not provided, use the source name instead."
           ])
    <*> (many . strOption $ mconcat [
             long "tag"
           , help "Apply a tag to the source. Can be used multiple times."
           ])
    <*> parseCreatedTime
    <*> (option auto $ mconcat [
             long "log-every"
           , value (logEvery def)
           , help "Show progress message every n records."
           ])

parseInput :: Parser (Input FilePath)
parseInput = argument (inputFromFilePath <$> str) $ metavar "INPUT"

parseForeignIdentifier :: Parser (String, String)
parseForeignIdentifier = (,)
    <$> (strOption $ mconcat [
            long "source-metadata-name"
          , help "Name of the external source containing the metadata"
          ])
    <*> (strOption $ mconcat [
            long "source-metadata-field"
          , help "Field of the external source containing the metadata"
          ])

parseFileType :: Parser FileType
parseFileType = asum [
      flag' (FileTypeTabular DelimComma) $ mconcat [
          long "comma"
        , help "File is comma-separated tabular data (.csv files)"
        ]
    , flag' (FileTypeTabular DelimTab) $ mconcat [
          long "tab"
        , help "File is tab-separated tabular data (.tsv files)"
        ]
    , flag' FileTypeJSON $ mconcat [
          long "json"
        , help "File is JSON data"
        ]
    ]

parseEncoding :: Parser Encoding
parseEncoding = asum [
      flag' Latin1 $ mconcat [
          long "latin1"
        , help "File is Latin1 encoded"
        ]
    , flag' UTF8 $ mconcat [
          long "UTF8"
        , help "File is UTF8 encoded (this is the default)"
        ]
    , pure UTF8
    ]

parseDecompressMethod :: Parser DecompressMethod
parseDecompressMethod = asum [
      flag' UnzipSingle $ mconcat [
          long "unzip"
        , help "Input is a .zip file containing a single file (this flag is enabled by default if the file extension is .zip)"
        ]
    ]

-- | Parse 'HasHeaders'
--
-- We don't want the user to /have to/ specify the number of rows to look at
-- when there is no header information; but since we are using an applicative
-- infrastructure for parsing, not a monadic, we cannot express that the
-- @--peek-at@ argument should only be present if @--no-headers@ is used.
parseHasHeaders :: Parser HasHeaders
parseHasHeaders = aux <$> go
  where
    aux :: (Bool, Int) -> HasHeaders
    aux (True,  _) = HasHeaders
    aux (False, n) = NoHeaders n

    go :: Parser (Bool, Int)
    go = (,) <$> (flag True False $ mconcat [
                     long "no-headers"
                   , help "The CSV file has no header row"
                   ])
             <*> (option auto $ mconcat [
                     long "peek-at"
                   , metavar "NUM"
                   , value 1000
                   , showDefault
                   , help "Peek at NUM rows of the input to determine data format. Only used when --no-headers is present."
                   ])

parseCreatedTime :: Parser CreatedTime
parseCreatedTime = asum [
      CreatedTimeFixed <$> (option readTimestamp $ mconcat [
          long "created"
        , help "Specify the time this table was created (mostly for unit testing purposes). Example: \"2016-10-01 12:00:00\""
        , internal
        ])
    , pure CreatedTimeNow
    ]

{-------------------------------------------------------------------------------
  Parse internal commands
-------------------------------------------------------------------------------}

parseInternalCommand :: Parser Command
parseInternalCommand = subparser $ mconcat [
      internal
    , command "dump-db-info" $ info (helper <*> parseDumpDbInfo) $
        progDesc "Dump database info (primarily for unit testing purposes)"
    , command "rebuild-can-read-cache" $ info (helper <*> parseRebuildCanRead) $
        progDesc "Rebuild the can-read cache (primarily for debugging)"
    , command "get-server-url" $ info (helper <*> parseGetServerURL) $
        progDesc "Show the URL of the Datalake server"
    ]

parseDumpDbInfo :: Parser Command
parseDumpDbInfo =
    (internalCommand . DumpDbInfo) <$> parseDbAdminPass

parseRebuildCanRead :: Parser Command
parseRebuildCanRead =
    (internalCommand . RebuildCanReadCache) <$> parseDbAdminPass

parseGetServerURL :: Parser Command
parseGetServerURL = pure $ internalCommand GetServerURL

{-------------------------------------------------------------------------------
  Parse configuration
-------------------------------------------------------------------------------}

parseClientConfig :: Parser ClientConfig
parseClientConfig = ClientConfig
    <$> optional (strOption $ mconcat [
            long "host"
          , help "datalake-server host"
          ])
    <*> optional (option auto $ mconcat [
            long "port"
          , help "datalake-server port"
          ])
    <*> optional (flag' True $ mconcat [
            long "secure"
          , help "Connect over SSL"
          ])
    <*> optional (flag' False $ mconcat [
            long "ignore-cert"
          , help "Don't verify SSL certificate"
          ])

{-------------------------------------------------------------------------------
  Parse commands
-------------------------------------------------------------------------------}

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
      command "login" $ info (helper <*> parseLogin) $
        progDesc "Obtain session token to use with --resume."
    , command "logout" $ info (helper <*> parseLogout) $
        progDesc "Terminate session started using login."
    , command "ingest" $ info (helper <*> parseIngest) $
        progDesc "Ingest a datasource."
    , command "list-sources" $ info (helper <*> parseListSources) $
        progDesc "List all available sources."
    , command "show-source" $ info (helper <*> parseShowSource) $
        progDesc "Show information about a specific source."
    , command "delete-source" $ info (helper <*> parseDeleteSource) $
        progDesc "Delete a particular source."
    , command "make-typed" $ info (helper <*> parseMakeTyped) $
        progDesc "Construct typed table."
    , command "set-type" $ info (helper <*> parseSetType) $
        progDesc "Override inferred type for a column."
    , command "tag" $ info (helper <*> parseTagSource) $
        progDesc "Tag a source."
    , command "untag" $ info (helper <*> parseUntagSource) $
        progDesc "Untag a source"
    , command "infer-json-type" $ info (helper <*> parseInferJsonType) $
        progDesc "Infer type of JSON file."
    , command "manage" $ info (helper <*> parseManageDataset) $
        progDesc "Manage a dataset (e.g. change permissions)."
    , command "create-group" $ info (helper <*> parseCreateGroup) $
        progDesc "Create a new group"
    , command "manage-group" $ info (helper <*> parseManageGroup) $
        progDesc "Manage a group (e.g. change membership)."
    , command "manage-user" $ info (helper <*> parseManageUser) $
        progDesc "Manage a user (e.g. grant or revoke privileges)."
    , command "download" $ info (helper <*> parseDownload) $
        progDesc "Download a previously ingested source"
    , command "compact" $ info (helper <*> parseCompact) $
        progDesc "Compact previously ingested sources"
    ]

parseLogin :: Parser Command
parseLogin = aux
    <$> parseCredentials
    <*> argument str (metavar "FILE")
  where
    aux :: Credentials -> FilePath -> Command
    aux creds fp =
      execProg $ ClosedProg $
        P.Login (Left creds) $
        GetAuthToken fp      $
        Pretty (SimpleOutput "ok") done

parseLogout :: Parser Command
parseLogout = aux
    <$> argument str (metavar "FILE")
  where
    aux :: FilePath -> Command
    aux fp =
      execProg $ ClosedProg $
        P.Login (Right fp) $
        Logout             $
        Pretty (SimpleOutput "ok") done

parseIngest :: Parser Command
parseIngest = aux
    <$> parseAuthMethod
    <*> parseSourceName
    <*> parseIngestOptions
    <*> parseCreateOptions
    <*> parseCreateIndices
    <*> parseIngestPrivate
    <*> parseInput
  where
    aux :: AuthMethod
        -> SourceName
        -> IngestOptions
        -> CreateOptions
        -> CreateIndices
        -> IngestPrivate
        -> Input FilePath
        -> Command
    aux authMethod sourceName ingestOpts createOpts createIndices private input = (\cmd -> cmd { cmdInput = Just input }) $
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        Ingest sourceName ingestOpts createOpts createIndices private input $ \result ->
        logout                                             $
        Pretty (Output pretty result) done

parseManageDataset :: Parser Command
parseManageDataset = aux
    <$> parseAuthMethod
    <*> parseSourceVersion
    <*> parseManageDataset'
  where
    aux :: AuthMethod
        -> (SourceName, Maybe Version)
        -> ManageDataset
        -> Command
    aux authMethod (sourceName, mVersion) change =
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        GetSourceName sourceName               $ \source ->
        GetVersion source mVersion             $ \sourceIx ->
        ManageDataset source sourceIx [change] $
        logout                                 $
        Pretty (SimpleOutput (ok change)) done

    ok :: ManageDataset -> Doc
    ok change = pretty change PP.<+> " OK"

parseCreateGroup :: Parser Command
parseCreateGroup = aux
    <$> parseAuthMethod
    <*> parseGroupName
  where
    aux :: AuthMethod -> GroupName -> Command
    aux authMethod groupName =
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        CreateGroup groupName $
        logout                $
        Pretty (SimpleOutput "Group created") done

parseManageGroup :: Parser Command
parseManageGroup = aux
    <$> parseAuthMethod
    <*> parseGroupName
    <*> parseManageGroup'
  where
    aux :: AuthMethod
        -> GroupName -> ManageGroup -> Command
    aux authMethod groupName change =
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        ManageGroup groupName [change] $
        logout                         $
        Pretty (SimpleOutput (ok change)) done

    ok :: ManageGroup -> Doc
    ok change = pretty change PP.<+> " OK"

parseManageUser :: Parser Command
parseManageUser = aux
    <$> parseDbAdminPass
    <*> parseManageUser'
  where
    aux :: PlaintextPassword -> ManageUser -> Command
    aux adminPass change =
      execProg $ ClosedProg $
        LoginAdmin adminPass $
        ManageUser [change]  $
        Pretty (SimpleOutput (ok change)) done

    ok :: ManageUser -> Doc
    ok change = pretty change PP.<+> " OK"

parseListSources :: Parser Command
parseListSources = aux
    <$> parseAuthMethod
    <*> parseSourcesSpec
  where
    aux :: AuthMethod -> SourcesSpec -> Command
    aux authMethod spec = (\cmd -> cmd { cmdSourcesSpec = Just spec }) $
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        GetSources spec $ \sources ->
        logout          $
        Pretty (Output pretty sources) done

parseShowSource :: Parser Command
parseShowSource = aux
    <$> parseAuthMethod
    <*> parseSourceVersion
  where
    aux :: AuthMethod
        -> (SourceName, Maybe Version) -> Command
    aux authMethod (sourceName, version) =
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        GetSourceName sourceName  $ \source ->
        GetVersion source version $ \sourceIx ->
        GetSourceInfo sourceIx    $ \sourceInfo ->
        logout                    $
        Pretty (Output pretty sourceInfo) done

parseDeleteSource :: Parser Command
parseDeleteSource = aux
    <$> parseAuthMethod
    <*> parseSourceVersion
  where
    aux :: AuthMethod
        -> (SourceName, Maybe Version) -> Command
    aux authMethod (sourceName, mVersion) =
      case mVersion of
        Nothing ->
          execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
            GetSourceName sourceName  $ \source ->
            GetAllVersions source     $ \sourceIxs ->
            DeleteSources sourceIxs   $
            logout                    $
            Pretty (SimpleOutput ("Deleted all versions of source"
                              PP.<+> pretty sourceName)) done
        Just version ->
          execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
            GetSourceName sourceName            $ \source ->
            GetVersion source (Just version)    $ \sourceIx ->
            DeleteSources (fmap (:[]) sourceIx) $
            logout                              $
            Pretty (SimpleOutput ("Deleted version"
                              PP.<+> pretty version
                              PP.<+> "of source"
                              PP.<+> pretty sourceName)) done

parseDownload :: Parser Command
parseDownload = aux
    <$> parseAuthMethod
    <*> parseSourceVersion
  where
    aux :: AuthMethod -> (SourceName, Maybe Version) -> Command
    aux authMethod (sourceName, version) =
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        GetSourceName sourceName  $ \source ->
        GetVersion source version $ \sourceIx ->
        DownloadSource sourceIx   $
        logout                    $
        Pretty NoOutput done

parseCompact :: Parser Command
parseCompact = aux
    <$> parseAuthMethod
    <*> parseSourceName
    <*> parseCreateOptions
    <*> parseCreateIndices
    <*> parseIngestPrivate
    <*> some parseSourceVersion
  where
    aux :: AuthMethod
        -> SourceName
        -> CreateOptions
        -> CreateIndices
        -> IngestPrivate
        -> [(SourceName, Maybe Version)]
        -> Command
    aux authMethod sourceName createOpts createIndices private sources =
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        getSourceIxs sources $ \ixs ->
        Compact sourceName createOpts createIndices private ixs $ \result ->
        logout $
        Pretty (Output format result) done

    format :: SourceInfo -> Doc
    format sourceInfo = PP.vcat [
        "Compacted sources. Resulting source:"
      , pretty sourceInfo
      ]

    -- For each source name, fetch the source index
    getSourceIxs :: (Applicative f) => [(SourceName, Maybe Version)]
                -> ((f [SourceIx]) -> Prog f (f Done))
                -> Prog f (f Done)
    getSourceIxs sources k = go k (pure []) sources

    go :: (Applicative f)
       => (f [SourceIx] -> Prog f (f Done))
       -> f [SourceIx]
       -> [(SourceName, Maybe Version)]
       -> Prog f (f Done)
    go k = fix $ \f acc -> \case
      [] -> k (acc)
      ((sourceName, Nothing):xs) ->
          GetSourceName sourceName $ \source ->
          GetAllVersions source    $ \sourceIxs ->
            f (pure (++) <*> sourceIxs <*> acc) xs
      ((sourceName, Just version):xs) ->
          GetSourceName sourceName         $ \source ->
          GetVersion source (Just version) $ \sourceIx ->
            f (pure (:)  <*> sourceIx <*> acc) xs

parseMakeTyped :: Parser Command
parseMakeTyped = aux
    <$> parseAuthMethod
    <*> parseSourceVersion
    <*> parseCreateIndices
  where
    aux :: AuthMethod
        -> (SourceName, Maybe Version)
        -> CreateIndices
        -> Command
    aux authMethod (sourceName, version) createIndices =
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        GetSourceName sourceName         $ \source ->
        GetVersion source version        $ \sourceIx ->
        MakeTyped sourceIx createIndices $ \result ->
        logout                           $
        Pretty (Output format result) done

    format :: SourceInfo -> Doc
    format sourceInfo = PP.vcat [
        "Created typed table. Updated info:"
      , pretty sourceInfo
      ]

parseSetType :: Parser Command
parseSetType = aux
    <$> parseAuthMethod
    <*> parseSourceVersion
    <*> (option readString $ mconcat [
            short 'c'
          , help "Column name (use show-source to see the column names)"
          ])
    <*> parseColumnType
  where
    aux :: AuthMethod
        -> (SourceName, Maybe Version) -> ColumnName -> ColumnType -> Command
    aux authMethod (sourceName, version) columnName typ =
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        GetSourceName sourceName      $ \source ->
        GetVersion source version     $ \sourceIx ->
        GetColumn sourceIx columnName $ \columnIx ->
        SetColumnType columnIx typ    $
        logout                        $
        Pretty (SimpleOutput $ "Set type to" PP.<+> pretty typ) done

parseTagSource :: Parser Command
parseTagSource = aux
    <$> parseAuthMethod
    <*> parseSourceVersion
    <*> argument str (metavar "TAG")
  where
    aux :: AuthMethod
        -> (SourceName, Maybe Version) -> TagName -> Command
    aux authMethod (sourceName, version) tag =
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        GetSourceName sourceName  $ \source ->
        GetVersion source version $ \sourceIx ->
        TagSource sourceIx tag    $
        logout                    $
        Pretty (SimpleOutput $ "Tag" PP.<+> pretty tag PP.<+> "added") done

parseUntagSource :: Parser Command
parseUntagSource = aux
    <$> parseAuthMethod
    <*> parseSourceVersion
    <*> argument str (metavar "TAG")
  where
    aux :: AuthMethod
        -> (SourceName, Maybe Version) -> TagName -> Command
    aux authMethod (sourceName, version) tag =
      execProg $ ClosedProg $ withAuthMethod authMethod $ \logout ->
        GetSourceName sourceName  $ \source ->
        GetVersion source version $ \sourceIx ->
        UntagSource sourceIx tag  $
        logout                    $
        Pretty (SimpleOutput $ "Tag" PP.<+> pretty tag PP.<+> "removed") done

parseInferJsonType :: Parser Command
parseInferJsonType = aux <$> parseInput
  where
    -- TODO: Support DecompressMethod
    aux :: Input FilePath -> Command
    aux input =
      execProg $ ClosedProg $
        InferJsonType Nothing input $ \typ ->
        Pretty (Output pretty typ) done

parseShowVersion :: Parser Command
parseShowVersion = fmap showVersionCommand $ switch $ mconcat
      [ long "version"
      , help "Show version and exit"
      ]

parseSourcesSpec :: Parser SourcesSpec
parseSourcesSpec = SourcesSpec
    <$> (optional . option auto $ mconcat [
            long "offset"
          , help "Offset (starting row number)"
          ])
    <*> (optional . option auto $ mconcat [
            long "limit"
          , help "Limit (number of rows)"
          ])
    <*> (optional . option (tsQueryFromString <$> str) $ mconcat [
            long "search"
          , help "Search for sources matching the specified query"
          ])
    <*> (optional . option readIx $ mconcat [
            long "ix"
          , help "Only show the source with the specified id"
          ])
    <*> (many . strOption $ mconcat [
            long "tag"
          , help "Only show sources with the specified tag"
          ])
    <*> (optional . strOption $ mconcat [
            long "description"
          , help "Only show sources matching the specified description"
          ])
    <*> (optional . strOption $ mconcat [
            long "name"
          , help "Only show sources matching the specified name"
          ])
    <*> (many . strOption $ mconcat [
            long "user"
          , help "Only show sources uploaded by the specified user"
          ])
    <*> (many . option readString $ mconcat [
            long "column"
          , help "Only show sources with the specified column"
          ])
    <*> (optional . option readTimestamp $ mconcat [
             long "created-after"
           , help "Only show sources created after the specified date"
           ])
    <*> (optional . option readTimestamp $ mconcat [
             long "created-before"
           , help "Only show sources created before the specified date"
           ])
    <*> (defaultOrderBy <$> many parseOrderBy)
    <*> (switch $ mconcat [
             long "include-deprecated"
           , help "Include deprecated sources"
           ])
    <*> pure Nothing
  where
    defaultOrderBy :: [(SourcesColumn, SortDirection)]
                   -> [(SourcesColumn, SortDirection)]
    defaultOrderBy [] = [(SourcesCreated, Ascending)]
    defaultOrderBy cs = cs

parseOrderBy :: Parser (SourcesColumn, SortDirection)
parseOrderBy = asum [
      option (sort Ascending <$> readSourcesColumn) $ mconcat [
          long "order-asc"
        , help "Order by the specified column (ascending)"
        ]
    , option (sort Descending <$> readSourcesColumn) $ mconcat [
          long "order-desc"
        , help "Order by the specified column (descending)"
        ]
    ]
  where
    sort :: SortDirection -> a -> (a, SortDirection)
    sort dir a = (a, dir)

parseDbAdminPass :: Parser PlaintextPassword
parseDbAdminPass = parseDbAdminPass' ""

parseDbAdminPass' :: String -> Parser PlaintextPassword
parseDbAdminPass' prefix = option readPassword $ mconcat [
      long $ prefix ++ "db-admin-pass"
    , help "Database administrator password"
    ]

parseGroupName :: Parser GroupName
parseGroupName = strOption $ mconcat [
      long "group"
    , help "Name of the group"
    , metavar "GROUP"
    ]

{-------------------------------------------------------------------------------
  Manage datasets and groups
-------------------------------------------------------------------------------}

parseManageDataset' :: Parser ManageDataset
parseManageDataset' = asum [
      SetGroupAccessLevel
        <$> (strOption $ mconcat [
                long "set-group-access"
              , help "Set group access level"
              , metavar "GROUP"
              ])
        <*> parseDatasetAccessLevel
    , SetUserAccessLevel
        <$> (strOption $ mconcat [
                long "set-user-access"
              , help "Set user access level"
              , metavar "USER"
              ])
        <*> parseDatasetAccessLevel
    , flag' (SetDeprecated True) $ mconcat [
          long "deprecated"
        , help "Mark the source as deprecated"
        ]
    , flag' (SetDeprecated False) $ mconcat [
          long "not-deprecated"
        , help "Remove the deprecated annotation"
        ]
    , flag' (SetPublic True) $ mconcat [
          long "public"
        , help "Mark source as public"
        ]
    , flag' (SetPublic False) $ mconcat [
          long "private"
        , help "Mark source as private"
        ]
    ]

parseManageGroup' :: Parser ManageGroup
parseManageGroup' = asum [
      AddUser <$> (strOption $ mconcat [
          long "add-user"
        , help "Add user to a group"
        , metavar "USER"
        ])
    , RemoveUser <$> (strOption $ mconcat [
          long "remove-user"
        , help "Remove user from a group"
        , metavar "USER"
        ])
    , GrantManageGroup <$> (strOption $ mconcat [
          long "grant-manage"
        , help "Grant MANAGEGROUP privileges to a user"
        , metavar "USER"
        ])
    , RevokeManageGroup <$> (strOption $ mconcat [
          long "revoke-manage"
        , help "Revoke MANAGEGROUP privileges from a user"
        , metavar "USER"
        ])
    ]

parseManageUser' :: Parser ManageUser
parseManageUser' = asum [
      GrantCreateSource <$> (strOption $ mconcat [
          long "grant-create"
        , help "Grant CREATE privileges (the ability to create new datasets)"
        , metavar "USER"
        ])
    , RevokeCreateSource <$> (strOption $ mconcat [
          long "revoke-create"
        , help "Revoke CREATE privileges"
        , metavar "USER"
        ] )
    , GrantCreateGroup <$> (strOption $ mconcat [
          long "grant-create-group"
        , help "Grant CREATEGROUP privileges (the ability to create new groups)"
        , metavar "USER"
        ])
    , RevokeCreateGroup <$> (strOption $ mconcat [
          long "revoke-create-group"
        , help "Revoke CREATEGROUP privileges"
        , metavar "USER"
        ])
    , CreateUser <$> (strOption $ mconcat [
          long "create-user"
        , help "Create a new user. Careful: we have no way of verifying the username."
        , metavar "USER"
        ])
    ]

parseDatasetAccessLevel :: Parser DatasetAccessLevel
parseDatasetAccessLevel = asum [
      flag' DatasetAccessLevelNone $ mconcat [
          long "none"
        , help "No access at all"
        ]
    , flag' DatasetAccessLevelRead $ mconcat [
          long "read"
        , help "READ: Read-only access"
        ]
    , flag' DatasetAccessLevelUpdate $ mconcat [
          long "update"
        , help "UPDATE: Allowed to upload new versions of the dataset"
        ]
    , flag' DatasetAccessLevelManage $ mconcat [
          long "manage"
        , help "MANAGE: Allowed to manage the dataset (e.g. change permissions)"
        ]
    ]

{-------------------------------------------------------------------------------
  How does the user want to authenticate?
-------------------------------------------------------------------------------}

data AuthMethod =
    -- | Specify explicit credentiails
    AuthCreds Credentials

    -- | Resume a previous session (given path of a file with the auth token)
  | AuthResume FilePath

    -- | Don't authenticate
  | AuthNone

parseAuthMethod :: Parser AuthMethod
parseAuthMethod = asum [
      AuthCreds <$> parseCredentials
    , AuthResume <$> (strOption $ mconcat [
          long "resume"
        , metavar "FILE"
        , help "Resume session previously started using the \"login\" command."
        ])
    , pure AuthNone
    ]

parseCredentials :: Parser Credentials
parseCredentials = Credentials
    <$> (strOption $ mconcat [
             long "user"
           , short 'u'
           , help "User name"
           ])
    <*> (strOption $ mconcat [
             long "pass"
           , short 'p'
           , help "Password"
           ])

-- | Wrap the program with @Login .. Logout@ if credentials are passed,
-- or just with @Login ..@ if we are resuming an existing session.
withAuthMethod :: AuthMethod
               -> ((Prog f a -> Prog f a) -> Prog f a)
               -> Prog f a
withAuthMethod (AuthCreds  creds) prog = P.Login (Left creds) (prog Logout)
withAuthMethod (AuthResume fp)    prog = P.Login (Right fp)   (prog id)
withAuthMethod AuthNone           prog = prog id

{-------------------------------------------------------------------------------
  optparse-applicative auxiliary: readers
-------------------------------------------------------------------------------}

readString :: IsString a => ReadM a
readString = fromString <$> str

readVersion :: ReadM Version
readVersion = Version <$> auto

-- TODO: I think this currently cannot contain any spaces; not because of
-- the parsec parser, but because of optparse-applicative.
readJsonPath :: ReadM JsonPath
readJsonPath = do
    pathStr <- str
    case parseJsonPath pathStr of
      Left err -> fail $ "Cannot parse '" ++ pathStr ++ "': " ++ err
      Right p  -> return p

-- TODO: The format for timestamps is not very user friendly
readTimestamp :: ReadM Timestamp
readTimestamp = do
    timeStr <- str
    Timestamp <$> parseTimeM True defaultTimeLocale "%F %T" timeStr

readSourcesColumn :: ReadM SourcesColumn
readSourcesColumn = do
    scStr <- str
    case sourcesColumnFromString scStr of
      Just sc -> return sc
      Nothing -> fail $ "Invalid SourcesColumn " ++ show scStr

readIx :: ReadM Ix
readIx = (toEnum . read) <$> str

readPassword :: ReadM PlaintextPassword
readPassword = PlaintextPassword <$> str

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Check if this is a local file, or if not, if it an be parsed as a URI
--
-- IMPLEMENTATION NOTE: Uses 'unsafePerformIO' as a shortcut to distinguishing
-- between local and remote paths. We check if the file exists, and if it does,
-- we record it as a local path; otherwise, we regard it as remote if it looks
-- a bit like a URL.
--
-- This is a little bold but syntactically distinguishing is a bit painful.
-- (We cannot use IO in optparse-applicative parsers.)
inputFromFilePath :: FilePath -> Input FilePath
inputFromFilePath fp = unsafePerformIO $ do
    exists <- doesFileExist fp
    case
      ( exists
      , looksLikeURL
      , looksLikeS3
      , parseURI fp
      , parseURI ("http://" ++ fp)
      , parseS3
      ) of
      (True, _, _, _, _, _)        -> return $ Upload (takeFileName fp) fp
      -- Looks like a url, and _is_ a URL
      (_, True, _   , Just uri, _       , _       ) -> return $ Remote uri
      -- Looks like a url, and is a URL when prefixed with @http@
      (_, True, _   , _       , Just uri, _       ) -> return $ Remote uri
      -- Looks like S3, and _is_ a URL
      (_, _   , True, Just uri, _       , _       ) -> return $ Remote uri
      -- Looks like S3, and is a URL when bucket and okey are URL encoded
      (_, _   , True, _       , _       , Just uri) -> return $ Remote uri
      _otherwise             -> throwIO $ UnrecognizedInput fp
  where
    looksLikeURL :: Bool
    looksLikeURL = '.' `elem` fp || "::" `isInfixOf` fp
    looksLikeS3 :: Bool
    looksLikeS3 = isPrefixOf "s3://" fp

    -- The server expects an URI like this:
    -- @s3://bucket/okey@ where both @bucket@ and @okey@ are URL encoded.
    parseS3 :: Maybe URI
    parseS3 = do
      rest <- stripPrefix "s3://" fp

      (bucket, okey) <- do
        let res@(bucket, okey) = break (== '/') rest
        if null bucket || null okey then Nothing else Just res

      let
        bucket' = URI.encode bucket
        okey' = URI.encode okey

      parseURI ("s3://" ++ bucket' ++ "/" ++ okey')

newtype UnrecognizedInputException = UnrecognizedInput FilePath
  deriving Show

instance Exception UnrecognizedInputException

instance FriendlyException UnrecognizedInputException where
  displayFriendly (UnrecognizedInput fp) =
    "Unrecognized input " ++ fp ++ ". Does the file exist?"
