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
module Pfizer.Datalake.Server.AppSkeleton (
    SkeletonCmdline(..)
  , appSkeleton
  , parseSkeletonCmdline
  , InvalidConfigFile(..)
  ) where

import Control.Exception
import Control.Monad
import Data.Foldable (asum)
import GHC.Generics
import System.Directory
import System.IO
import Options.Applicative
import Text.Show.Pretty
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.Yaml                  as Y

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Application skeleton
-------------------------------------------------------------------------------}

-- | Cmdline arguments that the skeleton needs
data SkeletonCmdline = SkeletonCmdline {
      -- | PostgreSQL connection settings
      pgConnection :: ConnectionConfig

      -- | Path to configuration file
    , configFile :: Maybe FilePath

      -- | Which log messages do we want to see?
    , logLevel :: LogLevel
    }
  deriving (Generic, PrettyVal)

-- | Application skeleton
appSkeleton :: SkeletonCmdline
            -> (IO Connection -> Schema -> IO ())
            -> IO ()
appSkeleton SkeletonCmdline{..} app = do
    -- Set encoding to UTF8
    -- We always output in UTF8; explicitly setting the encoding to
    -- UTF8 is necessary on Windows (and doesn't hurt on Linux).
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    -- Read configuration (if any)
    filePGConfig <- readConfigFile configFile
    let config@ConnectionConfig{..} = mappend pgConnection filePGConfig
        schema      = maybe "public"  fromString cSchema
        sqlLogLevel = maybe "WARNING" fromString cLogLevel
        settings = connectionSettings config
    when (logLevel <= Debug) $
      hPutStrLn stderr $ "Using database configuration " ++ dumpStr config

    -- Make sure to set logging options on every connection
    let mkConn :: IO Connection
        mkConn = do
          conn <- newConnection settings
          _ <- PG.execute_ conn $ "SET client_min_messages=" <> sqlLogLevel
          return conn

    -- App proper
    app mkConn schema

{-------------------------------------------------------------------------------
  Parse command line options
-------------------------------------------------------------------------------}

parseSkeletonCmdline :: Parser SkeletonCmdline
parseSkeletonCmdline = SkeletonCmdline
    <$> parseConnectionConfig (Just "pg")
    <*> (optional . strOption $ mconcat [
            long "config"
          , help "Read configuration file FILE instead of ./ingest.yaml"
          , metavar "FILE"
          ])
    <*> parseLogLevel

parseLogLevel :: Parser LogLevel
parseLogLevel = asum [
      flag' Warning $ mconcat [
          long "quiet"
        , help "Suppress progress messages (but not warnings and errors)"
        ]
    , flag' Debug $ mconcat [
          long "verbose"
        , help "Verbose output"
        ]
    , pure Notice
    ]

{-------------------------------------------------------------------------------
  Read configuration file
-------------------------------------------------------------------------------}

-- | Read configuration file if one is specified or default one exists
readConfigFile :: Maybe FilePath -> IO ConnectionConfig
readConfigFile mConfigFile = do
    shouldRead <- case mConfigFile of
                    Just configFile -> return $ Just configFile
                    Nothing -> do
                      exists <- doesFileExist defaultPath
                      if exists
                        then return $ Just defaultPath
                        else return Nothing
    case shouldRead of
      Nothing -> return mempty
      Just fp -> do mConfig <- Y.decodeFileEither fp
                    case mConfig of
                      Left  ex  -> throwIO $ InvalidConfigFile fp ex
                      Right cfg -> return cfg
  where
    defaultPath :: FilePath
    defaultPath = "ingest.yaml"

data InvalidConfigFile = InvalidConfigFile FilePath Y.ParseException
  deriving (Show)

instance Exception InvalidConfigFile

instance FriendlyException InvalidConfigFile where
  displayFriendly (InvalidConfigFile fp ex) =
       "Could not parse configuration file " ++ show fp
    ++ ": "
    ++ Y.prettyPrintParseException ex
