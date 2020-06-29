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
module Lagoon.Client.AppSkeleton (
    SkeletonCmdline(..)
  , ClientConfig(..)
  , Port
  , appSkeleton
  , parseSkeletonCmdline
  , InvalidConfigFile(..)
  ) where

import Control.Exception
import Data.Aeson
import Data.Default
import Data.Foldable (asum)
import GHC.Generics
import System.Directory
import System.IO
import Options.Applicative
import Text.Show.Pretty
import qualified Data.Yaml as Y

import Lagoon.Interface

{-------------------------------------------------------------------------------
  Application skeleton
-------------------------------------------------------------------------------}

-- | Cmdline arguments that the skeleton needs
data SkeletonCmdline = SkeletonCmdline {
      -- | Path to configuration file
      configFile :: Maybe FilePath

      -- | Which log messages do we want to see?
    , logLevel :: LogLevel
    }
  deriving (Generic, PrettyVal)

-- | Application skeleton
appSkeleton :: SkeletonCmdline -> (ClientConfig -> IO ()) -> IO ()
appSkeleton SkeletonCmdline{..} app = do
    -- Set encoding to UTF8
    -- We always output in UTF8; explicitly setting the encoding to
    -- UTF8 is necessary on Windows (and doesn't hurt on Linux).
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    clientConfig <- readConfigFile configFile
    app clientConfig

{-------------------------------------------------------------------------------
  Parse command line options
-------------------------------------------------------------------------------}

parseSkeletonCmdline :: Parser SkeletonCmdline
parseSkeletonCmdline = SkeletonCmdline
    <$> (optional . strOption $ mconcat [
            long "config"
          , help "Read configuration file FILE"
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

type Port = Int

data ClientConfig = ClientConfig {
      -- | Lagoon server hostname (default: @localhost@)
      configHost :: Maybe String

      -- | Lagoon server port (default: @22089@)
    , configPort :: Maybe Port

      -- | Should we connect over SSL? (default: @False@)
    , configSecure :: Maybe Bool

      -- | If using SSL, should we verify server certificate? (default: @True@)
    , configVerifyCert :: Maybe Bool
    }
  deriving (Show, Generic, PrettyVal)

-- | Default values
--
-- NOTE: When using this to supply defaults, it should be the last value:
--
-- > commandLineConfig <> yamlConfig <> def
instance Default ClientConfig where
  def = ClientConfig {
            configHost       = Just "localhost"
          , configPort       = Just 22089
          , configSecure     = Just False
          , configVerifyCert = Just True
          }

instance Monoid ClientConfig where
  mempty        = ClientConfig {
                      configHost       = Nothing
                    , configPort       = Nothing
                    , configSecure     = Nothing
                    , configVerifyCert = Nothing
                    }
  a `mappend` b = ClientConfig {
                      configHost       = combine configHost
                    , configPort       = combine configPort
                    , configSecure     = combine configSecure
                    , configVerifyCert = combine configVerifyCert
                    }
    where
      combine :: (ClientConfig -> Maybe a) -> Maybe a
      combine f =
        case (f a, f b) of
          (Just x, _) -> Just x
          (_, Just x) -> Just x
          _otherwise  -> Nothing

instance Y.FromJSON ClientConfig where
  parseJSON = withObject "ClientConfig" $ \obj -> do
    configHost       <- obj .:? "lagoonserver_host"
    configPort       <- obj .:? "lagoonserver_port"
    configSecure     <- obj .:? "lagoonserver_secure"
    configVerifyCert <- obj .:? "lagoonserver_verify_cert"
    return ClientConfig{..}

-- | Read configuration file if one is specified or default one exists
readConfigFile :: Maybe FilePath -> IO ClientConfig
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
    defaultPath = "lagoon.yaml"

data InvalidConfigFile = InvalidConfigFile FilePath Y.ParseException
  deriving (Show)

instance Exception InvalidConfigFile

instance FriendlyException InvalidConfigFile where
  displayFriendly (InvalidConfigFile fp ex) =
       "Could not parse configuration file " ++ show fp
    ++ ": "
    ++ Y.prettyPrintParseException ex
