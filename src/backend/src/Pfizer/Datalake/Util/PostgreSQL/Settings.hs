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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Util.PostgreSQL.Settings (
    -- * Connection settings
    ConnectionSettings(..)
  , ConnectionException(..)
  , connectionSettings
  , newConnection
  , closeConnection
    -- * Connection configuration
  , ConnectionConfig(..)
  , parseConnectionConfig
  ) where

import Control.Exception (throwIO, IOException)
import Control.Monad
import Control.Monad.Catch
import Database.PostgreSQL.Simple (Connection)
import Data.List
import Data.Maybe
import Data.Yaml (FromJSON(..), (.:?))
import GHC.Generics (Generic)
import Options.Applicative hiding (header)
import Text.Show.Pretty
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.ByteString            as BS.S
import qualified Data.ByteString.Char8      as BS.S.C8
import qualified Data.Yaml                  as Y

import Pfizer.Datalake.Interface

{-------------------------------------------------------------------------------
  Connection settings

  This is adapted from the hasql and hasql-optparse-applicative packages
-------------------------------------------------------------------------------}

newtype ConnectionSettings = ConnectionSettings BS.S.ByteString
  deriving (Show, Pretty)

instance PrettyVal ConnectionSettings where
  prettyVal (ConnectionSettings cs) = prettyVal (BS.S.C8.unpack cs)

data ConnectionException = ConnectionException IOException ConnectionSettings
  deriving (Show)

instance Exception ConnectionException

instance FriendlyException ConnectionException where
  displayFriendly (ConnectionException _ _) = unlines [
      "Could not connect to the database."
    , "Is the database online and the database information correct?"
    ]

-- | Create new connection
--
-- Connection should be closed with 'closeConnection'.
-- May throw a 'ConnectionException'
newConnection :: ConnectionSettings -> IO Connection
newConnection settings@(ConnectionSettings cs) =
    catch (PG.connectPostgreSQL cs)
          (\ex -> throwIO (ConnectionException ex settings))

-- | Terminate connection
closeConnection :: Connection -> IO ()
closeConnection = PG.close

{-------------------------------------------------------------------------------
  Connection configuration
-------------------------------------------------------------------------------}

-- Configuration settings comming either from the command line or a config file.
data ConnectionConfig = ConnectionConfig {
    cHost     :: Maybe String
  , cPort     :: Maybe Int
  , cUser     :: Maybe String
  , cPassword :: Maybe String
  , cDatabase :: Maybe String
  , cLogLevel :: Maybe String
  , cSchema   :: Maybe String
  }
  deriving (Generic)

instance PrettyVal ConnectionConfig

connectionSettings :: ConnectionConfig -> ConnectionSettings
connectionSettings ConnectionConfig {..} =
    ConnectionSettings . BS.S.C8.pack
                       . intercalate " "
                       . catMaybes
                       $ fields
  where
    fields :: [Maybe String]
    fields = [
        ("host="     ++)        <$> cHost
      , ("port="     ++) . show <$> cPort
      , ("user="     ++)        <$> cUser
      , ("password=" ++)        <$> cPassword
      , ("dbname="   ++)        <$> cDatabase
      ]

instance FromJSON ConnectionConfig where
  parseJSON (Y.Object v) =
    ConnectionConfig   <$>
    v .:? "pghost"     <*>
    v .:? "pgport"     <*>
    v .:? "pguser"     <*>
    v .:? "pgpassword" <*>
    v .:? "pgdatabase" <*>
    v .:? "pgloglevel" <*>
    v .:? "pgschema"
  parseJSON _ = fail "Expected Object for Config value"

instance Monoid ConnectionConfig where
  mempty = ConnectionConfig Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  mappend c1 c2 = ConnectionConfig
      (merge cHost)
      (merge cPort)
      (merge cUser)
      (merge cPassword)
      (merge cDatabase)
      (merge cLogLevel)
      (merge cSchema)
    where
      merge :: MonadPlus m => (ConnectionConfig -> m b) -> m b
      merge f = mplus (f c1) (f c2)

parseConnectionConfig :: Maybe String -> Parser ConnectionConfig
parseConnectionConfig prefix = ConnectionConfig
    <$> (optional . strOption $ mconcat [
            long (prefixed "host")
          , help "Server host (can also set PGHOST env var)"
          ])
    <*> (optional . option auto $ mconcat [
            long (prefixed "port")
          , help "Server port (can also set PGPORT env var)"
          ])
    <*> (optional . strOption $ mconcat [
            long (prefixed "user")
          , help "Username (can also set PGUSER env var)"
          ])
    <*> (optional . strOption $ mconcat [
            long (prefixed "password")
          , help "Password (can also set PGPASSWORD env var)"
          ])
    <*> (optional . strOption $ mconcat [
            long (prefixed "database")
          , help "Database name (can also PGDATABASE env var)"
          ])
    <*> (optional . strOption $ mconcat [
            long (prefixed "loglevel")
          , help "PostgreSQL log level (client_min_messages)"
          ])
    <*> (optional . strOption $ mconcat [
             long (prefixed "schema")
           , help "Database schema to use"
           ])
  where
    prefixed :: String -> String
    prefixed s = maybe s (++ s) prefix
