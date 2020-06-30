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
module Pfizer.Datalake.Server.Cmdline (
    Cmdline(..)
  , TlsSettings(..)
  , RequestLogging(..)
  , getCmdline
  , S3Settings(..)
  , S3Endpoint(..)
  ) where

import Data.Foldable (asum)
import Data.Time
import Network.Wai.Handler.Warp
import Options.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text

import Pfizer.Datalake.Auth
import Pfizer.Datalake.Interface
import Pfizer.Datalake.ManageDb
import Pfizer.Datalake.Server.AppSkeleton
import Pfizer.Datalake.Server.Auth.BasicAuth
import Pfizer.Datalake.Server.Auth.LDAP
import Pfizer.Datalake.Server.Auth.VerifyCreds

data Cmdline = Cmdline {
      -- | Core app skeleton arguments
      skeletonCmdline :: SkeletonCmdline

      -- | Server port
    , port :: Port

      -- | Time to keep unused database connections open
    , dbGcTime :: NominalDiffTime

      -- | Maximum number of database connections
    , dbMaxConn :: Int

      -- | Authentication provider
    , authProvider :: AuthProvider

      -- | Server TLS settings (if using)
    , tlsSettings :: Maybe TlsSettings

      -- | Request logigng
    , requestLogging :: RequestLogging

      -- | Enable gzip compression of server responses
    , enableGzip :: Bool

      -- | DB management command (if any)
    , manageDb :: Maybe ManageDb

      -- | Settings for retrieving S3 credentials
    , s3Settings :: Maybe S3Settings

      -- | Settings for custom S3 endpoints
    , s3Endpoint :: Maybe S3Endpoint
    }

data TlsSettings = TlsSettings {
      tlsCertFile      :: FilePath
    , tlsKeyFile       :: FilePath
    , tlsAllowInsecure :: Bool
    }

data S3Settings =
    S3FromEnv
  | S3FromFile {
      s3CredentialsPath :: Maybe FilePath
    , s3Profile :: Maybe Text.Text
    }
  | S3KeySecret {
      s3CredentialsKey :: BS.ByteString
    , s3CredentialsSecret :: BS.ByteString
    }
  deriving Show

data S3Endpoint = S3Endpoint {
    s3EndpointHost     :: BS.ByteString
  , s3EndpointPort     :: Int
  , s3EndpointInsecure :: Bool
  }

data RequestLogging =
    RequestLoggingNone
  | RequestLoggingProduction
  | RequestLoggingDev
  | RequestLoggingRaw

getCmdline :: IO Cmdline
getCmdline = execParser opts
  where
    opts = info (helper <*> parseCmdline) fullDesc

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline = Cmdline
    <$> parseSkeletonCmdline
    <*> (option auto $ mconcat [
            long "port"
          , value 22089
          , help "Port to run on"
          , showDefault
          ])
    <*> (option (fromInteger <$> auto) $ mconcat [
            long "db-gc-time"
          , value 60 -- TODO: What's a good value here?
          , metavar "SECONDS"
          , help "Time to keep unused database connections open"
          , showDefault
          ])
    <*> (option auto $ mconcat [
            long "db-max-conn"
          , value 100 -- TODO: What's a good value here?
          , metavar "INT"
          , help "Maximum number of open database connections"
          , showDefault
          ])
    <*> parseAuthProvider
    <*> optional parseTlsSettings
    <*> parseRequestLogging
    <*> (flag True False $ mconcat [
            long "disable-gzip"
          , help "Disable gzip compression of server responses"
          ])
    <*> optional parseManageDb
    <*> optional parseS3Settings
    <*> optional parseS3Endpoint

parseTlsSettings :: Parser TlsSettings
parseTlsSettings = TlsSettings
    <$> (strOption $ mconcat [
            long "tls-cert"
          , metavar "FILE"
          , help "TLS certificate"
          ])
    <*> (strOption $ mconcat [
            long "tls-key"
          , metavar "FILE"
          , help "TLS key"
          ])
    <*> (switch $ mconcat [
            long "allow-insecure"
          , help "Allow inscure connections from clients"
          ])

parseS3Endpoint :: Parser S3Endpoint
parseS3Endpoint = S3Endpoint
    <$> (option (BS8.pack <$> str) $ mconcat [
             long "s3-host"
           , metavar "HOST"
           , help "S3 server host"
           ])
    <*> (option auto $ mconcat [
            long "s3-port"
          , metavar "PORT"
          , help "S3 server port"
          ])
    <*> (switch $ mconcat [
            long "s3-insecure"
          , help "When enabled, S3 is accessed over HTTP instead of HTTPS"
          ])

parseS3Settings :: Parser S3Settings
parseS3Settings = asum [
        (flag' S3FromEnv $ mconcat [
            long "s3-env"
          , help "Look up S3 credentials from environment (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY)"
          ])
    ,   (flag' S3FromFile $ mconcat [
            long "s3-file"
          , help "Look up S3 credentials from file"
          ])
        <*> (optional $ strOption $ mconcat [
                 long "s3-credentials"
               , metavar "FILE"
               , help "S3 credentials file (line format: key aws_key aws_secret), defaults to ~/.aws-keys"
               ])
        <*> (optional $ option (Text.pack <$> str) $ mconcat [
                 long "s3-key"
               , metavar "KEY"
               , help "S3 credentials key, see --s3-credentials"
               ])
    ,   (flag' S3KeySecret $ mconcat [
            long "s3-cli"
          , help "Enable S3 support and read the credentials from the command line"
          , help "(Note that this is discouraged because your credentials are passed in plain text)"
          ])
        <*> (option (BS8.pack <$> str) $ mconcat [
                 long "s3-access-key"
               , metavar "ACCESS_KEY"
               , help "S3 Access key"
               ])
        <*> (option (BS8.pack <$> str) $ mconcat [
                 long "s3-secret"
               , metavar "SECRET"
               , help "S3 Access key secret"
               ])
    ]

parseAuthProvider :: Parser AuthProvider
parseAuthProvider = asum [
      authProviderLDAP
        <$> (strOption $ mconcat [
                 long "ldap"
               , metavar "URL"
               , help "Use LDAP for authentication. Caution: use LDAPS instead of LDAP if you don't want passwords to be transmitted in plaintext."
               ])
        <*> (option (Text.pack <$> str) $ mconcat [
                 long "ldap-template"
               , metavar "MUSTACHE"
               , help "LDAP template for constructing distinguished names. Example: 'cn={{user}},dc=example,dc=com'"
               ])
    , authProviderBasicAuth
        <$> (strOption $ mconcat [
                 long "basic-auth"
               , metavar "URL"
               , help "Use HTTP basic auth for authentication. Caution: use HTTPS instead of HTTP if you don't want passwords to be transmitted in plaintext."
               ])
        <*> parseVerifyCert
    , flag' authProviderApproveAll $ mconcat [
          long "dummy-auth"
        , help "Use a dummy authentication provider that approves all requests. Use only for debugging!"
        ]
    , pure authProviderDenyAll
    ]

parseVerifyCert :: Parser VerifyCert
parseVerifyCert = flag True False $ mconcat [
      long "auth-ignore-cert"
    , help "Don't verify certificate of the authentication server (only for testing purposes)."
    ]

parseRequestLogging :: Parser RequestLogging
parseRequestLogging = asum [
      flag' RequestLoggingProduction $ mconcat [
          long "log"
        , help "Enable production logging"
        ]
    , flag' RequestLoggingDev $ mconcat [
          long "log-verbose"
        , help "Enable development logging"
        ]
    , flag' RequestLoggingRaw $ mconcat [
          long "log-raw"
        , help "Log the raw requests and responses. You may also want to use --disable-gzip in this case."
        ]
    , pure RequestLoggingNone
    ]

{-------------------------------------------------------------------------------
  DB management options
-------------------------------------------------------------------------------}

parseManageDb :: Parser ManageDb
parseManageDb = subparser $ mconcat [
      command "init-db" $ info (helper <*> parseInitDb) $
        progDesc "Initialize database."
    , command "reset-db" $ info (helper <*> parseResetDb) $
        progDesc "Reset the database. NOTE: This deletes all data in the database."
    , command "migrate" $ info (helper <*> parseMigrate) $
        progDesc "Run migration (bring the version of the database up to date with the version of ingest)."
    , command "change-db-admin-pass" $ info (helper <*> parseChangeDbAdminPass) $
        progDesc "Change the DB administrator password."
    ]

parseInitDb :: Parser ManageDb
parseInitDb = InitDb <$> parseDbAdminPass ""

parseResetDb :: Parser ManageDb
parseResetDb = ResetDb <$> parseDbAdminPass ""

parseMigrate :: Parser ManageDb
parseMigrate = pure Migrate

parseChangeDbAdminPass :: Parser ManageDb
parseChangeDbAdminPass = ChangeDbAdminPass
    <$> parseDbAdminPass "old-"
    <*> parseDbAdminPass "new-"

parseDbAdminPass :: String -> Parser PlaintextPassword
parseDbAdminPass prefix = option (PlaintextPassword <$> str) $ mconcat [
      long $ prefix ++ "db-admin-pass"
    , help "Database administrator password"
    ]
