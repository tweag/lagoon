-- | DB management
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.ManageDb (
    ManageDb(..)
  , ManageDbException(..)
  , runManageDb
  , rethrowManageDbException
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Text.Show.Pretty
import qualified Database.PostgreSQL.Simple as PG

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.Exception
import Pfizer.Datalake.Util.PostgreSQL
import Pfizer.Datalake.Verified

-- | Database management (only the DB admin can do this)
data ManageDb =
    -- | Initialize the database
    InitDb PlaintextPassword

    -- | Reset the database
  | ResetDb PlaintextPassword

    -- | Change DB admin password
  | ChangeDbAdminPass PlaintextPassword PlaintextPassword

    -- | Run migration
    --
    -- TODO: We might want to insist on a password here
  | Migrate
  deriving (Generic, PrettyVal)

instance Pretty ManageDb where
  pretty (InitDb _)              = "initialize database"
  pretty (ResetDb _)             = "reset database"
  pretty (ChangeDbAdminPass _ _) = "change admin password"
  pretty Migrate                 = "migrate"

runManageDb :: Connection -> Schema -> ManageDb -> IO ()
runManageDb conn schema = go
  where
    go :: ManageDb -> IO ()
    go (InitDb pw) = do
      runTransaction conn schema $ initDb pw
    go (ResetDb pw) = do
      loginResult <- runTransaction conn schema $ do
        checkDbVersion -- We can't reset if we need to migrate first
        loginAdmin pw
      admin <- fromLoginOk loginResult
      resetDb conn schema admin
    go (ChangeDbAdminPass old new) = do
      admin <- fromLoginOk =<< runTransaction conn schema (loginAdmin old)
      runTransaction conn schema $ changeDbAdminPass admin new
    go Migrate = do
      runTransaction conn schema $ migrate

data ManageDbException =
    DbSchemaDoesNotExist (WithCallStack SomeSqlException) Schema
  | DbAlreadyInitialized (WithCallStack SomeSqlException)
  | DbNotInitialized     (WithCallStack SomeSqlException)
  | DbMissingPgTrgm      (WithCallStack SomeSqlException)
  deriving Show

instance Exception ManageDbException

instance FriendlyException ManageDbException where
  displayFriendly (DbSchemaDoesNotExist _e schema) = unlines [
      "Schema '" ++ prettyStr schema ++ "' does not exist."
    , "(Note that the server cannot create database schemas automatically;"
    , "you may have to contact your database administrator.)"
    ]
  displayFriendly (DbAlreadyInitialized _e) = unlines [
      "The database has already been initialized."
    , "Use reset-db to bring the database back to its initial state."
    ]
  displayFriendly (DbNotInitialized _e) = unlines [
      "Some database tables were not found. Has the database been initialized?"
    ]
  displayFriendly (DbMissingPgTrgm _e) = unlines [
      "The Datalake server needs the PostgreSQL pg_trgm extension."
    , "Please contact your database administrator."
    ]

isManageDbException :: WithCallStack SomeSqlException -> Maybe ManageDbException
isManageDbException e@(WithCallStack _ e') = aux e'
  where
    aux :: SomeSqlException -> Maybe ManageDbException
    aux (SqlError schema PG.SqlError{sqlState = "3F000"}) =
      Just $ DbSchemaDoesNotExist e schema
    aux (SqlError _schema PG.SqlError{sqlState}) | isDup sqlState =
      Just $ DbAlreadyInitialized e
    aux (SqlError _schema PG.SqlError{sqlState = "42P01"}) =
      Just $ DbNotInitialized e
    aux (SqlError _schema PG.SqlError{sqlState = "42704"}) =
      Just $ DbMissingPgTrgm e
    aux _other =
      Nothing

    -- Error code that correspond to try to create an object that already exists
    isDup :: ByteString -> Bool
    isDup "42P07" = True
    isDup "42710" = True
    isDup _other  = False

rethrowManageDbException :: IO a -> IO a
rethrowManageDbException = handleJust isManageDbException throwIO
