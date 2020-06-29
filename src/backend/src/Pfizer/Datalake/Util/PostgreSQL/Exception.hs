-- | SQL exceptions
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Util.PostgreSQL.Exception (
    SomeSqlException(..)
  , MaybeSqlException(..)
  , isSqlException
  , trySql
  ) where

import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Maybe
import qualified Database.PostgreSQL.Simple       as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Data.ByteString.UTF8             as BS.S.UTF8

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.Exception

{-------------------------------------------------------------------------------
  Combine all SQL exceptions into a single type
-------------------------------------------------------------------------------}

data SomeSqlException =
    SqlError    Schema PG.SqlError
  | ResultError Schema PG.ResultError
  | FormatError Schema PG.FormatError
  deriving Show

instance Exception SomeSqlException
instance FriendlyException SomeSqlException where
  displayFriendly e = fromMaybe defaultMsg (toSafeFriendlyText e)
    where
      defaultMsg = unwords
        [ "A unexpected error occurred."
        , "Please contact an administrator."
        ]

-- | @Either@ specialized to @SomeSqlException@
--
-- The reason for introducing this type is that we can give it a specialized
-- 'ToJSON' instance.
data MaybeSqlException a =
    NoSqlException a
  | RaisedSqlException SomeSqlException

toMaybeSqlException :: Either SomeSqlException a -> MaybeSqlException a
toMaybeSqlException (Left  e) = RaisedSqlException e
toMaybeSqlException (Right a) = NoSqlException     a

-- | Convert SQL exceptions to text that is user friendly. "User friendly" here
-- means:
--   * The user can understand the error
--   * It is not a security risk to show the error to the user
toSafeFriendlyText :: SomeSqlException -> Maybe String
toSafeFriendlyText = \case
    SqlError _schema PG.SqlError {sqlState, sqlErrorMsg} ->
      if sqlState `elem`
          [ "54011" -- Too many columns
          ]
      then Just (BS.S.UTF8.toString sqlErrorMsg)
      else Nothing
    _ -> Nothing

isSqlException :: Schema -> SomeException -> Maybe SomeSqlException
isSqlException schema e
  | Just e' <- fromException e = Just $ SqlError    schema e'
  | Just e' <- fromException e = Just $ ResultError schema e'
  | Just e' <- fromException e = Just $ FormatError schema e'
  | Just (WithCallStack _stack e') <- fromException e = Just $ e'
  | otherwise                  = Nothing

trySql :: MonadCatch m => Schema -> m a -> m (MaybeSqlException a)
trySql schema = liftM toMaybeSqlException . tryJust (isSqlException schema)

{-------------------------------------------------------------------------------
  JSON encoding
-------------------------------------------------------------------------------}

-- | The 'ToJSON' instance for 'MaybeSqlException' does not in any way
-- explicitly indicate whether we have 'NoSqlException' or 'JustSqlException'.
-- It is assumed that readers can tell the difference by looking at the specific
-- JSON value.
--
-- XXX: This should not be used to return exceptions to the user in general, as
-- this is potential a security risk. These instances should only be used when
-- the user interacts with the raw SQL interface (@/sql@).
instance ToJSON a => ToJSON (MaybeSqlException a) where
  toJSON (NoSqlException     a) = toJSON a
  toJSON (RaisedSqlException e) = toJSON e

-- | The encoding of 'SomeSqlException' is loosely modelled on the one used in
-- PostgREST.
instance ToJSON SomeSqlException where
  toJSON (SqlError    _schema e) = jsonSqlError    e
  toJSON (ResultError _schema e) = jsonResultError e
  toJSON (FormatError _schema e) = jsonFormatError e

jsonSqlError :: PG.SqlError -> Value
jsonSqlError err = object [
      "code"    .= BS.S.UTF8.toString (PG.sqlState       err)
    , "status"  .= show               (PG.sqlExecStatus  err)
    , "message" .= BS.S.UTF8.toString (PG.sqlErrorMsg    err)
    , "details" .= BS.S.UTF8.toString (PG.sqlErrorDetail err)
    , "hint"    .= BS.S.UTF8.toString (PG.sqlErrorHint   err)
    ]

jsonResultError :: PG.ResultError -> Value
jsonResultError err = object [
      "sqlType"     .=                    PG.errSQLType     err
    , "sqlTableOid" .= fmap oidToInteger (PG.errSQLTableOid err)
    , "sqlField"    .=                    PG.errSQLField    err
    , "haskellType" .=                    PG.errHaskellType err
    , "message"     .=                    PG.errMessage     err
    ]

jsonFormatError :: PG.FormatError -> Value
jsonFormatError err = object [
      "message" .=                         PG.fmtMessage err
    , "query"   .= show                   (PG.fmtQuery   err)
    , "params"  .= map BS.S.UTF8.toString (PG.fmtParams  err)
    ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

oidToInteger :: PG.Oid -> Integer
oidToInteger (PG.Oid oid) = toInteger oid
