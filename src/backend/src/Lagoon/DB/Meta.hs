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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Database meta information
--
-- One of the primary goals of the meta information is to support migration.
module Lagoon.DB.Meta (
    DbVersion(..)
  , initDbMeta
  , getDbMeta
  , setDbMeta
    -- * Admin password
  , DbAdminPass
  , initDbAdminPass
  , hashDbAdminPass
  , validateDbAdminPass
  , changeDbAdminPass
    -- * Default permissions
  , DefaultCanCreateSource(..)
  , DefaultCanCreateGroup(..)
  , DefaultSourcePublicAccess(..)
    -- * DB versions (from new to old)
  , dbVersionCurrent
  , dbVersion_944d04a
  , dbVersion_0c931db
  , dbVersion_54ed20c
  , dbVersion_4b98410
  , dbVersion_2a1f612
  , dbVersion_1189a33
  , dbVersion_28ec204
  , dbVersion_480c65a
  , dbVersion_ce2b9f1
  ) where

import Data.Proxy

import Lagoon.Interface
import Lagoon.DB.Passwords
import Lagoon.DB.Schema
import Lagoon.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Database versions
-------------------------------------------------------------------------------}

newtype DbVersion = DbVersion { dbVersionToInt :: Int }
  deriving (Show, Eq, Ord, Enum)

instance IsVariable DbVersion where
  varName _ = "version"
  toValue   = show . dbVersionToInt
  fromValue = DbVersion . read

-- | Database version corresponding to the schema we create right now
dbVersionCurrent :: DbVersion
dbVersionCurrent = DbVersion 10

{-------------------------------------------------------------------------------
  Historical DB versions

  The names refer to the commit hash of the last commit in which this database
  version was stil used.
-------------------------------------------------------------------------------}

dbVersion_944d04a :: DbVersion
dbVersion_944d04a = DbVersion 9

dbVersion_0c931db :: DbVersion
dbVersion_0c931db = DbVersion 8

dbVersion_54ed20c :: DbVersion
dbVersion_54ed20c = DbVersion 7

dbVersion_4b98410 :: DbVersion
dbVersion_4b98410 = DbVersion 6

dbVersion_1189a33 :: DbVersion
dbVersion_1189a33 = DbVersion 5

dbVersion_2a1f612 :: DbVersion
dbVersion_2a1f612 = DbVersion 4

dbVersion_28ec204 :: DbVersion
dbVersion_28ec204 = DbVersion 3

dbVersion_480c65a :: DbVersion
dbVersion_480c65a = DbVersion 2

dbVersion_ce2b9f1 :: DbVersion
dbVersion_ce2b9f1 = DbVersion 1

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

-- | Initialize DB metadata
initDbMeta :: MonadIO m => DbAdminPass -> Transaction m ()
initDbMeta dbAdminPass = do
    setDbMeta dbVersionCurrent
    setDbMeta dbAdminPass
    setDbMeta $ DefaultCanCreateSource    True
    setDbMeta $ DefaultCanCreateGroup     True
    setDbMeta $ DefaultSourcePublicAccess DatasetAccessLevelUpdate

{-------------------------------------------------------------------------------
  Admin password
-------------------------------------------------------------------------------}

-- | (Hashed) admin password
newtype DbAdminPass = DbAdminPass { dbAdminPassToHash :: HashedPassword }
  deriving (Show, Eq, Ord)

instance IsVariable DbAdminPass where
  varName _ = "adminPass"
  toValue   = hashedPasswordToString . dbAdminPassToHash
  fromValue = DbAdminPass . hashedPasswordFromString

-- | Initial DB admin password (empty)
initDbAdminPass :: IO DbAdminPass
initDbAdminPass = hashDbAdminPass ""

-- | Hash admin password
hashDbAdminPass :: PlaintextPassword -> IO DbAdminPass
hashDbAdminPass = fmap DbAdminPass . hashPassword

-- | Validate admin password
--
-- Returns 'Just' the hash of the password if correct, 'Nothing' otherwise.
validateDbAdminPass :: MonadIO m
                    => PlaintextPassword
                    -> Transaction m (Maybe DbAdminPass)
validateDbAdminPass str = do
    Just pass@(DbAdminPass hash) <- getDbMeta
    return $ if validatePassword hash str
               then Just pass
               else Nothing

-- | Change admin password
changeDbAdminPass :: MonadIO m => PlaintextPassword -> Transaction m ()
changeDbAdminPass new =
    setDbMeta =<< liftIO (hashDbAdminPass new)

{-------------------------------------------------------------------------------
  Default permissions
-------------------------------------------------------------------------------}

-- | Should new users be granted CREATE permission?
--
-- If this is set to 'False', new users can only read existing (public)
-- datasets.
newtype DefaultCanCreateSource = DefaultCanCreateSource Bool

-- | Should new users be granted CREATEGROUP permission?
newtype DefaultCanCreateGroup = DefaultCanCreateGroup Bool

-- | Access level granted to the public group for new sources.
--
-- If this is set to 'DatasetAccessLevelNone' new sources will only be readable
-- by the user who created them until additional permissions are explicily
-- granted.
newtype DefaultSourcePublicAccess = DefaultSourcePublicAccess DatasetAccessLevel

instance IsVariable DefaultCanCreateSource where
  varName _ = "defaultCanCreateSource"
  toValue (DefaultCanCreateSource x) = show x
  fromValue = DefaultCanCreateSource . read

instance IsVariable DefaultCanCreateGroup where
  varName _ = "defaultCanCreateGroup"
  toValue (DefaultCanCreateGroup x) = show x
  fromValue = DefaultCanCreateGroup . read

instance IsVariable DefaultSourcePublicAccess where
  varName _ = "defaultSourcePublicAccess"
  toValue (DefaultSourcePublicAccess x) = show x
  fromValue = DefaultSourcePublicAccess . read

{-------------------------------------------------------------------------------
  Internal: untyped variables
-------------------------------------------------------------------------------}

type VarName = String
type Value   = String

getVar :: (MonadIO m, ?loc :: CallStack)
       => VarName -> Transaction m (Maybe Value)
getVar var = do
    rows <- queryS (\schema -> intercalateM " " [
                       "SELECT value"
                     , "FROM " <> quoted (schema, tableDbMeta)
                     , "WHERE variable = ?"
                     ])
                   (Only var)
    case rows of
      [Only value] -> return $ Just value
      _otherwise   -> return Nothing

setVar :: (MonadIO m, ?loc :: CallStack)
       => VarName -> Value -> Transaction m ()
setVar var value = do
    mValue <- getVar var
    case mValue of
      Nothing ->
        executeS (\schema -> intercalateM " " [
                     "INSERT INTO " <> quoted (schema, tableDbMeta)
                   , "VALUES(?, ?)"
                   ])
                 (var, value)
      Just _oldValue ->
        executeS (\schema -> intercalateM " " [
                     "UPDATE " <> quoted (schema, tableDbMeta)
                   , "SET value = ?"
                   , "WHERE variable = ?"
                   ])
                 (value, var)

{-------------------------------------------------------------------------------
  Translation from and to untyped variables
-------------------------------------------------------------------------------}

class IsVariable a where
  varName   :: proxy a -> VarName
  toValue   :: a -> Value
  fromValue :: Value -> a

getDbMeta :: forall m a. (MonadIO m, IsVariable a, ?loc :: CallStack)
          => Transaction m (Maybe a)
getDbMeta = fmap fromValue <$> getVar (varName (Proxy :: Proxy a))

setDbMeta :: forall m a. (MonadIO m, IsVariable a, ?loc :: CallStack)
          => a -> Transaction m ()
setDbMeta = setVar (varName (Proxy :: Proxy a)) . toValue
