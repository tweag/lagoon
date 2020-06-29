{-# LANGUAGE OverloadedStrings #-}
-- | Functions for quering and managing access permissions
--
-- NOTE: Just like everything else in the DB.* module hiearchy, these
-- functions do NOT check that the user has the right to call them.
-- See "Pfizer.Datalake.Verified" instead.
module Pfizer.Datalake.DB.Security (
    -- * Dealing with groups
    newGroup
  , getGroup
  , getGroupMembers
    -- * Permissions
  , DatasetHasAccessLevel(..)
  , hasNoAccess
    -- * Querying and updating permissions
  , PermissionQuery(..)
  , queryPermissions
  , PermissionUpdate(..)
  , updatePermissions
    -- * Initialize security model
  , InitSecurityData(..)
  , initSecurity
  , getPublicGroup
  , getUnauthUser
  , getAdminUser
  , rebuildCanReadCache
  ) where

import Data.Maybe (isNothing)
import Database.PostgreSQL.Simple.ToField
import qualified Data.ByteString as BS.S

import Pfizer.Datalake.Interface
import Pfizer.Datalake.DB.IfNotFound
import Pfizer.Datalake.DB.Users
import Pfizer.Datalake.DB.Schema
import Pfizer.Datalake.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Low-level functions for dealing with groups
-------------------------------------------------------------------------------}

-- | Create a new group
--
-- This is a low-level function that does no permission checks or set any
-- permissions for the new group
newGroup :: MonadIO m => GroupName -> Transaction m GroupIx
newGroup groupName = do
    getGroup (errorIfFound groupName) groupName
    [Only gix] <- queryS
      (\schema -> intercalateM " " [
          "INSERT INTO " <> quoted (schema, tableGroups) <> "(name)"
        , "VALUES (?)"
        , "RETURNING ix"
        ])
      (Only groupName)
    return gix

-- | Get group identifier
getGroup :: MonadIO m
         => IfNotFound () (Transaction m) GroupIx a
         -> GroupName -> Transaction m a
getGroup ifNotFound groupName = do
    rows <- queryS
      (\schema -> intercalateM " " [
          "SELECT ix FROM " <> quoted (schema, tableGroups)
        , "WHERE name = ?"
        ])
      (Only groupName)
    ifNotFound (rowsToMaybe rows) $ \() -> newGroup groupName

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Dataset access level that excludes 'DatasetAccessLevelNone'
--
-- This type is mostly internal to this module (it is used also in migration);
-- it is needed because at the SQL level 'DatasetNoAccess' is represented by the
-- absence of a row, rather than a specific value; hence,
-- 'DatasetHasAccessLevel' has a 'ToField' instance but 'DatasetAccessLevel'
-- does not.
data DatasetHasAccessLevel =
    DatasetHasAccessLevelRead
  | DatasetHasAccessLevelUpdate
  | DatasetHasAccessLevelManage

hasAccess :: DatasetAccessLevel -> Maybe DatasetHasAccessLevel
hasAccess DatasetAccessLevelNone   = Nothing
hasAccess DatasetAccessLevelRead   = Just $ DatasetHasAccessLevelRead
hasAccess DatasetAccessLevelUpdate = Just $ DatasetHasAccessLevelUpdate
hasAccess DatasetAccessLevelManage = Just $ DatasetHasAccessLevelManage

hasNoAccess :: DatasetAccessLevel -> Bool
hasNoAccess = isNothing . hasAccess

{-------------------------------------------------------------------------------
  Query permissions
-------------------------------------------------------------------------------}

-- | Permission query
--
-- TODO: We use these queries also in exceptions, to explain to the user which
-- permission they do not have. For this reason, 'UserIx' here should be
-- replaced with 'User', 'GroupIx' with 'Group', and 'SourceNameIx' with
-- 'Source', so that these permissions are easier to understand.
data PermissionQuery =
    -- | Does the use have CREATE permission?
    CanCreateSource UserIx

    -- | Does the user have CREATEGROUP permission?
  | CanCreateGroup UserIx

    -- | Does the use have MANAGEGROUP access for this group?
  | CanManageGroup GroupIx UserIx

    -- | Does the user have MANAGE access to the dataset?
  | CanManageDataset SourceNameIx UserIx

    -- | Does the user have UPDATE access to the dataset?
  | CanUpdateDataset SourceNameIx UserIx

    -- | Does the user have READ access to the dataset?
  | CanReadDataset SourceNameIx UserIx
  deriving (Show)

queryPermissions :: forall m. MonadIO m
                 => PermissionQuery -> Transaction m Bool
queryPermissions = go
  where
    go :: PermissionQuery -> Transaction m Bool
    go (CanCreateSource      uix) = canCreateSource      uix
    go (CanCreateGroup       uix) = canCreateGroup       uix
    go (CanManageGroup   gix uix) = canManageGroup   gix uix
    go (CanManageDataset six uix) = canManageDataset six uix
    go (CanUpdateDataset six uix) = canUpdateDataset six uix
    go (CanReadDataset   six uix) = canReadDataset   six uix

{-------------------------------------------------------------------------------
  Check permissions
-------------------------------------------------------------------------------}

canCreateSource :: MonadIO m => UserIx -> Transaction m Bool
canCreateSource uix = do
    [Only b] <- queryS
      (\schema -> intercalateM " " [
          "SELECT EXISTS("
        , "  SELECT * FROM " <> quoted (schema, tableCanCreate)
        , "  WHERE usr = ?"
        , ")"
        ])
      (Only uix)
    return b

canCreateGroup :: MonadIO m => UserIx -> Transaction m Bool
canCreateGroup uix = do
    [Only b] <- queryS
      (\schema -> intercalateM " " [
          "SELECT EXISTS("
        , "  SELECT * FROM " <> quoted (schema, tableCanCreateGroup)
        , "  WHERE usr = ?"
        , ")"
        ])
      (Only uix)
    return b

canManageGroup :: MonadIO m => GroupIx -> UserIx -> Transaction m Bool
canManageGroup gix uix = do
    [Only b] <- queryS
      (\schema -> intercalateM " " [
          "SELECT EXISTS("
        , "  SELECT * FROM " <> quoted (schema, tableCanManageGroup)
        , "  WHERE usr = ? AND grp = ?"
        , ")"
        ])
      (uix, gix)
    return b

-- | Get the maximum access level for the specified user, taking into account
-- group membership.
getDatasetAccess :: MonadIO m
                 => SourceNameIx -> UserIx -> Transaction m DatasetAccessLevel
getDatasetAccess six uix = do
    userAccessLevel <- queryS
      (\schema -> intercalateM " " [
          "SELECT level"
        , "FROM " <> quoted (schema, tableDatasetUserAccess)
        , "WHERE usr = ? AND sourcename = ?"
        ])
      (uix, six)
    groupAccessLevel <- queryS
      (\schema -> intercalateM " " [
          "SELECT groupaccess.level"
        , "FROM " <> quoted (schema, tableMembership)         <> " membership"
        , "JOIN " <> quoted (schema, tableDatasetGroupAccess) <> " groupaccess"
        , "ON membership.grp = groupaccess.grp"
        , "WHERE membership.usr = ? AND groupaccess.sourcename = ?"
        ])
      (uix, six)
    let accessLevels :: [DatasetAccessLevel]
        accessLevels = DatasetAccessLevelNone
                     : map fromOnly (userAccessLevel ++ groupAccessLevel)
    return $ maximum accessLevels

canManageDataset :: MonadIO m => SourceNameIx -> UserIx -> Transaction m Bool
canManageDataset six uix =
    (>= DatasetAccessLevelManage) <$> getDatasetAccess six uix

canUpdateDataset :: MonadIO m => SourceNameIx -> UserIx -> Transaction m Bool
canUpdateDataset six uix =
    (>= DatasetAccessLevelUpdate) <$> getDatasetAccess six uix

canReadDataset :: MonadIO m => SourceNameIx -> UserIx -> Transaction m Bool
canReadDataset six uix =
    (>= DatasetAccessLevelRead) <$> getDatasetAccess six uix

{-------------------------------------------------------------------------------
  Update permissions
-------------------------------------------------------------------------------}

data PermissionUpdate :: * -> * where
    GrantCreateSource     :: UserIx -> PermissionUpdate ()
    RevokeCreateSource    :: UserIx -> PermissionUpdate ()
    GrantCreateGroup      :: UserIx -> PermissionUpdate ()
    RevokeCreateGroup     :: UserIx -> PermissionUpdate ()
    NewGroup              :: GroupName -> PermissionUpdate GroupIx
    GrantManageGroup      :: UserIx -> GroupIx -> PermissionUpdate ()
    RevokeManageGroup     :: UserIx -> GroupIx -> PermissionUpdate ()
    AddUserToGroup        :: UserIx -> GroupIx -> PermissionUpdate ()
    RemoveUserFromGroup   :: UserIx -> GroupIx -> PermissionUpdate ()
    SetUserDatasetAccess  :: UserIx  -> SourceNameIx -> DatasetAccessLevel -> PermissionUpdate ()
    SetGroupDatasetAccess :: GroupIx -> SourceNameIx -> DatasetAccessLevel -> PermissionUpdate ()

updatePermissions :: forall m a. MonadIO m
                  => PermissionUpdate a -> Transaction m a
updatePermissions update = do
    a <- go update
    updateCanReadCache update
    return a
  where
    go :: PermissionUpdate a -> Transaction m a
    go (GrantCreateSource     uix)           = grantCreateSource  uix
    go (RevokeCreateSource    uix)           = revokeCreateSource uix
    go (GrantCreateGroup      uix)           = grantCreateGroup   uix
    go (RevokeCreateGroup     uix)           = revokeCreateGroup  uix
    go (NewGroup              groupName)     = newGroup groupName
    go (GrantManageGroup      uix gix)       = grantManageGroup    uix gix
    go (RevokeManageGroup     uix gix)       = revokeManageGroup   uix gix
    go (AddUserToGroup        uix gix)       = addUserToGroup      uix gix
    go (RemoveUserFromGroup   uix gix)       = removeUserFromGroup uix gix
    go (SetUserDatasetAccess  uix six level) = setUserDatasetAccess  uix six level
    go (SetGroupDatasetAccess gix six level) = setGroupDatasetAccess gix six level

{-------------------------------------------------------------------------------
  User specific permissions
-------------------------------------------------------------------------------}

grantCreateSource :: MonadIO m => UserIx -> Transaction m ()
grantCreateSource uix =
    executeS (\schema -> intercalateM " " [
        "INSERT INTO " <> quoted (schema, tableCanCreate) <> "(usr)"
      , "SELECT ?"
      , "WHERE NOT EXISTS ("
      , "  SELECT * FROM " <> quoted (schema, tableCanCreate)
      , "  WHERE usr = ?"
      , ")"
      ])
      (uix, uix)

revokeCreateSource :: MonadIO m => UserIx -> Transaction m ()
revokeCreateSource uix =
    executeS (\schema -> intercalateM " " [
        "DELETE FROM " <> quoted (schema, tableCanCreate)
      , "WHERE usr = ?"
      ])
      (Only uix)

grantCreateGroup :: MonadIO m => UserIx -> Transaction m ()
grantCreateGroup uix =
    executeS (\schema -> intercalateM " " [
        "INSERT INTO " <> quoted (schema, tableCanCreateGroup) <> "(usr)"
      , "SELECT ?"
      , "WHERE NOT EXISTS ("
      , "  SELECT * FROM " <> quoted (schema, tableCanCreateGroup)
      , "  WHERE usr = ?"
      , ")"
      ])
      (uix, uix)

revokeCreateGroup :: MonadIO m => UserIx -> Transaction m ()
revokeCreateGroup uix =
    executeS (\schema -> intercalateM " " [
        "DELETE FROM " <> quoted (schema, tableCanCreateGroup)
      , "WHERE usr = ?"
      ])
      (Only uix)

{-------------------------------------------------------------------------------
  Permission management: groups

  We use @INSERT INTO .. WHERE NOT EXISTS .. SELECT ..@ as a poor man's
  UPSERT (@ON CONFLICT DO NOTHING@), which is only available from PostgreSQL
  9.5 and up.
-------------------------------------------------------------------------------}

-- | Grant MANAGEGROUP permission
grantManageGroup :: MonadIO m => UserIx -> GroupIx -> Transaction m ()
grantManageGroup uix gix = do
    executeS
      (\schema -> intercalateM " " [
          "INSERT INTO " <> quoted (schema, tableCanManageGroup)
                         <> "(usr, grp)"
        , "SELECT ?, ?"
        , "WHERE NOT EXISTS ("
        , "  SELECT * FROM " <> quoted (schema, tableCanManageGroup)
        , "  WHERE usr = ? AND grp = ?"
        , ")"
        ])
      (uix, gix, uix, gix)

-- | Revoke MANAGEGROUP permission
revokeManageGroup :: MonadIO m => UserIx -> GroupIx -> Transaction m ()
revokeManageGroup uix gix = do
    executeS
      (\schema -> intercalateM " " [
          "DELETE FROM " <> quoted (schema, tableCanManageGroup)
        , "WHERE usr = ? AND grp = ?"
        ])
      (uix, gix)

-- | Add user to a group
addUserToGroup :: MonadIO m => UserIx -> GroupIx -> Transaction m ()
addUserToGroup uix gix = do
    executeS
      (\schema -> intercalateM " " [
          "INSERT INTO " <> quoted (schema, tableMembership)
                         <> "(usr, grp)"
        , "SELECT ?, ?"
        , "WHERE NOT EXISTS ("
        , "  SELECT * FROM " <> quoted (schema, tableMembership)
        , "  WHERE usr = ? AND grp = ?"
        , ")"
        ])
      (uix, gix, uix, gix)

-- | Remove user from group
removeUserFromGroup :: MonadIO m => UserIx -> GroupIx -> Transaction m ()
removeUserFromGroup uix gix = do
    executeS
      (\schema -> intercalateM " " [
          "DELETE FROM " <> quoted (schema, tableMembership)
        , "WHERE usr = ? AND grp = ?"
        ])
      (uix, gix)

-- | List of group members
getGroupMembers :: MonadIO m => GroupIx -> Transaction m [UserName]
getGroupMembers gix = do
    map fromOnly <$> queryS
      (\schema -> intercalateM " " [
          "SELECT users.name"
        , "FROM " <> quoted (schema, tableMembership) <> " membership"
        , "JOIN " <> quoted (schema, tableUsers)      <> " users"
        , "ON membership.usr = users.ix"
        , "WHERE grp = ?"
        ])
      (Only gix)

{-------------------------------------------------------------------------------
  Dataset permissions
-------------------------------------------------------------------------------}

-- | Set dataset access level for a specific user
--
-- Requires MANAGE permissions on the dataset.
setUserDatasetAccess :: MonadIO m
                     => UserIx
                     -> SourceNameIx
                     -> DatasetAccessLevel
                     -> Transaction m ()
setUserDatasetAccess uix six level = do
    executeS
      (\schema -> intercalateM " " [
          "DELETE FROM " <> quoted (schema, tableDatasetUserAccess)
        , "WHERE usr = ? AND sourcename = ?"
        ])
      (uix, six)
    case hasAccess level of
      Nothing     -> return () -- No access represented by absence of row
      Just level' ->
        executeS
          (\schema -> intercalateM " " [
              "INSERT INTO " <> quoted (schema, tableDatasetUserAccess)
                             <> "(usr, sourcename, level)"
            , "VALUES (?, ?, ?)"
            ])
          (uix, six, level')

-- | Set dataset access level for a whole group
setGroupDatasetAccess :: MonadIO m
                      => GroupIx
                      -> SourceNameIx
                      -> DatasetAccessLevel
                      -> Transaction m ()
setGroupDatasetAccess gix six level = do
    executeS
      (\schema -> intercalateM " " [
          "DELETE FROM " <> quoted (schema, tableDatasetGroupAccess)
        , "WHERE grp = ? AND sourcename = ?"
        ])
      (gix, six)
    case hasAccess level of
      Nothing     -> return () -- No access represented by absence of row
      Just level' ->
        executeS
          (\schema -> intercalateM " " [
              "INSERT INTO " <> quoted (schema, tableDatasetGroupAccess)
                             <> "(grp, sourcename, level)"
            , "VALUES (?, ?, ?)"
            ])
          (gix, six, level')

{-------------------------------------------------------------------------------
  Maintaining the can-read cache
-------------------------------------------------------------------------------}

updateCanReadCache :: forall m a. MonadIO m
                   => PermissionUpdate a -> Transaction m ()
updateCanReadCache = go
  where
    go :: PermissionUpdate a -> Transaction m ()
    go (GrantCreateSource  _uix)       = return ()
    go (RevokeCreateSource _uix)       = return ()
    go (GrantCreateGroup   _uix)       = return ()
    go (RevokeCreateGroup  _uix)       = return ()
    go (NewGroup           _groupName) = return ()
    go (GrantManageGroup   _uix _gix)  = return ()
    go (RevokeManageGroup  _uix _gix)  = return ()
    go (AddUserToGroup uix gix) =
      executeS
        (\schema -> intercalateM " " [
            "INSERT INTO " <> quoted (schema, tableCachedCanRead)
                           <> "(usr, sourcename, grp)"
          , "SELECT ?, access.sourcename, ?"
          , "FROM " <> quoted (schema, tableDatasetGroupAccess) <> " AS access"
          , "WHERE access.grp = ?"
          , "AND NOT EXISTS ("
          , "  SELECT *"
          , "  FROM " <> quoted (schema, tableCachedCanRead) <> " AS cache"
          , "  WHERE cache.sourcename = access.sourcename"
          , "    AND cache.usr        = ?"
          , "    AND cache.grp        = ?"
          , ")"
          ])
        (uix, gix, gix, uix, gix)
    go (RemoveUserFromGroup uix gix) =
      executeS
        (\schema -> intercalateM " " [
            "DELETE FROM " <> quoted (schema, tableCachedCanRead)
          , "WHERE usr = ? AND grp = ?"
          ])
        (uix, gix)
    go (SetUserDatasetAccess uix six level) =
      if hasNoAccess level
        then
          executeS
            (\schema -> intercalateM " " [
                "DELETE FROM " <> quoted (schema, tableCachedCanRead)
              , "WHERE usr = ? AND sourcename = ? AND grp IS NULL"
              ])
            (uix, six)
        else
          executeS
            (\schema -> intercalateM " " [
                "INSERT INTO " <> quoted (schema, tableCachedCanRead)
                               <> "(usr, sourcename, grp)"
              , "SELECT ?, ?, NULL"
              , "WHERE NOT EXISTS ("
              ,   "SELECT * FROM " <> quoted (schema, tableCachedCanRead)
              ,   "WHERE usr = ? AND sourcename = ? AND grp IS NULL"
              , ")"
              ])
            (uix, six, uix, six)
    go (SetGroupDatasetAccess gix six level) =
      if hasNoAccess level
        then
          executeS
            (\schema -> intercalateM " " [
                "DELETE FROM " <> quoted (schema, tableCachedCanRead)
              , "WHERE sourcename = ? AND grp = ?"
              ])
            (six, gix)
        else
          executeS
            (\schema -> intercalateM " " [
                "INSERT INTO " <> quoted (schema, tableCachedCanRead)
                               <> "(usr, sourcename, grp)"
              , "SELECT membership.usr, ?, ?"
              , "FROM " <> quoted (schema, tableMembership) <> " AS membership"
              , "WHERE membership.grp = ?"
              , "AND NOT EXISTS ("
              , "  SELECT * FROM " <> quoted (schema, tableCachedCanRead) <> " AS cache"
              , "  WHERE cache.usr = membership.usr AND cache.sourcename = ? AND cache.grp = ?"
              , ")"
              ])
            (six, gix, gix, six, gix)

-- | Rebuild the can-read cache
--
-- Mostly for migration
rebuildCanReadCache :: MonadIO m => Transaction m ()
rebuildCanReadCache = do
    -- Delete all existing entries
    executeS_ (\schema -> intercalateM " " [
        "DELETE FROM " <> quoted (schema, tableCachedCanRead)
      ])

    -- Add entries for permissions granted specifically to users
    executeS_ (\schema -> intercalateM " " [
        "INSERT INTO " <> quoted (schema, tableCachedCanRead)
                       <> "(usr, sourcename, grp)"
      , "SELECT usr, sourcename, NULL"
      , "FROM " <> quoted (schema, tableDatasetUserAccess)
      ])

    -- Add entries for permissions granted to whole groups
    executeS_ (\schema -> intercalateM " " [
        "INSERT INTO " <> quoted (schema, tableCachedCanRead)
                       <> "(usr, sourcename, grp)"
      , "SELECT membership.usr, access.sourcename, access.grp"
      , "FROM " <> quoted (schema, tableDatasetGroupAccess) <> " AS access"
      , "JOIN " <> quoted (schema, tableMembership)         <> " AS membership"
      , "ON access.grp = membership.grp"
      ])

{-------------------------------------------------------------------------------
  Initial security model
-------------------------------------------------------------------------------}

-- | Some information about the security model after it's been initialized
data InitSecurityData = InitSecurityData {
     -- | Public group
     initSecurityPublic :: GroupIx

     -- | Admin user
     --
     -- The admin used is given a user ID so that we can upload data sets etc
     -- as admin and still fit within the standard security model.
   , initSecurityAdmin :: User

     -- | Unauthenticated user
     --
     -- The unauthenticated user is given a user ID so that we can use the
     -- standard security model to control what users can do before or without
     -- logging in
   , initSecurityUnauth :: User
   }

-- | Initial structure for a newly initialized database
--
-- For now we add the unauthenticated user to the public group and moreover
-- him it CREATE and CREATEGROUP rights. This should facilitate the
-- transition from the pre-security to the post-security world.
initSecurity :: MonadIO m => Transaction m InitSecurityData
initSecurity = do
    -- Create public group
    initSecurityPublic <- newGroup publicGroupName

    -- Create admin user
    -- We don't need to explicitly grant the admin user any rights as they
    -- implicitly have /all/ rights
    initSecurityAdmin <- newUser adminUserName

    -- Create unauthenticated user and give grant rights
    initSecurityUnauth@User{userIx = unauthUserIx} <- newUser unauthUserName
    addUserToGroup    unauthUserIx initSecurityPublic
    grantCreateSource unauthUserIx
    grantCreateGroup  unauthUserIx

    return InitSecurityData{..}

-- | The public group
--
-- New users are added to the public group by default, and members of the public
-- group are given access to all new sources by default, depending on the
-- setting of 'DefaultSourcePublicAccess'.
--
-- If the database is properly initialized this group always exists.
getPublicGroup :: MonadIO m => Transaction m GroupIx
getPublicGroup = getGroup (errorIfNotFound publicGroupName) publicGroupName

-- | The unauthenticated user
--
-- If the unauthenticated user is part of the "public" group, then
-- users have access to all public data sources without having to login.
--
-- If the database is properly initialized this user always exists.
getUnauthUser :: MonadIO m => Transaction m User
getUnauthUser = getUser (errorIfNotFound unauthUserName) unauthUserName

-- | The admin user
--
-- If the database is properly initialized this user always exists.
getAdminUser :: MonadIO m => Transaction m User
getAdminUser = getUser (errorIfNotFound adminUserName) adminUserName

-- | Name of the public group
publicGroupName :: GroupName
publicGroupName = "public"

-- | Name of the admin user
adminUserName :: UserName
adminUserName = "admin"

-- | Name of the "unauthenticated user"
unauthUserName :: UserName
unauthUserName = "unauthenticated-user"

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Pretty PermissionQuery where
  pretty (CanCreateSource      _uix) = "CREATE"
  pretty (CanCreateGroup       _uix) = "CREATEGROUP"
  pretty (CanManageGroup   gix _uix) = "MANAGEGROUP(" <> pretty gix <> ")"
  pretty (CanManageDataset six _uix) = "MANAGE("      <> pretty six <> ")"
  pretty (CanUpdateDataset six _uix) = "UPDATE("      <> pretty six <> ")"
  pretty (CanReadDataset   six _uix) = "READ("        <> pretty six <> ")"

{-------------------------------------------------------------------------------
  Serialization

  NOTE: We cannot define fromField and toField in terms of String instance

  > instance FromField DatasetAccessLevel where
  >   fromField f mdata = do
  >     str :: String <- fromField f mdata
  >     -- .. do something with str ..
  >
  > instance ToField DatasetAccessLevel where
  >   toField DatasetHasAccessLevelRead   = toField ("Read" :: String)
  >   ..

  as this will interfere with postgresql-simple's algorithm for detecting
  whether the SQL type matches the Haskell type and result in runtime errors
  about the Haskell type 'String' (or 'Text') not matching the SQL type
  'datdasetaccesslevel'.
-------------------------------------------------------------------------------}

instance ToField DatasetHasAccessLevel where
  toField = Escape . go
    where
      go :: DatasetHasAccessLevel -> BS.S.ByteString
      go DatasetHasAccessLevelRead   = "Read"
      go DatasetHasAccessLevelUpdate = "Update"
      go DatasetHasAccessLevelManage = "Manage"
