-- | Wrappers around other functions that obtain access permissions
--
-- Any user facing code should probably import this module rather than the
-- lower level modules that export the raw functionality. That way we can
-- be sure we are always verifying access permissions. We intentially reuse
-- the names from those modules to encourage client code not to import both
-- (as this would lead to name clashes).
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Pfizer.Datalake.Verified (
    -- * Authentication
    VerifiedAdmin -- opaque
  , VerifiedUser  -- opaque
  , adminAllPerms
  , adminToUser
  , userToAdmin
  , unverifyUser
    -- * Permissions
  , PermissionDeniedException(..)
  , KnownPermission -- opaque
  , Can             -- opaque
  , Permission(..)
  , checkHasPermission
  , checkHasPermissions
  , checkVersionPerm
    -- * Wrappers around permission management
    -- ** Users
  , login
  , loginAdmin
  , loginUnauth
  , lookupUserName
  , createUser
  , grantCreateSource
  , revokeCreateSource
  , grantCreateGroup
  , revokeCreateGroup
    -- ** Groups
  , getGroup
  , getPublicGroup
  , createGroup
  , grantManageGroup
  , revokeManageGroup
  , addUserToGroup
  , removeUserFromGroup
  , getGroupMembers
    -- ** Datasets
  , setUserDatasetAccess
  , setGroupDatasetAccess
    -- ** Database management
  , initDb
  , resetDb
  , changeDbAdminPass
  , migrate
  , rebuildCanReadCache
  , checkDbVersion
    -- ** Individual Sources
  , getSourceByIdentifier
  , getExistingSourceName
  , deleteSource
  , getVersion
  , getVersions
  , getColumn
  , getSourceInfo
  , makeTyped
  , setColumnType
  , setDeprecated
  , tagSource
  , untagSource
  , downloadSource
  , downloadSourceToHandle
    -- ** Creating new sources and searching for existing sources
  , newSourceName
  , ingest
  , ingestFoo
  , inferJsonTypeOfFile
  , getSources
  , getSourcesCount
    -- ** Executing arbitrary SQL
  , verifyUserQuery
  , execUserQuery
    -- ** Debugging
  , dumpDbInfo
    -- * Type re-exports for the modules .Verified is meant to replace
  , AuthProvider
  , DB.MigrationFailure(..)
  , DB.MigrationRequired(..)
  , DB.NotFoundException(..)
  , DB.AlreadyExistsException(..)
  , SourceBS
  , IngestS3Config(..)
  , NotifyInput
  , InputStatus(..)
  ) where

import Prelude hiding (Read)
import Control.Exception (Exception, throwIO)
import Control.Monad
import Control.Monad.Catch (MonadCatch)
import Data.ByteString.Builder (Builder)
import Data.Conduit hiding (Source)
import Data.Functor.Identity
import Data.Proxy
import System.IO (Handle)

import Pfizer.Datalake.Auth
import Pfizer.Datalake.Ingest.Progress
import Pfizer.Datalake.Ingest (SourceBS, IngestS3Config(..), NotifyInput, InputStatus(..), SourceIngestConfig(..))
import Control.Monad.IO.Unlift
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL
import qualified Pfizer.Datalake.DB                        as DB
import qualified Pfizer.Datalake.Ingest                    as Ingest
import qualified Pfizer.Datalake.Download                  as Download
import qualified Pfizer.Datalake.Util.PostgreSQL.QueryPlan as QueryPlan

-- import some types unqualified
import Pfizer.Datalake.DB (
    DbAdminPass
  , DefaultCanCreateGroup(..)
  , DefaultCanCreateSource(..)
  , DefaultSourcePublicAccess(..)
  , IfNotFound
  , PermissionQuery(..)
  , PermissionUpdate(..)
  )

{-------------------------------------------------------------------------------
  Verified users
-------------------------------------------------------------------------------}

-- | Verified DB admin
--
-- The DB admin is authenticated using the DB admin password which we
-- store separately in the database. This means the DB user is independent
-- from the external authentication service.
--
-- See also 'VerifiedUserAdmin'.
data VerifiedAdmin = VerifiedAdmin DbAdminPass User

-- | Verified user
data VerifiedUser =
    -- | Authenticated user
    --
    -- Users are authenticated using an external 'AuthProvider'.
    VerifiedUser User

    -- | The DB admin can be treated as a regular user
  | VerifiedUserAdmin VerifiedAdmin

-- | The administrator user has all permissions
adminAllPerms :: VerifiedAdmin -> a -> Can p a
adminAllPerms (VerifiedAdmin _ user) = Verified user

-- | Treat administrator as normal user
adminToUser :: VerifiedAdmin -> VerifiedUser
adminToUser = VerifiedUserAdmin

-- | Not all users are admins
userToAdmin :: VerifiedUser -> Maybe VerifiedAdmin
userToAdmin (VerifiedUserAdmin admin) = Just admin
userToAdmin (VerifiedUser _)          = Nothing

-- | "Forget" that a user was verified
--
-- This is occassionally useful to get some information about an already
-- verified user.
unverifyUser :: VerifiedUser -> User
unverifyUser (VerifiedUser user)                        = user
unverifyUser (VerifiedUserAdmin (VerifiedAdmin _ user)) = user

{-------------------------------------------------------------------------------
  Permission language
-------------------------------------------------------------------------------}

data Permission a = Create a | Read | Update | Manage

-- | We verified that a particular user has the specified permission
data Can (p :: Permission *) (a :: *) = Verified User a
  deriving Functor

{-------------------------------------------------------------------------------
  Obtain permissions
-------------------------------------------------------------------------------}

class KnownPermission (p :: Permission *) (a :: *) where
  permission :: MonadIO m
             => Proxy p -> UserIx -> a -> Transaction m DB.PermissionQuery

-- | Permission denied exception
--
-- The reason we provide here is mostly for debugging. It could appear in the
-- server log perhaps, but is not intended to be shown to the client as this
-- might lead to information leakage.
data PermissionDeniedException = PermissionDenied String
  deriving (Show)

instance Exception PermissionDeniedException

instance FriendlyException PermissionDeniedException where
  displayFriendly (PermissionDenied reason) = "Permission denied: " ++ reason

-- | Constructor for the common case where we have a user and a permission query
permissionDenied :: User -> DB.PermissionQuery -> PermissionDeniedException
permissionDenied User{..} requiredPermission = PermissionDenied explanation
  where
    explanation :: String
    explanation = concat [
        "User " ++ userName ++ " (" ++ prettyStr userIx ++ ") "
      , "does not have " ++ prettyStr requiredPermission ++ " permission"
      ]

-- | Check if the specified user has permission @p@ on entity @a@
checkHasPermission :: forall m p a. (MonadIO m, KnownPermission p a)
                   => VerifiedUser -> a -> Transaction m (Can p a)
checkHasPermission vu a =
    fmap runIdentity <$> checkHasPermissions vu (Identity a)

-- | Check if the specified user has permission @p@ on entity @a@
checkHasPermissions :: forall m p a t. (MonadIO m, KnownPermission p a, Traversable t)
                    => VerifiedUser -> t a -> Transaction m (Can p (t a))
checkHasPermissions (VerifiedUser user@User{..}) as = Verified user <$>
    (
    forM as $ \a -> do

    requiredPermission <- permission (Proxy :: Proxy p) userIx a
    hasPerm <- DB.queryPermissions requiredPermission
    if hasPerm
      then pure a
      else liftIO $ throwIO $ permissionDenied user requiredPermission
    )
checkHasPermissions (VerifiedUserAdmin admin) as =
    return $ adminAllPerms admin as

-- | Special case of 'checkHasPermissions' for source versions
--
-- Any permissions that are granted to the source itself automatically
-- extends to any version of that source.
checkVersionPerm :: forall m p. (MonadIO m, KnownPermission p SourceNameIx)
                 => VerifiedUser -> SourceIx -> Transaction m (Can p SourceIx)
checkVersionPerm verifiedUser sourceIx = do
    Source{..} <- DB.getSourceOfVersion sourceIx
    aux <$> checkHasPermission verifiedUser sourceNameIx
  where
    aux :: Can p SourceNameIx -> Can p SourceIx
    aux (Verified user _source) = Verified user sourceIx

{-------------------------------------------------------------------------------
  Users
-------------------------------------------------------------------------------}

-- | Login
--
-- If the user name provided is @admin@, we don't use the authentication
-- provider but instead attempt to authenticate the user as the DB admin.
-- (We treat this case special because the admin user implicitly has all
-- permissions.)
login :: MonadIO m
      => AuthProvider
      -> Credentials
      -> Transaction m (LoginResult VerifiedUser)
login _auth (Credentials { credsUser = "admin", credsPass = pw }) =
    fmap adminToUser <$> loginAdmin (PlaintextPassword pw)
login authProvider creds = do
    loginResult <- liftIO $ verifyCreds authProvider creds
    case loginResult of
      LoginFailed err -> return $ LoginFailed err
      LoginOk () -> do
        uix <- DB.getUser createIfNotFound (credsUser creds)
        return $ LoginOk $ VerifiedUser uix
  where
    -- | Create a new user if none was found
    createIfNotFound :: MonadIO m => IfNotFound () (Transaction m) User User
    createIfNotFound (Just a) _      = return a
    createIfNotFound Nothing  create = createDefaultUser (create ())

-- | Login as the admin user
loginAdmin :: MonadIO m
           => PlaintextPassword -> Transaction m (LoginResult VerifiedAdmin)
loginAdmin pw = do
    isValid <- DB.validateDbAdminPass pw
    case isValid of
      Nothing   -> return $ LoginFailed LoginInvalidCreds
      Just pass -> LoginOk . VerifiedAdmin pass <$> DB.getAdminUser

-- | We can trivially " login " as the unauthorized user
--
-- Our access rights when logging in as the unauthenticated user
-- (in other words, our access rights if we don't login) depend on the
-- access rights of the unauthenticated-user in the security model.
loginUnauth :: MonadIO m => Transaction m VerifiedUser
loginUnauth = VerifiedUser <$> DB.getUnauthUser

-- | Get identity of existing user
--
-- The returned user is not verified in any way; this function is primarily
-- useful when granting or revoking permissions to users: if user A has
-- MANAGE permissions over dataset D, and wants to grant access to, or revoke
-- access from, user B, then
--
-- * User A first authenticates using 'getUser'
-- * Obtains the appropriate permissions using 'checkHasPermission'
-- * Obtains user B's ID using 'lookupUserName'
-- * Finally calls 'setUserDatasetAccess'
lookupUserName :: MonadIO m => UserName -> Transaction m User
lookupUserName userName = DB.getUser (DB.errorIfNotFound userName) userName

-- | Create a new user
--
-- Usually we create entries for users implicitly when users log in. We avoid
-- typos in usernames because we only do this after the user's credentials
-- have been verified by the authentication service.
--
-- In commands such as 'grantCreateSource', we have no way of verifying whether
-- the username is correct and therefore we insist in 'lookupUserName' that the
-- username must already exist (typically because the user has logged in
-- previously).
--
-- Occassionally it is necessary however to grant privileges to, or revoke
-- privileges from, users that have never logged in yet. In that case the
-- administrator can explicitly create an entry for this user. We allow _only_
-- the administrator to do this as we have no way of verifying that the
-- username is correct.
createUser :: MonadIO m => VerifiedAdmin -> UserName -> Transaction m User
createUser _verifiedAdmin = createDefaultUser . DB.newUser

-- | Grant a user CREATE privileges
--
-- New users are granted this by default (depending 'DefaultCanCreateSource');
-- but only the DB admin can explicitly grant or revoke this.
grantCreateSource :: MonadIO m => VerifiedAdmin -> User -> Transaction m ()
grantCreateSource _verifiedAdmin User{..} =
    DB.updatePermissions $ GrantCreateSource userIx

-- | Revoke CREATE privileges
--
-- New users are granted this by default (depending 'DefaultCanCreateSource');
-- but only the DB admin can explicitly grant or revoke this.
revokeCreateSource :: MonadIO m => VerifiedAdmin -> User -> Transaction m ()
revokeCreateSource _verifiedAdmin User{..} =
    DB.updatePermissions $ RevokeCreateSource userIx

-- | Grant a user CREATEGROUP privileges
--
-- New users are granted this by default (depending 'DefaultCanCreateGroup');
-- but only the DB admin can explicitly grant or revoke this.
grantCreateGroup :: MonadIO m => VerifiedAdmin -> User -> Transaction m ()
grantCreateGroup _verifiedAdmin User{..} =
    DB.updatePermissions $ GrantCreateGroup userIx

-- | Revoke CREATEGROUP privileges
--
-- New users are granted this by default (depending 'DefaultCanCreateGroup');
-- but only the DB admin can explicitly grant or revoke this.
revokeCreateGroup :: MonadIO m => VerifiedAdmin -> User -> Transaction m ()
revokeCreateGroup _verifiedAdmin User{..} =
    DB.updatePermissions $ RevokeCreateGroup userIx

{-------------------------------------------------------------------------------
  Groups
-------------------------------------------------------------------------------}

instance KnownPermission ('Create GroupIx) () where
  permission _ userIx () = return $ DB.CanCreateGroup userIx

instance KnownPermission 'Manage GroupIx where
  permission _ userIx gix = return $ DB.CanManageGroup gix userIx

-- | Get ID of given group
getGroup :: MonadIO m => GroupName -> Transaction m GroupIx
getGroup groupName = DB.getGroup (DB.errorIfNotFound groupName) groupName

-- | Get ID of public group
getPublicGroup :: MonadIO m => Transaction m GroupIx
getPublicGroup = DB.getPublicGroup

-- | Create a new group
createGroup :: MonadIO m
            => Can ('Create GroupIx) ()
            -> GroupName
            -> Transaction m GroupIx
createGroup (Verified User{..} ()) groupName = do
    gix <- DB.updatePermissions $ NewGroup groupName
    DB.updatePermissions $ GrantManageGroup userIx gix
    return gix

-- | Grant MANAGEGROUP permission
grantManageGroup :: MonadIO m
                 => Can 'Manage GroupIx -> User -> Transaction m ()
grantManageGroup (Verified _user gix) User{..} =
    DB.updatePermissions $ GrantManageGroup userIx gix

-- | Revoke MANAGEGROUP permission
revokeManageGroup :: MonadIO m
                  => Can 'Manage GroupIx -> User -> Transaction m ()
revokeManageGroup (Verified _user gix) User{..} =
    DB.updatePermissions $ RevokeManageGroup userIx gix

-- | Add user to a group
addUserToGroup :: MonadIO m
               => Can 'Manage GroupIx -> User -> Transaction m ()
addUserToGroup (Verified _user gix) User{..} =
    DB.updatePermissions $ AddUserToGroup userIx gix

-- | Remove user from group
removeUserFromGroup :: MonadIO m
                    => Can 'Manage GroupIx -> User -> Transaction m ()
removeUserFromGroup (Verified _user gix) User{..} =
    DB.updatePermissions $ RemoveUserFromGroup userIx gix

-- | List of group members
getGroupMembers :: MonadIO m
                => Can 'Manage GroupIx -> Transaction m [UserName]
getGroupMembers (Verified _user gix) =
    DB.getGroupMembers gix

-- | Set dataset access level for a specific user
--
-- TODO: It might be a good idea to add a 'Force' argument which would be
-- required when _reducing_ permissions.
setUserDatasetAccess :: MonadIO m
                     => Can 'Manage SourceNameIx
                     -> User
                     -> DatasetAccessLevel
                     -> Transaction m ()
setUserDatasetAccess (Verified _user sourceNameIx) User{..} level =
    DB.updatePermissions $ SetUserDatasetAccess userIx sourceNameIx level

-- | Set dataset access level for a whole group
--
-- TODO: It might be a good idea to add a 'Force' argument which would be
-- required when _reducing_ permissions.
setGroupDatasetAccess :: MonadIO m
                      => Can 'Manage SourceNameIx
                      -> GroupIx
                      -> DatasetAccessLevel
                      -> Transaction m ()
setGroupDatasetAccess (Verified _user sourceNameIx) gix level =
    DB.updatePermissions $ SetGroupDatasetAccess gix sourceNameIx level

{-------------------------------------------------------------------------------
  Database management
-------------------------------------------------------------------------------}

-- | Initialize the database
--
-- No password required (how would we obtain the password if we didn't have
-- a database containing passwords yet?); however, will fail if the database
-- has already been initialized.
initDb :: MonadIO m => PlaintextPassword -> Transaction m ()
initDb pw = do
    dbAdminPass <- liftIO $ DB.hashDbAdminPass pw
    DB.initDb dbAdminPass

-- | Reset the state of the database
resetDb :: Connection -> Schema -> VerifiedAdmin -> IO ()
resetDb conn schema _ = DB.resetDb conn schema

-- | Change admin password
changeDbAdminPass :: MonadIO m
                  => VerifiedAdmin -> PlaintextPassword -> Transaction m ()
changeDbAdminPass _ new =
    DB.setDbMeta =<< liftIO (DB.hashDbAdminPass new)

-- | Run migration
--
-- Currently no password required.
-- TODO: We might want to require administrator access here.
migrate :: MonadIO m => Transaction m ()
migrate = DB.migrate

-- | Rebuild the can-read cache
rebuildCanReadCache :: MonadIO m => VerifiedAdmin -> Transaction m ()
rebuildCanReadCache _verifiedAdmin = DB.rebuildCanReadCache

-- | Make sure that the DB version is current
--
-- No permissions requireed.
checkDbVersion :: MonadIO m => Transaction m ()
checkDbVersion = DB.checkDbVersion

{-------------------------------------------------------------------------------
  Individual sources
-------------------------------------------------------------------------------}

instance KnownPermission 'Manage SourceNameIx where
  permission _ userIx sourceNameIx =
    return $ DB.CanManageDataset sourceNameIx userIx

instance KnownPermission 'Update SourceNameIx where
  permission _ userIx sourceNameIx =
    return $ DB.CanUpdateDataset sourceNameIx userIx

instance KnownPermission 'Read SourceNameIx where
  permission _ userIx sourceNameIx =
    return $ DB.CanReadDataset sourceNameIx userIx

-- | Permissions for sources extend to all versions of that source
instance KnownPermission p SourceNameIx => KnownPermission p SourceIx where
  permission p userIx sourceIx = do
    Source{..} <- DB.getSourceOfVersion sourceIx
    permission p userIx sourceNameIx

-- | Only UPDATE permission makes sense for columns
--
-- This is used when overriding column types.
instance KnownPermission 'Update SourceIx => KnownPermission 'Update ColumnIx where
  permission p userIx columnIx = do
    sourceIx <- DB.getSourceWithColumn columnIx
    permission p userIx sourceIx

-- | Get existing source name
--
-- This doesn't require any permissions, though we might subsequently have to
-- obtain permissions specifically for this source. There is a minor information
-- leak here (we can check if a source name exists or not without any
-- permissions) but this is hard to avoid.
getExistingSourceName :: MonadIO m => SourceName -> Transaction m (Maybe Source)
getExistingSourceName sourceName =
    DB.getSourceName DB.emptyIfNotFound sourceName

-- | Delete an existing source
deleteSource :: MonadIO m
             => Can 'Update SourceIx -> Transaction m ()
deleteSource (Verified _user sourceIx) = do
    sinfo <- getSourceInfo' sourceIx
    DB.dropSource sinfo

-- | Get ID for a specific version of a source
getVersion :: MonadIO m
           => Can 'Read SourceNameIx -> Maybe Version -> Transaction m SourceIx
getVersion (Verified _ sourceNameIx) = DB.getVersion sourceNameIx

-- | Get IDs for all versions of a source
getVersions :: MonadIO m
            => Can 'Read SourceNameIx -> Transaction m [SourceIx]
getVersions (Verified _ sourceNameIx) = DB.getVersions sourceNameIx

-- | Get column ID
getColumn :: MonadIO m
          => Can 'Read SourceIx -> ColumnName -> Transaction m ColumnIx
getColumn (Verified _ six) = DB.getColumn six

-- | Get information about a specific source
getSourceInfo :: MonadIO m
              => Can 'Read SourceIx -> Transaction m (Maybe SourceInfo)
getSourceInfo (Verified _ six) = DB.getSourceInfo six

-- | Create typed version of the specified source
makeTyped :: (MonadMask m, MonadIO m)
          => Logger IO IngestProgress
          -> CreateIndices
          -> Can 'Update SourceIx
          -> Transaction m SourceInfo
makeTyped logger createIndices (Verified _ sourceIx) = do
    sourceInfo <- getSourceInfo' sourceIx
    DB.makeTyped logger createIndices sourceInfo

-- | Override column type
setColumnType :: MonadIO m
              => Can 'Update ColumnIx -> ColumnType -> Transaction m ()
setColumnType (Verified _user columnIx) typ =
    DB.setColumnType columnIx typ

-- | (Un)deprecate a source
--
-- TODO: Not sure what the permissions on this should be. For now I've chosen
-- for @MANAGE@; users with only @UPDATE@ permissions can of course still upload
-- new versions, thereby deprecating the older ones; but explicitly deprecating
-- an existing source without uploading a new one requires @UPDATE@ permissions.
-- This also fits nicely with 'SetAttribute' in the EDSL spec.  Of course if it
-- doesn't work well from a usability perspective we'll need to change this.
setDeprecated :: MonadIO m
              => Can 'Manage SourceIx -> Bool -> Transaction m ()
setDeprecated (Verified _user sourceIx) deprecated =
    DB.setDeprecated sourceIx deprecated

-- | Tag a datasource
tagSource :: MonadIO m
          => Can 'Update SourceIx -> TagName -> Transaction m Bool
tagSource (Verified _user sourceIx) tagName =
    DB.tagSource sourceIx tagName

-- | Untag a datasource
untagSource :: MonadIO m
            => Can 'Update SourceIx -> TagName -> Transaction m Bool
untagSource (Verified _user sourceIx) tagName =
    DB.untagSource sourceIx tagName

-- | Download the specified source and stream it to a handle
downloadSourceToHandle :: MonadIO m
                       => Can 'Read SourceIx
                       -> Handle
                       -> Transaction m ()
downloadSourceToHandle (Verified _user sourceIx) handle = do
    sourceInfo <- getSourceInfo' sourceIx
    Download.downloadSourceToHandle sourceInfo handle

-- | Download the specified source
downloadSource :: MonadIO m
               => Can 'Read SourceIx
               -> Producer (Transaction m) (Flush Builder)
downloadSource (Verified _user src) = do
    sourceInfo <- lift $ getSourceInfo' src
    Download.downloadSource sourceInfo

{-------------------------------------------------------------------------------
  Creating new sources or searching for sources
-------------------------------------------------------------------------------}

instance KnownPermission ('Create Source) () where
  permission _ userIx () = return $ DB.CanCreateSource userIx

-- | Create a new source name
--
-- The user creating the new source will be granted MANAGE permissions on the
-- new source by default. Moreover, if 'DefaultSourcePublic' is 'True' the
-- the public group will be given read access to the new source.
newSourceName :: MonadIO m
              => Can ('Create Source) ()
              -> SourceName
              -> IngestPrivate
              -> Transaction m Source
newSourceName (Verified User{..} ()) sourceName private = do
    source@Source{sourceNameIx} <- DB.newSourceName sourceName userIx
    DB.updatePermissions $
      SetUserDatasetAccess userIx sourceNameIx DatasetAccessLevelManage
    Just (DefaultSourcePublicAccess publicAccess) <- DB.getDbMeta
    unless (private || DB.hasNoAccess publicAccess) $ do
      publicGroupIx <- DB.getPublicGroup
      DB.updatePermissions $
        SetGroupDatasetAccess publicGroupIx sourceNameIx publicAccess
    return source

-- | Retrieve a source by its 'SourceIdentifier'.
--
-- Only sources visible to the user are considered. If there are more than one
-- such sources there are no guarantees as to which will be returned.
getSourceByIdentifier :: MonadIO m
                      => VerifiedUser
                      -> SourceIdentifier
                      -> Transaction m (Maybe SourceInfo)
getSourceByIdentifier user sourceId = do
    Sources sources <- getSources user spec
    case sources of
      [] -> pure Nothing
      sinfo:_xs -> pure (Just sinfo)
  where
    spec = def { sourcesFilterTags = [ getSourceIdentifierTag sourceId ]
               , sourcesLimit = Just 1
               }

-- | Ingest a (new version of) a data source
--
-- NOTE: The 'Can p Source' permission carries both the user identity /and/
-- evidence that that user can update this particular source. It would not be
-- correct to have the user and the permission to update the source as two
-- separate unconnected values.
ingest :: Connection
       -> Maybe IngestS3Config
       -> Schema
       -> Logger IO IngestProgress
       -> CreateIndices
       -> Can 'Update SourceNameIx
       -> IngestOptions
       -> CreateOptions
       -> Input SourceBS
       -> NotifyInput
       -> IO SourceInfo
ingest conn
       creds
       schema
       logger
       createIndices
       (Verified user sourceNameIx)
       ingestOptions
       createOptions
       input
       notifyInput
     = do
    source <- runTransaction conn schema $ DB.getSourceNameWithId sourceNameIx
    Ingest.ingest conn
                  creds
                  schema
                  logger
                  user
                  SourceIngestConfig
                    { sourceSource = source
                    , sourceCreateIndices = createIndices
                    , sourceIngestOptions = ingestOptions
                    , sourceInput = input
                    , sourceCreateOptions = createOptions
                    }
                  notifyInput

-- | Infer JSON type
--
-- This does not require any permissions
inferJsonTypeOfFile :: Maybe IngestS3Config
                    -> Maybe DecompressMethod
                    -> Input SourceBS
                    -> NotifyInput
                    -> IO JsonType
inferJsonTypeOfFile = Ingest.inferJsonTypeOfFile

-- | Get sources matching the given specification
--
-- We take a 'VerifiedUser' as an argument, so that we can return only
-- sources that this user has access to.
--
-- If the user happens to be the database administrator then of course they have
-- access to _all_ sources; in that case, we leave the 'SourcesSpec' alone (so
-- that in principle the admin can ask for "all sources readable by
-- such-and-such a person").
getSources :: forall m. MonadIO m
           => VerifiedUser -- ^ So we know which sources to filter out
           -> SourcesSpec
           -> Transaction m Sources
getSources (VerifiedUser User{..}) spec =
    DB.getSources spec { sourcesReadableBy = Just userIx }
getSources (VerifiedUserAdmin (VerifiedAdmin _ _)) spec =
    DB.getSources spec

-- | Get the count of sources matching the given specification
--
-- We take a 'VerifiedUser' as an argument, so that we can consider only
-- sources that this user has access to.
--
-- If the user happens to be the database administrator then of course they have
-- access to _all_ sources; in that case, we leave the 'SourcesSpec' alone (so
-- that in principle the admin can ask for "all sources readable by
-- such-and-such a person").
getSourcesCount :: forall m. MonadIO m
           => VerifiedUser -- ^ So we know which sources to filter out
           -> SourcesSpec
           -> Transaction m SourcesCount
getSourcesCount (VerifiedUser User{..}) spec =
    DB.getSourcesCount spec { sourcesReadableBy = Just userIx }
getSourcesCount (VerifiedUserAdmin (VerifiedAdmin _ _)) spec =
    DB.getSourcesCount spec

-- TODO: explain why there are so many connections
ingestFoo ::
          (forall a. (Connection -> Schema -> IO a) -> IO a)
          -> Logger IO IngestProgress
          -> CreateOptions
          -> CreateIndices
          -> Can 'Update SourceNameIx
          -> Can 'Read [SourceIx]
          -> NotifyInput
          -> IO SourceInfo
ingestFoo
          withConn
          logger
          createOptions
          createIndices
          (Verified user sourceNameIx)
          (Verified _user' sixs) -- TODO match users
          notifyInput
        = do
    (source, sources) <- withConn $ \conn schema -> do
      source <- runTransaction conn schema $ DB.getSourceNameWithId sourceNameIx
      sources <- mapM (runTransaction conn schema . DB.getSourceInfo') sixs
      pure (source, sources)
    Ingest.ingestFoo
                     withConn
                     logger
                     user
                     createOptions
                     createIndices
                     source
                     sources
                     notifyInput

{-------------------------------------------------------------------------------
  Executing arbitrary SQL
-------------------------------------------------------------------------------}

-- | Verify a query
verifyUserQuery :: forall m. MonadIO m
                => VerifiedUser
                -> UserQuery
                -> Transaction m (Can 'Read UserQuery)
verifyUserQuery (VerifiedUserAdmin admin) q =
    return $ adminAllPerms admin q
verifyUserQuery (VerifiedUser user@User{..}) q@(UserQuery userQuery) = do
    schema <- getSchema
    mPlan  <- QueryPlan.getQueryPlan $ fromString userQuery
    case QueryPlan.summary =<< mPlan of
      Left _err -> denied "Could not parse query plan"
      Right QueryPlan.Summary{..} -> do
        forM_ summaryWrites $ \QueryPlan.Relation{..} ->
          denied $ "Write to " ++ show relName ++ " not allowed"
        forM_ summaryReads $ \rel@QueryPlan.Relation{..} -> do
          if isMetadataTable schema rel
            then do
              unless allowReadMetadata $
                denied $ "Direct access to the metadata not allowed"
            else do
              unless (relSchema `isSchema` schema) $
                denied $ "Unknown schema " ++ show relSchema
              mSource <- DB.getSourceOfTable relName
              Source{..} <- case mSource of
                              Nothing -> denied $ "Unknown table " ++ show relName
                              Just source -> return source
              let requiredPermission = CanReadDataset sourceNameIx userIx
              hasReadAccess <- DB.queryPermissions requiredPermission
              unless hasReadAccess $
                liftIO $ throwIO $ permissionDenied user requiredPermission
        return $ Verified user q
  where
    denied :: String -> Transaction m a
    denied reason = liftIO $ throwIO $ PermissionDenied reason

    isSchema :: String -> Schema -> Bool
    "public" `isSchema` (Schema "") = True
    s'       `isSchema` (Schema s)  = s' == s

    -- Should we allow clients to read the metadata tables directly?
    allowReadMetadata :: Bool
    allowReadMetadata = True

    -- Is metadata table?
    isMetadataTable :: Schema -> QueryPlan.Relation -> Bool
    isMetadataTable schema QueryPlan.Relation{..} =
         (relSchema `isSchema` schema && relName `elem` ourMetadataTables)
      || relSchema == "pg_catalog"

    -- Our metadata tables
    ourMetadataTables :: [String]
    ourMetadataTables = [
        "users"
      , "sourcenames"
      , "sources"
      , "sourcecolumns"
      , "typedsources"
      , "tagnames"
      , "tags"
      ]

-- | Execute an arbitrary SQL query
execUserQuery :: (MonadIO m, MonadIO m', MonadCatch m')
              => Can 'Read UserQuery
              -> CopyToFormat
              -> Transaction m' (Conduit i (Transaction m) (Flush Builder))
execUserQuery (Verified _ userQuery) format =
    DB.execUserQuery userQuery format

{-------------------------------------------------------------------------------
  Auxiliary internal functions (not exported, no permission checks)
-------------------------------------------------------------------------------}

-- | Get source info for specific Ix
--
-- Throws 'NotFound' exception if the index is invalid.
getSourceInfo' :: MonadIO m => SourceIx -> Transaction m SourceInfo
getSourceInfo' sourceIx = do
    mSourceInfo <- DB.getSourceInfo sourceIx
    case mSourceInfo of
      Just sourceInfo -> return sourceInfo
      Nothing         -> liftIO $ throwIO $ DB.NotFound sourceIx

-- | Create new user with default privileges
createDefaultUser :: MonadIO m => Transaction m User -> Transaction m User
createDefaultUser create = do
    user@User{..} <- create
    publicGroupIx <- DB.getPublicGroup
    DB.updatePermissions $ AddUserToGroup userIx publicGroupIx
    Just (DefaultCanCreateSource canCreateSource) <- DB.getDbMeta
    Just (DefaultCanCreateGroup  canCreateGroup)  <- DB.getDbMeta
    when canCreateSource $ DB.updatePermissions $ GrantCreateSource userIx
    when canCreateGroup  $ DB.updatePermissions $ GrantCreateGroup  userIx
    return user

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Dump DB info
dumpDbInfo :: VerifiedAdmin -> Connection -> Schema -> IO Sources
dumpDbInfo _verifiedAdmin = DB.dumpDbInfo
