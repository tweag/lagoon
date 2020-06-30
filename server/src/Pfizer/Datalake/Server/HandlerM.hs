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
{-# LANGUAGE UndecidableInstances #-}
-- | The handler monad
module Pfizer.Datalake.Server.HandlerM (
    -- * Custom server monad
    STrans
  , HandlerM -- opaque
  , WithConnection(..)
  , fromSTrans
    -- * Server environment
  , ServerEnv -- opaque
  , newServerEnv
  , getAuthProvider
  , getIngestS3Config
    -- * Executing transactions
  , execTransaction
  , forkConnection
  , getWithConnection
  , withConnectionC
  , trySql
    -- * Sessions
  , S.ServerSession(..)
  , S.SessionInfo(..)
  , getSessionUser
  , getSessionAdmin
  , startSession
  , persistSession
  , resumeSession
  , closeSession
    -- * Convenience exports
  , ResourceT
  , SetCookie
  , throwM
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception (throwIO)
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Control.Monad.IO.Unlift
import Control.Natural
import Data.Conduit
import Data.Maybe (fromMaybe)
import Data.Pool
import Network.HTTP.Client.TLS (getGlobalManager)
import Servant
import Servant.Utils.Enter
import Web.Cookie
import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3   as S3

import Pfizer.Datalake.Auth
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Server.Cmdline
import Pfizer.Datalake.Server.FriendlyException.Rethrow
import Pfizer.Datalake.Server.Servant.Handler
import Pfizer.Datalake.Util.Exception
import Pfizer.Datalake.Util.PostgreSQL hiding (trySql)
import Pfizer.Datalake.Verified
import qualified Pfizer.Datalake.Util.PostgreSQL        as PG
import qualified Pfizer.Datalake.Server.Servant.Session as S

{-------------------------------------------------------------------------------
  Custom server monad
-------------------------------------------------------------------------------}

-- | Replacement for @servant@'s 'Server' type synonym with
--
-- * Server state, including support for running transactions
-- * a proper 'MonadMask' instance
--
-- See 'fromSTrans' to translate back.
type STrans a = ServerT a HandlerM

-- | The monad we write the handlers in
newtype HandlerM a = HandlerM {
      unHandlerM :: ReaderT ServerEnv Handler' a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadBase IO
           , MonadThrow
           , MonadCatch
           , MonadMask
           , MonadUnliftIO
           )

instance MonadBaseControl IO HandlerM where
  type StM HandlerM a = StM (ReaderT ServerEnv Handler') a

  liftBaseWith :: (RunInBase HandlerM IO -> IO a) -> HandlerM a
  liftBaseWith f = HandlerM $ liftBaseWith (\g -> f (g . unHandlerM))

  restoreM :: StM HandlerM a -> HandlerM a
  restoreM f = HandlerM $ restoreM f

-- | Translate 'STrans' to 'Server'
--
-- Never mind the 'Enter' constraint. This Just Works (TM).
fromSTrans :: Enter (STrans a) HandlerM  Handler (Server a)
           => ServerEnv -> Proxy a -> STrans a -> Server a
fromSTrans env _ =
  enter $ NT (fromHandler' . (`runReaderT` env) . unHandlerM . rethrowFriendly)

{-------------------------------------------------------------------------------
  Server environment
-------------------------------------------------------------------------------}

-- | Server environment
data ServerEnv = ServerEnv {
      serverDbPool   :: Pool Connection
    , serverS3Config :: Maybe IngestS3Config
    , serverDbSchema :: Schema
    , serverAuth     :: AuthProvider
    , serverSessions :: S.SessionState
    }

newServerEnv :: Cmdline
             -> S.SessionState
             -> IO Connection
             -> Schema
             -> IO ServerEnv
newServerEnv Cmdline{..} sessions mkConn schema = do
    dbPool <- createPool mkConn
                         closeConnection
                         numSubPools
                         dbGcTime
                         dbMaxConn

    mgr <- liftIO getGlobalManager
    let mkCfg creds = Aws.Configuration Aws.Timestamp creds
                      (Aws.defaultLog Aws.Warning) Nothing

        setEndpoint = maybe id (\s3Endpoint' x -> x {
                          S3.s3Endpoint     = s3EndpointHost s3Endpoint'
                        , S3.s3Port         = s3EndpointPort s3Endpoint'
                        , S3.s3RequestStyle = S3.PathStyle
                        , S3.s3Protocol     =
                            if s3EndpointInsecure s3Endpoint'
                            then Aws.HTTP
                            else Aws.HTTPS
                        }) s3Endpoint
        s3Config = setEndpoint Aws.defServiceConfig

    awsCfg <- mapM (fmap mkCfg . createS3Credentials) s3Settings

    return ServerEnv{
        serverDbPool   = dbPool
      , serverS3Config = IngestS3Config <$> awsCfg
                                        <*> Just mgr
                                        <*> Just s3Config
      , serverDbSchema = schema
      , serverAuth     = authProvider
      , serverSessions = sessions
      }
  where
    numSubPools :: Int
    numSubPools = 1

-- | Retrieve 'Credentials' from 'S3Settings', throw an exception if the
-- credentials can't be created.
createS3Credentials :: S3Settings -> IO Aws.Credentials
createS3Credentials s =
    runMaybeT (go s) >>=
      maybe (throwIO $ Aws.NoCredentialsException (show s)) pure
  where
    go :: S3Settings -> MaybeT IO Aws.Credentials
    go S3KeySecret{s3CredentialsKey,s3CredentialsSecret} =
      Aws.makeCredentials s3CredentialsKey s3CredentialsSecret
    go S3FromEnv = MaybeT Aws.loadCredentialsFromEnv
    go S3FromFile{s3CredentialsPath, s3Profile} = do
      fp <- MaybeT (pure s3CredentialsPath)
            <|> MaybeT Aws.credentialsDefaultFile
      MaybeT $ Aws.loadCredentialsFromFile fp
        (fromMaybe Aws.credentialsDefaultKey s3Profile)

-- | Return server environment (internal function, not exported)
getServerEnv :: HandlerM ServerEnv
getServerEnv = HandlerM ask

-- | Get authentication provider
getAuthProvider :: HandlerM AuthProvider
getAuthProvider = serverAuth <$> getServerEnv

-- | Get ingest S3 configuration (if any)
getIngestS3Config :: HandlerM (Maybe IngestS3Config)
getIngestS3Config = serverS3Config <$> getServerEnv

-- | Get session state
getSessionState :: HandlerM S.SessionState
getSessionState = serverSessions <$> getServerEnv

{-------------------------------------------------------------------------------
  Executing transactions
-------------------------------------------------------------------------------}

-- | Run a database transaction
execTransaction :: Transaction HandlerM a -> HandlerM a
execTransaction trans = do
    ServerEnv{..} <- getServerEnv
    withResource serverDbPool $ \conn ->
      runTransaction conn serverDbSchema trans

-- | Spawn a separate thread to handle a request
--
-- The thread will be given a 'Connection' from the pool, which will be returned
-- to the pool when the thread returns. Communication between the thread and
-- the server is the responsibility of the thread itself.
--
-- Any exception thrown by the callback before running the IO action to be
-- run in the new thread will be rethrown. Exceptions inside the IO action
-- itself will be thrown inside the new thread: the thread will be killed
-- and the connection will not be returned to the pool since we must assume
-- it's in some unusable state.
forkConnection :: (Connection -> Schema -> HandlerM (IO ()))
               -> HandlerM ThreadId
forkConnection constructThreadBody = do
    ServerEnv{..} <- getServerEnv
    -- We mask exceptions until the new thread has been spawned and an
    -- exception handler has been installed to return the connection to the pool
    mask $ \unmaskHere -> do
      (conn, pool) <- liftIO $ takeResource serverDbPool
      mBody <- try $ unmaskHere $ constructThreadBody conn serverDbSchema
      liftIO $ case mBody of
        Left (ex :: SomeException) -> do
          closeConnection conn
          destroyResource serverDbPool pool conn
          throwIO ex
        Right body ->
          forkIOWithUnmask $ \unmaskThere ->
            bracketE_ (return ())
                      (\a -> closeConnection conn >> destroyResource serverDbPool pool conn >> return a)
                      (closeConnection conn >> destroyResource serverDbPool pool conn)
                      (unmaskThere body)

newtype WithConnection = WithConnection (forall a. ((Connection -> Schema -> IO a) -> IO a))

getWithConnection :: HandlerM WithConnection
getWithConnection = do
    ServerEnv{..} <- getServerEnv
    -- When the conduit terminates successfully, we return the DB connection
    -- back to the pool. When it aborts, however, we have no choice but to
    -- assume the connection has become unuseable and so we just close it.
    -- In particular, if a client closes a connection while streaming some
    -- file, the connection will be in COPY OUT mode which we have no way to
    -- cancel.
    pure $ WithConnection $ \act -> mask $ \unmask -> do
      (conn, pool) <- liftIO $ takeResource serverDbPool
      mRes <- try $ unmask $ liftIO $ act conn serverDbSchema
      case mRes of
        Left (ex :: SomeException) -> liftIO $ do
          closeConnection conn
          destroyResource serverDbPool pool conn
          throwIO ex
        Right res -> do
          closeConnection conn
          destroyResource serverDbPool pool conn
          pure res

-- | Construct a conduit using a connection from the pool;
-- return the connection to the pool when the conduit terminates.
--
-- Any exceptions thrown by the callback before returning a conduit will be
-- rethrown by 'withConnectionC'. Obviously, any exceptions thrown inside
-- the conduit itself will only be thrown if and where the conduit is run.
--
-- This function is very similar in idea to 'forkConnection', but slightly
-- different in details since we are returning a conduit rather than forking
-- a new thread.
withConnectionC :: forall i o m a. MonadResource m
                => (Connection -> Schema -> HandlerM (ConduitM i o m a))
                -> HandlerM (ConduitM i o m a)
withConnectionC constructConduit  = do
    ServerEnv{..} <- getServerEnv
    -- When the conduit terminates successfully, we return the DB connection
    -- back to the pool. When it aborts, however, we have no choice but to
    -- assume the connection has become unuseable and so we just close it.
    -- In particular, if a client closes a connection while streaming some
    -- file, the connection will be in COPY OUT mode which we have no way to
    -- cancel.
    mask $ \unmask -> do
      (conn, pool) <- liftIO $ takeResource serverDbPool
      mConduit <- try $ unmask $ constructConduit conn serverDbSchema
      case mConduit of
        Left (ex :: SomeException) -> liftIO $ do
          closeConnection conn
          throwIO ex
        Right conduit ->
          return $ bracketPE_ (return ())
                              (\a -> closeConnection conn >> destroyResource serverDbPool pool conn >> return a)
                              (closeConnection conn >> destroyResource serverDbPool pool conn)
                              conduit

-- | Lift 'trySql' to 'HandlerM'
trySql :: HandlerM a -> HandlerM (MaybeSqlException a)
trySql act = do
    ServerEnv{..} <- getServerEnv
    PG.trySql serverDbSchema act

{-------------------------------------------------------------------------------
  Sessions
-------------------------------------------------------------------------------}

getSessionUser :: S.ServerSession -> HandlerM VerifiedUser
getSessionUser session = execTransaction $
    case session of
      S.SessionUnauth                 -> loginUnauth
      S.SessionAuth S.SessionInfo{..} -> return sessionUser

getSessionAdmin :: S.ServerSession -> HandlerM VerifiedAdmin
getSessionAdmin session = do
    user <- getSessionUser session
    case userToAdmin user of
      Just admin -> return admin
      Nothing    -> liftIO $ throwIO $ PermissionDenied "Not admin"

startSession :: VerifiedUser -> Bool -> HandlerM (S.SessionInfo, SetCookie)
startSession user persistent = do
    sessionState <- getSessionState
    S.startSession sessionState user persistent

resumeSession :: AuthToken -> HandlerM (S.SessionInfo, SetCookie)
resumeSession token = do
    sessionState <- getSessionState
    S.resumeSession sessionState token

persistSession :: S.SessionId -> HandlerM ()
persistSession sid = do
    sessionState <- getSessionState
    S.persistSession sessionState sid

closeSession :: S.SessionInfo -> HandlerM SetCookie
closeSession info = do
    sessionState <- getSessionState
    S.closeSession sessionState info
