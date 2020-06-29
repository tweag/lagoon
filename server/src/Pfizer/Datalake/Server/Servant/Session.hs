-- | Basic session support
--
-- For now we implement a very simple session support layer. If this needs
-- to be more advanced, we might want to take a look at
--
-- * http://hackage.haskell.org/package/servant-auth-cookie
-- * http://hackage.haskell.org/package/servant-auth-server /
--   http://hackage.haskell.org/package/servant-auth
--
-- TODO: Both the client and the server need to be aware of the concept of a
-- 'Session', but they have different interpretations. Therefore, we introduce
-- the (empty) 'Session' type in the interface library, and provide the
-- 'HasServer' instance here. However, that makes that 'HasServer' instance
-- an orphan; I'm not sure exactly what the right approach here is.
-- (Same problem occurs in other modules too.)
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Pfizer.Datalake.Server.Servant.Session (
    SessionState -- opaque
  , ServerSession(..)
  , SessionInfo(..)
  , SessionId
  , SessionException(..)
  , newSessionState
  , startSession
  , persistSession
  , resumeSession
  , closeSession
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Time
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Server.Internal.RoutingApplication
import Web.Cookie
import qualified Data.ByteString  as BS.S
import qualified Data.Map.Strict  as Map
import qualified Data.UUID        as UUID
import qualified Data.UUID.V4     as UUID
import qualified Text.PrettyPrint as PP

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Interface.API
import Pfizer.Datalake.Verified
import Pfizer.Datalake.Server.FriendlyException.ReportError

{-------------------------------------------------------------------------------
  Servant layer
-------------------------------------------------------------------------------}

instance ( HasContextEntry ctxt SessionState
         , HasServer sub ctxt
         ) => HasServer (Session :> sub) ctxt where
  type ServerT (Session :> sub) m = ServerSession -> ServerT sub m

  route Proxy ctxt sub =
      route (Proxy :: Proxy sub)
            ctxt
            (sub `addAuthCheck` (withRequest . aux . getContextEntry $ ctxt))
    where
      aux :: SessionState -> Request -> DelayedIO ServerSession
      aux sst req = do
          case lookup sessionIdCookieName cookies of
            Nothing        -> return SessionUnauth
            Just sessionId -> do
              -- It is important that we don't throw any exceptions here
              mSession <- liftIO $ try $ getSession sst sessionId
              case mSession of
                Left (ex :: SessionException) ->
                  delayedFailFatal $ reportError ex
                Right session ->
                  return $ SessionAuth session
        where
          cookies :: Cookies
          cookies = fromMaybe []
                  $ fmap parseCookies
                  $ lookup hCookie
                  $ requestHeaders req

{-------------------------------------------------------------------------------
  Session infrastructure proper
-------------------------------------------------------------------------------}

type SessionId = BS.S.ByteString

-- | Internal type recording the session state
--
-- We export this only as an opaque type. To create a new session state,
-- use 'newSessionState', then use @servant@'s 'serveWithContext' to
-- make the state available.
data SessionState = SessionState {
      -- | Current active sessions
      sessionsVar :: MVar (Map SessionId SessionInfo)

      -- | Default expiry time for non-persisent sessions
    , sessionsExpiryTime :: NominalDiffTime

      -- | How often should we check for stale sessions?
    , sessionsGcInterval :: MuSec

      -- | Thread ID of the thread cleaning up stale sessions
    , sessionsGc :: ThreadId
    }

newSessionState :: MonadIO m => m SessionState
newSessionState = liftIO $ mdo
    let sessionsExpiryTime = oneDay
        sessionsGcInterval = 30 * oneMinute -- NOTE: overflow if longer
    sessionsVar <- newMVar Map.empty
    sessionsGc  <- forkIO $ gc st
    let st = SessionState{..}
    return st

-- | Garbage collect stale sessions
gc :: SessionState -> IO ()
gc SessionState{..} = forever $ do
    threadDelay sessionsGcInterval

    now <- getCurrentTime
    let notExpired :: SessionInfo -> Bool
        notExpired SessionInfo{..} = or [
            sessionPersistent
          , now `diffUTCTime` sessionLastAccess < sessionsExpiryTime
          ]

    modifyMVar_ sessionsVar $ return . Map.filter notExpired

startSession :: MonadIO m
             => SessionState
             -> VerifiedUser
             -> Bool          -- ^ Persistent session?
             -> m (SessionInfo, SetCookie)
startSession SessionState{..} user persistent = liftIO $ do
    sessionId <- UUID.toASCIIBytes <$> UUID.nextRandom
    now       <- getCurrentTime
    let sessionInfo = SessionInfo {
            sessionInfoId     = sessionId
          , sessionToken      = AuthToken sessionId
          , sessionUser       = user
          , sessionPersistent = persistent
          , sessionLastAccess = now
          , sessionLoginInfo  = LoginInfo {
                loginInfoUsername = userName (unverifyUser user)
              }
          }
    cookie <- sessionSetCookie sessionId (persistentToExpiry persistent)
    modifyMVar_ sessionsVar $ return . Map.insert sessionId sessionInfo
    return (sessionInfo, cookie)

-- | Access the session state (internal function)
--
-- This automatically updates the session last access field.
withSession :: MonadIO m
            => SessionState
            -> SessionId
            -> ((Map SessionId SessionInfo, SessionInfo) -> IO (Map SessionId SessionInfo, a))
            -> m a
withSession SessionState{..} sessionId f = liftIO $ do
    modifyMVar sessionsVar $ \sessions ->
      case Map.lookup sessionId sessions of
        Nothing      -> throwIO $ InvalidSessionId sessionId
        Just session -> do
          now <- getCurrentTime
          let session' = session { sessionLastAccess = now }
          f (Map.insert sessionId session' sessions, session')

getSession :: MonadIO m => SessionState -> SessionId -> m SessionInfo
getSession st sid = withSession st sid return

-- | Make a session persistent
--
-- Persistent sessions are not cleaned up automatically and should explicitly
-- be logged out by the user.
persistSession :: MonadIO m => SessionState -> SessionId -> m ()
persistSession st sid = withSession st sid $ \(sessions, session) -> do
    let session' = session { sessionPersistent = True }
    return (Map.insert sid session' sessions, ())

-- | Resume existing session
resumeSession :: MonadIO m
              => SessionState -> AuthToken -> m (SessionInfo, SetCookie)
resumeSession st (AuthToken sessionId) = do
    sessionInfo <- getSession st sessionId
    cookie      <- sessionSetCookie sessionId (persistentToExpiry True)
    return (sessionInfo, cookie)

-- | Terminate a session
closeSession :: MonadIO m => SessionState -> SessionInfo -> m SetCookie
closeSession SessionState{..} SessionInfo{..} = liftIO $ do
    -- We don't use withSession so that we don't throw an error if the
    -- session ID does not exist (has already been deleted)
    modifyMVar_ sessionsVar $ return . Map.delete sessionInfoId

    -- Return an already expired cookie to immediately expire the client cookie
    sessionSetCookie sessionInfoId AlreadyExpired

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

sessionIdCookieName :: BS.S.ByteString
sessionIdCookieName = "sessionId"

data Expiry =
    ExpireAtEndOfSession
  | NeverExpire
  | AlreadyExpired

persistentToExpiry :: Bool -> Expiry
persistentToExpiry True  = NeverExpire
persistentToExpiry False = ExpireAtEndOfSession

sessionSetCookie :: MonadIO m => SessionId -> Expiry -> m SetCookie
sessionSetCookie sessionId expiry = do
    now <- liftIO $ getCurrentTime
    return def {
        setCookieName    = sessionIdCookieName
      , setCookieValue   = sessionId
      , setCookiePath    = Just "/"
      , setCookieExpires = case expiry of
                             ExpireAtEndOfSession -> Nothing
                             NeverExpire    -> Just $ addUTCTime longTime now
                             AlreadyExpired -> Just $ pastDate
      }

{-------------------------------------------------------------------------------
  Sessions
-------------------------------------------------------------------------------}

-- | Server-side interpretation of 'Session'
data ServerSession =
    -- | Unauthenticated session
    --
    -- This is the default
    SessionUnauth

    -- | Logged in session
  | SessionAuth SessionInfo

-- | Information about the current session
data SessionInfo = SessionInfo {
      -- | Session ID
      sessionInfoId :: SessionId

      -- | Authentication token
      --
      -- This is used by clients who want to persist a session between calls;
      -- it's a token that can be used to resume the session. Right now we reuse
      -- the session ID as the authentication token but that's not a necessity.
    , sessionToken :: AuthToken

      -- | The user logged in in this session
    , sessionUser :: VerifiedUser

      -- | Login info as we report it to the client on request
    , sessionLoginInfo :: LoginInfo

      -- | Is this session persistent?
      --
      -- A persistent session is one that was explicitly persisted by a client
      -- requesting an auth token, or one that is implicitly persisted in the
      -- web client when the user selects " remember me ".
      --
      -- We record this information because non-persistent should periodically
      -- be cleaned up to avoid infinite buildup of stale sessions in the
      -- server.
    , sessionPersistent :: Bool

      -- | When was the session last used?
      --
      -- Necessary to be able to cleanup stale sessions (see also
      -- 'sessionPersistent').
    , sessionLastAccess :: UTCTime
    }

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data SessionException =
    InvalidSessionId SessionId
  deriving (Show)

instance Pretty SessionException where
  pretty (InvalidSessionId sid) = PP.vcat [
      "Invalid session ID " <> pretty sid <> "."
    , "Perhaps the session expired or logged out?"
    ]

instance Exception SessionException where
  displayException = prettyStr

instance FriendlyException SessionException where
  displayFriendly = prettyStr

instance ReportError SessionException

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

type MuSec = Int

oneMinute :: MuSec
oneMinute = 60 * 1000000

oneDay :: NominalDiffTime
oneDay = 60 * 60 * 24

oneYear :: NominalDiffTime
oneYear = oneDay * 365

-- | Arbitrary long time for sessions that are never supposed to expire
longTime :: NominalDiffTime
longTime = 100 * oneYear

-- | Arbitrary date in the past
pastDate :: UTCTime
pastDate = UTCTime (fromGregorian 2000 1 1) 0
