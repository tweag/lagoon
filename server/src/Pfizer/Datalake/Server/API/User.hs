{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Server.API.User (server) where

import Servant

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Server.HandlerM
import Pfizer.Datalake.Server.Serialization ()
import Pfizer.Datalake.Server.Servant.Cookie
import Pfizer.Datalake.Verified
import qualified Pfizer.Datalake.Interface.API as API

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

server :: STrans API.User
server = userLogin
    :<|> userLogout
    :<|> userGetAuthToken
    :<|> userResumeSession
    :<|> userSetCanCreateSource
    :<|> userSetCanCreateGroup

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

-- | User login
userLogin :: STrans API.UserLogin
userLogin creds persistent = do
    auth   <- getAuthProvider
    result <- execTransaction $ login auth creds
    case result of
      LoginFailed err  -> return $ NoCookie $ LoginFailed err
      LoginOk     user -> do
        (SessionInfo{..}, setCookie) <- startSession user persistent
        return $ WithCookie setCookie $ LoginOk sessionLoginInfo

-- | User logout
userLogout :: STrans API.UserLogout
userLogout SessionUnauth      = return $ NoCookie $ NoContent
userLogout (SessionAuth info) = do
    setCookie <- closeSession info
    return $ WithCookie setCookie $ NoContent

-- | Get authentication token
userGetAuthToken :: STrans API.UserGetAuthToken
userGetAuthToken SessionUnauth =
    throwM $ err401 { errBody = "Not logged in" }
userGetAuthToken (SessionAuth SessionInfo{..}) = do
    persistSession sessionInfoId
    return sessionToken

-- | Resume ssession
--
-- We could verify the session ID here but there isn't much point. We just
-- return the cookie, and if the session ID was invalid, the user will notice
-- on the next request. After all, it's possible that the session will be
-- logged out from somewhere else anyway after the resume but before the next
-- request.
userResumeSession :: STrans API.UserResumeSession
userResumeSession token = do
    (SessionInfo{..}, setCookie) <- resumeSession token
    return $ WithCookie setCookie
           $ LoginOk sessionLoginInfo

-- | Grant or revoke CREATE privileges
userSetCanCreateSource :: STrans API.UserSetCanCreateSource
userSetCanCreateSource session userName val = do
    sessionAdmin <- getSessionAdmin session
    execTransaction $ do
      user <- lookupUserName userName
      case val of
        True  -> grantCreateSource  sessionAdmin user
        False -> revokeCreateSource sessionAdmin user
      return NoContent

-- | Grant or revoke CREATEGROUP privileges
userSetCanCreateGroup :: STrans API.UserSetCanCreateGroup
userSetCanCreateGroup session userName val = do
    sessionAdmin <- getSessionAdmin session
    execTransaction $ do
      user <- lookupUserName userName
      case val of
        True  -> grantCreateGroup  sessionAdmin user
        False -> revokeCreateGroup sessionAdmin user
      return NoContent
