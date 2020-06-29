{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
module Pfizer.Datalake.Interface.Auth (
    Credentials(..)
  , LoginResult(..)
  , LoginInfo(..)
  , LoginFailure(..)
  , fromLoginOk
  ) where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson
import Data.Monoid
import GHC.Generics (Generic)
import Text.Show.Pretty

import Pfizer.Datalake.Interface.FriendlyException
import Pfizer.Datalake.Interface.Pretty

{-------------------------------------------------------------------------------
  Auth specification
-------------------------------------------------------------------------------}

data Credentials = Credentials {
      credsUser :: String
    , credsPass :: String
    }
  deriving (Show, Generic, PrettyVal)

data LoginResult a =
    LoginOk a
  | LoginFailed LoginFailure
  deriving (Show, Functor)

data LoginInfo = LoginInfo { loginInfoUsername :: String }

data LoginFailure =
    -- | Credentials invalid
    LoginInvalidCreds

    -- | There was a problem with the auth server
  | LoginServerError String

    -- | Unknown failure
  | LoginUnknownError String
  deriving (Show)

instance Exception LoginFailure

fromLoginOk :: MonadIO m => LoginResult a -> m a
fromLoginOk (LoginOk     a)   = return a
fromLoginOk (LoginFailed err) = liftIO $ throwIO err

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Pretty LoginFailure where
  pretty LoginInvalidCreds       = "Invalid credentials"
  pretty (LoginServerError  err) = "Server error: " <> pretty err
  pretty (LoginUnknownError err) = "Unknown login error: " <> pretty err

instance FriendlyException LoginFailure where
  displayFriendly LoginInvalidCreds =
    "Invalid username or password"
  displayFriendly (LoginServerError err) =
    "There was a problem connecting to the authentication server: " ++ err
  displayFriendly (LoginUnknownError err) =
    "Unknown authentication error: " ++ err

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

instance ToJSON a => ToJSON (LoginResult a) where
  toJSON (LoginOk     a) = object [ "ok"     .= a ]
  toJSON (LoginFailed e) = object [ "failed" .= e ]

instance ToJSON LoginInfo where
  toJSON LoginInfo{loginInfoUsername} = object [ "username" .= loginInfoUsername ]

instance ToJSON LoginFailure where
  toJSON LoginInvalidCreds     = "Invalid credentials"
  toJSON (LoginServerError  e) = object [ "serverError"  .= e ]
  toJSON (LoginUnknownError e) = object [ "unknownError" .= e ]

instance ToJSON Credentials where
  toJSON Credentials{..} = object [ "user" .= credsUser
                                  , "pass" .= credsPass
                                  ]

instance FromJSON a => FromJSON (LoginResult a) where
  parseJSON = withObject "LoginResult" $ \obj -> parseLoginOk obj     <|>
                                                 parseLoginFailed obj
    where
      parseLoginOk o     = LoginOk <$> o .: "ok"
      parseLoginFailed o = LoginFailed <$> o .: "failed"

instance FromJSON LoginInfo where
  parseJSON = withObject "LoginInfo" $ \obj -> LoginInfo <$> obj .: "username"

instance FromJSON LoginFailure where
  parseJSON obj = parseInvalidCreds obj                      <|>
                  (parseLoginServerError =<< parseJSON obj)  <|>
                  (parseLoginUnknownError =<< parseJSON obj)
    where
      parseInvalidCreds "Invalid credentials" = pure LoginInvalidCreds
      parseInvalidCreds _      = fail "(Auth.hs) InvalidCreds: no parse"
      parseLoginServerError o  = LoginServerError <$> o .: "serverError"
      parseLoginUnknownError o = LoginUnknownError <$> o .: "unknownError"

instance FromJSON Credentials where
  parseJSON = withObject "Credentials" $ \obj -> do
    credsUser <- obj .: "user"
    credsPass <- obj .: "pass"
    return Credentials{..}
