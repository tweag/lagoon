-- | Authentication provider: LDAP simple bind
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Server.Auth.LDAP (authProviderLDAP) where

import Control.Exception
import Data.Text (Text)
import LDAP
import Text.Mustache
import Text.Parsec.Error (ParseError)
import qualified Data.Text as Text

import Pfizer.Datalake.Auth
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Server.Auth.VerifyCreds

-- | Verify using LDAP simple bind
authProviderLDAP :: String -> Text -> AuthProvider
authProviderLDAP url templateString =
    authProvider "authProviderLDAP" $ \Credentials{..} -> do
      case mTemplate of
        Left  err ->
          throwError $ LoginServerError ("Invalid LDAP template: " ++ show err)
        Right template -> do
          mError <- liftIO $ try $ do
            ldap <- ldapInitialize url
            let pass = credsPass
                user = Text.unpack $ substitute template LdapTemplate {
                           ldapUser = credsUser
                         }
            ldapSimpleBind ldap user pass
          case mError of
            Right () ->
              return ()
            Left LDAPException{code = LdapInvalidCredentials} ->
              throwError $ LoginInvalidCreds
            Left LDAPException{description} ->
              throwError $ LoginServerError description
  where
    mTemplate :: Either ParseError Template
    mTemplate = compileTemplate "LDAP distinguished name" templateString

-- | The LDAP template (very minimal for now)
data LdapTemplate = LdapTemplate {
    -- | The LDAP username (not the full DN)
    ldapUser :: String
  }

instance ToMustache LdapTemplate where
  toMustache LdapTemplate{..} = object [
      "user" ~> ldapUser
    ]
