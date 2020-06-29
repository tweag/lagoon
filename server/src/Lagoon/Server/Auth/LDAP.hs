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
-- | Authentication provider: LDAP simple bind
{-# LANGUAGE OverloadedStrings #-}
module Lagoon.Server.Auth.LDAP (authProviderLDAP) where

import Control.Exception
import Data.Text (Text)
import LDAP
import Text.Mustache
import Text.Parsec.Error (ParseError)
import qualified Data.Text as Text

import Lagoon.Auth
import Lagoon.Interface
import Lagoon.Server.Auth.VerifyCreds

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
