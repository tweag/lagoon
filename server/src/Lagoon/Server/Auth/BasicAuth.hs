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
-- | Authentication provider: HTTP Basic Auth
module Lagoon.Server.Auth.BasicAuth (authProviderBasicAuth) where

import Control.Monad.Except
import Network.Connection
import Network.HTTP.Types
import qualified Data.ByteString.UTF8    as BS.S.UTF8
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http.TLS

import Lagoon.Auth
import Lagoon.Interface
import Lagoon.Server.Auth.VerifyCreds

-- | Verify using HTTP basic auth
authProviderBasicAuth :: String -> VerifyCert -> AuthProvider
authProviderBasicAuth url verifyCert =
    authProvider "authProviderBasicAuth" $ \creds -> do
      req <- case Http.parseRequest url of
               Nothing -> throwError invalidUrl
               Just r  -> return $ applyBasicAuth creds r
      mgr <- liftIO $ Http.newManager managerSettings
      rsp <- liftIO $ Http.httpNoBody req mgr
      let status = Http.responseStatus rsp
      if | status == ok200               -> return ()
         | status == movedPermanently301 -> return ()
         | status == unauthorized401     -> throwError $ LoginInvalidCreds
         | otherwise                     -> throwError $ unknownStatus status
  where
    invalidUrl :: LoginFailure
    invalidUrl = LoginServerError $ "Could not parse URL " ++ show url

    unknownStatus :: Status -> LoginFailure
    unknownStatus status = LoginServerError $ "Unknown status code "
                                           ++ show (statusCode status)

    applyBasicAuth :: Credentials -> Http.Request -> Http.Request
    applyBasicAuth Credentials{..} =
        Http.applyBasicAuth
          (BS.S.UTF8.fromString credsUser)
          (BS.S.UTF8.fromString credsPass)

    managerSettings :: Http.ManagerSettings
    managerSettings
      | verifyCert = Http.TLS.tlsManagerSettings
      | otherwise  = Http.TLS.mkManagerSettings noVerifyTlsSettings Nothing

noVerifyTlsSettings :: TLSSettings
noVerifyTlsSettings = TLSSettingsSimple {
      settingDisableCertificateValidation = True
    , settingDisableSession               = True
    , settingUseServerName                = False
    }
