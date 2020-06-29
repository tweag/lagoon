-- | Authentication provider: HTTP Basic Auth
module Pfizer.Datalake.Server.Auth.BasicAuth (authProviderBasicAuth) where

import Control.Monad.Except
import Network.Connection
import Network.HTTP.Types
import qualified Data.ByteString.UTF8    as BS.S.UTF8
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http.TLS

import Pfizer.Datalake.Auth
import Pfizer.Datalake.Interface
import Pfizer.Datalake.Server.Auth.VerifyCreds

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
