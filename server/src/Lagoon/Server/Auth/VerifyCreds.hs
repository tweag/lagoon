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
-- | Monad for verifying credentials
module Lagoon.Server.Auth.VerifyCreds (
    VerifyCreds
  , VerifyCert
  , authProvider
    -- ** Convenience re-exports
  , liftIO
  , throwError
  ) where

import Control.Exception
import Control.Monad.Except
import qualified Network.HTTP.Client as Http
import qualified Network.TLS         as TLS

import Lagoon.Auth
import Lagoon.Interface

-- | Should we verify the certificate of the remote authentication server?
type VerifyCert = Bool

newtype VerifyCreds a = VerifyCreds (ExceptT LoginFailure IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError LoginFailure
           , MonadIO
           )

authProvider :: AuthProviderName
             -> (Credentials -> VerifyCreds ())
             -> AuthProvider
authProvider name verify = AuthProvider {
      authProviderName = name
    , verifyCreds      = runVerifyCreds . verify
    }

runVerifyCreds :: VerifyCreds () -> IO (LoginResult ())
runVerifyCreds (VerifyCreds act) =
    catchJust certificateUnknown
              (either LoginFailed LoginOk <$> runExceptT act)
              (return . LoginFailed)

certificateUnknown :: SomeException -> Maybe LoginFailure
certificateUnknown e
  | Just (Http.HttpExceptionRequest _ e') <- fromException e = goHttp e'
  | Just e' <- fromException e = goTLS e'
  | otherwise                  = Nothing
  where
    goHttp :: Http.HttpExceptionContent -> Maybe LoginFailure
    goHttp (Http.InternalException    e'')     = certificateUnknown e''
    goHttp (Http.ConnectionFailure    e'')     = certificateUnknown e''
    goHttp _                                   = Nothing

    goTLS :: TLS.TLSException -> Maybe LoginFailure
    goTLS (TLS.HandshakeFailed (TLS.Error_Protocol (_, _, TLS.CertificateUnknown))) =
      Just $ LoginServerError "Could not verify server certificate"
    goTLS _ =
      Nothing
