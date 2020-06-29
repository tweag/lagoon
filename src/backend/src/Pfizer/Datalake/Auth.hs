{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
module Pfizer.Datalake.Auth (
    AuthProvider(..)
  , AuthProviderName
  , authProviderDenyAll
  , authProviderApproveAll
  ) where

import Pfizer.Datalake.Interface

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

type AuthProviderName = String

-- | Authentication providers
--
-- 'AuthProvider' is an abstract definition of an authentication provider. We
-- don't provide many concrete instances here because they aren't particularly
-- useful in the command line client and incur additional dependencies (for
-- instance, the OpenLDAP C libraries). The backend does however need to be
-- aware of the concept of an 'AuthProvider' because it is used in the
-- 'Verified' module.
--
-- See also 'authProviderAlwaysFail' and 'authProviderAlwaysOk'.
data AuthProvider = AuthProvider {
      -- | The name of this service (used for the 'Show' instance)
      authProviderName :: AuthProviderName

      -- | Verify credentials (the raison d'etre of auth providers)
    , verifyCreds :: Credentials -> IO (LoginResult ())
    }

instance Show AuthProvider where
  show ap = "<<AuthProvider " ++ authProviderName ap ++ ">>"

-- | Authentication provider that denies all authentication requests.
authProviderDenyAll :: AuthProvider
authProviderDenyAll = AuthProvider {
      authProviderName = "authProviderDenyAll"
    , verifyCreds      = \_creds ->
        return $ LoginFailed
               $ LoginServerError "No authentication provider configured"
    }

-- | Authentication provider that approves all authentication requests
--
-- This is primarily useful during testing and when using the ingest command
-- line tool, which anyway needs full access to the PostgreSQL database and
-- hence does not benefit from an additional authentication step.
authProviderApproveAll :: AuthProvider
authProviderApproveAll = AuthProvider {
      authProviderName = "authProviderApproveAll"
    , verifyCreds      = \_creds -> return $ LoginOk ()
    }
