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
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Server (datalakeServer) where

import Control.Exception
import Data.ByteString (ByteString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.Gzip (gzip)
import Servant
import qualified Network.HTTP.Types          as HTTP
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS

import Pfizer.Datalake.DB
import Pfizer.Datalake.Interface
import Pfizer.Datalake.ManageDb
import Pfizer.Datalake.Server.API
import Pfizer.Datalake.Server.Cmdline
import Pfizer.Datalake.Server.HandlerM
import Pfizer.Datalake.Server.Logging
import Pfizer.Datalake.Server.Servant.Session
import Pfizer.Datalake.Util.PostgreSQL
import qualified Pfizer.Datalake.Interface.API as API

-- | Allow blanket Cross-Origin Resource Sharing (CORS)
--
-- NOTE: This sets up the server to allow requests from anywhere. This is
-- useful for development but probably not what we want for the final thing.
allowBlanketCORS :: Middleware
allowBlanketCORS app req respond =
    case requestMethod req of
      "OPTIONS" -> do
        let resp = responseLBS ok200 (hAllow : hsAC) ""
        respond resp
      _otherwise -> app req $ \resp -> do
        let resp' = mapResponseHeaders (hsAC ++) resp
        respond resp'
  where
    hAllow, hACAC, hACAO, hACAH :: HTTP.Header
    hAllow = ("Allow", "GET,POST,OPTIONS,DELETE")
    hACAC  = ("Access-Control-Allow-Credentials", "true")
    hACAM  = ("Access-Control-Allow-Methods", "GET,POST,OPTIONS,DELETE")
    hACEH  = ("Access-Control-Expose-Headers", "X-Total-Count")
    hACAO  = setTo "Access-Control-Allow-Origin" "Origin" "*"
    hACAH  = setTo "Access-Control-Allow-Headers" "Access-Control-Request-Headers" ""

    hsAC :: [HTTP.Header]
    hsAC = [hACAC, hACAO, hACAH, hACAM, hACEH]

    setTo :: HeaderName -> HeaderName -> ByteString -> HTTP.Header
    setTo respHeader reqHeader defv =
      case lookup reqHeader (requestHeaders req) of
        Just val -> (respHeader, val)
        Nothing  -> (respHeader, defv)

-- | Main server entry point
datalakeServer :: Cmdline -> IO Connection -> Schema -> IO ()
datalakeServer cmdline mkConn schema = do
    -- Initialize server context
    sessionState <- newSessionState
    let ctxt = sessionState :. EmptyContext

    -- Run the server unless we need to run some DB management
    case manageDb cmdline of
      Just cmd -> do
        conn <- mkConn
        runManageDb conn schema cmd
      Nothing -> do
        -- Refuse to run if the DB version does not match server's
        bracket mkConn closeConnection $ \conn ->
          runTransaction conn schema $ checkDbVersion

        -- Start the server
        serverEnv <- newServerEnv cmdline sessionState mkConn schema
        run cmdline $ logging (requestLogging cmdline)
                    $ (if enableGzip cmdline then gzip def else id)
                    $ allowBlanketCORS
                    $ serveWithContext API.proxy ctxt
                    $ fromSTrans serverEnv API.proxy
                    $ server

run :: Cmdline -> Application -> IO ()
run Cmdline{..} app =
    case tlsSettings of
      Nothing ->
        Warp.runSettings warpSettings app
      Just TlsSettings{..} -> do
        let warpTlsSettings = (WarpTLS.tlsSettings tlsCertFile tlsKeyFile) {
                WarpTLS.onInsecure =
                  if tlsAllowInsecure
                    then WarpTLS.AllowInsecure
                    else WarpTLS.DenyInsecure "Insecure connection denied"
              }
        WarpTLS.runTLS warpTlsSettings warpSettings app
  where
    warpSettings :: Warp.Settings
    warpSettings = Warp.setPort port
                 $ Warp.defaultSettings
