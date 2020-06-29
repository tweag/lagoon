module Main where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Data.Monoid
import Data.Proxy
import Data.Typeable
import Network.Connection
import Network.HTTP.Types
import Servant.API
import Servant.Client
import System.Exit
import qualified Data.ByteString.Lazy.UTF8 as BS.L.UTF8
import qualified Data.Version              as V (showVersion)
import qualified Network.HTTP.Client       as Client
import qualified Network.HTTP.Client.TLS   as TLS

import Pfizer.Datalake.Client
import Pfizer.Datalake.Client.AppSkeleton
import Pfizer.Datalake.Client.Cmdline
import Pfizer.Datalake.Client.FriendlyException.Orphans ()
import Pfizer.Datalake.Client.Prog
import Pfizer.Datalake.Client.Servant.Cookie
import Pfizer.Datalake.Interface
import qualified Paths_datalake_cmdline as Paths

app :: Cmdline -> IO ()
app Cmdline{..} = appSkeleton skeletonCmdline $ \yamlConfig -> do
    let config          = cmdlineConfig <> yamlConfig <> def
        Just host       = configHost       config
        Just port       = configPort       config
        Just secure     = configSecure     config
        Just verifyCert = configVerifyCert config
        scheme          = if secure then Https else Http
        baseUrl         = BaseUrl scheme host port ""
        initMgrSettings = case (secure, verifyCert) of
                            (False, _)    -> Client.defaultManagerSettings
                            (True, True)  -> TLS.tlsManagerSettings
                            (True, False) -> TLS.mkManagerSettings
                                               noVerifyTlsSettings
                                               Nothing

    (cookieJar, mgrSettings) <- newGlobalCookieJar initMgrSettings
    manager <- Client.newManager mgrSettings
    let clientEnv = ClientEnv manager baseUrl

    result <- (`runClientM` clientEnv) $ runResourceT $
      case cmdBody cmd of
        CmdInternal (DumpDbInfo (PlaintextPassword pw)) -> lift $ do
          let creds      = Credentials "admin" pw
              persistent = False
          _loginInfo <- fromLoginOk =<< userLogin creds persistent cookieJar
          sources    <- debugDumpDbInfo
          NoContent  <- userLogout cookieJar
          liftIO $ putStrLn $ prettyStr sources
        CmdInternal (RebuildCanReadCache (PlaintextPassword pw)) -> lift $ do
          let creds      = Credentials "admin" pw
              persistent = False
          _loginInfo <- fromLoginOk =<< userLogin creds persistent cookieJar
          NoContent  <- debugRebuildCanReadCache
          NoContent  <- userLogout cookieJar
          return ()
        CmdInternal GetServerURL -> liftIO $ do
          putStrLn $ showBaseUrl baseUrl
        CmdProg prog -> void $
          runProg cookieJar (logLevel skeletonCmdline) prog

    case result of
      Left err -> throwIO err
      Right () -> exitSuccess

-- TODO: All the "friendly" error cases that we have in the current ingest
-- main somehow need to be translated to server side friendly error messages,
-- and subsequently displayed here.
main :: IO ()
main = do
    cmdLine@Cmdline{..} <- getCmdline
    when (cmdShowVersion cmd) $ do
      putStrLn $ "datalake version " ++ V.showVersion Paths.version
      exitSuccess
    catches (app cmdLine) [
        handler (Proxy :: Proxy ServantError)
      , handler (Proxy :: Proxy LoginFailure)
      , handler (Proxy :: Proxy IngestAbortedException)
      , handler (Proxy :: Proxy UnrecognizedInputException)
      , handler (Proxy :: Proxy InvalidConfigFile)
      ]
  where
    handler :: forall e. ReportError e => Proxy e -> Handler ()
    handler _ = Handler $ \err -> do
      let (msg, exitCode) = reportError (err :: e)
      putStrLn msg
      exitWith (toExitCode exitCode)

{-------------------------------------------------------------------------------
  Friendly error messages
-------------------------------------------------------------------------------}

-- | Special exit codes
--
-- We return some special exit codes so that test scripts can verify we're
-- getting the error we expect. As is customary, exit code 0 is reserved to
-- indicate the absence of errors. Exit code 2 is usually reserved for
-- invalid command line arguments, though we don't make use of that for now.
data CustomExitCode =
    -- | Generic other error (1)
    ErrOther

    -- | Standard login failure (invalid username or password) (3)
  | ErrLogin

    -- | 403 forbidden response (43)
  | ErrForbidden

    -- | 404 not found (44)
  | ErrNotFound

toExitCode :: CustomExitCode -> ExitCode
toExitCode err = ExitFailure (code err)
  where
    code ErrOther     = 1
    code ErrLogin     = 3
    code ErrForbidden = 43
    code ErrNotFound  = 44

class Exception e => ReportError e where
  reportError :: e -> (String, CustomExitCode)
  default reportError :: FriendlyException e => e -> (String, CustomExitCode)
  reportError e = (displayFriendly e, reportExitCode e)

  reportExitCode :: e -> CustomExitCode
  reportExitCode _ = ErrOther

instance ReportError IngestAbortedException
instance ReportError UnrecognizedInputException
instance ReportError InvalidConfigFile

instance ReportError LoginFailure where
  reportExitCode LoginInvalidCreds = ErrLogin
  reportExitCode _otherwise        = ErrOther

instance ReportError ServantError where
  reportError err
    | Just body <- isStatus forbidden403 =
        ("Forbidden: " ++ body, ErrForbidden)
    | Just body <- isStatus internalServerError500 =
        ("Internal server error: " ++ body, ErrOther)
    | Just body <- isStatus badRequest400 =
        ("Bad request: " ++ body, ErrOther)
    | Just body <- isStatus notFound404 =
        ("Not found: " ++ body, ErrNotFound)
    | Just e <- isHttpException =
        (displayFriendly e, ErrOther)
    | Just (SomeException e) <- isConnectionError =
        ("Could not connect to server.\nUnknown " ++ show (typeOf e) ++ ": " ++ displayException e ++ "(" ++ show err ++ ")", ErrOther)
    | otherwise =
        ("Unknown servant error: " ++ displayException err, ErrOther)
    where
      isStatus :: Status -> Maybe String
      isStatus status = do
        (status', body) <- isFailureResponse
        guard $ status == status'
        return body

      isHttpException :: Maybe Client.HttpException
      isHttpException = isConnectionError >>= fromException

      isConnectionError :: Maybe SomeException
      isConnectionError = unwrap Nothing (Just err)

      -- For some reason we get nesting of connection errors. Not sure where
      -- this is coming from, possibly bug in servant. For now we just unwrap.
      unwrap :: Maybe SomeException -> Maybe ServantError -> Maybe SomeException
      unwrap _    (Just (ConnectionError e)) = unwrap (Just e) (fromException e)
      unwrap _    (Just _otherError)         = Nothing
      unwrap prev Nothing                    = prev

      isFailureResponse :: Maybe (Status, String)
      isFailureResponse =
        case err of
          FailureResponse{..} -> Just ( responseStatus
                                      , BS.L.UTF8.toString responseBody
                                      )
          _otherwise          -> Nothing

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

noVerifyTlsSettings :: TLSSettings
noVerifyTlsSettings = TLSSettingsSimple {
      settingDisableCertificateValidation = True
    , settingDisableSession               = True
    , settingUseServerName                = False
    }
