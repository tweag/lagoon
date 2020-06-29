module Pfizer.Datalake.Server.FriendlyException.Rethrow (
    rethrowFriendly
  , finalLogMessage
  ) where

import Control.Exception (SomeException(..), throwIO)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Data.Typeable
import Servant
import System.IO
import qualified Control.Monad.Catch       as C
import qualified Data.ByteString.Lazy.UTF8 as BS.L.UTF8
import qualified Network.HTTP.Client       as Client

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.Exception
import Pfizer.Datalake.Util.PostgreSQL.Exception
import Pfizer.Datalake.Verified
import Pfizer.Datalake.Server.FriendlyException.ReportError
import Pfizer.Datalake.Server.Servant.Session

rethrowFriendly :: forall m a. (MonadCatch m, MonadIO m) => m a -> m a
rethrowFriendly act = catches act $ concat [
      friendly (Proxy :: Proxy PermissionDeniedException)
    , friendly (Proxy :: Proxy NotFoundException)
    , friendly (Proxy :: Proxy AlreadyExistsException)
    , friendly (Proxy :: Proxy SessionException)
    , friendly (Proxy :: Proxy Client.HttpException)
    , friendly (Proxy :: Proxy ServantErr)
    , friendly (Proxy :: Proxy SomeSqlException)
    , [ C.Handler $ \(SomeException e) -> liftIO $ do
          -- We don't want to return arbitrary exceptions to the client as
          -- this is a potential security risk.
          hPutStrLn stderr $ "Warning: uncaught "
                          ++ show (typeOf e)
                          ++ ": "
                          ++ displayException e
          throwIO e
      ]
    ]
  where
    friendly :: forall e. ReportError e => Proxy e -> [C.Handler m a]
    friendly _ =
      [ C.Handler $ \(WithCallStack cs e) -> do
          liftIO $ hPutStrLn stderr $ "Warning: Dropping callstack for:"
                                   ++ show (typeOf e)
                                   ++ ": "
                                   ++ show cs
          liftIO $ throwIO $ reportError (e :: e)
      , C.Handler $ \err -> liftIO $ throwIO $ reportError (err :: e)
      ]

finalLogMessage :: Either ServantErr a -> Either IngestProgress a
finalLogMessage = either (Left . aux) Right
  where
    aux :: ServantErr -> IngestProgress
    aux = IngestAborted . Just . BS.L.UTF8.toString . errBody
