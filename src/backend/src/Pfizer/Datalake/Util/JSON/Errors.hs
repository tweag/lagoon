module Pfizer.Datalake.Util.JSON.Errors (
    JsonError(..)
  , throwOnError
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Conduit

newtype JsonError = JsonError String
  deriving (Show)

instance Exception JsonError

-- | Throw exception when we get a JSON error
--
-- TODO: In the future it might be nicer to make an attempt to recover from
-- JSON errors.
throwOnError :: MonadIO m => Conduit (Either JsonError a) m a
throwOnError = awaitForever $ \case
                 Left err -> liftIO $ throwIO err
                 Right a  -> yield a
