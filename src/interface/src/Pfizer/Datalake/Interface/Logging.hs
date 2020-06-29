{-# LANGUAGE DeriveAnyClass #-}
module Pfizer.Datalake.Interface.Logging (
    LogLevel(..)
  , Logger(..)
  , filterLogMessages
  ) where

import Control.Monad
import Data.Functor.Contravariant
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

-- | Level of a log message: how important is it?
--
-- Order of the constructors is important for the derived 'Ord' instance
data LogLevel =
    Debug
  | Notice
  | Warning
  | Error
  deriving (Show, Eq, Ord, Generic, PrettyVal)

data Logger m a = Logger {
      logMessage :: LogLevel -> a -> m ()
    }

instance Contravariant (Logger m) where
  contramap f Logger{..} = Logger $ \l -> logMessage l . f

-- | Filter log messages
--
-- This is a helper function for constructing 'Logger' instances; we only
-- pass log messages to the provided 'Logger' instance if their log level
-- is at or above the level we want to see.
filterLogMessages :: LogLevel -> Logger IO a -> Logger IO a
filterLogMessages minLogLevel logger = Logger {
    logMessage = \logLevel msg ->
      when (logLevel >= minLogLevel) $ logMessage logger logLevel msg
  }
