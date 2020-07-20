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
{-# LANGUAGE DeriveAnyClass #-}
module Lagoon.Interface.Logging (
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
