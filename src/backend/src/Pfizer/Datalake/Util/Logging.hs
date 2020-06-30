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
-- | Some utilities to make logging more convenient
module Pfizer.Datalake.Util.Logging (
    logNotice
  , logWarning
  , logError
  , bracketLog
  , bracketLogC
  , newLogSource
  , newJsonLogSource
  ) where

import Control.Concurrent hiding (yield)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson (ToJSON(..), fromEncoding, toEncoding)
import Data.ByteString.Builder (Builder)
import Data.Conduit
import Data.Monoid
-- import qualified Data.Aeson.Encode       as AE
import qualified Data.ByteString.Builder as Bld

import Pfizer.Datalake.Interface hiding (Source)
import Pfizer.Datalake.Util.Exception

logNotice :: MonadIO m => Logger IO a -> a -> m ()
logNotice l a = liftIO $ logMessage l Notice a

logWarning :: MonadIO m => Logger IO a -> a -> m ()
logWarning l a = liftIO $ logMessage l Warning a

logError :: MonadIO m => Logger IO a -> a -> m ()
logError l a = liftIO $ logMessage l Error a

bracketLog :: (MonadMask m, MonadIO m)
           => Logger IO a
           -> a -- ^ Notice before starting
           -> a -- ^ Notice after finishing
           -> a -- ^ Message on abort
           -> m b -> m b
bracketLog l before after aborted =
    bracketE_ (logNotice l before)
              (\b -> logNotice l after >> return b)
              (logError l aborted)

bracketLogC :: (MonadResource m)
            => Logger IO a
            -> a -- ^ Notice before starting
            -> a -- ^ Notice after finishing
            -> a -- ^ Message on abort
            -> ConduitM i o m r -> ConduitM i o m r
bracketLogC l before after aborted =
    bracketPE_ (logNotice l before)
               (\b -> logNotice l after >> return b)
               (logError l aborted)

-- | Create a logger that sends log messages to a 'Source'
--
-- Returns the 'Logger', an IO action that can be used to terminate the source
-- (with some final tail 'Source'), and the source itself.
newLogSource :: forall m a b. MonadIO m
             => LogLevel
             -> (a -> Source m b)
             -> IO ( Logger IO a
                   , Source m b -> IO ()
                   , Source m b
                   )
newLogSource minLogLevel f = do
    ch <- newChan

    let logger :: Logger IO a
        logger = filterLogMessages minLogLevel $ Logger {
            logMessage = \_logLevel -> writeChan ch . Right
          }

        terminate :: Source m b -> IO ()
        terminate = writeChan ch . Left

        source :: Source m b
        source = do
            ma <- liftIO $ readChan ch
            case ma of
              Left  final -> final
              Right a     -> f a >> source

    return (logger, terminate, source)

-- | Logger which outputs JSON, terminating every JSON value with a newline.
--
-- The progress messages have type @a@; the final value which terminates the
-- log either has type @b@ to indicate successful termination with some final
-- value, or type @a@ to indicate abnormal termination with some final
-- log message. The resulting JSON does not have any explicit markers
-- indicating which case it is; it is assumed that the encodings of @a@ and
-- @b@ are sufficiently different that they can easily be distinguished
-- (see also 'ProgressOr').
newJsonLogSource :: forall m a b. (MonadIO m, ToJSON a, ToJSON b)
                 => LogLevel
                 -> IO (Logger IO a, Either a b -> IO (), Source m (Flush Builder))
newJsonLogSource minLogLevel = do
    (logger, terminateLog, logSource) <- newLogSource minLogLevel yieldJSON
    let terminateLog' :: Either a b -> IO ()
        terminateLog' (Left a)  = terminateLog $ yieldJSON a
        terminateLog' (Right b) = terminateLog $ yieldJSON b
    return (logger, terminateLog', logSource)
  where
    yieldJSON :: ToJSON x => x -> Producer m (Flush Builder)
    yieldJSON x = do
      yield $ Chunk $ (fromEncoding $ toEncoding x) <> (Bld.charUtf8 '\n')
      yield $ Flush
