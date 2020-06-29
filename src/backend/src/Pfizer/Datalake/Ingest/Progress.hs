{-# LANGUAGE OverloadedStrings #-}
-- | Ingest progress messages
module Pfizer.Datalake.Ingest.Progress (
    -- * Specialized logging functions
    bracketLog
  , bracketLogC
  , logNotice
  , logProgress
    -- ** Convenience re-exports
  , MonadMask
  , Logger(..)
  , LogLevel(..)
  , when
  , L.newJsonLogSource
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.Conduit
import qualified Pfizer.Datalake.Util.Logging as L

{-------------------------------------------------------------------------------
  Specialized logging functions for ingest
-------------------------------------------------------------------------------}

-- | Bracket an action so that it outputs a final @ok@ log message if succesful
-- or @aborted@ log message otherwise.
--
-- TODO: Ideally we would report _which_ exception was thrown. This is easy
-- to do for 'bracketLog' but much much harder for 'bracketLogC', so we don't
-- do it for now.
bracketLog :: (MonadMask m, MonadIO m)
           => Logger IO IngestProgress
           -> IngestStart -> m b -> m b
bracketLog logger start =
    L.bracketLog logger (IngestStart start) IngestOk (IngestAborted Nothing)

-- | Conduit variant of 'bracketLog'.
bracketLogC :: MonadResource m
            => Logger IO IngestProgress
            -> IngestStart -> ConduitM i o m r -> ConduitM i o m r
bracketLogC logger start =
    L.bracketLogC logger (IngestStart start) IngestOk (IngestAborted Nothing)

logNotice :: MonadIO m => Logger IO IngestProgress -> IngestNotice -> m ()
logNotice logger notice = L.logNotice logger (IngestNotice notice)

logProgress :: MonadIO m => Logger IO IngestProgress -> Int -> Conduit a m a
logProgress logger n = every n (liftIO . logNotice logger . IngestProcessed)
