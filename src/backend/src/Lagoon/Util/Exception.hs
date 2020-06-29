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
module Lagoon.Util.Exception (
    -- * Generic exception handling
    ignoreAllExceptions
  , WithCallStack(..)
  , addCallStack
  , bracketE
  , bracketE_
    -- * Conduit
  , bracketPE
  , bracketPE_
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.IORef
import Data.Typeable
import GHC.Stack

{-------------------------------------------------------------------------------
  Generic exception handling
-------------------------------------------------------------------------------}

-- | Ignore all exceptions inside the body
--
-- This is primarily useful for cleanup code
ignoreAllExceptions :: forall m. MonadCatch m => m () -> m ()
ignoreAllExceptions = handle handler
  where
    handler :: SomeException -> m ()
    handler _ = return ()

-- | Add a call stack to an exception
data WithCallStack a = WithCallStack CallStack a

instance Show a => Show (WithCallStack a) where
  show (WithCallStack cs a) = show a ++ "\n" ++ prettyCallStack cs

instance (Show a, Typeable a) => Exception (WithCallStack a)

addCallStack :: ( MonadCatch m
                , Exception e
                , Show b
                , Typeable b
                , ?loc :: CallStack
                )
             => (e -> Maybe b) -> m a -> m a
addCallStack f = handleJust f (throwM . WithCallStack ?loc)

-- | Variation on 'bracket' with two cleanup handlers
bracketE :: MonadMask m
         => m a              -- ^ Allocation
         -> (a -> r -> m r') -- ^ Finalize (cleanup on normal termination)
         -> (a -> m ())      -- ^ Abort (cleanup on abnormal termination)
         -> (a -> m r)       -- ^ Body
         -> m r'
bracketE alloc finalize abort k =
  mask $ \restore -> do
    a  <- alloc
    r  <- restore (k a) `onException` abort a
    finalize a r

-- | Variation on 'bracketE' where we are not interested in the result of
-- allocation.
bracketE_ :: MonadMask m
         => m ()        -- ^ Allocation
         -> (r -> m r') -- ^ Finalize (cleanup on normal termination)
         -> m ()        -- ^ Abort (cleanup on abnormal termination)
         -> m r         -- ^ Body
         -> m r'
bracketE_ alloc finalize abort k =
    bracketE alloc (\() -> finalize) (\() -> abort) (\() -> k)

{-------------------------------------------------------------------------------
  Conduit
-------------------------------------------------------------------------------}

type IsReleased = Bool

-- | Conduit equivalent of 'bracketE'
bracketPE :: forall i o m r r' a. MonadResource m
          => IO a              -- ^ Allocation
          -> (a -> r -> IO r') -- ^ Finalize (cleanup on normal termination)
          -> (a -> IO ())      -- ^ Abort (cleanup on abnormal termination)
          -> (a -> ConduitM i o m r) -- ^ Body
          -> ConduitM i o m r'
bracketPE alloc finalize abort k =
    bracketP acq rel body
  where
    acq :: IO (a, IORef IsReleased)
    acq = (,) <$> alloc <*> newIORef False

    rel :: (a, IORef IsReleased) -> IO ()
    rel (a, ref) = do
        isReleased <- readIORef ref
        unless isReleased $ abort a

    body :: (a, IORef IsReleased) -> ConduitM i o m r'
    body (a, ref) = do
        r <- k a
        liftIO $ uninterruptibleMask_ $ do
          r' <- finalize a r
          writeIORef ref True
          return r'

-- | Conduit equivalent of 'bracketE_'
bracketPE_ :: MonadResource m
           => IO ()            -- ^ Allocation
           -> (r -> IO r')     -- ^ Finalize (cleanup on normal termination)
           -> IO ()            -- ^ Abort (cleanup on abnormal termination)
           -> ConduitM i o m r -- ^ Body
           -> ConduitM i o m r'
bracketPE_ alloc finalize abort k =
    bracketPE alloc (\() -> finalize) (\() -> abort) (\() -> k)
