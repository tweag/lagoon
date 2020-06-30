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
module Pfizer.Datalake.Util.Conduit (
    zipListWith
  , chunksOf
  , peekAt
  , every
  , sinkBuilderHandle
  , compactManyConduits
  , compactConduitsWith
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.ByteString.Builder (Builder)
import Data.Conduit
import Data.Conduit.Internal (Pipe(..), ConduitM(..))
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import System.IO
import qualified Data.ByteString.Builder as Bld
import qualified Data.Conduit.List as CL
import qualified Data.List.NonEmpty as NE

zipListWith :: forall m a b c. Monad m
            => (a -> b -> c) -> [a] -> ConduitM b c m ()
zipListWith f = go
  where
    go :: [a] -> ConduitM b c m ()
    go []     = return ()
    go (a:as) = do
        mb <- await
        case mb of
          Nothing -> return ()
          Just b  -> yield (f a b) >> go as

-- | Introduce bounded buffering into the pipeline
chunksOf :: forall m a. Monad m => Int -> ConduitM a [a] m ()
chunksOf bufSz = go 0 []
  where
    go :: Int -> [a] -> ConduitM a [a] m ()
    go !n acc =
      if n == bufSz
        then yield (reverse acc) >> go 0 []
        else do ma <- await
                case ma of
                  Nothing -> unless (null acc) $ yield (reverse acc)
                  Just a  -> go (n + 1) (a:acc)

-- | Look at (at most) the first n elements, without consuming them
peekAt :: forall m a. Monad m => Int -> Consumer a m [a]
peekAt = go []
  where
    go :: [a] -> Int -> Consumer a m [a]
    go acc 0 = do mapM_ leftover acc ; return (reverse acc)
    go acc n = do ma <- await
                  case ma of
                    Nothing -> go    acc   0
                    Just a  -> go (a:acc) (n-1)

-- | Run an action every n processed values from upstream
every :: Monad m => Int -> (Int -> Conduit a m a) -> Conduit a m a
every n act = go 1
  where
    go !i = do
      ma <- await
      case ma of
        Nothing -> unless (i `mod` n == 1) $ act (i - 1)
        Just a  -> do
          yield a
          when (i `mod` n == 0) $ act i
          go (i + 1)

-- | Variation on 'sinkHandle' that takes 'Builder's as input values.
sinkBuilderHandle :: MonadIO m => Handle -> Sink (Flush Builder) m ()
sinkBuilderHandle h =
    awaitForever $ \flush -> liftIO $
      case flush of
        Flush     -> hFlush h
        Chunk bld -> Bld.hPutBuilder h bld

compactManyConduits
  :: (Monad m, Eq a)
  => [ConduitM () a m ()]
  -> ConduitM () a m ()
compactManyConduits cs = compactConduitsWith id cs .| CL.map NE.head

-- | NonEmpty i : a list of values that are equal on 'a'
compactConduitsWith
  :: (Monad m, Eq eq)
  => (o -> eq)
  -> [ConduitM () o m ()]
  -> ConduitM () (NE.NonEmpty o) m ()
compactConduitsWith f cs = ConduitM $ \rest -> let
    -- | Reads from the list of conduits and perform steps until either (1) an
    -- input is available or (2) the conduit is empty. If any of the conduits
    -- are empty, returns the up-to-date list of conduit (i.e. where e.g all
    -- monadic actions have been performed, in order not to duplicate those).
    -- Otherwise returns a list of 3-tuples isomorphic to HaveOutput, meaning
    -- that all conduits are ready to yield.
    getAll = \case
      pip@(HaveOutput src fin x) :| xs ->
        case xs of
          (x' : xs') ->
            bimap
              (NE.cons pip)
              (NE.cons (src, fin, x))
              <$>
              getAll (x' :| xs')
          [] -> pure $ Right ((src, fin, x) :| [])
      (Leftover src ()) :| xs -> getAll (src :| xs)
      (Done ()) :| xs -> pure $ Left (Done () :| xs)
      (NeedInput _ cont) :| xs -> getAll ((cont ()) :| xs)
      (PipeM mx) :| xs -> PipeM $ mx >>= \x -> pure (getAll (x :| xs))

    go xs = getAll xs >>= \case
        -- Some conduits are done and as such won't be able to yield a value
        -- equal to that of the other conduits, so we just drain the conduits
        -- one by one.
        Left xs' -> drainMany xs'

        -- if all conduits have yielded a value, and all the values are equal,
        -- spit that value out. Otherwise drain the resulting conduits, one by
        -- one.
        Right vs ->
          if allEqual $ (\(_,_,v) -> f v) <$> (NE.toList vs)
          then
            HaveOutput
              (go $ NE.map (\(src,_,_) -> src) vs)
              (fix
                (\loop fins -> case fins of
                  fin1 :| (fin2 : fins') -> fin1 >> loop (fin2 :|fins')
                  fin1 :| [] -> fin1
                )
                  (NE.map (\(_,fin,_) -> fin) vs)
              )
              (NE.map (\(_,_,v) -> v) vs)
          else
            drainMany (NE.map (\(src, fin, v) -> HaveOutput src fin v) vs)

    -- Drain all the conduits.
    drainMany (x :| []) = drain x
    drainMany (x :| (x':xs)) = drain x >> drainMany (x' :| xs)

    drain = \case
      Done () -> rest ()
      NeedInput _ c -> drain (c ())
      Leftover src () -> drain src
      HaveOutput src fin x -> HaveOutput (drain src) fin (x :| [])
      PipeM m -> PipeM (liftM drain m)

  in case cs of
      [] -> rest ()
      x:xs -> go $ NE.map (\(ConduitM pip) -> pip Done) $ x :| xs

allEqual :: Eq a => [a] -> Bool
allEqual (x:xs) = all (== x) xs
allEqual [] = True
