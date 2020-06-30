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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Strict list
--
-- Intended for qualified import
module Pfizer.Datalake.Util.StrictList (
    StrictList(..)
  , cons
  , fromList
  , reverse
  , singleton
  ) where

import Prelude hiding (reverse, zipWith)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Data.Foldable
import Data.Monoid
import GHC.Generics (Generic)
import Text.Show.Pretty

-- | Strict list
--
-- We cache the length of the list, so that calling 'length' on a 'StrictList'
-- is an @O(1)@ operation.
data StrictList a = Nil | Cons {-# UNPACK #-} !Int !a !(StrictList a)
  deriving (Show, Eq, Ord, Generic, Functor)

instance PrettyVal a => PrettyVal (StrictList a) where
  prettyVal xs = Con "fromList" [prettyVal (toList xs)]

cons :: a -> StrictList a -> StrictList a
cons x xs = Cons (length xs + 1) x xs

instance Foldable StrictList where
  length :: StrictList a -> Int
  length Nil          = 0
  length (Cons n _ _) = n

  foldMap :: forall m a. Monoid m => (a -> m) -> StrictList a -> m
  foldMap f = go mempty
    where
      go :: m -> StrictList a -> m
      go !acc Nil           = acc
      go !acc (Cons _ x xs) = go (acc <> f x) xs

reverse :: StrictList a -> StrictList a
reverse = go Nil
  where
    go :: StrictList a -> StrictList a -> StrictList a
    go acc Nil           = acc
    go acc (Cons _ x xs) = go (cons x acc) xs

-- | Convert a lazy list into a strict list
fromList :: [a] -> StrictList a
fromList []     = Nil
fromList (x:xs) = cons x (fromList xs)

singleton :: a -> StrictList a
singleton = fromList . (:[])

instance ToField a => ToRow (StrictList a) where
  toRow = toRow . toList
