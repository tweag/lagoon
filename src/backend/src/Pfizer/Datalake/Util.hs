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
-- | Simple utilities
module Pfizer.Datalake.Util (
    intercalateM
  , Dict(..)
  , orElse
  , adjustUnion
  , groupOn
  ) where

import Data.Map.Strict (Map)
import Data.Monoid
import GHC.Exts (Constraint)
import Text.Show.Pretty
import qualified Data.Map.Strict as Map

-- | Generalization of 'Data.List.intercalate'
intercalateM :: forall m. Monoid m => m -> [m] -> m
intercalateM sep = go
  where
    go :: [m] -> m
    go []        = mempty
    go [m]       = m
    go (m:m':ms) = m <> sep <> go (m':ms)

-- | Reified constraints
data Dict :: Constraint -> * where
    Dict :: forall (c :: Constraint). c => Dict c

instance PrettyVal (Dict c) where
    prettyVal _ = String "<<Dict>>"

-- | Fallback
orElse :: Monad m => m (Maybe a) -> m a -> m a
orElse f g = f >>= maybe g return

-- | Variation on 'Map.unionWith' that allows to adjust the keys which are
-- present in only one of the two maps.
adjustUnion :: Ord k
            => (a -> b -> c)  -- ^ For keys present in both
            -> (a -> c)       -- ^ For keys present only in the first map
            -> (b -> c)       -- ^ For keys present only in the second map
            -> Map k a -> Map k b -> Map k c
adjustUnion f g h = Map.mergeWithKey (\_k a b -> Just (f a b)) (fmap g) (fmap h)

-- | Group by identity
--
-- The groups returned by 'groupBy' are guaranteed not to be empty, but this is
-- not obvious from the type. 'groupOn' is a variation in which elements are
-- given some kind of " identity " of type @b@, and we want to group elements
-- with the same identity. For each group we return we additionally return the
-- identity of the elements in that group.
--
-- NOTE: This version uses the standard Prelude 'span' function and as such is
-- not as lazy as one might wish. Not suitable for streaming.
groupOn :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOn _ []     = []
groupOn f (x:xs) = case span (\x' -> f x' == fx) xs of
                     (group, xs') -> (fx, x:group) : groupOn f xs'
  where
    fx = f x
