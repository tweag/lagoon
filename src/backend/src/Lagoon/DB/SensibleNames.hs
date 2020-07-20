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
module Lagoon.DB.SensibleNames (
    PreferredName(..)
  , sanitize
  , noDupNames
  , addVersion
  ) where

import Data.Char (toLower)
import Data.Hashable
import Data.HashSet (HashSet)
import GHC.Generics (Generic)
import qualified Data.HashSet as Hash

import Lagoon.Util.PostgreSQL (MaxIdLen, avoidSpecialChars)

-- | Preferred name (prior to any kind of sanitization)
--
-- We record the suffix of the name separately, because when we truncate names
-- to their maximum length we don't want to cut off the suffix.
--
-- NOTE: Throughout we assume the suffix does not need any kind of sanitization.
data PreferredName = PrefName {
      prefName   :: String
    , prefSuffix :: String
    }
  deriving (Eq, Generic, Hashable)

-- | Construct a valid identifier from a 'PreferredName'
--
-- NOTE:
--
-- * See 'noDupNames' for making sure a set of identifiers does not contain
--   any duplicates.
-- * We do not change capitalization or avoid SQL keywords. The resulting name
--   must still be quoted.
sanitize :: MaxIdLen -> PreferredName -> String
sanitize maxIdLen PrefName{..} =
    take (maxIdLen - length prefSuffix)
         (avoidSpecialChars prefName) ++ prefSuffix

-- | Avoid duplicates in a set of preferred names
--
-- NOTE: Two names are considered a duplicate if their /sanitized/ names are
-- identical.
noDupNames :: forall a.
              MaxIdLen
           -> (a -> PreferredName)
           -> (PreferredName -> a -> a)
           -> [a] -> [a]
noDupNames maxIdLen getName setName = go Hash.empty
  where
    go :: HashSet String -> [a] -> [a]
    go _    []     = []
    go used (a:as) =
        let nm           = getName a
            (nm', used') = mkUnique 0 nm
        in setName nm' a : go used' as
      where
        mkUnique :: Int -> PreferredName -> (PreferredName, HashSet String)
        mkUnique v nm
          | ident `Hash.member` used = mkUnique (v + 1) nm
          | otherwise                = (nm', Hash.insert ident used)
          where
            nm' :: PreferredName
            nm' = addVersion v nm

            -- Although we keep capitalization, for the purposes of detecting
            -- duplicates, we treat the names as case insensitive
            -- ('ident' is used solely for the purposes of detecting dups)
            ident :: String
            ident = map toLower $ sanitize maxIdLen nm'

-- | Add version prefix to the suffix of a 'PreferredName'
--
-- If the version is 0, leave the names untouched.
addVersion :: Int -> PreferredName -> PreferredName
addVersion 0 nm           = nm
addVersion v PrefName{..} = PrefName prefName ("_" ++ show v ++ prefSuffix)
