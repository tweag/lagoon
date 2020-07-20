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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lagoon.Interface.Users (
    UserIx(..)
  , User(..)
  , UserName
  ) where

import GHC.Generics (Generic)
import Text.Show.Pretty

import Lagoon.Interface.DB
import Lagoon.Interface.Pretty

newtype UserIx = UserIx Ix
  deriving (Show, PrettyVal, Pretty)

data User = User {
      userIx   :: UserIx
    , userName :: UserName
    }
  deriving (Show, Generic)

instance PrettyVal User

-- | Username (owner of a dataset)
type UserName = String
