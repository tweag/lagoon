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
-- | Wrapper around the @postgresql-simple@ library
--
-- Intended to be imported /instead of/ "Database.PostgreSQL.Simple"
module Lagoon.Util.PostgreSQL (
    module Lagoon.Util.PostgreSQL.CopyFrom
  , module Lagoon.Util.PostgreSQL.CopyTo
  , module Lagoon.Util.PostgreSQL.Exception
  , module Lagoon.Util.PostgreSQL.HList
  , module Lagoon.Util.PostgreSQL.Keywords
  , module Lagoon.Util.PostgreSQL.PartialQuery
  , module Lagoon.Util.PostgreSQL.Queries
  , module Lagoon.Util.PostgreSQL.Quoted
  , module Lagoon.Util.PostgreSQL.Reserved
  , module Lagoon.Util.PostgreSQL.Schema
  , module Lagoon.Util.PostgreSQL.Settings
  , module Lagoon.Util.PostgreSQL.Transaction
  , module Lagoon.Util.PostgreSQL.TsQuery
  -- * Convenience re-exports
  -- ** PostgreSQL opaque types
  , Connection
  , Query
  -- ** PostgreSQL classes
  , FromField(..)
  , FromRow(..)
  , ToField(..)
  , ToRow(..)
  , field
  -- ** Wrappers for constructing queries
  , In(..)
  , PGArray(..)
  , Only(..)
  -- ** Constructing queries
  , intercalateM
  , (<>)
  , fromString
  ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Data.Monoid ((<>))
import Data.String (fromString)

import Lagoon.Util (intercalateM)
import Lagoon.Util.PostgreSQL.CopyFrom
import Lagoon.Util.PostgreSQL.CopyTo
import Lagoon.Util.PostgreSQL.Exception
import Lagoon.Util.PostgreSQL.HList
import Lagoon.Util.PostgreSQL.Keywords
import Lagoon.Util.PostgreSQL.PartialQuery
import Lagoon.Util.PostgreSQL.Queries
import Lagoon.Util.PostgreSQL.Quoted
import Lagoon.Util.PostgreSQL.Reserved
import Lagoon.Util.PostgreSQL.Schema
import Lagoon.Util.PostgreSQL.Settings
import Lagoon.Util.PostgreSQL.Transaction
import Lagoon.Util.PostgreSQL.TsQuery
