-- | Wrapper around the @postgresql-simple@ library
--
-- Intended to be imported /instead of/ "Database.PostgreSQL.Simple"
module Pfizer.Datalake.Util.PostgreSQL (
    module Pfizer.Datalake.Util.PostgreSQL.CopyFrom
  , module Pfizer.Datalake.Util.PostgreSQL.CopyTo
  , module Pfizer.Datalake.Util.PostgreSQL.Exception
  , module Pfizer.Datalake.Util.PostgreSQL.HList
  , module Pfizer.Datalake.Util.PostgreSQL.Keywords
  , module Pfizer.Datalake.Util.PostgreSQL.PartialQuery
  , module Pfizer.Datalake.Util.PostgreSQL.Queries
  , module Pfizer.Datalake.Util.PostgreSQL.Quoted
  , module Pfizer.Datalake.Util.PostgreSQL.Reserved
  , module Pfizer.Datalake.Util.PostgreSQL.Schema
  , module Pfizer.Datalake.Util.PostgreSQL.Settings
  , module Pfizer.Datalake.Util.PostgreSQL.Transaction
  , module Pfizer.Datalake.Util.PostgreSQL.TsQuery
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

import Pfizer.Datalake.Util (intercalateM)
import Pfizer.Datalake.Util.PostgreSQL.CopyFrom
import Pfizer.Datalake.Util.PostgreSQL.CopyTo
import Pfizer.Datalake.Util.PostgreSQL.Exception
import Pfizer.Datalake.Util.PostgreSQL.HList
import Pfizer.Datalake.Util.PostgreSQL.Keywords
import Pfizer.Datalake.Util.PostgreSQL.PartialQuery
import Pfizer.Datalake.Util.PostgreSQL.Queries
import Pfizer.Datalake.Util.PostgreSQL.Quoted
import Pfizer.Datalake.Util.PostgreSQL.Reserved
import Pfizer.Datalake.Util.PostgreSQL.Schema
import Pfizer.Datalake.Util.PostgreSQL.Settings
import Pfizer.Datalake.Util.PostgreSQL.Transaction
import Pfizer.Datalake.Util.PostgreSQL.TsQuery
