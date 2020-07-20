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
-- | Heteroneous lists
module Lagoon.Util.PostgreSQL.HList (
    (:-)(..)
  ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- | Zwaluw-style "push" operator
--
-- Although @postgresql-simple@ has both
--
-- > instance (ToField a, ToField b) => ToRow (a, b)
-- > instance (ToRow a, ToRow b) => ToRow (a :. b)
--
-- it doesn't out of the box provide a datatype that allows us to push something
-- to the front of a row. @(:-)@ provides this missing functionality:
--
-- > instance (ToField a, ToRow b) => ToRow (a :- b)
data (:-) a b = a :- b

infixr 8 :-

instance (ToField a, ToRow b) => ToRow (a :- b) where
  toRow (a :- b) = toRow (Only a :. b)
