-- | Heteroneous lists
module Pfizer.Datalake.Util.PostgreSQL.HList (
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
