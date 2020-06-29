{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pfizer.Datalake.Interface.Users (
    UserIx(..)
  , User(..)
  , UserName
  ) where

import GHC.Generics (Generic)
import Text.Show.Pretty

import Pfizer.Datalake.Interface.DB
import Pfizer.Datalake.Interface.Pretty

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
