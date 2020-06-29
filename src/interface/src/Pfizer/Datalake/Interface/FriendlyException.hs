{-# LANGUAGE UndecidableInstances #-}
module Pfizer.Datalake.Interface.FriendlyException (FriendlyException(..)) where

import Control.Exception

-- | User-friendly rendering of an exception
--
-- The goal here is to make the error understandable to application users
-- (non-Haskell experts), even if some detail is lost
class Exception e => FriendlyException e where
  displayFriendly :: e -> String
  displayFriendly = displayException
