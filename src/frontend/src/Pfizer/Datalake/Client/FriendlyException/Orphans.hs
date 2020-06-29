{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pfizer.Datalake.Client.FriendlyException.Orphans () where

import Control.Exception
import Network.HTTP.Client

import Pfizer.Datalake.Interface

-- | Annoyingly, this is a copy of the definition in the backend library.
-- But we don't depend on the backend here and it seems overkill
instance FriendlyException HttpException where
  displayFriendly e =
    case e of
      HttpExceptionRequest req ConnectionTimeout ->
        let secure = "(secure)"
        in  "Timeout when making " ++ secure ++ " connection to " ++ addr (host req) (port req) ++ ".\n"
        ++ "Details: " ++ displayException e
      InvalidUrlException url reason ->
           "Invalid URL " ++ show url ++ ": " ++ reason
      _otherwise ->
        "Unknown HTTP exception: " ++ displayException e
    where
      addr host port = show host ++ ":" ++ show port
