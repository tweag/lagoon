{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pfizer.Datalake.FriendlyException.Orphans () where

import Control.Exception
import Network.HTTP.Client

import Pfizer.Datalake.Interface

-- NOTE: If we change this here we must also update the copy in
-- the command line client (which does not depend on the backend lib)
instance FriendlyException HttpException where
  displayFriendly e =
    case e of
      HttpExceptionRequest req ConnectionTimeout ->
        let secure' = if secure req then "(secure)" else "(insecure)"
        in  "Timeout when making " ++ secure' ++ " connection to " ++ addr (host req) (port req) ++ ".\n"
        ++ "Details: " ++ displayException e
      InvalidUrlException url reason ->
           "Invalid URL " ++ show url ++ ": " ++ reason
      _otherwise ->
        "Unknown HTTP exception: " ++ displayException e
    where
      addr host port = show host ++ ":" ++ show port
