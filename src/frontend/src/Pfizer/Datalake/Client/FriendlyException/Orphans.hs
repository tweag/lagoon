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
