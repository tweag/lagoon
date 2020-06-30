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
module Main (main) where

import Control.Exception
import Data.Proxy
import System.Exit

import Pfizer.Datalake.DB
import Pfizer.Datalake.Interface
import Pfizer.Datalake.ManageDb
import Pfizer.Datalake.Server
import Pfizer.Datalake.Server.AppSkeleton
import Pfizer.Datalake.Server.Cmdline
import Pfizer.Datalake.Util.PostgreSQL

app :: Cmdline -> IO ()
app cmdline@Cmdline{skeletonCmdline} =
      rethrowManageDbException
    $ appSkeleton skeletonCmdline
    $ datalakeServer cmdline

main :: IO ()
main = do
    cmdline <- getCmdline
    catches (app cmdline) [
        friendly (Proxy :: Proxy ManageDbException)
      , friendly (Proxy :: Proxy MigrationRequired)
      , friendly (Proxy :: Proxy MigrationFailure)
      , friendly (Proxy :: Proxy ConnectionException)
      , friendly (Proxy :: Proxy InvalidConfigFile)
      ]
  where
    friendly :: forall e. FriendlyException e => Proxy e -> Handler ()
    friendly _ = Handler $ \(ex :: e) -> do
      putStrLn $ displayFriendly ex
      putStrLn $ ""
      putStrLn $ "Details: " ++ displayException ex
      exitFailure
