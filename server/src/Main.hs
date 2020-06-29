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
