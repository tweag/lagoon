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
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Lagoon.Util.PostgreSQL.Transaction (
    -- * Transactions
    Transaction -- opaque
  , runTransaction
  , runTransactionC
  , partialTransaction
  , partialTransactionC
  , deferConstraints
  , bracketT_
  , getConnection
  , getSchema
    -- * Execute queries
  , execute
  , query
  , execute_
  , query_
  , copy_
  , putCopyData
  , putCopyEnd
  , putCopyError
  , getCopyData
    -- * Convenience methods for queries that require the schema
  , QueryS
  , executeS
  , executeS_
  , queryS
  , queryS_
    -- * Exceptions
  , SomeSqlException(..)
    -- ** Convenience re-exports
  , MonadTrans(..)
  , MonadIO(..)
  , CallStack
  , PG.CopyOutResult(..)
  ) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple (Query, Connection)
import Database.PostgreSQL.Simple.ToRow
import Data.Conduit
import Data.Int
import GHC.Stack
import qualified Database.PostgreSQL.Simple      as PG
import qualified Database.PostgreSQL.Simple.Copy as PG
import qualified Data.ByteString                 as BS.S

import Lagoon.Interface
import Lagoon.Util.Exception
import Lagoon.Util.PostgreSQL.Exception

{-------------------------------------------------------------------------------
  Transactions
-------------------------------------------------------------------------------}

-- | Database transaction
--
-- We are explicit at the type level where transaction boundaries are; moreover,
-- this also gives us a convenient way to pass around some information that we
-- need in lots of places.
newtype Transaction m a = Transaction {
    unTransaction :: ReaderT (Connection, Schema) m a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

instance MonadUnliftIO m => MonadUnliftIO (Transaction m) where
  askUnliftIO = Transaction $ do
      UnliftIO unlift <- askUnliftIO
      pure $ UnliftIO $ unlift . unTransaction

instance MonadBase b m => MonadBase b (Transaction m) where
  liftBase = Transaction . liftBase

instance ( MonadThrow    m
         , MonadIO       m
         , MonadBase IO  m
         , MonadResource m
         ) => MonadResource (Transaction m) where
  liftResourceT = Transaction . liftResourceT

-- | Unwrap 'Transaction' without bracketing with @BEGIN@ and @COMMIT@
partialTransaction :: Connection -> Schema -> Transaction m a -> m a
partialTransaction conn schema act =
    runReaderT (unTransaction act) (conn, schema)

-- | Conduit analogue of 'partialTransaction'
partialTransactionC :: Monad m
                    => Connection -> Schema
                    -> ConduitM i o (Transaction m) r
                    -> ConduitM i o m r
partialTransactionC conn schema = transPipe $ partialTransaction conn schema

-- | Run a transaction
runTransaction :: (MonadIO m, MonadMask m)
               => Connection -> Schema -> Transaction m a -> m a
runTransaction conn schema act =
    bracketE_ (liftIO $ PG.begin conn)
              (\r -> do liftIO $ PG.commit conn ; return r)
              (liftIO $ PG.rollback conn) $
      partialTransaction conn schema act

-- | Conduit analogue of 'runTransaction'
runTransactionC :: forall i o m r. MonadResource m
                => Connection
                -> Schema
                -> ConduitM i o (Transaction m) r
                -> ConduitM i o m r
runTransactionC conn schema k =
    bracketPE_ (PG.begin conn)
               (\r -> do liftIO $ PG.commit conn ; return r)
               (PG.rollback conn) $
      partialTransactionC conn schema k

-- | Defer constraint checking
--
-- TODO: Currently it's not save to nest calls to deferConstraints
deferConstraints :: MonadIO m => Transaction m a -> Transaction m a
deferConstraints k = do
    execute_ "SET CONSTRAINTS ALL DEFERRED"
    a <- k
    execute_ "SET CONSTRAINTS ALL IMMEDIATE"
    return a

-- | Equivalent of 'bracketPE_' for conduits
bracketT_ :: MonadResource m
          => Transaction IO ()
          -> (r -> Transaction IO r')
          -> Transaction IO ()
          -> ConduitM i o (Transaction m) r -> ConduitM i o (Transaction m) r'
bracketT_ alloc finalize abort k = do
    conn <- lift $ Transaction $ ask
    let run :: Transaction IO a -> IO a
        run c = runReaderT (unTransaction c) conn
    bracketPE_
      (run alloc)
      (run . finalize)
      (run abort)
      k

getConnection :: Monad m => Transaction m Connection
getConnection = Transaction $ fst <$> ask

getSchema :: Monad m => Transaction m Schema
getSchema = Transaction $ snd <$> ask

{-------------------------------------------------------------------------------
  Transaction primitives
-------------------------------------------------------------------------------}

-- | Construct primitive transactions (not exported)
prim :: (MonadIO m, ?loc :: CallStack)
     => (Connection -> IO a) -> Transaction m a
prim k = do
    conn   <- getConnection
    schema <- getSchema
    liftIO $ addCallStack (isSqlException schema) $ k conn

-- | Execute a query
--
-- We don't return the number of affected rows, as we don't use it anywhere
execute :: (MonadIO m, ToRow q, ?loc :: CallStack)
        => Query -> q -> Transaction m ()
execute q vals = prim $ \c -> logQuery q $ void $ PG.execute c q vals

execute_ :: (MonadIO m, ?loc :: CallStack)
         => Query -> Transaction m ()
execute_ q = prim $ \c -> logQuery q $ void $ PG.execute_ c q

query :: (MonadIO m, ToRow q, FromRow r, ?loc :: CallStack)
      => Query -> q -> Transaction m [r]
query q vals = prim $ \c -> logQuery q $ PG.query c q vals

query_ :: (MonadIO m, FromRow r, ?loc :: CallStack)
       => Query -> Transaction m [r]
query_ q = prim $ \c -> logQuery q $ PG.query_ c q

copy_ :: (MonadIO m, ?loc :: CallStack)
      => Query -> Transaction m ()
copy_ q = prim $ \c -> logQuery q $ PG.copy_ c q

putCopyEnd :: (MonadIO m, ?loc :: CallStack)
           => Transaction m Int64
putCopyEnd = prim $ \c -> PG.putCopyEnd c

putCopyError :: (MonadIO m, ?loc :: CallStack)
             => BS.S.ByteString -> Transaction m ()
putCopyError errMsg = prim $ \c -> PG.putCopyError c errMsg

putCopyData :: (MonadIO m, ?loc :: CallStack)
            => BS.S.ByteString -> Transaction m ()
putCopyData bs = prim $ \c -> PG.putCopyData c bs

getCopyData :: (MonadIO m, ?loc :: CallStack)
            => Transaction m PG.CopyOutResult
getCopyData = prim $ \c -> PG.getCopyData c

logQuery :: Query -> IO a -> IO a
{-# INLINE logQuery #-}
logQuery q k = print q >> k

{-------------------------------------------------------------------------------
  Convenience methods for dealing with the DB schema
-------------------------------------------------------------------------------}

-- | Query that is parametric in the DB schema
--
-- TODO: Make this a newtype with a FromString instance. This will cleanup.
type QueryS = Schema -> Query

executeS :: (MonadIO m, ToRow q, ?loc :: CallStack)
         => QueryS -> q -> Transaction m ()
executeS q vals = do
    schema <- getSchema
    execute (q schema) vals

executeS_ :: (MonadIO m, ?loc :: CallStack)
          => QueryS -> Transaction m ()
executeS_ q = do
    schema <- getSchema
    execute_ (q schema)

queryS :: (MonadIO m, ToRow q, FromRow r, ?loc :: CallStack)
       => QueryS -> q -> Transaction m [r]
queryS q vals = do
    schema <- getSchema
    query (q schema) vals

queryS_ :: (MonadIO m, FromRow r, ?loc :: CallStack)
        => QueryS -> Transaction m [r]
queryS_ q = do
    schema <- getSchema
    query_ (q schema)
