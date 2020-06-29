-- | Auxiliary: policy for what to do if requested entity does not exist
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Pfizer.Datalake.DB.IfNotFound (
    -- * If-not-found policy
    IfNotFound
  , createIfNotFound
  , errorIfNotFound
  , errorIfFound
  , emptyIfNotFound
    -- * Auxiliary
  , rowsToMaybe
    -- * Custom exceptions
  , NotFoundException(..)
  , AlreadyExistsException(..)
    -- ** Convenience re-exports
  , throwIO
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Only(..))

import Pfizer.Datalake.Interface

{-------------------------------------------------------------------------------
  If-not-found policy
-------------------------------------------------------------------------------}

-- | Policy for what to do if we look for something but didn't find it
--
-- We pass the function the value, if we found it, and a callback to create
-- a new value.
type IfNotFound c m a b = Maybe a -> (c -> m a) -> m b

-- | If the key was not found, create it
createIfNotFound :: Monad m => c -> IfNotFound c m a a
createIfNotFound _ (Just a) _      = return a
createIfNotFound c Nothing  create = create c

-- | If the key was not found, throw an exception
errorIfNotFound :: (MonadIO m, Show k) => k -> IfNotFound c m a a
errorIfNotFound _ (Just a) _ = return a
errorIfNotFound k Nothing  _ = liftIO $ throwIO (NotFound k)

-- | If the key was found, throw an exception
errorIfFound :: (MonadIO m, Show k) => k -> IfNotFound c m a ()
errorIfFound k (Just _) _ = liftIO $ throwIO (AlreadyExists k)
errorIfFound _ Nothing  _ = return ()

-- | If the key was not found, return empty
emptyIfNotFound :: (Monad m, Alternative f) => IfNotFound c m a (f a)
emptyIfNotFound (Just a) _ = return $ pure a
emptyIfNotFound Nothing  _ = return $ empty

-- | Auxiliary function for calling 'IfNotFound' functions
rowsToMaybe :: [Only a] -> Maybe a
rowsToMaybe []       = Nothing
rowsToMaybe [Only x] = Just x
rowsToMaybe (_:_)    = error "rowsToMaybe: more than one value"

{-------------------------------------------------------------------------------
  Custom exceptions
-------------------------------------------------------------------------------}

data NotFoundException      = forall k. Show k => NotFound      k
data AlreadyExistsException = forall k. Show k => AlreadyExists k

deriving instance Show NotFoundException
deriving instance Show AlreadyExistsException

instance Exception NotFoundException
instance Exception AlreadyExistsException

instance FriendlyException NotFoundException where
  displayFriendly (NotFound k) = show k ++ " not found"

instance FriendlyException AlreadyExistsException where
  displayFriendly (AlreadyExists k) = show k ++ " already exists"
