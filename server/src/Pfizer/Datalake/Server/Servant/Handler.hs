{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Server.Servant.Handler (
    Handler' -- opaque
  , fromHandler'
  ) where

import Control.Monad.Catch hiding (Handler)
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.IO.Unlift
import Servant

{-------------------------------------------------------------------------------
  Replacement for @servant@'s 'Handler' type

  We avoid 'ExceptT' and the mess it incurs (difficult to coherent definitions
  of 'MonadMask', 'MonadThrow', and 'MonadCatch'). Instead we throw 'ServantErr'
  as exceptions always and only at the very top-level ('fromHandler'') do we
  translate back to 'ExceptT'.
-------------------------------------------------------------------------------}

newtype Handler' a = Handler' { unHandler' :: IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadCatch
           , MonadThrow
           , MonadMask
           , MonadBase IO
           , MonadUnliftIO
           )

instance MonadError ServantErr Handler' where
  throwError err       = Handler' $ throwM err
  catchError act onErr = Handler' $ catch (unHandler' act) (unHandler' . onErr)

instance MonadBaseControl IO Handler' where
  type StM Handler' a = StM IO a

  -- liftBaseWith is confusing due to all the negative and doubly-negative
  -- arguments. But if we squint a bit we can see that it's just the identity:
  --
  --   \f -> Handler' $ liftBaseWith (\g -> f (g . unHandler'))
  -- = \f -> liftBaseWith (\g -> f g)
  -- = \f -> liftBaseWith f
  -- = liftBaseWith
  liftBaseWith :: (RunInBase Handler' IO -> IO a) -> Handler' a
  liftBaseWith f = Handler' $ liftBaseWith (\g -> f (g . unHandler'))

  restoreM :: StM Handler' a -> Handler' a
  restoreM f = Handler' $ restoreM f

fromHandler' :: Handler' a -> Handler a
fromHandler' = Handler . ExceptT . try . unHandler'
