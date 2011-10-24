{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Control.Monad.Supply
  ( SupplyT(..)
  , Supply
  , MonadSupply(..)
  , runSupplyT
  , runSupplyTAction
  , runSupplyTList
  , runSupply
  , runSupplyList
    -- * Example
    -- $example
  ) where

import Control.Monad.Trans.Supply
import Control.Monad.Supply.Class

import Control.Monad.Trans
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.RWS.Class

instance (MonadIO m) => MonadIO (SupplyT s m) where
  liftIO = SupplyT . liftIO

instance (MonadCont m) => MonadCont (SupplyT s m) where
  callCC f = SupplyT $ callCC f'
    where f' g = unwrapSupplyT $ f (SupplyT . g)

instance (MonadError e m) => MonadError e (SupplyT s m) where
  throwError = SupplyT . throwError
  catchError (SupplyT m) f = SupplyT $ catchError m (unwrapSupplyT . f)

instance (MonadRWS r w s' m) => MonadRWS r w s' (SupplyT s m)

instance (MonadReader r m) => MonadReader r (SupplyT s m) where
  ask = SupplyT ask
  local f (SupplyT m) = SupplyT $ local f m

instance (MonadState s' m) => MonadState s' (SupplyT s m) where
  get = SupplyT $ lift get
  put = SupplyT . lift . put

instance (MonadWriter w m) => MonadWriter w (SupplyT s m) where
  tell = SupplyT . tell
  listen (SupplyT m) = SupplyT $ listen m
  pass (SupplyT m) = SupplyT $ pass m

-- $example
--
-- See 'Control.Monad.Trans.Supply' for an example.
