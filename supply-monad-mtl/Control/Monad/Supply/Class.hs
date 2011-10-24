{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , UndecidableInstances
  #-}

module Control.Monad.Supply.Class
  ( MonadSupply(..)
  ) where

import Data.Monoid
import Control.Monad.Trans
import qualified Control.Monad.Trans.Cont as Cont
import qualified Control.Monad.Trans.Error as Error
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Lazy as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter

class (Monad m) => MonadSupply s m | m -> s where
  -- | Gets the next value from the supply.
  supply :: m s

instance (MonadSupply s m) => MonadSupply s (Cont.ContT r m) where
  supply = lift supply

instance (MonadSupply s m, Error.Error e) => MonadSupply s (Error.ErrorT e m) where
  supply = lift supply

instance (MonadSupply s m) => MonadSupply s (Identity.IdentityT m) where
  supply = lift supply

instance (MonadSupply s m) => MonadSupply s (Maybe.MaybeT m) where
  supply = lift supply

instance (MonadSupply s m, Monoid w) => MonadSupply s (LazyRWS.RWST r w s' m) where
  supply = lift supply

instance (MonadSupply s m, Monoid w) => MonadSupply s (StrictRWS.RWST r w s' m) where
  supply = lift supply

instance (MonadSupply s m) => MonadSupply s (Reader.ReaderT r m) where
  supply = lift supply

instance (MonadSupply s m) => MonadSupply s (LazyState.StateT s' m) where
  supply = lift supply

instance (MonadSupply s m) => MonadSupply s (StrictState.StateT s' m) where
  supply = lift supply

instance (MonadSupply s m, Monoid w) => MonadSupply s (LazyWriter.WriterT w m) where
  supply = lift supply

instance (MonadSupply s m, Monoid w) => MonadSupply s (StrictWriter.WriterT w m) where
  supply = lift supply
