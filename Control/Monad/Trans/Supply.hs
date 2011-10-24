module Control.Monad.Trans.Supply
  ( SupplyT(..)
  , Supply
  , supply
  , runSupplyT
  , runSupplyTAction
  , runSupplyTList
  , runSupplyList
    -- * Example
    -- $example
  ) where

import Data.Functor.Identity
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- | A monad for giving computations a supply of arbitrary values;
-- potential sources for these values include an action in the
-- transformed monad and a list.
newtype SupplyT s m a = SupplyT { unwrapSupplyT :: StateT (SupplyT s m s) m a }

type Supply s = SupplyT s Identity

instance (Monad m) => Functor (SupplyT s m) where
  fmap f (SupplyT m) = SupplyT (liftM f m)

instance (Monad m) => Applicative (SupplyT s m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (SupplyT s m) where
  return = SupplyT . return
  SupplyT m >>= f = SupplyT (m >>= unwrapSupplyT . f)
  fail = SupplyT . fail

instance MonadTrans (SupplyT s) where
  lift = SupplyT . lift

-- | Gets the next value from the supply.
supply :: (Monad m) => SupplyT s m s
supply = SupplyT $ get >>= unwrapSupplyT

-- | This is the most general form of @runSupplyT@. For an example of
-- its use, see the source of 'runSupplyTList'.
--
-- You're probably better off using one of the simpler forms.
runSupplyT :: (Monad m) => SupplyT s m a -> SupplyT s m s -> m a
runSupplyT = evalStateT . unwrapSupplyT

-- | Runs a SupplyT computation, executing the given action in the
-- transformed monad whenever a value is requested.
runSupplyTAction :: (Monad m) => SupplyT s m a -> m s -> m a
runSupplyTAction m = runSupplyT m . lift

-- | Runs a SupplyT computation, taking values from the list in
-- sequence whenever a value is requested.
--
-- If a value is requested after all values have been exhausted, then
-- 'supply' results in failure in the transformed monad:
--
-- > GHCi> runSupplyTList supply [] :: IO ()
-- > *** Exception: user error (runSupplyTList: out of supply)
-- > GHCi> runSupplyTList supply [] :: Maybe ()
-- > Nothing
runSupplyTList :: (Monad m) => SupplyT s m a -> [s] -> m a
runSupplyTList m = runSupplyT m . prov
  where prov [] = fail "runSupplyTList: out of supply"
        prov (x:xs) = SupplyT $ put (prov xs) >> return x

-- | See 'runSupplyTList'.
runSupplyList :: Supply s a -> [s] -> a
runSupplyList m = runIdentity . runSupplyTList m

-- $example
--
-- Here is a simple computation in SupplyT:
--
-- > example :: (Monad m, Num s) => SupplyT s m s
-- > example = do
-- >   a <- supply
-- >   b <- supply
-- >   return (a + b)
--
-- We can run this at the GHCi prompt, with the supply being a simple
-- list:
--
-- > GHCi> runSupplyList example [4, 8]
-- > 12
--
-- Or we could define a more interesting supply source, like the
-- console:
--
-- > runSupplyPrompt :: (Read s) => SupplyT s IO a -> IO a
-- > runSupplyPrompt m = runSupplyTAction m $ do
-- >   putStr "Provide a value: "
-- >   hFlush stdout
-- >   readLn
--
-- And so:
--
-- > GHCi> runSupplyPrompt example
-- > Provide a value: 4
-- > Provide a value: 8
-- > 12
--
-- Or the standard random number generator:
--
-- > runSupplyRandom :: (Random s) => SupplyT s IO a -> IO a
-- > runSupplyRandom m = runSupplyTAction m randomIO
--
-- Resulting in the following:
--
-- > GHCi> runSupplyRandom example
-- > 4744862929307519485
-- > GHCi> runSupplyRandom example
-- > -9094281091336404359
