{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Instances where

import Control.Monad (liftM)
import Control.Monad.Trans.Class
--import Control.Monad.Trans.Either
--import Control.Monad.Trans.State.Lazy

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift :: Monad m
       => m a 
       -> EitherT e m a
  lift = EitherT . liftM Right

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift :: Monad m
       => m a
       -> StateT s m a
  lift ma = StateT $ \s -> liftM (,s) ma
  -- Version that doesn't use TupleSections is below.
  -- Note: I just looked back at a definition from earlier
  -- in the book, and that implemented StateT using `>>=`;
  -- bind allowed direct access to the `a` inside `m a`, 
  -- and the righthand arg to bind was `return (a, s)`
--  lift ma = StateT $ \s -> liftM (toTup s) ma
--    where 
--      toTup s a = (a, s)

