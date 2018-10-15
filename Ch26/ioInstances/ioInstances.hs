{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module IoInstances where

import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- newtype and functor, applicative, and monad instances
-- are copied from maybeT.hs
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure a = MaybeT $ pure $ pure a

  (<*>) :: MaybeT m (a -> b)
        -> MaybeT m a
        -> MaybeT m b
  MaybeT mf <*> MaybeT ma = 
    MaybeT $ (<*>) <$> mf <*> ma

instance Monad m => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  MaybeT ma >>= f = 
    MaybeT $ do
      v <- ma
      case v of 
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)

instance MonadTrans MaybeT where
  lift :: Monad m
       => m a
       -> MaybeT m a
  lift = MaybeT . liftM Just

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  -- this is really just copied from the instance that
  -- the book showed for EitherT
  liftIO = lift . liftIO


-- newtype and instances copied from readerT.hs
newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b)
       -> ReaderT r m a
       -> ReaderT r m b
  fmap f (ReaderT rMa) = ReaderT $ (fmap . fmap) f rMa

instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a = ReaderT $ pure $ pure a

  (<*>) :: ReaderT r m (a -> b)
        -> ReaderT r m a
        -> ReaderT r m b
  ReaderT rMaB <*> ReaderT rMa =
    ReaderT $ (<*>) <$> rMaB <*> rMa

instance Monad m => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  ReaderT rMa >>= f =
    ReaderT $ \r ->
      rMa r >>= \a ->
        runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift :: Monad m
       => m a
       -> ReaderT r m a
  lift ma = ReaderT $ \_ -> ma
      
instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  -- again, same as book's definition for Either
  liftIO = lift . liftIO

-- newtype and instances from stateT.hs; MonadTrans instance
-- from instances/instances.hs 
newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b)
       -> StateT s m a
       -> StateT s m b
  fmap f (StateT sMas) =
    StateT $ (fmap . fmap) tupF sMas
      where tupF (a, b) = (f a, b)

instance (Monad m) 
      => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b)
        -> StateT s m a
        -> StateT s m b
  StateT sMaB <*> StateT sMa =
    StateT $ \s -> do
      (aB, s0) <- sMaB s
      (a, s1) <- sMa s0
      return (aB a, s1)

instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a
        -> (a -> StateT s m b)
        -> StateT s m b
  StateT sMa >>= f =
    StateT $ \s -> do
      (a, s') <- sMa s
      runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift :: Monad m
       => m a
       -> StateT s m a
  lift ma = StateT $ \s -> liftM (,s) ma

instance MonadIO m => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  -- Once again, just a copy of Either instance; it's 
  -- starting to make more sense to me, though. We use
  -- liftIO to transform from `IO a` to whatever arbitrary
  -- `m a` already has MonadIO defined and needs to be
  -- wrapped by StateT, then we lift that into StateT.
  liftIO = lift . liftIO
