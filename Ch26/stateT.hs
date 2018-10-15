{-# LANGUAGE InstanceSigs #-}

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
  -- A desugared version (pretty ugly on account of the
  -- in-lambda pattern matching):
--  StateT sMaB <*> StateT sMa =
--    StateT $ \s ->
--      sMaB s >>= \(aB, s0) ->
--        sMa s0 >>= \(a, s1) ->
--          return (aB a, s1)
  -- Below is my first implementation that typechecked;
  -- while its types are good, it presumably would not
  -- successfully update state, because it's passing the
  -- same initial state to both sMaB and sMa; it has no
  -- access to the state outcomes (and executes no effects
  -- on state) because it doesn't use monadic bind.
--    StateT $ \s ->
--      let maB = sMaB s
--          ma  = sMa s
--          mb  = tupF <$> maB <*> ma
--          tupF :: ((a -> b), s) -> (a, s) -> (b, s)
--          tupF (f, s0) (x, s1) = (f x, s1)
--      in mb


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
  -- Desugared version below
--  StateT sMa >>= f =
--    StateT $ \s ->
--      sMa s >>= \(a, s') ->
--        runStateT (f a) $ s'

-- TODO: contemplate how monads can thread state through
--   repeated calls to bind, but applicatives cannot
--   do the same using (<*>)
