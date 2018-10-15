{-# LANGUAGE InstanceSigs #-}

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
  -- Also valid; the definition below emphasizes that 
  -- ReaderT contains a function that returns a monad,
  -- rather than containing a monad that itself contains
  -- a function
--  pure a = ReaderT $ \_ -> pure a

  (<*>) :: ReaderT r m (a -> b)
        -> ReaderT r m a
        -> ReaderT r m b
  ReaderT rMaB <*> ReaderT rMa =
    ReaderT $ (<*>) <$> rMaB <*> rMa

instance Monad m => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  -- I wasn't able to get this instance by myself; the solution
  -- is from the book.
  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
--  ReaderT rMa >>= f =
--    ReaderT $ \r -> do
--      a <- rMa r
--      runReaderT (f a) r
  -- My desugared version of the book's solution
  ReaderT rMa >>= f =
    ReaderT $ \r ->
      rMa r >>= \a ->
        runReaderT (f a) r
      
{- NOTES
  A reflection after reading the book's ReaderT monad instance
  above: when implementing a monad transformer's bind method,
  it's our job to get everything out of the way of the
  polymorphic `m` monad (so that it can join from m (m b) to
  just m b). We need to get our own concrete monad's
  structure out of its way and then wrapped back around
  the polymorphic m. In the case of ReaderT, that structure 
  we have to remove then wrap with is not only the ReaderT
  data constructor but also the `\r` anonymous function
  and its `r` argument. To reiterate, we need to define our
  transformer's bind in terms of the polymorphic monad's bind,
  and before we can use the polymorphic monad's bind we must
  remove our concrete monad's structure (in a way consistent 
  with its type, be that Maybe, Either, Reader, or whatever 
  else).

  A brief clarification, though: generally part of our
  structure (e.g. the actual Maybe in MaybeT) will be inside
  the polymorphic `m`, not outside of it.

  Generally, when implementing a monad transformer, it seems
  our goal is to implement our transformer's `bind` in terms
  of the polymorphic `m`'s bind, where the `a -> m b` argument
  to that inner bind is derived from the passed-in `f` arg
  (that is, `a -> WhateverT z m b`). To build that `a -> m b`,
  we generally need some kind of catamorphism on the 
  `WhateverT` structure that we built up as part of applying
  `f`.
-}
