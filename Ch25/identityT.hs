{-# LANGUAGE InstanceSigs #-}

import Control.Monad (join)

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

-- At first I was surprised that IdentityT could only take
-- one argument, but it's no surprise -- from the record
-- syntax, you can see that IdentityT only takes a single
-- argument that can be accessed via the runIdentityT getter.
-- It's described by two type variables, but there's only one
-- term-level argument that IdentityT takes.
instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT $ fmap f ma

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) = Identity (f a)

instance Applicative m => Applicative (IdentityT m) where
  pure = IdentityT . pure

  (IdentityT mf) <*> (IdentityT ma) = IdentityT (mf <*> ma)

instance Monad Identity where
  return = pure

  (Identity a) >>= faMb = faMb a

instance Monad m => Monad (IdentityT m) where
  return = pure

  -- I wasn't able to get this one myself; both the main
  -- definition below and the commented-out one are from 
  -- the book
  (>>=) :: IdentityT m a 
        -> (a -> IdentityT m b)
        -> IdentityT m b
  (IdentityT ma) >>= f = 
    IdentityT $ ma >>= runIdentityT . f
--    let aimb = join $ fmap runIdentityT $ fmap f ma
--    in IdentityT aimb
