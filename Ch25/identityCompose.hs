{-# LANGUAGE InstanceSigs #-}

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h)
      => Functor (Three f g h) where
  fmap f (Three fgha) =
    Three $ (fmap . fmap . fmap) f fgha

instance Applicative f => Applicative (One f) where
  pure :: a -> One f a
  pure a = One $ pure a

  (<*>) :: One f (a -> b)
        -> One f a
        -> One f b
  (One faB) <*> (One fa) = One $ faB <*> fa

instance (Applicative f, Applicative g)
      => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure $ pure a

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  -- I can't believe this actually worked. My original version,
  -- which unnecessarily used a lambda, is commented out below;
  -- the more terse version is what I've left in place.
  -- The way I think of it is that, by wrapping the Applicative
  -- apply in `pure`, I'm telling the compiler that `<*>` needs
  -- to be applied at the inner `g` level, since `pure` will
  -- be adding the outer `f` level. From there, though, I
  -- haven't grokked exactly what happens during the two
  -- calls to `<*>`. I guess the first is preparing `fgaB` to
  -- do an apply using its inner `g` structure, and then
  -- the second `<*>` is actually applying that inner `g`
  -- apply call.
  -- Note: in the next chapter (Ch26, Monad Transformers), the
  -- book actually provides this solution (or rather a slightly
  -- better equivalent one that uses fmap) and explains it.
  (Compose fgaB) <*> (Compose fga) =
    Compose $ (pure (<*>)) <*> fgaB <*> fga
--    Compose $ (pure (\f x -> f <*> x)) <*> fgaB <*> fga

instance (Foldable f, Foldable g)
      => Foldable (Compose f g) where
  foldMap :: Monoid m
          => (a -> m)
          -> Compose f g a
          -> m
  foldMap aM (Compose fga) = foldMap (foldMap aM) fga

instance (Traversable f, Traversable g)
      => Traversable (Compose f g) where
  traverse :: Applicative h
           => (a -> h b)
           -> Compose f g a
           -> h (Compose f g b)
  traverse aHb (Compose fga) = 
    Compose <$> hfgb
      where
        fghb = (fmap . fmap) aHb fga
        fhgb = sequenceA <$> fghb
        hfgb = sequenceA fhgb

