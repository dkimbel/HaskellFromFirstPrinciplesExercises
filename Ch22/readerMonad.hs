{-# LANGUAGE InstanceSigs #-}

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable f => f a -> (f a, Int)
barOne fa = (fa, length fa)

barPlus :: (Num a, Foldable t, Functor t) => t a -> (t a, Int)
barPlus r = (foo r, length r)

-- I wrote froot'; as the book pointed out with the function
-- immediately below, though, the application of foo to 
-- bar's second argument was unnecessary
froot' :: Num a => [a] -> ([a], Int)
froot' r = bar (foo r) (foo r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- Note that fooBind (provided by the book) is behaving
-- differently from the Applicative of functions by passing
-- in the unapplied `r` as the second argument to `k` and
-- not the first. It also differs by taking the one-argument
-- function before the two-argument function. Both differences
-- are apparent just from the type signature.
-- As the book then points out, if we replace `r ->` with `m`
-- in the type signature (and add an opening `Monad m =>`
-- constraint), we'll see that this is in fact a version of
-- monadic bind, `>>=`.
fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

froot'' :: Num a => [a] -> ([a], Int)
froot'' = barOne . foo

-- I had to use flip below because bar needs foo applied to
-- its first argument, not its second argument; however, per
-- the expanded type signature of the Applicative apply of 
-- functions:
-- (r -> a -> b) -> (r -> a) -> (r -> b)
-- we can see that the leftmost function, which would be bar,
-- is going to take foo's output `a` as its second argument,
-- not its first.
froot''' :: Num a => [a] -> ([a], Int)
froot''' = (flip bar) <*> foo

frootM :: Num a => [a] -> ([a], Int)
frootM = foo >>= bar

-- Pulled in the Reader Functor and Applicative instances
-- so that I can implement Monad as an exercise
newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure

  -- Note: I had to use runReader on aRb after applying it
  -- to an argument of type `a` in order to extract its
  -- function of type `r -> b`. This was necessary because,
  -- unlike in the case of `ra` (and `rab` in the Applicative
  -- instance), we did not pattern match `r -> b` out of its
  -- Reader on the lefthand side of the equals sign.
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r
