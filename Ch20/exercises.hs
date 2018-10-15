module Exercises where

import qualified Data.Monoid as M

data Constant a b =
  Constant a
  deriving Show

instance Foldable (Constant a) where
  foldr _ x _ = x

data Two a b =
  Two a b
  deriving Show

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c =
  Three a b c
  deriving Show

instance Foldable (Three a b) where
  foldMap f (Three a b c ) = f c

data Three' a b =
  Three' a b b
  deriving Show

instance Foldable (Three' a) where
  foldMap f (Three' a b0 b1) = f b0 M.<> f b1
  -- this seems to work just fine, but I'm commenting it out
  --foldr f z (Three' a b0 b1) = foldr f z [b0, b1]

data Four' a b =
  Four' a b b b
  deriving Show

instance Foldable (Four' a) where
  -- all three of my definitions work
  --foldMap f (Four' a b0 b1 b2) = f b0 M.<> f b1 M.<> f b2
  --foldMap f (Four' a b0 b1 b2) = mconcat $ map f [b0, b1, b2]
  foldr f z (Four' a b0 b1 b2) = foldr f z [b0, b1, b2]

-- per the book, this is to use foldMap
-- my implementation is monoidally combining the results;
-- it's unclear whether or not this was the book's intention
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f ta = foldMap g ta
  where
    g x = if f x
          then pure x
          else mempty
