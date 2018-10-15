{-# LANGUAGE FlexibleInstances #-}

module FlipFunctor where

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

-- Note that the type variable a is referring to the type 
-- of the term-level variable b; this example is straight
-- from the book, and they made that a little bit confusing,
-- probably because that happens in the real world also
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b


-- I wrote everything below this comment
extractFromFlip :: Flip Tuple b a -> Tuple a b
extractFromFlip (Flip (Tuple x y)) = Tuple x y

data EasyFlip a b =
  EasyFlip b a
  deriving (Eq, Show)

instance Functor (EasyFlip a) where
  fmap f (EasyFlip x y) = EasyFlip (f x) y

--myFlip = fmap (+1) (Flip (Tuple 1 "blah"))
--extractFromFlip myFlip
