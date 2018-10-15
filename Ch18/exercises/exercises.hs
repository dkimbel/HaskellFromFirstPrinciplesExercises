module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a =
  NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  Left' f <*> Left' a = Left' (f a)
  Right' b <*> _ = Right' b
  _ <*> Right' b = Right' b

instance Monad (PhhhbbtttEither b) where
  return = pure
  Left' a >>= f = f a
  Right' b >>= _ = Right' b

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (PhhhbbtttEither a b) where
  arbitrary = phhhGen

phhhGen :: (Arbitrary a, Arbitrary b)
        => Gen (PhhhbbtttEither a b)
phhhGen = do
  a <- arbitrary
  b <- arbitrary
  elements [ Left' a
           , Right' b ]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where
  (=-=) = eq

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- pulled in from a functor exercise I did (Ch16/exercises.hs)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

-- pulled in from an applicative exercise I did
-- (Ch17/checkerTests/checkerTests.hs)
instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> (Cons a as) = go (Cons f fs) (Cons a as)
    where go Nil _ = Nil
          go (Cons _ fs') Nil = go fs' (Cons a as)
          go (Cons f' fs') (Cons a' as') =
            Cons (f' a') (go (Cons f' fs') as')
-- note: after using the book's provided helper methods,
-- the below was my solution for `<*>`
--  Nil <*> _ = Nil
--  _ <*> Nil = Nil
--  (Cons f fs) <*> as =
--    fmap f as `append` (fs <*> as)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a as) >>= f = f a `append` (as >>= f)
-- Note that the below is more elegant, but relies
-- on more of the helper methods that were provided
-- by the book for Ch17/checkerTests/checkerTests.hs
--  ma >>= f = flatMap f ma

-- the append, fold, concat', and flatMap helper methods
-- were provided as a hint by the book for the
-- accompanying 'checkerTests' exercises for Ch17
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (xs `append` ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

-- admittedly janky; drawn from my previous work at
-- Ch17/checkerTests/checkerTests.hs
listGen :: Arbitrary a => Gen (List a)
listGen = do
  a0 <- arbitrary
  a1 <- arbitrary
  a2 <- arbitrary
  frequency [ (1, return Nil)
            , (2, return $ Cons a0 Nil)
            , (2, return $ Cons a0 (Cons a1 Nil))
            , (2, return $ Cons a0 (Cons a1 (Cons a2 Nil))) ]

instance Eq a => EqProp (List a) where (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor (undefined     :: Nope (Int, Int, Int))
  quickBatch $ applicative (undefined :: Nope (Int, Int, Int))
  quickBatch $ monad (undefined       :: Nope (Int, Int, Int))

  quickBatch $ functor (undefined     :: PhhhbbtttEither Char 
                                         (Int, Int, Int))
  quickBatch $ applicative (undefined :: PhhhbbtttEither Char
                                         (Int, Int, Int))
  quickBatch $ monad (undefined       :: PhhhbbtttEither Char
                                         (Int, Int, Int))

  quickBatch $ functor (undefined     :: Identity
                                         (Int, Int, Int))
  quickBatch $ applicative (undefined :: Identity 
                                         (Int, Int, Int))
  quickBatch $ monad (undefined       :: Identity 
                                         (Int, Int, Int))

  quickBatch $ functor (undefined     :: List
                                         (Int, Int, Int))
  quickBatch $ applicative (undefined :: List 
                                         (Int, Int, Int))
  quickBatch $ monad (undefined       :: List
                                         (Int, Int, Int))
