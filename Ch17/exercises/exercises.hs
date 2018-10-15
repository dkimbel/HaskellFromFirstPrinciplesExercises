module Exercises where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a0 a1) = Pair (f a0) (f a1)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f0 f1) <*> (Pair a0 a1) = 
    Pair (f0 a0) (f1 a1)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a0 <- arbitrary
  a1 <- arbitrary
  return $ Pair a0 a1 

instance Eq a => Eq (Pair a) where
  (Pair x0 y0) == (Pair x1 y1) = (x0 == x1) && (y0 == y1)

instance Eq a => EqProp (Pair a) where (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two a0 f) <*> (Two a1 x) = Two (a0 <> a1) (f x)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a0 b0 f) <*> (Three a1 b1 x) =
    Three (a0 <> a1) (b0 <> b1) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
  arbitrary = threeGen

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c)
         => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b0 b1) = Three' a (f b0) (f b1)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a0 f0 f1) <*> (Three' a1 x0 x1) =
    Three' (a0 <> a1) (f0 x0) (f1 x1)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Three' a b) where
  arbitrary = three'Gen

three'Gen :: (Arbitrary a, Arbitrary b)
          => Gen (Three' a b)
three'Gen = do
  a <- arbitrary
  b0 <- arbitrary
  b1 <- arbitrary
  return $ Three' a b0 b1

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
  Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a0 b0 c0 f) <*> (Four a1 b1 c1 x) = 
    Four (a0 <> a1) (b0 <> b1) (c0 <> c1) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
  arbitrary = fourGen

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
        => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => 
  EqProp (Four a b c d) where
  (=-=) = eq

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a0 a1 a2 b) = Four' a0 a1 a2 (f b)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' a0 b0 c0 f) <*> (Four' a1 b1 c1 x) =
    Four' (a0 <> a1) (b0 <> b1) (c0 <> c1) (f x)

instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Four' a b) where
  arbitrary = four'Gen

four'Gen :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
four'Gen = do
  a0 <- arbitrary
  a1 <- arbitrary
  a2 <- arbitrary
  b <- arbitrary
  return $ Four' a0 a1 a2 b

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: 
                              Pair (Int, Int, Int))

  quickBatch $ applicative (undefined :: 
                              Two [Char] (Int, Int, Int))

  quickBatch $ applicative (undefined :: 
                              Three [Char] [Bool] 
                              (Int, Int, Int))

  quickBatch $ applicative (undefined :: 
                              Three' [Char] (Int, Int, Int))

  quickBatch $ applicative (undefined :: 
                              Four [Char] [Bool] [Integer]
                              (Int, Int, Int))

  quickBatch $ applicative (undefined :: 
                              Four' [Char] (Int, Int, Int))
