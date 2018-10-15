{-# LANGUAGE ViewPatterns #-}

module FunctorTests where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a))
                => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) 
               => (a -> b) -> (b -> c)
               -> f a -> Bool
functorCompose g h f =
  fmap (h . g) f == ((fmap h) . (fmap g)) f

li :: [Int] -> Bool
li = functorCompose (+1) (*2)

functorCompose' :: (Eq (f c), Functor f)
                => f a -> Fun a b
                -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return $ Identity a

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a0 a1) = Pair (f a0) (f a1)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a <- arbitrary
  return $ Pair a a

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

type TwoFC = Two Bool Int -> IntToInt -> IntToInt -> Bool

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

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

type ThreeFC = Three Bool String Int -> IntToInt ->
               IntToInt -> Bool

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b0 b1) = Three' a (f b0) (f b1)

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

type Three'FC = Three' Char Int -> IntToInt -> IntToInt -> Bool

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
  arbitrary = fourGen

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c,
           Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

type FourFC = Four Char Bool String Int -> IntToInt ->
              IntToInt -> Bool

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a0 a1 a2 b) = Four' a0 a1 a2 (f b)

instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Four' a b) where
  arbitrary = four'Gen

four'Gen :: (Arbitrary a, Arbitrary b)
         => Gen (Four' a b)
four'Gen = do
  a0 <- arbitrary
  a1 <- arbitrary
  a2 <- arbitrary
  b <- arbitrary
  return $ Four' a0 a1 a2 b

type Four'FC = Four' Char Int -> IntToInt -> IntToInt -> Bool

-- It is not possible to create a Functor instance for type
-- Trivial because Trivial is of kind *, and to have a
-- Functor instance a type must be ok kind (* -> *)
data Trivial = Trivial

--instance Functor Trivial where
--  fmap = undefined

main :: IO ()
main = do 
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck li
  quickCheck (functorCompose' :: IntFC)

  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck (functorCompose' :: IdentityFC)

  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck (functorCompose' :: PairFC)

  quickCheck $ \x -> functorIdentity (x :: Two Bool Int)
  quickCheck (functorCompose' :: TwoFC)

  quickCheck $ \x -> functorIdentity (x :: Three Bool 
                                           String Int)
  quickCheck (functorCompose' :: ThreeFC)

  quickCheck $ \x -> functorIdentity (x :: Three' Char Int)
  quickCheck (functorCompose' :: Three'FC)

  quickCheck $ \x -> functorIdentity (x :: Four Char Bool
                                           String Int)
  quickCheck (functorCompose' :: FourFC)

  quickCheck $ \x -> functorIdentity (x :: Four' Char Int)
  quickCheck (functorCompose' :: Four'FC)
