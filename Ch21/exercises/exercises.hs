module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary
 
instance Eq a => EqProp (Constant a b) where 
  (=-=) = eq

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = optionalGen

optionalGen :: Arbitrary a => Gen (Optional a)
optionalGen = do
  a <- arbitrary
  frequency [ (1, return Nada)
            , (3, return $ Yep a) ]

instance Eq a => EqProp (Optional a) where (=-=) = eq

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a rest) = f a `mappend` foldMap f rest

instance Traversable List where
  -- I didn't expect this to work at first. I think the reason
  -- it does is that the fmapped `Cons` is partially applied;
  -- it's awaiting a second argument. Thus, we actually do
  -- have a function and not a fully-evaluated value inside
  -- of the applicative structure, and so can use `<*>`.
  sequenceA Nil = pure Nil
  sequenceA (Cons a rest) = (Cons <$> a) <*> sequenceA rest

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

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b0 b1) = Three' a (f b0) (f b1)

instance Foldable (Three' a) where
  foldMap f (Three' _ b0 b1) = (f b0) `mappend` (f b1)
  -- the implementation below also works
  --foldr f z (Three' _ b0 b1) = foldr f z [b0, b1]

{-
   According to the book, Traversable is "commonly described as
   a way to traverse a data structure, mapping a function
   inside a structure while accumulating the applicative
   contexts along the way." Perhaps the easiest mental example
   is applying a function of type `(a -> Maybe b)` to `[a]`,
   and 'accumulating' the contents of the outer Just -- or, 
   should there be a Nothing, breaking short with Nothing 
   as the result. That "mapping and accumulation" is also on
   display in the definition of a Traversable instance for
   Three' below; we fmap, then use applicative 'apply' to
   in a sense accumulate our applicative contexts.
 -}
instance Traversable (Three' a) where
  traverse f (Three' a b0 b1) = 
    Three' a <$> f b0 <*> f b1

instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where 
  (=-=) = eq

data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = (foldMap f na) `mappend` f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Arbitrary (n a), Arbitrary a) => 
  Arbitrary (S n a) where
  arbitrary = S <$> arbitrary
                <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t0 a t1) = Node (fmap f t0) (f a) (fmap f t1)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t0 a t1) = 
    foldMap f t0 `mappend` f a `mappend` foldMap f t1
--    foldr = foldTree

-- Note: inorder and foldTree are from Ch11/binaryTrees.hs;
-- while I was able to implement foldMap easily, I was not
-- able to implement foldr. I remembered that we had folded
-- trees in an earlier exercise, though, and used `grep -R`
-- to find the relevant file.
-- NOTES:
-- In retrospect, this implementation makes a LOT of sense:
-- the essence of Foldable is largely toList. If you want to
-- fold any given data structure, what you really need to do
-- is convert that data structure to a list, at which point
-- you can use list's implementation of foldr.
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Leaf x) = [x]
inorder (Node left x right) =
    inorder left ++ [x] ++ inorder right

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f z tree = foldr f z (inorder tree)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t0 a t1) =
    Node <$> traverse f t0 <*> f a <*> traverse f t1

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = treeGen

-- very, very janky; based on my listGen function
treeGen :: Arbitrary a => Gen (Tree a)
treeGen = do
  a0  <- arbitrary
  a1  <- arbitrary
  a2  <- arbitrary
  a3  <- arbitrary
  a4  <- arbitrary
  a5  <- arbitrary
  a6  <- arbitrary
  a7  <- arbitrary
  a8  <- arbitrary
  a9  <- arbitrary
  a10 <- arbitrary
  a11 <- arbitrary
  a12 <- arbitrary
  frequency [ (1, return Empty)
            , (2, return $ Leaf a0)
            , (2, return $ Node Empty a1 (Leaf a2))
            , (2, return $ Node (Leaf a3) a4 (Leaf a5))
            , (2, return $ Node (Node (Leaf a6) a7 (Leaf a8))
                                a9
                                (Node (Leaf a10) a11 (Leaf a12))
              ) ]

instance Eq a => EqProp (Tree a) where (=-=) = eq


main :: IO ()
main = do
  putStrLn "\n~~~~~~~~~~ Identity ~~~~~~~~~~"
  quickBatch (functor     (undefined :: Identity
                                        (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Identity
                                        (Int, Int, [Int])))

  putStrLn "\n~~~~~~~~~~ Constant ~~~~~~~~~~"
  quickBatch (functor     (undefined :: Constant Bool
                                        (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Constant Bool
                                        (Int, Int, [Int])))

  putStrLn "\n~~~~~~~~~~ Optional ~~~~~~~~~~"
  quickBatch (functor     (undefined :: Optional
                                        (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Optional
                                        (Int, Int, [Int])))

  putStrLn "\n~~~~~~~~~~ List ~~~~~~~~~~"
  quickBatch (functor     (undefined :: List
                                        (Int, Int, [Int])))
  quickBatch (traversable (undefined :: List
                                        (Int, Int, [Int])))

  putStrLn "\n~~~~~~~~~~ Three ~~~~~~~~~~"
  quickBatch (functor     (undefined :: Three Char Bool
                                        (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Three Char Bool
                                        (Int, Int, [Int])))

  putStrLn "\n~~~~~~~~~~ Three' ~~~~~~~~~~"
  quickBatch (functor     (undefined :: Three' Char
                                        (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Three' Char
                                        (Int, Int, [Int])))

  putStrLn "\n~~~~~~~~~~ S ~~~~~~~~~~"
  quickBatch (functor     (undefined :: S Maybe
                                        (Int, Int, [Int])))
  quickBatch (traversable (undefined :: S Maybe
                                        (Int, Int, [Int])))

  putStrLn "\n~~~~~~~~~~ Tree ~~~~~~~~~~"
  quickBatch (functor     (undefined :: Tree
                                        (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Tree
                                        (Int, Int, [Int])))
