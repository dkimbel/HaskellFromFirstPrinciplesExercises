module LibraryFunctions where

import Data.Foldable
import Data.Monoid

-- Per the book, all functions to be implemented in terms of 
-- foldMap or foldr

sum' :: (Foldable t, Num a) => t a -> a
sum' ta = getSum $ foldMap Sum ta

product' :: (Foldable t, Num a) => t a -> a
product' ta = getProduct $ foldMap Product ta

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a ta = getAny $ foldMap (Any . (== a)) ta

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' ta = foldr f Nothing ta
  where
    f :: Ord a => a -> Maybe a -> Maybe a
    f a ma = case fmap (a <=) ma of
               Just True  -> Just a
               Just False -> ma
               Nothing    -> Just a

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' ta = foldr f Nothing ta
  where
    f :: Ord a => a -> Maybe a -> Maybe a
    f a ma = case fmap (a >=) ma of
               Just True  -> Just a
               Just False -> ma
               Nothing    -> Just a

-- the idea here is that we'll definitely return False if
-- there's any structure to actually fold over, becaues the
-- folding function always returns False; however, the default
-- return value is True, so we'll return True if there is no
-- structure at all
null' :: Foldable t => t a -> Bool
null' ta = foldr f True ta
  where
    f :: a -> Bool -> Bool
    f _ _ = False

null'' :: Foldable t => t a -> Bool
null'' ta = getAll $ foldMap (\_ -> All False) ta

length' :: Foldable t => t a -> Int
length' ta = getSum $ foldMap (\_ -> Sum 1) ta

toList' :: Foldable t => t a -> [a]
toList' ta = foldMap (: []) ta

toList'' :: Foldable t => t a -> [a]
toList'' ta = foldr (:) [] ta

fold' :: (Foldable t, Monoid m) => t m -> m
fold' tm = foldMap id tm

-- per the book, we must define it in terms of foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f ta = foldr g mempty ta
  where
    g a m = f a <> m


