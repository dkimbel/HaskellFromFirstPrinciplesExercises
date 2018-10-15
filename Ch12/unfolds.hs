module Unfolds where

import Data.List

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n + x) xs

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n * x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
    where go :: [a] -> [[a]] -> [a]
          go xs' [] = xs'
          go xs' (x:xs) = go (xs' ++ x) xs

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

-- The definitions above are from the book; the ones below are mine

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
    Just (x, y) -> x : myUnfoldr f y
    Nothing     -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\x -> Just (x, f x)) a

