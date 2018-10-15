module Exercises where

import Data.Char

upperOnly :: [Char] -> [Char]
upperOnly = filter isUpper

capitalize :: [Char] -> [Char]
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

capitalizeAll :: [Char] -> [Char]
capitalizeAll "" = ""
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

firstCapped :: [Char] -> Char
firstCapped "" = error "Cannot capitalize first char of empty list"
firstCapped (x:_) = toUpper x

firstCapped' :: [Char] -> Char
firstCapped' = toUpper . head

firstCapped'' :: [Char] -> Char
firstCapped'' str = toUpper . head $ str

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = if x then True else myOr' xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ [] = False
myAny' f (x:xs)
    | f x       = True 
    | otherwise = myAny' f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
    | x == y    = True
    | otherwise = myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x ys = any (== x) ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish = concat

squish' :: [[a]] -> [a]
squish' [] = []
squish' (x:xs) = x ++ squish' xs 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish' . map f $ xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Cannot find max value of an empty list"
myMaximumBy f (x:xs) = go f x xs
    where go _ max [] = max
          go f max (x:xs) = case f max x of
                                LT -> go f x xs
                                _  -> go f max xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Cannot find min value of an empty list"
myMinimumBy f (x:xs) = go f x xs
    where go _ min [] = min
          go f min (x:xs) = case f min x of
                                GT -> go f x xs
                                _  -> go f min xs

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare
