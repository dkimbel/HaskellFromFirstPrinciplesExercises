module Exercises where

stops = "pbtdkg"
vowels = "aeiou"

combineThree :: [a] -> [b] -> [(a, b, a)]
combineThree xs ys = [(x1, y, x2) | x1 <- xs, y <- ys, x2 <- xs]

allSvs = combineThree stops vowels

combineThreeP :: [Char] -> [b] -> [(Char, b, Char)]
combineThreeP xs ys = [(x1, y, x2) | x1 <- xs, y <- ys, x2 <- xs, x1 == 'p']

pSvs = combineThreeP stops vowels

nouns = ["dog", "cat", "building", "ball", "chandelier"]
verbs = ["bites", "lights", "obscures", "runs", "barks"]

allNvn = combineThree nouns verbs

seekritFunc :: [Char] -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))
-- The 'secret function' above will give the average number of 
-- characters in each word in the passed-in string. That's because
-- it's dividing the total number of characters by the number
-- of words.

seekritFunc' :: Fractional a => [Char] -> a
seekritFunc' x = fromIntegral (sum (map length (words x))) /
                 fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myOr' :: [Bool] -> Bool
myOr' xs = foldr 
           (\a b ->
             if a
             then True
             else b) False xs

-- we could easily remove the xs arg from the below, but I don't
-- think there's any way to remove the f arg or replace the lambda
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr 
             (\a b ->
               if f a
               then True
               else b) False xs

myElem :: Eq a => a -> [a] -> Bool
myElem el = any (== el)

-- I don't see a way to go fully pointfree and eliminate the el arg.
myElem' :: Eq a => a -> [a] -> Bool
myElem' el xs = foldr
                (\a b ->
                 if a == el
                 then True
                 else b) False xs

myReverse :: [a] -> [a]
myReverse = foldl (\b a -> a : b) []

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> 
                     if f a
                     then a : b
                     else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = squish . myMap f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []     = error "cannot find max value of empty list"
myMaximumBy f (x:xs) = foldl (\a b ->
                               case f a b of
                               GT -> a
                               _  -> b) x xs

-- Note that I can safely replace foldl with foldr1 because,
-- in both cases, the head of the argument list is being used
-- as the very first value for the fold. In the foldr case,
-- the 'zero' value passed in would ordinarily go to the very
-- end (which makes it 'initial' in a right-associative sense),
-- but foldr1 does no such thing.
myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f = foldr1 (\a b ->
                          case f a b of
                          GT -> a
                          _  -> b)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []     = error "cannot find min value of empty list"
myMinimumBy f (x:xs) = foldl (\a b -> 
                               case f a b of
                               LT -> a
                               _  -> b) x xs

myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' f = foldr1 (\a b -> 
                          case f a b of
                          LT -> a
                          _  -> b)
