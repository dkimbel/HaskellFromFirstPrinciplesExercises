module Exercises where

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

-- yes, due to alpha equivalence c and c'' ARE the same thing
c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r (x:xs) = xs

r' :: [a] -> [a]
r' xs = xs ++ xs

r'' :: [a] -> [a]
r'' xs = reverse xs

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a :: (a -> c) -> a -> a
a xToY x = x

a' :: (a -> b) -> a -> b
a' xToY x = xToY x
