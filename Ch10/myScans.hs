module MyScans where

scanl'' :: (a -> b -> a) -> a -> [b] -> [a]
scanl'' f q ls = map (foldl f q) $ reverse $ myPermutations ls
    
myPermutations :: [a] -> [[a]]
myPermutations [] = [[]]
myPermutations xs = xs : myPermutations (init xs)

-- very slight variation from what's in the book,
-- derived from the book's definition to make a point
scanl''' :: (a -> b -> a) -> a -> [b] -> [a]
scanl''' f q ls =
  case ls of
       []   -> [q] -- same as q : []
       x:xs -> q : scanl' f (f q x) xs

-- From the book (more efficient than mine, though,
-- because I repeated the work of folding some of the
-- same list items, as all items but the last were 
-- repeated an increasing number of times by my map);
-- this implementation is O(n) and mine is more like
-- O(n^2) or O(n log n)
--
-- In retrospect, the book's implementation makes
-- a lot of sense -- it's basically identical to the
-- book's foldl, except it also does a cons of the
-- accumulator (here called q) every call. One other
-- difference is that, in the empty list case, it returns
-- an empty list instead of the accumulator. That's because
-- it already essentially handled the final return of the
-- accumulator by doing the cons before every pattern match,
-- including the final pattenr match that will hit the `[]`
-- datay construtor. See my scanl''' above to see how the 
-- function would return [q] instead in the empty list case 
-- if it didn't have the pattern-match-preceding cons.
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f q ls =
  q : (case ls of
        []   -> []
        x:xs -> scanl' f (f q x) xs)

-- NOTES
-- Let's get to the bottom of this example from the book:
-- fibs = 1 : scanl (+) 1 fibs
-- Specifically:
-- take 5 fibs
-- 1 : scanl (+) 1 fibs
-- 1 : (1 : scanl (+) ((+) 1 1) (tail fibs))
-- ^x   ^q                   
-- 1 : 1 : (2 : scanl (+) ((+) 2 1) (tail $ tail fibs))
--     ^x   ^q                     
-- 1 : 1 : 2 : (3 : scanl (+) ((+) 3 2) (tail $ tail $ tail fibs))
--         ^x   ^q                     
-- As we can see from the definition of scanl, whatever
-- q we calculate in one call will be the next call's s.
--
-- For a maybe-better explanation, see the Stack Overflow
-- at https://stackoverflow.com/questions/32576983/big-0-of-fibonacci-number-using-scanl-haskell.

fibs = 1 : scanl (+) 1 fibs
fibsN n = fibs !! n
takeFibs n = take n fibs
fibs20 = take 20 fibs
fibsUnder100 = takeWhile (< 100) fibs

factorial = scanl (*) 1 [2..]

-- What's the explanation for the following behavior?
-- doubles = scanl (+) 1 doubles
-- take 8 doubles => [1, 2, 4, 8, 16, 32, 64, 128]
-- I guess it always treats q and x as the same value?
-- Whatever we're prepending in this very call is also
-- considered the head of the list that we're building?
--
-- Also: why do we seemingly have to use scanl with infinite
-- lists, rather than scanr?
