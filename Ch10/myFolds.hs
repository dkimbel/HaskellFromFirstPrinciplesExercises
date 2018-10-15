module MyFolds where

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

length' :: [a] -> Int
length' xs = foldr (+) 0 . map (\x -> 1) $ xs

product' :: Num a => [a] -> a
product' = foldr (*) 1

concat' :: [[a]] -> [a]
concat' = foldr (++) []

foldlWrong :: (b -> a -> b) -> b -> [a] -> b
foldlWrong _ acc [] = acc
foldlWrong f acc (x:xs) = f (foldlWrong f acc xs) x

foldRe :: (a -> b -> b) -> b -> [a] -> b
foldRe _ acc [] = acc
foldRe f acc (x:xs) = foldRe f (f x acc) xs

foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' _ initial [] = initial
foldr'' f initial (x:xs) = f x (foldr'' f initial xs)

-- this one is straight from the book
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' _ acc [] = acc
foldl'' f acc (x:xs) = foldl'' f (f acc x) xs


-- NOTES
--
-- Folds are also a type of "catamorphism", which means they
-- break down structure.
--
-- Recursion of the spine is unconditional for foldl (but not
-- foldr). The difference is that foldr passes control back
-- and forth between foldr and f, while foldl maintains
-- control until the entire spine has been traversed.
--
-- Because foldl traverses the spine unconditionally, it'll
-- traverse the entire spine even if the foldl call is
-- preceded by, say, `take n $`.
--
-- Generally in Haskell, I believe that the only role of
-- parentheses is to specify which blocks of code should
-- be treated as a single argument for a function to be
-- applied to. In that sense they affect evaluation order,
-- but evaluation still always (always!) proceeds from
-- the outermost position to the innermost, as in the
-- lambda calculus.
--
-- Note that the types `a` and `[b]` could be the same.
-- That is, the type variable `a` may be `[Integer]` and
-- the type variable `b` may be `Integer`.
--
-- Seven things in Haskell can be named: modules, type classes,
-- type variables, type constructors, data constructors, 
-- variables, and functions.
--
-- foldr (+) 0 [1,2,3]
-- (+) 1 (foldr (+) 0 [2,3]
-- (+) 1 ((+) 2 (foldr (+) 0 [3]))
-- (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [])))
-- (+) 1 ((+) 2 ((+) 3 (0)))
-- 1 + (2 + (3 + 0))
--
--
-- foldl (+) 0 [1,2,3]
-- foldl (+) ((+) 0 1) [2, 3]
-- foldl (+) ((+) ((+) 0 1) 2) [3]
-- foldl (+) ((+) ((+) ((+) 0 1) 2) 3) []
-- ((+) ((+) ((+) 0 1) 2) 3)
-- (((0 + 1) + 2) + 3)
--
-- const (const (const 0 1) 2) 3 -- if we were using const
-- in place of (+) above; must evaluate all three to reach 0.
-- Note that, like with lambda calculus, the evaluation
-- proceeded from the outside in. If we had been dealing with
-- (flip const) instead of (const), we would have discarded
-- the first argument to the outermost (flip const) -- that is,
-- discarded the other nested (flip const) calls -- and just
-- returned the outer (flip const)'s second argument, 3.
--
-- In the case of foldl, acc (accumulator) is well-named;
-- unlike the initial or "zero" value in foldr that is returned
-- into a context waiting for it, acc in foldl is actually the
-- full expression to be returned by the fold. This must have
-- to do with why the book says that foldl is a case of 
-- tail recursion.
--
--
-- Out of curiosity, here's how foldl would look if it
-- had the order of the arguments it passes to f in its
-- accumulator swapped:
--
-- foldRe (+) 0 [1,2,3]
-- foldRe (+) ((+) 1 0) [2,3]
-- foldRe (+) ((+) 2 ((+) 1 0)) [3]
-- foldRe (+) ((+) 3 ((+) 2 ((+) 1 0))) []
-- (+) 3 ((+) 2 ((+) 1 0))
-- 3 + (2 + (1 + 0))
--
-- That resulted in a genuine right fold, but with a
-- different order of arugments from foldr. Note that I
-- had to change the type signature to match foldr's.
--
-- My would-be foldl implementation from foldlWrong:
--
-- foldlWrong (+) 0 [1,2,3]
-- (+) (foldlWrong (+) 0 [2,3]) 1
-- (+) ((+) (foldlWrong (+) 0 [3]) 2) 1
-- (+) ((+) ((+) (foldlWrong (+) 0 []) 3) 2) 1
-- (+) ((+) ((+) (0) 3) 2) 1
-- ((0 + 3) + 2) + 1
--
-- This is still a left fold, but with the arguments
-- placed in a different order.
--
-- As can be seen above, you can make a left fold into
-- some kind of right fold, and vice versa, by reversing
-- the order of the arguments that the function argument f 
-- is applied to. I guess this isn't too much of a surprise,
-- given that the argument order that f expects is the one
-- difference in foldr and foldl's type signatures.


-- As an exercise from the book, writing out the evaluation
-- steps for the following:
--
-- foldl (flip (*)) 1 [1, 2, 3]
-- foldl (flip (*)) ((flip (*)) 1 1) [2, 3]
-- foldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3]
-- foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) []
-- ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3)
-- ((flip (*)) ((flip (*)) (1 * 1) 2) 3)
-- ((flip (*)) ((flip (*)) (1) 2) 3)
-- ((flip (*)) (2 * 1) 3)
-- ((flip (*)) (2) 3)
-- (3 * 2)
-- 6
--
-- foldl, more broadly with an 'f' function applied:
--
-- foldl f 0 [1,2,3]
-- foldl f (f 0 1) [2, 3]
-- foldl f (fl (f 0 1) 2) [3]
-- foldl f (f (f (f 0 1) 2) 3) []
-- f (f (f 0 1) 2) 3
--
-- Chapter example, evaluated:
-- f (f (f "" "Pizza") "Apple") "Banana"
-- f (f ("Piz") "Apple") "Banana"
-- f ("AppPiz") "Banana"
-- "BanAppPiz"
