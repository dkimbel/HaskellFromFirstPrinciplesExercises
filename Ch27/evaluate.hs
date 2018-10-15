ex1 :: Int
ex1 = const 1 undefined
-- const 1 undefined
-- (\a -> \b -> a) 1 undefined
-- (\b -> 1) undefined
-- 1

ex2 :: Int
ex2 = const undefined 1
-- const undefined 1
-- (\a -> \b -> a) undefined 1
-- (\b -> undefined) 1
-- undefined

ex3 :: Int
ex3 = flip const undefined 1
-- flip const undefined 1
-- (\f -> \x -> \y -> f y x) (\a -> \b -> a) undefined 1
-- (\x -> \y -> (\a -> \b -> a) y x) undefined 1
-- (\y -> (\a -> \b -> a) y undefined) 1
-- (\a -> \b -> a) 1 undefined
-- (\b -> 1) undefined
-- 1

ex4 :: Int
ex4 = flip const 1 undefined
-- flip const 1 undefined
-- (\f -> \x -> \y -> f y x) (\a -> \b -> a) 1 undefined
-- (\x -> \y -> (\a -> \b -> a) y x) 1 undefined
-- (\y -> (\a -> \b -> a) y 1) undefined
-- (\a -> \b -> a) undefined 1
-- (\b -> undefined) 1
-- undefined

ex5 :: Int
ex5 = const undefined undefined
-- const undefined undefined
-- (\a -> \b -> a) undefined undefined
-- (\b -> undefined) undefined
-- undefined

ex6 :: Char
ex6 = foldr const 'z' ['a'..'e']
-- foldr const 'z' "abcde"
-- const 'a' (foldr const 'z' "bcde")
-- 'a'

ex7 :: Char
ex7 = foldr (flip const) 'z' ['a'..'e']
-- foldr (flip const) 'z' "abcde"
-- (flip const) 'a' (foldr (flip const) 'z' "bcde")
-- (flip const) 'b' (foldr (flip const) 'z' "cde")
-- (flip const) 'c' (foldr (flip const) 'z' "de")
-- (flip const) 'd' (foldr (flip const) 'z' "e")
-- (flip const) 'e' (foldr (flip const) 'z' "")
-- 'z'
