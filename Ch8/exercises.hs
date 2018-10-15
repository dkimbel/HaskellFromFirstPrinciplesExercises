module Exercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sumTo :: (Eq a, Num a) => a -> a
sumTo 1 = 1
sumTo n = (sumTo (n - 1)) + n

multRecur :: Integral a => a -> a -> a
multRecur _ 0 = 0
multRecur x y = (multRecur x (y - 1)) + x

mc91 :: Integral a => a -> a
mc91 n
    | n > 110  = mc91 (n - 10)
    | n > 100  = n - 10
    | n <= 100 = 91

-- NOTES
--
-- Recursive functions can be thought of as indeterminate, data-driven (data ultimately tells them when to stop), self-referential, and composed (but unlike normal function composition, composed of calls to themselves).
--
-- Function composition is applying a function to the result of applying another function to a value.
