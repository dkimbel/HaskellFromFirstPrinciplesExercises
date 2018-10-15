module Factorial where

fact :: (Eq a, Num a) => a -> a
fact 0 = 1
fact x = x * fact (x - 1)

