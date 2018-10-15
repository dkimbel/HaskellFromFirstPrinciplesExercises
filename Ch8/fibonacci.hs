module Fibonacci where

fib :: (Integral a, Num b) => a -> b
--fib 0 = 1
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- fib 5
-- fib 4 + fib 3
-- fib 3 + fib 2 + fib 2 + fib 1
-- fib 2 + fib 1 + fib 1 + fib 0 + fib 1 + fib 0 + fib 1
-- fib 2 + 1 + 1 + 0 + 1 + 0
-- fib 1 + fib 0 + 3
-- 4
