module DividedBy where

dividedBy :: Integer -> Integer -> Integer
dividedBy x y
    | x < y     = 0
    | otherwise = (dividedBy (x - y) y) + 1

-- with remainder
dividedBy' :: Integer -> Integer -> (Integer, Integer)
dividedBy' x y = divRemRecurse x y 0

divRemRecurse :: Integer -> Integer -> Integer
                 -> (Integer, Integer)
divRemRecurse x y acc
    | x < y     = (acc, x)
    | otherwise = divRemRecurse (x - y) y (acc + 1)

-- the book's implementation
dividedBy'' :: Integral a => a -> a -> (a, a)
dividedBy'' num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

data DividedResult = Result Integer | DividedByZero
    deriving (Eq, Show)

divBy :: Integer -> Integer -> DividedResult
divBy _ 0 = DividedByZero
divBy 0 _ = Result 0
divBy x y = go x y 0
  where go n d acc
         | isBase         = Result acc
         | n < 0 && d < 0 = go (n - d) d (acc + 1)
         | n < 0 && d > 0 = go (n + d) d (acc - 1)
         | n > 0 && d < 0 = go (n + d) d (acc - 1)
         | n > 0 && d > 0 = go (n - d) d (acc + 1)
         where isBase = abs n < abs d && (n * d >= 0)
         -- the last part, checking that n * d >= 0,
         -- allows us to imitate div's behavior in
         -- cases like `div 9 (-2)` and `div (-9) 2`

-- dividedBy 15 2
-- (dividedBy 13 2) + 1
-- (dividedBy 11 2) + 1 + 1
-- (dividedBy 9 2) + 1 + 1 + 1
-- (dividedBy 7 2) + 1 + 1 + 1 + 1
-- (dividedBy 5 2) + 1 + 1 + 1 + 1 + 1
-- (dividedBy 3 2) + 1 + 1 + 1 + 1 + 1 + 1
-- (dividedBy 1 2) + 1 + 1 + 1 + 1 + 1 + 1 + 1
-- 0 + 1 + 1 + 1 + 1 + 1 + 1 + 1
-- 7
