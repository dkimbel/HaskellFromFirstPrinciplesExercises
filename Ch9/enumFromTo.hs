module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

eft :: (Enum a, Ord a) => a -> a -> [a]
eft f t
    | f > t     = []
    | f == t    = [f]
    | otherwise = f : (eft (succ f) t)
