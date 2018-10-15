module ThyFearfulSymmetry where

myWords :: [Char] -> [[Char]]
myWords ""  = []
myWords str = [word] ++ myWords rest
  where word = takeWhile (/= ' ') str
        rest = dropWhile (== ' ') . dropWhile (/= ' ') $ str
