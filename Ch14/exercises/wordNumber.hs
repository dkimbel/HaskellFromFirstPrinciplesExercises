module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "INVALID"

digits :: Int -> [Int]
digits n
    | n < 10    = [n]
    | otherwise = digits (div n 10) ++ [(mod n 10)]

wordNumber :: Int -> String
wordNumber n = combine . wordify $ ns
  where ns      = digits n
        wordify = map digitToWord
        combine = concat . intersperse "-" 