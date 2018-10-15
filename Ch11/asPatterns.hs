module AsPatterns where

import Data.Char (toUpper)

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf [x] (y:ys)
    | x == y = True
    | otherwise = isSubsequenceOf [x] ys
isSubsequenceOf allX@(x:xs) (y:ys)
    | x == y = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf allX ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = go (words s)
    where go [] = []
          go (w:ws) = makeTuple w : go ws
          makeTuple word@(c:cs) = (word, toUpper c : cs)
