module Reverse where

rvrs :: String -> String
rvrs str = drop 9 str ++ drop 5 (take 9 str) ++ take 5 str

str :: String
str = "Curry is awesome"

main :: IO ()
main = print $ rvrs str
