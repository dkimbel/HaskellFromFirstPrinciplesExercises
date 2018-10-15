module Filtering where

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = []
myFilter f (x:xs) = if f x
                    then x : myFilter f xs
                    else myFilter f xs

removeArticles :: [Char] -> [[Char]]
removeArticles [] = []
removeArticles xs = filter notArticle . words $ xs
  where notArticle x = notElem x articles
        articles     = ["a", "an", "the"]
