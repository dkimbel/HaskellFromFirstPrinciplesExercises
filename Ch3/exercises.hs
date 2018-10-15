module Chapter3Exercises where

exclaim :: String -> String
exclaim str = str ++ "!"

ind4 :: String -> Char
ind4 str = str !! 4

dropNine :: String -> String
dropNine str = drop 9 str

thirdLetter :: String -> Char
thirdLetter str = str !! 2

letterIndex :: Int -> Char
letterIndex n = str !! n
  where str = "Curry is awesome!"

letterIndex' :: Int -> Char
letterIndex' n = let str = "Curry is awesome!"
                 in str !! n

rvrs :: String -> String
rvrs str = drop 9 str ++ drop 5 (take 9 str) ++ take 5 str
