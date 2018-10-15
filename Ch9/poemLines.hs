module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines ""  = []
myLines str = [line] ++ myLines rest
  where line = takeWhile (/= '\n') str
        rest = dropWhile (== '\n') . dropWhile (/= '\n') $ str

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = do
  putStrLn $ "Are they equal? "
           ++ show (myLines sentences == shouldEqual)
  putStrLn $ "Even after the refactor? "
           ++ show (myLines' sentences == shouldEqual)

splitOn :: Char -> String -> [String]
splitOn _    ""  = []
splitOn char str = [item] ++ splitOn char rest
  where item = takeWhile (/= char) str
        rest = dropWhile (== char) . dropWhile (/= char) $ str

myLines' :: String -> [String]
myLines' = splitOn '\n'

myWords' :: String -> [String]
myWords' = splitOn ' '
