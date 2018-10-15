module StopsVowelsExercise where

import Control.Applicative (liftA3)

combineThreeLift :: [a] -> [b] -> [c] -> [(a, b, c)]
combineThreeLift as bs cs = liftA3 (,,) as bs cs

allSvsLifted :: [(Char, Char, Char)]
allSvsLifted = combineThreeLift stops vowels stops

-- the code below is from a similar exercise from 
-- Chapter 10 that the book referened
stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

combineThree :: [a] -> [b] -> [(a, b, a)]
combineThree xs ys = [(x1, y, x2) | x1 <- xs, y <- ys, x2 <- xs]

allSvs :: [(Char, Char, Char)]
allSvs = combineThree stops vowels

main :: IO ()
main = do
  putStr "new method works? "
  print $ allSvsLifted == allSvs 
