-- Entirely from the book; the use of the state monad
-- is purposefully complex and unnecessary. The commented-out
-- line in `main`, along with `fizzBuzz`, is all that's 
-- actually needed.

module FizzBuzz where

import Control.Monad.Trans.State
--import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

--fizzbuzzList :: [Integer] -> DL.DList String
--fizzbuzzList list = 
--  execState (mapM_ addResult list) DL.empty
fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

-- strangely, I had to add unit at the end of this type
-- signature to get it to compile; that's the first apparent
-- error I've found in the book's code
--addResult :: Integer -> State (DL.DList String) ()
addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
  -- "snoc" is probably "cons" backwards, since it appends
  -- to the end and is basically the opposite of cons
--  put (DL.snoc xs result)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo i j = fizzbuzzList [j, (j-1) .. i]

main :: IO ()
main =
--  mapM_ putStrLn $ fizzbuzzList [1..100]
  mapM_ putStrLn $ fizzbuzzFromTo 1 100
--  mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
--  mapM_ (putStrLn . fizzBuzz) [1..100]
