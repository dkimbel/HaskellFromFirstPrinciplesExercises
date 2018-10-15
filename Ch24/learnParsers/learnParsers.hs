module LearnParsers where

import Text.Trifecta
--import Control.Monad.Trans.State

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char
one = char '1'

one' :: Parser a
one' = one >> stop

--one'' :: Parser ()
--one'' = one >> eof

oneThruThree :: Parser String
oneThruThree = string "123"

-- This is a successful solution to "Parsing Practice"
-- exercise #3. I have a partial solution to #2 via my
-- `oneThruThree` function above. However, I'm moving on;
-- I don't have the sklils to fully complete these exercises
-- just yet.
oneThruThree' :: Parser String
oneThruThree' = mapM char ['1', '2', '3']
--oneThruThree' = sequence [char '1', char '2', char '3']
  
oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

stringParse :: Parser String -> String -> IO ()
stringParse p str =
  print $ parseString p mempty str

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
