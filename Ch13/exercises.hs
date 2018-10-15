module Exercises where

import Control.Monad (forever)
import Data.Char (isAlpha, toLower)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  putStr "Enter a word: "
  line1 <- getLine
  let parsedLine = parseLine line1
  case (parsedLine == reverse parsedLine) of
    True -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess

parseLine :: String -> String
parseLine s = map toLower alphaChars
  where alphaChars = filter isAlpha s

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter a name: "
  name <- getLine
  putStr "Enter an age: "
  ageStr <- getLine
  let age = read ageStr :: Integer
  case mkPerson name age of
    Right p -> putStrLn $ "Yay! Successfully got a person: "
                        ++ show p
    Left pi -> putStrLn $ "Got an error: " ++ show pi
