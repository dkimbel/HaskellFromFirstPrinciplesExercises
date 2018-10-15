module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

numIncorrectGuessesAllowed :: Int
numIncorrectGuessesAllowed = 4

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList 
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in  l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, maxIndex)
  return $ wl !! randomIndex
  where maxIndex = length wl - 1

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing  = '_'

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word concealed ""
  where concealed = fmap toNothing word
        toNothing c = Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) c = elem c guesses
 
--fillInCharacter :: Puzzle -> Char -> Puzzle
--fillInCharacter (Puzzle word discov guesses) c =
--  Puzzle word newDiscov (c : guesses)
--  where newDiscov = go word discov c
--        go [] _ c = []
--        go _ [] c = []
--        go (w:ws) allDs@(d:ds) c = case w == c of
--            True  -> (Just c) : go ws ds c
--            False -> d : go ws ds c

-- This is the book's implementation below; mine is commented
-- out above. I forgot to prepend c to guesses in the 
-- return value originally, because I didn't think correct
-- guesses would be added to that list.

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
     , alreadyGuessed puzzle guess) of
   (_, True) -> do
     putStrLn "You already guessed that\
               \ character, pick something else!"
     return puzzle
   (True, _) -> do
     putStrLn "This character was in the word,\
               \ filling in the word accordingly"
     return (fillInCharacter puzzle guess)
   (False, _) -> do
     putStrLn "This character wasn't in\
               \ the word, try again."
     return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if numIncorrect > numIncorrectGuessesAllowed then 
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()
  where numIncorrect =
          length $ filter (\c -> notElem c wordToGuess) guessed

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
