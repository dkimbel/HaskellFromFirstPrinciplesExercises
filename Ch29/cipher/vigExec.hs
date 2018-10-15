module Main where

import Cipher (vigenere)
import Control.Applicative ((<|>))
import Control.Monad (forever, when)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (hGetLine, hPutStr, stderr, stdin, stdout)
import Text.Trifecta

data Mode = 
    Encrypt
  | Decrypt
  deriving (Eq, Show)

type Key = String

parseArgs :: Parser (Mode, Key)
parseArgs = do
  resList <- some parseKeyOrMode
  let key = foldr fKey "" resList
      mode = foldr fMode Encrypt resList
      -- I believe that fKey and fMode will ultimately
      -- return the first match, or their default value.
      -- This isn't ideal -- I'd rather error out in the
      -- case where no value is present or multiple values
      -- are present -- but it'll do.
      fKey item rest = case item of
                         Left k -> k
                         Right _ -> rest
      fMode item rest = case item of 
                          Right m -> m
                          Left _ -> rest
  return (mode, key)

parseKeyOrMode :: Parser (Either Key Mode)
parseKeyOrMode = (Left <$> try parseKey) <|> 
                 (Right <$> parseMode)

parseKey :: Parser Key
parseKey = do
  _ <- char '-'
  _ <- char 'k'
  _ <- char '='
  s <- some letter
  return s

parseMode :: Parser Mode
-- Note: this `try .. <|> ..` pattern is considered
-- harmful, so it would be optimal to use something
-- else instead
parseMode = try parseEncrypt <|> parseDecrypt

parseEncrypt :: Parser Mode
parseEncrypt = do
  _ <- char '-'
  _ <- char 'e'
  return Encrypt

parseDecrypt :: Parser Mode
parseDecrypt = do
  _ <- char '-'
  _ <- char 'd'
  return Decrypt

getInputThenEncrypt :: Key -> IO ()
getInputThenEncrypt key = do
  -- I cheated a little here by using hGetLine instead of
  -- repeatedly using hGetChar the way the book suggested;
  -- my implementation might not work for piping in a file
  str <- hGetLine stdin
  let encrypted = vigenere key str
  hPutStr stdout encrypted

-- I took this from Ch24/iniParsing/iniParsing.hs
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  let joinedArgs = concat args
      res = parseString parseArgs mempty joinedArgs
  case (maybeSuccess res) of
    Just (mode, key) -> 
      case mode of
        Encrypt -> getInputThenEncrypt key
        Decrypt -> hPutStr stderr 
                   "Decryption not yet implemented"
    Nothing -> hPutStr stderr 
               "Could not parse command line args"
