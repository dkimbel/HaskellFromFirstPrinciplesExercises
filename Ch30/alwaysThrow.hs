module Main where

import Control.Exception

main :: IO ()
main = do
  throwIO DivideByZero
  putStrLn "lol"
