{-# LANGUAGE OverloadedStrings #-}

module ParsingFractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction :: String
badFraction = "1/0"

alsoBad :: String
alsoBad = "10"

shouldWork :: String
shouldWork = "1/2"

shouldAlsoWork :: String
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  -- to suppress a compiler warning, I'm changing the book's
  -- `char '/'` to `_ <- char '/'`
  _ <- char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

-- I could absolutely just use the provided method `double`,
-- but this is as an exercise
parseDouble :: Parser Double
parseDouble = do
  before <- decimal
  _ <- char '.'
  after <- decimal
  return (read (show before ++ "." ++ show after) :: Double)

parseIntegerEof :: Parser Integer
parseIntegerEof = do
  n <- integer
  _ <- eof
  return n

-- for my own edification, I'm desugaring my parseIntegerEof
parseIntegerEof' :: Parser Integer
parseIntegerEof' =
  integer >>= \n -> eof >> return n

type FractionOrDecimal = Either Rational Double

-- This didn't work without `try` because as soon as the parser
-- first saw a digit, it assumed it was looking at a fraction
-- and tried to parse it that way. I needed the parser to not
-- decide that it was looking at a fraction or decimal until it
-- found a `/` or `.` character -- or, at the very least, to
-- be willing to try its alternative possibilities once it
-- failed after assuming it was looking at a fraction.
parseFracDec :: Parser FractionOrDecimal
parseFracDec =
      (Left <$> try virtuousFraction) 
  <|> (Right <$> try parseDouble)
  -- again, I totally could have used `double` instead of
  -- my own `parseDouble`, but I wanted to write it, even if
  -- it was largely copied from parseFraction

main :: IO ()
main = do
--  let parseFraction' =
--        parseString parseFraction mempty
  let p f i= parseString f mempty i
  print $ p parseFracDec "1/2"
  print $ p parseFracDec "1.2"
--  let parseVirtuous = parseString virtuousFraction mempty
--  print $ parseVirtuous badFraction
--  print $ parseVirtuous shouldWork
--  print $ parseFraction' shouldWork
--  print $ parseFraction' shouldAlsoWork
--  print $ parseFraction' alsoBad
--  print $ parseFraction' badFraction
