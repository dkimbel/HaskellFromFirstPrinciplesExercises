{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IPAddresses where

import Control.Applicative
import Control.Monad (join)
import Data.Char (toLower)
import Data.Word
import Test.Hspec
import Text.Trifecta

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  n0 <- parseDecimalChunk
  _ <- char '.'
  n1 <- parseDecimalChunk
  _ <- char '.'
  n2 <- parseDecimalChunk
  _ <- char '.'
  n3 <- parseDecimalChunk
  return $ IPAddress $ fromIntegral $
    n0 * 256 ^ 3 + n1 * 256 ^ 2 +
    n2 * 256 ^ 1 + n3 * 256 ^ 0

parseDecimalChunk :: Parser Integer
parseDecimalChunk = do
  n <- integer
  case (n < 256) of
    True -> return n
    False -> fail "Each number must be below 256"

-- I'm at a loss on parsing IPv6 addresses. I'm going to move on to the next chapter.
-- NOTE: I think I've thought up how to solve it. I need an 
-- intermediate date type; for instance, `Either Compressor 
-- HexBlock`, so that I first use a `Parser [Either Compressor
-- HexBlock]`, then convert the Compressors into HexBlocks
-- (being easily able to confirm that there are zero or one
-- in my list), then do the full-on parse to an IPv6 address.

--parseIPAddress6 :: Parser String
--parseIPAddress6 = undefined
--parseIPAddress6 :: Parser IPAddress6
--parseIPAddress6 = do
--  input <- some anyChar
--  let numColons = length $ filter (\x -> x == ':') input
--      numBlocks = numColons + 1
--      missingBlocks = 8 - numBlocks
--  beforeDoubleColon <- testParse (manyTill anyChar 
--                       ((parseDoubleColon >> return ()) <|> eof)) input
--  return beforeDoubleColon
--  preParseIPAddress6a *> 
--  preParseIPAddress6b *>
--  parseExpandedIPAddress6

preParseIPAddress6a :: Parser String
preParseIPAddress6a = undefined
-- if this'll be the compressor parse, then first run some
-- sort of anyChar parse to get the whole string, take its
-- length, and then run another parser inside here that
-- does the actual double-colon finding and expanding

--preParseIPAddress6 = do
-- Fill in `0000` for ommitted sections
-- Also, throw a parse error if there's more than one ::

-- TODO: Add an initial preparsing step that expands any
-- compressor symbol to one or more instances of `0:`
-- TODO: simplify preParseStringBlock and parseHexDigits
--   however possible once compressor is in its own step

preParseIPAddress6b :: Parser String
preParseIPAddress6b = undefined

preParseStringBlock :: Parser String
preParseStringBlock =
  parseDoubleColon <|> parseHexDigits

parseHexDigits :: Parser String
parseHexDigits = do
  hexes <- manyTill hexDigit parseHexBlockEnd
  case (length hexes > 4 || length hexes < 1) of
    True -> fail "expected one to four hex digits"
    False -> let neededZeroes = 4 - length hexes
                 paddedHexes = replicate neededZeroes '0' 
                               ++ hexes
             in return $ toLower <$> paddedHexes

parseHexBlockEnd :: Parser ()
parseHexBlockEnd = (colon >> return ()) <|> eof
  
parseCompressor :: Parser String
parseCompressor = undefined
--parseCompressor = do
--  colons <- parseCompressorSymbol
  -- need lenth of the ENTIRE string, not just following,
  -- which feels like a real problem

parseDoubleColon :: Parser String
parseDoubleColon = count 2 colon

parseExpandedIPAddress6 :: Parser IPAddress6
parseExpandedIPAddress6 = undefined
-- Calls parseExpandedHextet many a time

parseExpandedHextet :: Parser Integer
parseExpandedHextet = undefined
-- Extra note: can expect four digits always thanks to preparse
-- Can probably just use `hexadecimal`

-- Taken again from iniParsing/iniParsing.hs
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  let ip4a = "172.16.254.1"
      ip4b = "204.120.0.15"
      ip4c = "256.120.0.15"
      ip4d = "204.120.0"
      ip6a = "0:0:0:0:0:ffff:ac10:fe01"
      ip6b = "0:0:0:0:0:ffff:cc78:f"
      ip6c = "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
      ip6d = "2001:DB8::8:800:200C:417A"
      ip6e = "FEB0::0202:B3FF:FE1E:8329"
      ip6f = "2001:DB8::8:800:200C:417A"

  describe "IPv4 address parsing" $
    it "Parses a standard IPv4 address" $ do
      let parsed = parseString parseIPAddress mempty ip4a
          res = maybeSuccess parsed
      res `shouldBe` Just (IPAddress 2886794753)

  describe "IPv4 address parsing, part 2" $
    it "Parses another standard IPv4 address" $ do
      let parsed = parseString parseIPAddress mempty ip4b
          res = maybeSuccess parsed
      res `shouldBe` Just (IPAddress 3430416399)

  describe "IPv4 address parsing, part 3" $
    it "Fails on an IP address with an excessive number" $ do
      let parsed = parseString parseIPAddress mempty ip4c
          res = maybeSuccess parsed
      res `shouldBe` Nothing

  describe "IPv4 address parsing, part 4" $
    it "Fails on an IP address that is too short" $ do
      let parsed = parseString parseIPAddress mempty ip4d
          res = maybeSuccess parsed
      res `shouldBe` Nothing

--  describe "IPv6 address parsing, part 1" $
--    it "Parses a standard IPv6 address" $ do
--
--  TODO: Add a test case that ends with `::`
--  TODO: Add a test case that starts with `::`
--  TODO: Add a test case that breaks if there are two `::`
--  TODO: Add a test case that breaks in there's any `:::`
