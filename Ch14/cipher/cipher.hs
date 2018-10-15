module Cipher where

import Data.Char
import Test.Hspec
import Test.QuickCheck

alphaOffset :: Int
alphaOffset = 97

charsInAlpha :: Int
charsInAlpha = 26

caesar :: Int -> [Char] -> [Char]
caesar n xs = map (shiftAlphaChar n) $ xs

caesarIo :: IO String
caesarIo = do
  putStr "Enter a message to encipher: "
  msg <- getLine
  putStr "Enter an integer offset: "
  iStr <- getLine
  let i = (read iStr) :: Int
  return $ caesar i msg

unCaesar :: Int -> [Char] -> [Char]
unCaesar n xs = caesar (-n) xs

shiftAlphaChar :: Int -> Char -> Char
shiftAlphaChar n c
    | isAlpha c && isUpper c = toUpper $ shift n c
    | isAlpha c = shift n c
    | otherwise = c
    where shift n c = intToChar $ mod (charToInt c + n) charsInAlpha

charToInt :: Char -> Int
charToInt c = ord (toLower c) - alphaOffset

intToChar :: Int -> Char
intToChar n = chr $ n + alphaOffset

-- Everything above here is from my Ch9 exercise; everything
-- below is newly for the Ch11 exercise. The one exception
-- is that I tweaked charToInt, making it do the toLower itself.

type Keyword = [Char]

-- This would have been a really great and elegant implementation,
-- but it doesn't quite work correctly because it keeps cycling
-- through offsets on nonalphanumeric characters, and it isn't
-- supposed to behave that way.
--vigenere :: Keyword -> [Char] -> [Char]
--vigenere key s = map (uncurry shiftAlphaChar) offsetsAndChars
--    where offsets = map charToInt key
--          offsetsAndChars = zip (cycle offsets) s

-- I think what I could really use here is a so-called
-- coroutine to pass me the next offset number on demand,
-- so I can just not ask for the next offset when I encounter
-- non-alphanumberic characters. I haven't learned about
-- coroutines yet, though, so I'll keep track of that would-be
-- state by passing a list of offsets along from call to call.
vigenere :: Keyword -> [Char] -> [Char]
vigenere key s = go s offsets
    where offsets = cycle (map charToInt key)
          go "" _ = ""
          go (s:ss) allOffs@(o:os)
              | isAlpha s = shiftAlphaChar o s : go ss os
              | otherwise = s : go ss allOffs

vigenereIo :: IO String
vigenereIo = do
  putStr "Enter a message to encipher: "
  msg <- getLine
  putStr "Enter a keyword: "
  key <- getLine
  return $ vigenere key msg

normalCharGen :: Gen Char
normalCharGen = elements ([' ', '.', ',', '!'] ++  ['A'..'z'])

-- This does return a string, but always with just one character;
-- I'd ideally like to be able to make multi-character strings here
normalStrGen :: Gen String
normalStrGen = sequence [normalCharGen]

prop_caesarLossless :: Property
prop_caesarLossless =
  forAll normalStrGen $
    \s i -> unCaesar (i :: Int) (caesar i s) == s

runQc :: IO ()
runQc = do
  quickCheck prop_caesarLossless

runTests :: IO ()
runTests = hspec $ do
  describe "Caesar Cipher" $ do
    it "Offsets only alphanumeric characters" $ do
      let before = "Meet at dawn"
      let after  = "Nffu bu ebxo"
      caesar 1 before `shouldBe` after

  describe "Vigenere Cipher" $ do
    it "Offsets only alphanumeric characters" $ do
      let before = "MEET AT DAWN"
      let after  = "MPPR AE OYWY"
      vigenere "ALLY" before `shouldBe` after
