module Cipher where

import Data.Char

alphaOffset :: Int
alphaOffset = 97

charsInAlpha :: Int
charsInAlpha = 26

caesar :: Int -> [Char] -> [Char]
caesar n xs = map (shiftAlphaChar n) $ xs

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

