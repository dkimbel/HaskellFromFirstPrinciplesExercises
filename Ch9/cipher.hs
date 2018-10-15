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
    | isAlpha c && isUpper c = toUpper $ shift n (toLower c)
    | isAlpha c = shift n c
    | otherwise = c
    where shift n c = intToChar $ mod (charToInt c + n) charsInAlpha

charToInt :: Char -> Int
charToInt c = ord c - alphaOffset

intToChar :: Int -> Char
intToChar n = chr $ n + alphaOffset
