module Exercises where

import Data.Char (toUpper, isAlpha)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = toUpper c : cs

capitalizeParagraph :: String -> String
capitalizeParagraph s = go s True
    -- Bool arg indicates whether to capitalize next letter.
    -- Note that it'd make more sense to call isUpper rather than
    -- using capitalizeWord, but reusing capitalizeWord was a requirement.
    where go :: String -> Bool -> String
          go "" _ = ""
          go allC@(c:cs) True
              | isAlpha c = go (capitalizeWord allC) False
              | otherwise = c : go cs True
          go (c:cs) False
              | c == '.' = c : go cs True
              | otherwise = c : go cs False
