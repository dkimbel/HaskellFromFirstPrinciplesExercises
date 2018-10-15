module Exercises where

import Data.Char (isAlpha, toLower)

notThe :: String -> Maybe String
notThe word
    | word /= "the" = Just word
    | otherwise     = Nothing

replaceThe :: String -> String
replaceThe s = go s ""
    where go :: String -> String -> String
          go "" currWord = getFinalWord currWord
          go (c:cs) currWord
              | isAlpha c = go cs (currWord ++ [c])
              | otherwise = getFinalWord currWord ++ [c] ++ go cs ""
          getFinalWord :: String -> String
          getFinalWord currWord = case notThe currWord of
                                      Just word -> word
                                      Nothing   -> "a"

type IsCheckingForVowel = Bool
type CurrWord = String

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = go s "" 0 False
    where vowels = "aeiou"
          go :: String -> CurrWord -> Int -> IsCheckingForVowel -> Int
          go "" _ n _ = n
          go (c:cs) currWord n b
              | b && elem c vowels = go cs [c] (n + 1) False
              | b && isAlpha c     = go cs [c] n False
              | isAlpha c          = go cs (currWord ++ [c]) n False
              | currWord == ""     = go cs "" n b
              | otherwise          = case notThe currWord of
                                         Just _  -> go cs "" n False
                                         Nothing -> go cs "" n True

-- the book insisted on Integer in the type signature, hence the call
-- to fromIntegral to make the Int return val of `length` into a Num
-- that can then be made more specific to an Integer
countVowels :: String -> Integer
countVowels s = fromIntegral . length . filter isVowel $ s
    where isVowel c = elem (toLower c) "aeiou"

-- assuming that the input is truly a word -- that is, any character
-- that isn't a vowel is necessarily a consonant
newtype Word' = 
    Word' String
    deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord ""   = Nothing
mkWord word = case moreVowels word of
    True  -> Nothing
    False -> Just (Word' word)
    where moreVowels :: String -> Bool
          moreVowels s = countVowels s > div (fromIntegral $ length s) 2

data Nat =
    Zero 
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0  = Nothing
    | otherwise = Just (go i)
    where go :: Integer -> Nat
          go 0 = Zero
          go i = Succ (go (i - 1))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a maybe = mayybee a id maybe

-- this is the book's name, but it's more like listToMaybeHead
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes ms = map extractVal $ filter isJust ms
    where extractVal (Just v) = v

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = go ms []
    where go :: [Maybe a] -> [a] -> Maybe [a]
          go [] acc = Just acc
          go (m:ms) acc = case m of
              Just v  -> go ms (acc ++ [v])
              Nothing -> Nothing

lefts' :: [Either a b] -> [a]
lefts' = foldr lefts []
    where lefts :: Either a b -> [a] -> [a]
          lefts (Left v) acc = v : acc
          lefts _ acc        = acc

rights' :: [Either a b] -> [b]
rights' = foldr rights []
    where rights :: Either a b -> [b] -> [b]
          rights (Right v) acc = v : acc
          rights _ acc         = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr splitEither ([], [])
    where splitEither :: Either a b 
                      -> ([a], [b])
                      -> ([a], [b])
          splitEither (Left v) (ls, rs)  = (v : ls, rs)
          splitEither (Right v) (ls, rs) = (ls, v : rs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (\_ -> Nothing) (\b -> Just (f b)) e
