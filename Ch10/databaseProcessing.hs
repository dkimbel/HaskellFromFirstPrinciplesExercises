module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (x:xs) =
    case x of
      DbDate time -> time : filterDbDate xs
      _ -> filterDbDate xs

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' xs = foldr buildTimes [] xs
    where buildTimes (DbDate time) rest = time : rest
          buildTimes _ rest = rest

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (x:xs) =
    case x of
      DbNumber int -> int : filterDbNumber xs
      _ -> filterDbNumber xs

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' [] = []
filterDbNumber' (DbNumber int : xs) = int : filterDbNumber' xs
filterDbNumber' (_            : xs) = filterDbNumber' xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = maximum $ filterDbDate xs

sumDb :: [DatabaseItem] -> Integer
sumDb xs = sum $ filterDbNumber xs

sumDb' :: [DatabaseItem] -> Integer
sumDb' xs = foldr (+) 0 $ filterDbNumber xs

avgDb :: [DatabaseItem] -> Double
avgDb xs = total / numItems
    where total    = fromIntegral $ sumDb xs
          numItems = fromIntegral $ length $ filterDbNumber xs
