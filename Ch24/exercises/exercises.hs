{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Exercises where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import Test.Hspec
import qualified Test.QuickCheck as QC
import Text.RawString.QQ
import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parseCorVer
  rel <- option [] parseRelease
  metadata <- option [] parseMetadata
  return $ SemVer major minor patch rel metadata

parseCorVer :: Parser (Major, Minor, Patch)
parseCorVer = do
  major <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  return (major, minor, patch)

parseRelease :: Parser Release
parseRelease = char '-' >> parseNumOrStrList

parseMetadata :: Parser Metadata
parseMetadata = char '+' >> parseNumOrStrList

parseNumOrStr :: Parser NumberOrString
parseNumOrStr = (NOSI <$> decimal) 
            <|> (NOSS <$> some letter)

parseNumOrStrList :: Parser [NumberOrString]
parseNumOrStrList = some $ parseNumOrStr <* 
                           (skipOptional $ char '.')

instance Ord SemVer where
  SemVer mj0 mn0 pt0 rl0 _ `compare` SemVer mj1 mn1 pt1 rl1 _
      | mj0 > mj1 = GT
      | mj0 < mj1 = LT
      | mn0 > mn1 = GT
      | mn0 < mn1 = LT
      | pt0 > pt1 = GT
      | pt0 < pt1 = LT
      | null rl0 && not (null rl1) = GT
      | not (null rl0) && null rl1 = LT
      | rl0 > rl1 = GT
      | rl0 < rl1 = LT
      | otherwise = EQ

instance Ord NumberOrString where
  NOSS s0 `compare` NOSS s1 = s0 `compare` s1
  NOSI i0 `compare` NOSI i1 = i0 `compare` i1
  NOSS _ `compare` NOSI _ = GT
  NOSI _ `compare` NOSS _ = LT

-- I took this from iniParsing/iniParsing.hs
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

-- Parsing digits and integers without using `digit` or
-- `integer` (or, presumably, `decimal`)
parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9'] <?> "digit"

base10Integer :: Parser Integer
base10Integer = do
  digits <- some parseDigit <?> "integer"
  return $ read digits

-- This is much less elegant than my earlier solution,
-- but based on a hint provided by the book, it sounds
-- like they wanted us to try something like this.
base10Integer' :: Parser Integer
base10Integer' = do
  digits <- some parseDigit
  return (foldl accumulate 0 digits)

accumulate :: Integer -> Char -> Integer
accumulate i c = charToInteger c + (i * 10)

-- To differentiate this solution from my earlier, better
-- one, I didn't want to use `read`
charToInteger :: Char -> Integer
charToInteger '0' = 0
charToInteger '1' = 1
charToInteger '2' = 2
charToInteger '3' = 3
charToInteger '4' = 4
charToInteger '5' = 5
charToInteger '6' = 6
charToInteger '7' = 7
charToInteger '8' = 8
charToInteger '9' = 9
charToInteger _ = error "expected digit"

negativeBase10Integer :: Parser Integer
negativeBase10Integer = do
  _ <- char '-'
  i <- base10Integer
  return (negate i)

-- parses positive or negative numbers
base10Integer'' :: Parser Integer
base10Integer'' = base10Integer <|> negativeBase10Integer

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)

-- Note: after some thought, I think that the best way to write
-- a phone parser would probably be to ignore every non-digit
-- and then group digits parsing from right-to-left (so that
-- a variable number of country code digits could be handled).
-- That might not work for absolutely every country, but it'd
-- work pretty widely.
parsePhone :: Parser PhoneNumber
parsePhone = do
  _ <- skipOptional $ try $ parseCountryCode <?> "country code"
  area <- parseAreaCode
  exchange <- parseExchange
  lineNum <- parseLineNumber
  return $ PhoneNumber area exchange lineNum

-- only written to expect '1'; can't be allowed to eat up the
-- entire input if that input were e.g. `"11234567890"`
parseCountryCode :: Parser Int
parseCountryCode = do
  i <- char '1'
  _ <- char '-'
  return (read $ i : "")

stringToInt :: String -> Int
stringToInt s = read s

parseAreaCode :: Parser NumberingPlanArea
parseAreaCode = (parseOptionalParens $ 
                stringToInt <$> count 3 digit)
                -- a more elegant parser might only allow
                -- a hyphen to be skipped in the no-paren case
                <* skipOptional (oneOf  ['-', ' '])

parseOptionalParens :: Parser a -> Parser a
parseOptionalParens p = 
  skipOptional (char '(') *> p <* skipOptional (char ')')

parseExchange :: Parser Exchange
parseExchange = stringToInt <$> count 3 digit
                <* skipOptional (char '-')

parseLineNumber :: Parser LineNumber
parseLineNumber = stringToInt <$> count 4 digit

data Log = Log [LogDate]
  deriving Show

data LogDate = LogDate Day [Activity]
  deriving Show

data Activity = Activity {
    getName :: Name
  , getStartTime :: StartTime
} deriving Show

data ActivityWithEnd = ActivityWithEnd {
    getAeName :: Name
  , getAeStartTime :: StartTime
  , getAeEndTime :: EndTime
} deriving Show

data ActivityMinutes = ActivityMinutes {
    getAmName :: Name
  , getAmMinutes :: Minutes
} deriving Show

type Name = String
type StartTime = UTCTime
type EndTime = UTCTime
type Minutes = Int

skipComment :: Parser ()
skipComment =
  skipMany (char ' ') >>
  parseComment >>
  skipEOL

skipComments :: Parser ()
skipComments =
  skipEOL >>
  skipMany skipComment

parseComment :: Parser ()
parseComment =
  parseCommentSymbol >>
  skipMany (notChar '\n')

parseCommentSymbol :: Parser ()
parseCommentSymbol = 
  count 2 (char '-') >> 
  return ()

parseAnyWhitespaceThenComment :: Parser ()
parseAnyWhitespaceThenComment =
  skipMany (char ' ') >>
  parseComment

-- to keep things clean, I copied this function (sort of --
-- I made a couple tweaks) from iniParsing/iniParsing.hs
skipEOL :: Parser ()
skipEOL = skipMany (char '\n')

parseHeader :: Parser Day
parseHeader = do
  _ <- string "# "
  year <- decimal
  _ <- char '-'
  month <- decimal
  _ <- char '-'
  day <- decimal
  let parsedDate = fromGregorianValid year 
                                      (fromInteger month)
                                      (fromInteger day)
  case parsedDate of
    Just retVal -> return retVal
    Nothing -> fail "invalid date"

parseActivity :: Day -> Parser Activity
parseActivity day = do
  hours <- decimal
  _ <- char ':'
  minutes <- decimal
  _ <- char ' '
  name <- parseActivityName
  skipComments
  let seconds = 60 * minutes + 60 * 60 * hours
      date = UTCTime day (secondsToDiffTime seconds)
  return $ Activity name date

parseActivityName :: Parser Name
parseActivityName = manyTill (notChar '\n')
                             parseActivityNameEnd

parseActivityNameEnd :: Parser ()
parseActivityNameEnd = 
      (char '\n' >> return ())
  <|> try parseAnyWhitespaceThenComment

parseLogDate :: Parser LogDate
parseLogDate = do
  skipComments
  day <- parseHeader
  skipComments
  activities <- some $ parseActivity day
  return $ LogDate day activities

parseLog :: Parser Log
parseLog = Log <$> some parseLogDate

mkNumDaysAndActivities :: Num a => Log -> (a, [Activity])
mkNumDaysAndActivities (Log logDates) =
  foldr f (0, []) logDates
    where
      f (LogDate _ activities) (days, rest) = 
        (days + 1, activities ++ rest)

addEndDates :: [Activity] -> [ActivityWithEnd]
addEndDates [] = []
addEndDates (a:[]) =
  ActivityWithEnd (getName a) startTime endTime : []
    where 
      startTime = getStartTime a
      endTime = getUpcomingMidnight startTime
addEndDates (a0:a1:as) = 
  ActivityWithEnd (getName a0) 
                  (getStartTime a0)
                  (getStartTime a1)
  : addEndDates (a1:as)

addSleepStart :: LogDate -> LogDate
addSleepStart (LogDate date acts) =
  LogDate date acts

getUpcomingMidnight :: UTCTime -> UTCTime
getUpcomingMidnight start =
  UTCTime nextDay midnightDiff
    where 
      day = utctDay start
      nextDay = addDays 1 day
      midnightDiff = secondsToDiffTime 0
  
mkActivityMinutes :: [ActivityWithEnd] -> [ActivityMinutes]
mkActivityMinutes awes = toMinutes <$> awes
  where
    toMinutes (ActivityWithEnd nm st en) =
      ActivityMinutes nm mins
        where
          secs = realToFrac $ diffUTCTime en st
          mins = floor $ secs / 60
  
mkMinutesMap :: [ActivityMinutes] -> Map Name Minutes
mkMinutesMap ams = foldr f M.empty ams
  where
    f am rest = M.insertWith (+) 
                             (getAmName am)
                             (getAmMinutes am)
                             rest

logToMinutesMap :: Log -> Map Name Minutes
logToMinutesMap =
  mkMinutesMap .
  mkActivityMinutes .
  addEndDates .
  snd .
  mkNumDaysAndActivities

logStringToMaybeMinutesMap :: String -> Maybe (Map Name Minutes)
logStringToMaybeMinutesMap str =
  logToMinutesMap <$> maybeLog
    where
      maybeLog = maybeSuccess $ p str
      p s = parseString parseLog mempty s

logToMinutesPerDayMap :: Log -> Map Name Minutes
logToMinutesPerDayMap log = 
  divByDays <$> minutesMap
    where
      minutesMap = logToMinutesMap log
      divByDays = \x -> div x days
      days = fst $ mkNumDaysAndActivities log

-- NOTE: a quirk of my parsing system is that I assume that
-- whatever the most recent activity you were doing on the
-- most recent day continues until the next logged day; for
-- instance, in the example the book provided, I assume that
-- the person slept through all of 2/6/25.
logStringToMaybeMinutesPerDayMap :: String 
                                 -> Maybe (Map Name Minutes)
logStringToMaybeMinutesPerDayMap str =
  logToMinutesPerDayMap <$> maybeLog
    where
      maybeLog = maybeSuccess $ p str
      p s = parseString parseLog mempty s

logStr = [r|
-- wheee a comment
-- oh boy, a second comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not necessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

main :: IO ()
main = hspec $ do
  let preParse0 = "2.1.1"
      parsed0 = SemVer 2 1 1 [] []
      preParse1 = "1.0.0-x.7.z.92"
      parsed1 = SemVer 1 0 0
                       [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []
      order0 = SemVer 1 0 0 [] []
      order1 = SemVer 1 0 0 [NOSS "rc", NOSI 1] []
      order2 = SemVer 1 0 0 [NOSS "beta", NOSI 11] []

      preParsePhone0 = "123-456-7890"
      preParsePhone1 = "1234567890"
      preParsePhone2 = "(123) 456-7890"
      preParsePhone3 = "1-123-456-7890"
      expectedPhoneNum = PhoneNumber 123 456 7890

  describe "Parsing" $
    it "Parses with no release or metadata" $ do
      let parsed = parseString parseSemVer mempty preParse0
          res = maybeSuccess parsed
      res `shouldBe` Just parsed0

  -- why am I getting an error when I try to have two `it`
  -- blocks inside of one `describe`?
  describe "Parsing, cont" $
    it "Parses with release" $ do
      let parsed = parseString parseSemVer mempty preParse1
          res = maybeSuccess parsed
      res `shouldBe` Just parsed1

  describe "Ordering" $
    it "Considers empty release field greater" $ do
      compare order0 order1 `shouldBe` GT

  describe "Ordering, cont" $
    it "Uses alphabetical order on release strings" $ do
      compare order1 order2 `shouldBe` GT

  describe "Phone numbers 0" $
    it "Parses a simple hyphenated number" $ do
      let parsed = parseString parsePhone mempty
                   preParsePhone0
          res = maybeSuccess parsed
      res `shouldBe` Just expectedPhoneNum

  describe "Phone numbers 1" $
    it "Parses an integer" $ do
      let parsed = parseString parsePhone mempty
                   preParsePhone1
          res = maybeSuccess parsed
      res `shouldBe` Just expectedPhoneNum

  describe "Phone numbers 2" $
    it "Parses a number with parentheses around area code" $ do
      let parsed = parseString parsePhone mempty
                   preParsePhone2
          res = maybeSuccess parsed
      res `shouldBe` Just expectedPhoneNum

  describe "Phone numbers 3" $
    it "Parses a number with country code" $ do
      let parsed = parseString parsePhone mempty
                   preParsePhone3
          res = maybeSuccess parsed
      res `shouldBe` Just expectedPhoneNum
