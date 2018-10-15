module Phone where

import Data.Char (toUpper, isUpper, toLower, isAlpha)
import Data.List (elemIndex)

data DaPhone = DaPhone [PhoneMapping]

data PhoneMapping = PhoneMapping InputKey Output

newtype InputKey = InputKey Char
                   deriving (Eq, Show)

newtype Output = Output [OutputItem]

data OutputItem = Key Char
                | NextCharTrans Transformation

data Transformation = ToUpper
                    | ID
                    deriving (Eq, Show)

instance Eq OutputItem where
    (==) (Key x) (Key y) = x == y
    (==) (NextCharTrans x) (NextCharTrans y) = x == y
    (==) _ _ = False

phoneMappings :: DaPhone
phoneMappings = DaPhone
  [ PhoneMapping (InputKey '1') (Output [Key '1'])
  , PhoneMapping (InputKey '2') (Output [Key 'a', Key 'b', Key 'c', Key '2'])
  , PhoneMapping (InputKey '3') (Output [Key 'd', Key 'e', Key 'f', Key '3'])
  , PhoneMapping (InputKey '4') (Output [Key 'g', Key 'h', Key 'i', Key '4'])
  , PhoneMapping (InputKey '5') (Output [Key 'j', Key 'k', Key 'l', Key '5'])
  , PhoneMapping (InputKey '6') (Output [Key 'm', Key 'n', Key 'o', Key '6'])
  , PhoneMapping (InputKey '7') (Output [Key 'p', Key 'q', Key 'r', Key 's', 
                                         Key '7'])
  , PhoneMapping (InputKey '8') (Output [Key 't', Key 'u', Key 'v', Key '8'])
  , PhoneMapping (InputKey '9') (Output [Key 'w', Key 'x', Key 'y', Key 'z', 
                                         Key '9'])
  , PhoneMapping (InputKey '*') (Output [NextCharTrans ToUpper, 
                                         NextCharTrans ID,
                                         Key '*'])
  , PhoneMapping (InputKey '0') (Output [Key ' ', Key '0'])
  , PhoneMapping (InputKey '#') (Output [Key '.', Key ',', Key '#'])
  ]

convo :: [String]
convo = 
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]

type Presses = Int

reverseTaps :: DaPhone -> Char -> [(InputKey, Presses)]
reverseTaps mapping c
    | isUpper c = lookUp mapping (NextCharTrans ToUpper) : 
                  lookUp mapping (Key $ toLower c) : []
    | otherwise = lookUp mapping (Key c) : []
    where lookUp :: DaPhone -> OutputItem -> (InputKey, Presses)
          lookUp (DaPhone ((PhoneMapping inputKey (Output os)):ms)) outputKey =
              case index of
                  Just i -> (inputKey, (i + 1) :: Presses)
                  Nothing -> lookUp (DaPhone ms) outputKey
              where index = elemIndex outputKey os

reverseTapsMsg :: DaPhone -> String -> [(InputKey, Presses)]
reverseTapsMsg mapping s = concat $ map (reverseTaps mapping) s

convertedConvo :: [[(InputKey, Presses)]]
convertedConvo = map (reverseTapsMsg phoneMappings) convo

fingerTaps :: [(InputKey, Presses)] -> Presses
fingerTaps keyPresses = foldr (+) 0 $ extractPresses keyPresses
    where extractPresses = map snd

tapsByMessage :: [Presses]
tapsByMessage = map fingerTaps convertedConvo

-- I would ordinarily do this using key-value pairs, but we haven't learned
-- about those yet, so I'm going to do it in a much less efficient way using
-- a list of tuples
mostPopularItem :: Eq a => (a -> Bool) -> (a -> a) -> [a] -> a
mostPopularItem filt trans xs = fst (mostPopWithCount filt trans xs)

mostPopWithCount :: Eq a => (a -> Bool) -> (a -> a) -> [a] -> (a, Int)
mostPopWithCount filt trans xs = getMostPop (itemCounts filt trans xs [])
    where itemCounts :: Eq a => (a -> Bool) -> (a -> a) -> 
                                [a] -> [(a, Int)] -> [(a, Int)]
          itemCounts _ _ [] acc = acc
          itemCounts f g (x:xs) acc
              | not (f x) = itemCounts f g xs acc
              | otherwise =
                  case elemIndex transX (map fst acc) of
                      Just i -> itemCounts f g xs 
                                (before ++ [(transX, num + 1)] ++ after)
                          where before = take i acc
                                num = snd (acc !! i)
                                after = drop (i + 1) acc
                      Nothing -> itemCounts f g xs ((transX, 1) : acc)
                  where transX = g x
          getMostPop :: [(a, Int)] -> (a, Int)
          getMostPop xs = foldr1 findMax xs
              where findMax :: (a, Int) -> (a, Int) -> (a, Int)
                    findMax xs@(x1, x2) ys@(y1, y2) =
                        case x2 >= y2 of
                            True -> xs
                            False -> ys

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopularItem isAlpha toLower

tapsForMostPopularLetter :: String -> Presses
tapsForMostPopularLetter s = tapsForLetter (mostPopularLetter s)

tapsForLetter :: Char -> Presses
tapsForLetter c = sum $ map snd (reverseTaps phoneMappings c)

coolestLtr :: [String] -> Char
coolestLtr s = mostPopularLetter (concat s)

coolestWord :: [String] -> String
coolestWord ss = mostPopularOf theWords
    where theWords = concat (map words ss)
          mostPopularOf = mostPopularItem (\x -> True) transF
          transF = (map toLower)

coolestWordWithCount :: [String] -> (String, Int)
coolestWordWithCount ss = mostPopularOf theWords
    where theWords = concat (map words ss)
          mostPopularOf = mostPopWithCount (\x -> True) transF
          transF = (map toLower)
          --transF = id
