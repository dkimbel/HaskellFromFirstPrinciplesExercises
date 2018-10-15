module Person where

import Control.Applicative (liftA3)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a
--  case mkName n of
--    Nothing -> Nothing
--    Just n' ->
--      case mkAddress a of
--        Nothing -> Nothing
--        Just a' ->
--          Just $ Person n' a'

data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  Cow <$> noEmpty name' 
      <*> noNegative age'
      <*> noNegative weight'
--  case noEmpty name' of
--    Nothing -> Nothing
--    Just nammy ->
--      case noNegative age' of
--        Nothing -> Nothing
--        Just agey ->
--          case noNegative weight' of
--            Nothing -> Nothing
--            Just weighty ->
--              Just (Cow nammy agey weighty)

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
  liftA3 Cow (noEmpty name')
             (noNegative age')
             (noNegative weight')
