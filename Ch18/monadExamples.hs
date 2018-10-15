twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs =
  xs >>=
  \x ->
  if even x
    then [x*x, x*x]
    else [x*x]

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

weightCheck :: Cow -> Maybe Cow
weightCheck c = 
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

-- I successfully implemented this myself.
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  name <- noEmpty name'
  age <- noNegative age'
  weight <- noNegative weight'
  weightCheck $ Cow name age weight

-- This is from the book, but I could've done it by
-- desugaring my implementation above.
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \ name ->
    noNegative age' >>=
    \ age ->
      noNegative weight' >>=
      \ weight ->
        weightCheck $ Cow name age weight


--mkSphericalCow :: String -> Int -> Int -> Maybe Cow
--mkSphericalCow name' age' weight' =
--  case noEmpty name' of
--    Nothing -> Nothing
--    Just nammy ->
--      case noNegative age' of
--        Nothing -> Nothing
--        Just agey -> 
--          case noNegative weight' of
--            Nothing -> Nothing
--            Just weighty ->
--              weightCheck (Cow nammy agey weighty)

