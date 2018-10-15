module EitherMonad where

type Founded = Int
type Coders = Int

data SoftwareShop = 
  Shop {
      founded     :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0     = Left $ NegativeYears n
    | n > 500   = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0     = Left $ NegativeCoders n
    | n > 5000  = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders
  -- this is drawn directly from the book, but I suspect
  -- it's a mistake -- I think they meant
  -- `if founded > div programmers 10`, considering that 
  -- the separate validation functions above allow
  -- programmers to be roughly 10x the `founded` value.
  -- That'd also be much more realistic.
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers
