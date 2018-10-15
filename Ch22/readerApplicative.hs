{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA2)

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName    :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
         (DogName "Papu")
         (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- pulled in from book's earlier Functor code so that
-- I can complete exercise below
newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

-- All of the above is from the book (though I did correctly
-- guess the getDogR implementation, and also thought of
-- the getDogR' implementation). Below are my answers
-- to the book's exercises.
myLiftA2 :: Applicative f
         => (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

-- Below is from the book, using the Reader monad
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- Pulling in the Reader Monad instance I implemented in
-- readerMonad.hs so that I can do the following exercise
-- that requires the Dog code in this file
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

-- The below works, finally, but I'll try to refactor it
-- to make it less clunky.
getDogRM' :: Person -> Dog
getDogRM' = 
  runReader $
    Reader dogName >>= 
      (\name ->
        Reader address >>=
          (\address ->
            return $ Dog name address))

getDogRM'' :: Person -> Dog
getDogRM'' = runReader $ do
  name <- Reader dogName
  addr <- Reader address
  return $ Dog name addr
