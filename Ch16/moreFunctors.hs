data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

-- I was not able to write this myself, and needed it
-- from the book. One thing I tripped up on was trying
-- to apply two arguments to the data constructor; while
-- the type constructor takes two arguments, the data
-- constructor only takes one. Furthermore, I was thinking
-- of the `f` in the data declaration as a function rather 
-- than as a functor.
instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

getInt :: IO Int
getInt = fmap read getLine

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ " and me too!")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)
