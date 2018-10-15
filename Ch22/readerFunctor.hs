newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

-- the above is from the book; what's below, I implemented
-- as an exercise assigned by the book
ask :: Reader a a
ask = Reader id
