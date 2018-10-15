traverse' :: (Applicative f, Traversable t)
          => (a -> f b) -> t a -> f (t b)
traverse' f = sequenceA . fmap f

sequenceA' :: (Applicative f, Traversable t)
           => t (f a) -> f (t a)
sequenceA' = traverse id
