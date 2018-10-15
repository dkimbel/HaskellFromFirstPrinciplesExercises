newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

innerMost :: [Maybe (Identity (a -> b))]
          -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second'   :: [Maybe (Identity a -> Identity b)]
          -> [Maybe (Identity a) -> Maybe (Identity b)]
second' = fmap (<*>)

final'    :: [Maybe (Identity a) -> Maybe (Identity b)]
          -> [Maybe (Identity a)] -> [Maybe (Identity b)]
final' = (<*>)

main :: IO ()
main = do
  print $ (<*>)
            (Identity (+1))
            (Identity 2)
  print $ ((<*>) . (fmap (<*>)))
            (Just (Identity (+1)))
            (Just (Identity 2))
  print $ (final' . second' . innerMost) 
            [Just (Identity (+1))] 
            [Just (Identity 2)]

{- NOTES
  Breaking down the types of (<*>) . (fmap (<*>))
  fmap  :: Functor f     =>   (a -> b) -> f a -> f b
  (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  (.)   :: (b -> c) -> (a -> b) -> a -> c
  
  fmap (<*>) :: f (g (a -> b)) -> f (g a -> g b)
  From the perspective of fmap:
  (a -> b) = (g (d -> e)) -> g d -> g e
  f a = f (g (d -> e))
  f b = f (g d -> g e)
  Because we've already applied fmap to one argument,
  our type signature no longer has the opening (a -> b)
  portion, and we're left with (f a -> f b) =
  f (g (d -> e)) -> f (g d -> g e)

  (<*>) . (fmap (<*>))
  From the perspective of (.):
  (b -> c) = h (i -> j) -> (h i -> h j)
  (a -> b) = f (g (d -> e)) -> f (g d -> g e)
  So, for a and c:
  a = f (g (d -> e))
  c = (h i -> h j)
  Equivalent values that we can determine from b:
  h = f
  i = g d
  j = g e
  Furthermore, we know that we've applied (.) to two
  arguments, and so only have (a -> c) left from the
  original type signature:
  f (g (d -> e)) -> (f (g d) -> f (g e))
-}
