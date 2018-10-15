-- from the book
ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' m m' = do
  x <- m
  x' <- m'
  return (x x')

-- desugared by me
ap'' :: (Monad m) => m (a -> b) -> m a -> m b
ap'' mf ma = do
  mf >>=
   \ f ->
    ma >>=
     \ a ->
      return (f a)
