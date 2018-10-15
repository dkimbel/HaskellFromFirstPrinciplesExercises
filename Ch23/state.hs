{-# LANGUAGE InstanceSigs #-}

newtype State' s a =
  State' { runState' :: s -> (a, s) }

instance Functor (State' s) where
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f (State' g) = 
    State' $ \s -> let (ga, gs) = g s
                   in (f ga, gs)

instance Applicative (State' s) where
  pure :: a -> State' s a
  pure a = State' $ \s -> (a, s)

  (<*>) :: State' s (a -> b) -> State' s a -> State' s b
  -- I have no idea which implementation is better,
  -- the one that I've left uncommented or the one
  -- that I've commented out
  (State' f) <*> (State' g) = 
    State' $ \s -> let (ga, gs) = g s
--                       (fab, _) = f s
--                       b = fab ga
--                       st = snd $ f gs
                       (fab, st) = f gs
                       b = fab ga
                   in (b, st)

instance Monad (State' s) where
  return :: a -> State' s a
  return = pure

  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  (State' f) >>= g = 
    State' $ \s -> let (fa, fs) = f s
                       State' sBs = g fa
                   in sBs fs
