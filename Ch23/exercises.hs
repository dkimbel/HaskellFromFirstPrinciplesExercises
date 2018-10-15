{-# LANGUAGE InstanceSigs #-}

-- copying this over from the state.hs work I did earlier
-- in the chapter for use in the chapter-closing exercises
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

  -- Originally the last line of my (>>=) implementation
  -- had been `in sBs s`, but I updated it to `in sBs fs`
  -- so that the state from the monad in the first
  -- argument is not being ignored. That allowed me to
  -- complete the final chapter exercise correctly.
  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  (State' f) >>= g = 
    State' $ \s -> let (fa, fs) = f s
                       State' sBs = g fa
                   in sBs fs

-- Chapter Exercises begin here
get :: State' s s
get = State' $ \s -> (s, s)

put :: s -> State' s ()
put s = State' $ \_ -> ((), s)

exec :: State' s a -> s -> s
exec (State' sa) s = snd $ sa s

eval :: State' s a -> s -> a
eval (State' sa) s = fst $ sa s

-- This version matches the book's suggestion of only matching
-- explicitly on a single argument. I kind of cheated,
-- though, by simply including the argument in a lambda.
eval' :: State' s a -> s -> a
eval' (State' sa) = \s -> fst $ sa s

modify :: (s -> s) -> State' s ()
modify f = State' $ \s -> ((), f s)
