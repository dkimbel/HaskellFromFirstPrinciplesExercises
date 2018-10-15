{-# LANGUAGE Strict #-}

module StrictList where

data List a =
    Nil
  | Cons a (List a)
  deriving Show

take' n _   | n <= 0 = Nil
take' _ Nil          = Nil
take' n (Cons x xs)  =
  (Cons x (take' (n-1) xs))

map' _ Nil         = Nil
map' f (Cons x xs) =
  (Cons (f x) (map' f xs))

repeat' x = xs where xs = (Cons x xs)

-- works the same as the above
--repeat'' x = Cons x (repeat'' x)

twoEls = Cons 1 (Cons undefined Nil)
oneEl  = take' 1 twoEls

threeElements = Cons 2 twoEls
oneElT = take' 1 threeElements

main = do
  print $ take' 10 $ map' (+1) (repeat' 1)
