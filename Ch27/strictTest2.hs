module StrictTest2 where

-- The only change from StrictTest1 is the `!` on the
-- first argument to the Cons data constructor
data List a =
    Nil
  | Cons !a (List a) 
  deriving Show

sTake :: Int -> List a -> List a
sTake n _
  | n <= 0 = Nil
sTake n Nil = Nil
sTake n (Cons x xs) =
  (Cons x (sTake (n - 1) xs))

twoEls = Cons 1 (Cons undefined Nil)
oneEl  = sTake 1 twoEls
