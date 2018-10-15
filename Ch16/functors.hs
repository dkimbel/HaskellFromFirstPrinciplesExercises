data FixMePls a =
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap f (Matter a) = Matter (f a)
  fmap _ WhatThisIsCalled = WhatThisIsCalled

-- The below implementation is law-breaking, and an example
-- of what NEVER to do
--instance Functor WhoCares where
--  fmap _ ItDoesnt = WhatThisIsCalled
--  fmap _ WhatThisIsCalled  = ItDoesnt
--  fmap f (Matter a) = Matter (f a)

data CountingBad a =
    Heisenberg Int a
    deriving (Eq, Show)

-- Another broken, bad implementation
--instance Functor CountingBad where
--  fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg n (f a)

{- NOTES
   The type (a -> b) can refer to any function in Haskell.
   Even, say, the function composition operator (.):
   (y -> z) -> (x -> y) -> x -> z
   In that case, (y -> z) is `a` and ((x -> y) -> (x -> z))
   is `b`.
   
   At the book's suggestion, writing out how it's possible
   that (fmap . fmap) can typecheck:
   (.) :: (b -> c) -> (a -> b) -> a -> c
   fmap :: Functor f => (m -> n) -> f m -> f n
   fmap :: Functor g => (x -> y) -> g x -> g y

   Following the book's advice and substituting in
   the fmap type signatures in their places in the (.)
   operator's signature, we'd use the following rules:
   (a -> b) becomes ((m -> n) -> (f m -> f n))
   a becomes (m -> n)
   (b -> c) becomes ((x -> y) -> (g x -> g y))
   c becomes (g x -> g y)

   And that would give us:
   (Functor f, Functor g) =>
   ((x -> y) -> (g x -> g y)) -> ((m -> n) -> (f m -> f n))
   -> (m -> n) -> (g x -> g y)


   The first two arguments have actually already been applied
   to (.) -- the two fmap functions -- so we're only left 
   now with this last part:
   (m -> n) -> (g x -> g y)

   That first (m -> n) argument will serve as the input to
   the ((m -> n) -> (f m -> f n)) fmap, resulting in an output
   of (f m -> f n). That output will serve as an input to the
   ((x -> y) -> (g x -> g y)) fmap, which results in an output
   of type (g (f m) -> g (f n)) after substituting `f m` for
   `x` and `f n` for `y`. And indeed, all we really needed
   to know was what x and y were, so we can substitute them
   back into:
   (m -> n) -> (g x -> g y)

   To get:
   (m -> n) -> (g (f m) -> g (f n))

   Of course, that's with the constraint that f and g have
   Functor typeclass instances, so it's really:
   (Functor f, Functor g) => (m -> n) -> (g (f m) -> g (f n))

   So as we'd expect, we provide an (m -> n) function that
   will be mapped over something, then we provide the input
   nested functors `g (f m)`, and the ultimate output
   is a now-mapped nested functor `g (f n)`.
 -}
