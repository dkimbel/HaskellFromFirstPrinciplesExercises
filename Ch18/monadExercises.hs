-- All of these were to be implemented using functions
-- provided by Monad and Functor

-- I was kind of surprised that this worked at first, but
-- it makes sense when you look at the bind operator's type:
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- In this case, `a` is actually `m a`, and so `id` is
-- able to fill the (a -> m b) role by actually providing
-- (m a -> m b) -- or more precisely (m a -> m a) (the types
-- CAN be different because of the different type variables,
-- but they don't have to be, and in this specialized case
-- they aren't). So that final `m b` we return is just `m a`
-- and not the original `m m a` -- the outermost `m` that was
-- shed when `m m a` was passed to `>>=` was not added back
-- by the id function. So the types ultimately work out like:
-- m (m a) -> (m a -> m a) -> m a
j :: Monad m => m (m a) -> m a
j mma = mma >>= id
-- note that the perhaps-not-allowed implementation would
-- have been to just import `join` and say `j = join`;
-- I say "perhaps not allowed" because join would have to
-- be imported

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- This could totally be solved with `l2 = liftA2` (or
-- liftM2), but those would have to be imported.
-- Using <*> also might not be allowed since it's a method
-- from applicative, not functor or monad. So while
-- my inmplementation right below works, I'll make another
-- one that only uses functor and monad methods.
--l2 f ma mb = f <$> ma <*> mb
-- The implementation below is inspired by my solution from
-- the next function immediately below. In fact, it uses it!
-- Were that not allowed, we could always just inline the
-- code from `a`.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = a mb (fmap f ma)

--a ma mf = mf <*> ma
-- Again, not sure that using <*> was allowed, so I
-- implemented it below using bind and fmap.
-- NOTES:
-- I approached this by thinking of it as being like
-- `<*>` with the arguments flipped. I wanted to apply
-- the function held inside the second argument's monadic
-- structure to the value inside the first argument's own
-- monadic structure. Unpacking that value from the first
-- argument and passing it to a function is generally done
-- with bind (aka >>=), so that's what I used. Then, to
-- typecheck as `m (a -> b)` instead of `(a -> m b)`, I
-- I lifted the unpacked `a` value from the first argument
-- to `bind` (represented by `x` in the lambda) into that 
-- monadic function's context using fmap. Since you have to 
-- fmap a function and not a value, though, I needed to pack my
-- `x` value up in a function (call it function 1) that would
-- be applied to another function (function 2), and would
-- ultimately apply function 2 to the value held by function 1.
-- I accomplished that using sectioning and the function
-- application operator `$`: that is, `($ x)`.  That was a way 
-- to lift type `a` into the monadic context of `m (a -> b)`, 
-- returning that desired final result type of `m b`. I
-- essentially wrapped the provided function `m (a -> b)` in
-- an interface (a lambda, specifically) that is of the
-- required type (`a -> m b`), much the way that Prelude
-- provides `curry` and `uncurry` functions that basically
-- mean "provide curried interface" and "provide uncurried
-- interface", respectively.
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = ma >>= (\x -> fmap ($ x) mf)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
--meh [] _ = return []
--meh (a:as) f = f a >>= (\x -> fmap (x :) (meh as f))
-- As a challenge to myself, rewritten as a fold below
meh as f = foldr g (return []) as
  where g x rest = f x >>= (\y -> fmap (y :) rest)

flipType :: (Monad m) => [m a] -> m [a]
-- Book says to reuse meh, so I shall. I can hardly believe
-- the below works, but it does. Note that, unlike the function
-- `j` earlier, we aren't binding into the `id` function to
-- permanently strip a layer of monadic structure. Instead,
-- `id` is telling `meh` to not transform each individual
-- `m a` value from the first argument's list in any way
-- before binding it into the fmap-cons function that
-- will result in the return type of `m [a]`.
flipType mas = meh mas id
