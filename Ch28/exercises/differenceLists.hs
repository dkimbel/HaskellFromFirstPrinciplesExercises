module Main where

import Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ \_ -> []
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL $ \_ -> [x]
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList dl = unDL dl []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
-- the book provided this implementation
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
-- same as the book's definition of `cons`
-- also could have just said `snoc = flip cons`
snoc xs x = DL ((x:) . unDL xs)
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL ys . unDL xs)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs =
          go (n-1)
          (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $
    whnf schlemiel 123456
  , bench "concat dlist" $
    whnf constructDlist 123456
  ]
