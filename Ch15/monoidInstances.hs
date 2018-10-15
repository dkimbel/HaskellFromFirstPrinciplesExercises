module MonoidInstances where

import Data.Monoid

data Booly a =
    False'
  | True'
  deriving (Eq, Show)

{- The point the book is making here is that the compiler
   doesn't require that type `a` already has a Monoid
   instance, because it knows that `a` is never used
   as an arg to the type's data constructors. Thus, we
   never call mappend or mempty on these `a` values. Contrast
   this with `Maybe`, which does have that constraint. -}
instance Monoid (Booly a) where
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'
  mempty = True'

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a1) (Only a2) = Only (mappend a1 a2)
  mappend Nada      (Only a2) = Only (mappend mempty a2)
  mappend (Only a1) Nada      = Only (mappend a1 mempty)
  mappend _ _ = mempty
