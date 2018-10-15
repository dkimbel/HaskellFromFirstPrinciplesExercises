{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import GHC.Arr

-- A valid functor instance cannot be written for Bool,
-- because it has kind *, not the requried kind (* -> *)

data BoolAndSomethingElse a =
  False' a | True' a
  deriving Show

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
  deriving Show

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

-- I don't think we can write a functor instance
-- for Mu, because per ghci it has kind ((* -> *) -> *);
-- I don't think that can be manipulated to fulfill functor's
-- required kind of (* -> *). Maybe we could do something with
-- a flip newtype, though; it could essentially have a
-- kind of (* -> (* -> *)), which would become exactly what
-- functor needs once a single type argument was applied.
newtype Mu f = InF { outF :: f (Mu f) }

-- It's definitely impossible to write a functor instance for
-- D, because it has kind *
data D =
  D (Array Word Word) Int Int
  deriving Show


data Sum b a =
    First a
  | Second b
  deriving Show

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b =
    DeepBlue a c
  | Something b
  deriving Show

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving Show

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b =
  K a
  deriving Show

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

data EvilGoateeConst a b =
  GoatyConst b
  deriving Show

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a =
  LiftItOut (f a)
  deriving Show

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving Show

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = 
    IgnoringSomething fa (fmap f gb)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving Show

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a =
    Nil
  | Cons a (List a)
  deriving Show

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving Show

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gl1 gl2 gl3) =
    MoreGoats (fmap f gl1) (fmap f gl2) (fmap f gl3)

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (fmap f g)


{- NOTES
   It's only the "innermost" (rightmost) type argument to the
   type constructor (what's between `data` and `=` in a data
   declaration) that can be manipulated by fmap in a
   Functor instance.
-}
