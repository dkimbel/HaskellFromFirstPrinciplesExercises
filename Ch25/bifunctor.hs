{-# LANGUAGE InstanceSigs #-}

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  
  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b
  deriving Show

instance Bifunctor Deux where
  first :: (a -> b)
        -> Deux a c
        -> Deux b c
  first aB (Deux a c) = Deux (aB a) c

  second :: (b -> c)
         -> Deux a b
         -> Deux a c
  second bC (Deux a b) = Deux a (bC b)

data Const a b = Const a
  deriving Show

instance Bifunctor Const where
  first aB (Const a) = Const (aB a)
  second _ (Const a) = Const a

data Drei a b c = Drei a b c
  deriving Show

instance Bifunctor (Drei a) where
  bimap bD cE (Drei a b c) = Drei a (bD b) (cE c)

data SuperDrei a b c = SuperDrei a b
  deriving Show

instance Bifunctor (SuperDrei a) where
  bimap bD _ (SuperDrei a b) = SuperDrei a (bD b)

data SemiDrei a b c = SemiDrei a
  deriving Show

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
  deriving Show

instance Bifunctor (Quadriceps a b) where
  bimap cE dF (Quadzzz a b c d) = Quadzzz a b (cE c) (dF d)

data Either' a b =
    Left' a
  | Right' b
  deriving Show

instance Bifunctor Either' where
  bimap aC _ (Left' a) = Left' (aC a)
  bimap _ (bD) (Right' b) = Right' (bD b)
