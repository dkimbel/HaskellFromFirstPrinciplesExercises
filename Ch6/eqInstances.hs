module EqInstances where

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x1 x2) (Two y1 y2) = 
    (x1 == y1) && (x2 == y2)

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x1) (TisAnInt x2) = x1 == x2
  (==) (TisAString s1) (TisAString s2) = s1 == s2
  (==) _ _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x1 x2) (Pair y1 y2) = x1 == y1 && x2 == y2

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x1 x2) (Tuple y1 y2) = 
    x1 == y1 && x2 == y2

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _ = False
