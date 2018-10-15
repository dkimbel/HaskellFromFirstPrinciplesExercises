import Data.Monoid
--import Test.QuickCheck

asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = 
  a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  a <> (b <> c) == (a <> b) <> c

-- for brevity
type S = String
type B = Bool

--quickCheck (monoidAssoc :: S -> S -> S -> B)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a <> mempty == a
