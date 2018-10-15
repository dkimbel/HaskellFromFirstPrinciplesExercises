module Exercises where

import Data.Monoid as M
import Data.Semigroup as S
import Test.QuickCheck as QC
import Data.List.NonEmpty

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c = 
  (a S.<> b) S.<> c == a S.<> (b S.<> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty M.<> a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a M.<> mempty == a

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (S.<>)

instance QC.Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity i0) <> (Identity i1) = Identity (i0 S.<> i1)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  (Identity i0) `mappend` (Identity i1) = Identity (i0 M.<> i1)

instance QC.Arbitrary a => QC.Arbitrary (Identity a) where
  arbitrary = identityGen

identityGen :: QC.Arbitrary a => Gen (Identity a)
identityGen = do
  a <- QC.arbitrary
  return (Identity a)

type IdentityAssoc a = Identity a -> Identity a ->
                       Identity a -> Bool

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
         Semigroup (Two a b) where
  (Two a0 b0) <> (Two a1 b1) =
    Two (a0 S.<> a1) (b0 S.<> b1)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  (Two a0 b0) `mappend` (Two a1 b1) =
    Two (a0 M.<> a1) (b0 M.<> b1)

instance (QC.Arbitrary a, QC.Arbitrary b) =>
         QC.Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (QC.Arbitrary a, QC.Arbitrary b) 
       => Gen (Two a b)
twoGen = do
  a <- QC.arbitrary
  b <- QC.arbitrary
  return (Two a b)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => 
         Semigroup (Three a b c) where
  (Three a0 b0 c0) <> (Three a1 b1 c1) =
    Three (a0 S.<> a1) (b0 S.<> b1) (c0 S.<> c1)

instance (QC.Arbitrary a, QC.Arbitrary b, QC.Arbitrary c) =>
         QC.Arbitrary (Three a b c) where
  arbitrary = threeGen

threeGen :: (QC.Arbitrary a, QC.Arbitrary b, QC.Arbitrary c)
         => Gen (Three a b c)
threeGen = do
  a <- QC.arbitrary
  b <- QC.arbitrary
  c <- QC.arbitrary
  return (Three a b c)

type ThreeAssoc a b c = Three a b c -> Three a b c ->
                        Three a b c -> Bool

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four a0 b0 c0 d0) <> (Four a1 b1 c1 d1) =
    Four (a0 S.<> a1) (b0 S.<> b1) (c0 S.<> c1) (d0 S.<> d1)

instance (QC.Arbitrary a, QC.Arbitrary b, QC.Arbitrary c, 
         QC.Arbitrary d) => QC.Arbitrary (Four a b c d) where
  arbitrary = fourGen

fourGen :: (QC.Arbitrary a, QC.Arbitrary b, QC.Arbitrary c, 
           QC.Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- QC.arbitrary
  b <- QC.arbitrary
  c <- QC.arbitrary
  d <- QC.arbitrary
  return (Four a b c d)

type FourAssoc a b c d = Four a b c d -> Four a b c d ->
                         Four a b c d -> Bool

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (S.<>)

instance QC.Arbitrary BoolConj where
  arbitrary = boolConjGen

boolConjGen :: Gen BoolConj
boolConjGen = do
  a <- QC.arbitrary
  return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (S.<>)

instance QC.Arbitrary BoolDisj where
  arbitrary = boolDisjGen

boolDisjGen :: Gen BoolDisj
boolDisjGen = do
  a <- QC.arbitrary
  return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  sec@(Snd _) <> _ = sec
  _ <> sec@(Snd _) = sec
  (Fst _) <> fir1@(Fst _) = fir1

instance (QC.Arbitrary a, QC.Arbitrary b) => 
         QC.Arbitrary (Or a b) where 
  arbitrary = orGen

orGen :: (QC.Arbitrary a, QC.Arbitrary b) => Gen (Or a b)
orGen = do
  a <- QC.arbitrary
  b <- QC.arbitrary
  QC.elements [ Fst a
           , Snd b]
  
type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

-- Both of the below implementations work. I'm honestly
-- surprised that the first did work (maybe it's just
-- pointfree?), but it's more terse so it's the one I'm 
-- sticking with.
instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f S.<> g)
--  (Combine f) <> (Combine g) =
--    Combine (\x -> (f x) S.<> (g x))

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)
  (Combine f) `mappend` (Combine g) = Combine (f M.<> g)

-- Note that I'm not familiar with how to use CoArbitrary
-- to generate functions for QuickCheck, and am not going
-- to dive in now.

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id
  (Comp f) `mappend` (Comp g) = Comp (f . g)

data Validation a b =
  Fail a | Succ b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Fail a0) <> (Fail a1) = Fail (a0 S.<> a1)
  failure@(Fail _) <> _ = failure
  _ <> failure@(Fail _) = failure
  first@(Succ _) <> _ = first

instance (QC.Arbitrary a, QC.Arbitrary b) =>
         QC.Arbitrary (Validation a b) where
  arbitrary = validationGen

validationGen :: (QC.Arbitrary a, QC.Arbitrary b)
              => Gen (Validation a b)
validationGen = do
  a <- QC.arbitrary
  b <- QC.arbitrary
  QC.frequency [ (1, return $ Succ a)
               , (3, return $ Fail b)]

type ValidationAssoc a b = Validation a b -> Validation a b ->
                           Validation a b -> Bool

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Succ s0)) <> (AccumulateRight (Succ s1)) = 
    AccumulateRight (Succ (s0 S.<> s1))
  success@(AccumulateRight (Succ _)) <> _ = success
  _ <> success@(AccumulateRight (Succ _)) = success
  first@(AccumulateRight (Fail _)) <> _ = first

instance (QC.Arbitrary a, QC.Arbitrary b) =>
         QC.Arbitrary (AccumulateRight a b) where
  arbitrary = accumulateRightGen

accumulateRightGen :: (QC.Arbitrary a, QC.Arbitrary b)
                   => Gen (AccumulateRight a b)
accumulateRightGen = do
  a <- QC.arbitrary
  b <- QC.arbitrary
  QC.frequency [ (1, return $ AccumulateRight (Fail a))
               , (3, return $ AccumulateRight (Succ b))]

type AccumulateRightAssoc a b = AccumulateRight a b ->
                                AccumulateRight a b ->
                                AccumulateRight a b ->
                                Bool

newtype AccumulateBoth a b = 
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
         Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Succ s0)) <> (AccumulateBoth (Succ s1)) =
    AccumulateBoth (Succ (s0 S.<> s1))
  (AccumulateBoth (Fail f0)) <> (AccumulateBoth (Fail f1)) =
    AccumulateBoth (Fail (f0 S.<> f1))
  success@(AccumulateBoth (Succ _)) <> _ = success
  _ <> success@(AccumulateBoth (Succ _)) = success

instance (QC.Arbitrary a, QC.Arbitrary b) =>
         QC.Arbitrary (AccumulateBoth a b) where
  arbitrary = accumulateBothGen

accumulateBothGen :: (QC.Arbitrary a, QC.Arbitrary b)
                  => Gen (AccumulateBoth a b)
accumulateBothGen = do
  a <- QC.arbitrary
  b <- QC.arbitrary
  QC.elements [ AccumulateBoth (Fail a)
              , AccumulateBoth (Succ b)]

type AccumulateBothAssoc a b = AccumulateBoth a b ->
                               AccumulateBoth a b ->
                               AccumulateBoth a b ->
                               Bool

newtype Mem s a = 
  Mem {
    runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  (Mem f) `mappend` (Mem g) =
    Mem $ \s -> let gTup = g (snd (f s)) 
                in (fst (f s) M.<> fst gTup, snd gTup)

f' :: Mem Integer String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  print $ runMem (f' M.<> mempty) 0
  print $ runMem (mempty M.<> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' M.<> mempty) 0 == runMem f' 0
  print $ runMem (mempty M.<> f') 0 == runMem f' 0
  QC.quickCheck (semigroupAssoc :: TrivialAssoc)
  QC.quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  QC.quickCheck (monoidRightIdentity :: Trivial -> Bool)

  QC.quickCheck (semigroupAssoc :: IdentityAssoc (NonEmpty Int))
  QC.quickCheck (monoidLeftIdentity :: Identity (Sum Int)
                                       -> Bool)
  QC.quickCheck (monoidRightIdentity :: Identity (Sum Int) 
                                        -> Bool)

  QC.quickCheck (semigroupAssoc :: TwoAssoc (NonEmpty Char) 
                                   Trivial)
  QC.quickCheck (monoidLeftIdentity :: Two (Sum Int) 
                                       (Product Int)
                                       -> Bool)
  QC.quickCheck (monoidRightIdentity :: Two (Sum Int) 
                                        (Product Int)
                                        -> Bool)

  QC.quickCheck (semigroupAssoc :: ThreeAssoc (NonEmpty Int) 
                                   Trivial (NonEmpty Char))
  QC.quickCheck (semigroupAssoc :: FourAssoc (NonEmpty Char)
                                   Trivial Trivial
                                   (NonEmpty Int))

  QC.quickCheck (semigroupAssoc :: BoolConjAssoc)
  QC.quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  QC.quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  QC.quickCheck (semigroupAssoc :: BoolDisjAssoc)
  QC.quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  QC.quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  QC.quickCheck (semigroupAssoc :: OrAssoc Int Bool)
  QC.quickCheck (semigroupAssoc :: ValidationAssoc 
                                   (NonEmpty Int) Bool)
  QC.quickCheck (semigroupAssoc :: AccumulateRightAssoc 
                                   Bool (NonEmpty Int))
  QC.quickCheck (semigroupAssoc :: AccumulateBothAssoc 
                                   (NonEmpty Char) 
                                   (NonEmpty Int))
