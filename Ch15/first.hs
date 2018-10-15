-- As usual, I'm using the simple Ch14/Addition 
-- directory to do `stack ghci` and have quickcheck
-- available locally, then pasting the file's contents
-- in using the :{ :} syntax

import Data.Monoid
--import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend first@(First' (Only _)) _ = first
  mappend (First' Nada) first@(First' (Only _)) = first
  mappend _ _ = mempty

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  frequency [ (1, return (First' Nada))
            , (3, return (First' (Only a)))]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

-- the three below methods largely copied over from my 
-- incorrectQuickcheck.hs file
monoidAssoc :: FirstMappend
monoidAssoc a b c =
  a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: FstId
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: FstId
monoidRightIdentity a = a <> mempty == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
