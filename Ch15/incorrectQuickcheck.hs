import Control.Monad
import Data.Monoid
--import Test.QuickCheck

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

monoidAssoc :: Bull -> Bull -> Bull -> Bool
monoidAssoc a b c =
  a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: Bull -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: Bull -> Bool
monoidRightIdentity a = a <> mempty == a

--main :: IO ()
--main = do
--  quickCheck (monoidAssoc :: BullMappend)
--  quickCheck (monoidLeftIdentity :: Bull -> Bool)
--  quickCheck (monoidRightIdentity :: Bull -> Gool)
