module Exponentiation where

data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

-- Sum type has cardinality of 3 + 3;
-- `Either Quantum Quantum` can be Left Yes, Left No, Left Both,
-- Right Yes, Right No, or Right Both
--
-- Product type has cardinality of 3 * 3;
-- `(Quantum, Quantum` can be (Yes, Yes), (Yes, No),
-- (Yes, Both), etc
--
-- Function `Quantum -> Quantum` has exponential cardinality
-- of 3 ^ 3, or 27; possible implementations are:
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes

-- Those three data constructors in the pattern match will
-- always be the same: all three, Yes, No, and Both. However,
-- the possible results are three different outcomes in each
-- of those three slots, or 3 * 3 * 3, aka 3^3. Note that
-- this is b^a for the function a -> b; the cardinality of
-- the output type, raised to the power of the input type.
-- This is plenty intuitive if you think of the case where
-- the output type has a cardinality of 1; the resulting
-- cardinality must surely be 1 ^ x rather than x ^ 1.
--
-- A demonstration of the above requested by the book:
-- that Quantum -> Bool has cardinality of 2^3 or 8.
convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

-- I'm not going to write them all out.


